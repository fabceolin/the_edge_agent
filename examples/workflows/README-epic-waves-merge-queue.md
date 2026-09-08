# Fila de merge do bmad-epic-waves

Relatório do desenvolvimento da fila de merge no port tea do `bmad-epic-waves`, escrito
**antes da primeira rodada real**. Serve para duas coisas: entender o que mudou sem reler
1.900 linhas de YAML, e diagnosticar rápido o que aparecer na rodada.

- Data: 2026-08-17
- Branch: `feat/bmad-tea-workflows`
- Commits: `5044f36` (modelo por story) e `429df03` (fila de merge), sobre `fb8e4af` (redes
  de segurança portadas do JS, de outra sessão)
- Testes: 83 passando (29 redes de segurança + 18 fila + 14 workflow + 22 shell provider).
  Duas falhas em `TestExistingProvidersUnchanged` são ambiente — `openai` não está no venv,
  falham em qualquer commit.

---

## 1. O problema

A integração era um lote no fim da execução. O `merge_worktrees` esperava **todas** as
worktrees da onda terminarem e mergeava num laço:

```
onda: [story A] [story B] [story C]   ← todas rodam
      ─────────── barreira ───────────
      merge A → merge B → merge C     ← só então
```

Uma story lenta segurava o merge de uma irmã já aprovada. E nada serializava duas
invocações do workflow no mesmo repo — isso o `fb8e4af` já tinha resolvido com um lock
cross-run, mas o lock não cobria merges *dentro* de uma mesma execução, porque eles não
existiam ainda.

## 2. A forma da solução

O comando de cada nó do DOT deixou de ser só o ciclo da story:

```
cd <repo> && integrate prepare … && cd <worktree> && tea run bmad-story-cycle … \
  && cd <repo> && integrate merge …
```

A story integra assim que o review dela aprova, e o próximo merge começa exatamente quando
o anterior termina. A serialização é um `flock` exclusivo no repo principal — o mesmo objeto
que os nós do workflow e as invocações irmãs disputam.

O `&&` não é só encadeamento: é a segunda defesa da invariante "nunca mergear sem review
aprovado". O `--fail-on-state` prova pelo exit code; o `&&` prova por nem chamar o merge.

## 3. Mapa de código

| Onde | O que faz |
|---|---|
| `python/src/the_edge_agent/bmad_epic_waves_lock.py` | O primitivo: `flock` exclusivo, re-entrante por processo, com timeout, guard de stash órfão e `hold`/`release` para holds que atravessam nós. O arquivo de lock mora no **git common dir** — em qualquer lugar da árvore de trabalho ele apareceria no `git status --porcelain` e dispararia o guard de "repo sujo" do próprio merge. |
| `python/src/the_edge_agent/bmad_epic_waves_integrate.py` | Os dois estágios que rodam dentro do nó de cada story: `prepare` (cria a worktree) e `merge` (mergeia, prova ancestralidade, flippa o status daquela story). Git determinístico, sem LLM. Escreve um recibo JSON por (story, estágio). |
| `bmad_epic_waves_git.py` → `acquire/heartbeat/release_main_repo_lock` | Mantêm a forma que os nós do `fb8e4af` já usavam e passaram a delegar para o `flock`. Uma fila só. |
| `bmad-epic-waves.yaml` → `build_waves` | Monta o comando encadeado, calcula `receipts_dir` e o python do venv, e **não** cria mais as worktrees adiantado. |
| `bmad-epic-waves.yaml` → `merge_worktrees` | Virou sweeper: reconhece pelo recibo o que a fila integrou e só mergeia o que sobrou. |

## 4. Ordem do lock

```
prep_lock ──► prep_reconcile ──► prep_commit ──► (solta)
                                                    │
                              run_waves: cada story pega e solta o lock no seu merge
                                                    │
integrate_lock ──► merge_worktrees ──► resolve_conflicts ──► verify_resolution
                   ──► migration_dag ──► mark_done ──► verify_epic ──► retro ──► summary (solta)
```

Durante o `run_waves` o processo pai **não** segura nada — quem pega são os processos de
story, um de cada vez. Da `integrate_lock` ao `summary` o pai segura o lock ininterrupto,
o que cobre os dois nós que são agentes LLM mexendo na árvore principal
(`resolve_conflicts` e a retro).

## 5. Recibos — o primeiro lugar para olhar

`/tmp/tea-epic-waves/receipts-<epic_key>/<story>.{prepare,merge}.json`

| `status` | Exit | Significa | O que fazer |
|---|---|---|---|
| `merged` | 0 | Integrou e flippou o status | — |
| `already_merged` | 0 | Retry de algo que já estava dentro | — |
| `merged_status_pending` | 0 | Código integrado, status **não** flipado | O `mark_done` varre no fim; não é perda |
| `prepared` | 0 | Worktree criada da ponta atual do target | — |
| `conflict` | 1 | `git merge` conflitou, abortado; branch e worktree preservadas | Ver `conflicts` no recibo; o sweeper reavalia com `classify_conflict` |
| `empty_branch` | 1 | O dev não commitou nada | Story não produziu código — olhar o log do ciclo |
| `dirty_target` | 1 | Repo principal sujo na hora do merge | Ver `notes`; alguém mexeu na árvore |
| `no_ancestry` | 1 | Merge saiu 0 mas a tip não ficou alcançável; rollback feito | Suspeito de bug — me traga o recibo inteiro |
| `wrong_branch` | 1 | HEAD do repo não está no target | Alguém trocou de branch no meio |
| `missing_branch` | 1 | `story/<key>` não existe | O ciclo morreu antes de commitar |
| `branch_not_integrated` | 1 | `prepare` recusou reciclar branch com commits não integrados | Trabalho de rodada anterior preservado; decidir o que fazer com ele |
| `worktree_failed` | 1 | `git worktree add` falhou | Ver `notes` |
| `lock_timeout` | 3 | Não conseguiu a vez na fila dentro de `lock_wait_seconds` | Operacional, não defeito do código |
| `orphan_stash` | 4 | Stash órfão no repo principal | `git stash list` **antes** de qualquer coisa |

## 6. Diagnóstico rápido

```bash
# o que a fila decidiu, story por story
cat /tmp/tea-epic-waves/receipts-<epic_key>/*.merge.json | jq -r '[.key,.status,.notes//""]|@tsv'

# quem está segurando a fila agora (pid, dono, timestamp)
cat "$(git rev-parse --git-common-dir)/bmad-epic-waves.lock"

# stash órfão — a causa nº 1 de tudo recusar de cara
git stash list

# o que realmente entrou no target
git log --oneline --merges -20
```

Uma story **sem** recibo de `merge` significa que o ciclo dela não saiu 0: o merge nem foi
chamado. O log da janela tmux é o lugar de olhar, não o recibo.

## 7. Válvulas de escape

| Flag | Default | Efeito |
|---|---|---|
| `integrate_in_flight` | `true` | `false` volta ao merge em lote no fim; a fila sai de cena inteira e o resto do workflow continua igual |
| `mark_status_in_flight` | `true` | `false` deixa todo o `sprint-status` para o `mark_done` |
| `lock_wait_seconds` | `2400` | Espera máxima na fila |
| `allow_orphan_stash` | `false` | Segue mesmo com stash órfão — ligar **só** depois de olhar o `git stash list` |

## 8. O que os testes cobrem

- Exclusão mútua real entre processos: dois merges concorrentes se intercalam nunca; o
  segundo espera o primeiro terminar (`test_second_merge_starts_only_after_the_first_one_finishes`).
- Dono morto libera a fila na hora, sem protocolo de roubo — o caso que importa, porque o
  `run_waves` mata a sessão tmux inteira no timeout.
- Dono vivo em outro processo nunca é deslocado, por mais que demore.
- A cadeia inteira, concorrente: duas stories `prepare && ciclo && merge` em paralelo, ambas
  integradas, ambas com status flipado, sem conflito no `sprint-status`.
- Conflito aborta e preserva branch/worktree; branch vazia é recusada; retry é idempotente;
  repo sujo é recusado; falha ao flipar status não desfaz o merge.
- `prepare` corta a worktree da dependência que acabou de mergear.
- Sweeper: aceita o que a fila integrou, mergeia o resto, e continua recusando branch
  ancestral sem recibo.

## 9. O que NÃO está exercitado

1. **`auto_model` × merge in-flight.** Uma story repromovida de tier e reexecutada nunca
   passou por essa combinação ponta a ponta. É o primeiro lugar onde eu olharia.
2. **`resolve_conflicts` (Nível 2) sob a fila.** O caminho existe e o lock cobre a janela,
   mas ninguém rodou o LLM de resolução com merges in-flight antes dele.
3. **Volume.** Os testes usam 2 stories. Com 10+ stories numa onda, a contenção da fila e o
   tempo de espera são desconhecidos na prática.
4. **Barreira de onda.** O `--from-dot` executa as ondas com barreira (fan-in por fase): a
   onda N+1 só começa quando toda a onda N termina. A fila elimina a espera *dentro* da
   onda, não *entre* ondas — diferente do JS, onde não há barreira nenhuma.

## 10. Decisões, e por quê

**flock em vez do lockdir.** O `fb8e4af` tinha portado o protocolo do JS: mkdir atômico,
token de dono, heartbeat, roubo quando comprovadamente parado. Toda essa maquinaria existe
porque um diretório de lock sobrevive ao processo que o criou — alguém precisa decidir
quando é seguro arrombar. O `flock` não tem essa janela: o kernel solta quando o fd fecha,
inclusive quando o processo morre. E esse caso não é hipotético aqui: o `run_waves` mata a
sessão tmux inteira no timeout. Junto com o roubo foram embora seus dois modos de falha
(dono vivo julgado parado; heartbeat que alguém esquece de chamar).

**Recibo em vez de só ancestralidade.** O sweeper precisa saber o que a fila já integrou. A
primeira versão usava "a tip é ancestral do HEAD" — e um teste existente pegou o erro: uma
branch onde o dev não commitou nada também é trivialmente ancestral. Marcaria como feita uma
story que não produziu uma linha.

**Worktree preguiçosa.** Antes, todas nasciam de uma vez, da mesma ponta do target: uma story
da onda 2 desenvolvia sobre uma base *anterior* à dependência dela. Agora nasce no início do
próprio nó. Sem isso a fila não faria sentido.

**`git commit --only`.** Vários repos aqui têm sessões concorrentes; `git add` nominal
seguido de `git commit` leva o índice inteiro, inclusive o que outra sessão stageou.

---

## O que me trazer depois da rodada

1. `cat /tmp/tea-epic-waves/receipts-<epic_key>/*.json` — todos, prepare e merge.
2. O stdout do `tea run` (o `run_waves:` com `succeeded=`/`failed=`, e as linhas do
   `merge_worktrees:`).
3. `git log --oneline --merges -20` no repo alvo.
4. Se algo travou: `cat "$(git rev-parse --git-common-dir)/bmad-epic-waves.lock"` **enquanto**
   está travado — depois do fim não diz nada.
5. Se uma story falhou: o log da janela tmux dela (o ciclo, não o merge).
