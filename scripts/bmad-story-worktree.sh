#!/usr/bin/env bash
#
# bmad-story-worktree.sh — roda UMA story BMad dentro de um git worktree isolado
# e faz o merge de volta ao branch alvo sob lock exclusivo.
#
# Existe para resolver o problema que o TEA não cobre hoje: `mode: graph` dá
# dependências mas desabilita worktree (execution/graph.py), e `mode: parallel`
# dá worktree mas dispara tudo de uma vez sem grafo. Este wrapper é chamado como
# `command=` de um nó DOT — o DOT cuida das ondas/dependências, este script cuida
# do isolamento. Nenhuma modificação no TEA é necessária.
#
# Por que worktree importa (evidência, não teoria): os retros de Epic 3/4/6 deste
# projeto registram 3 sintomas recorrentes de paralelismo no mesmo working tree —
# corrida em .env.example/sprint-status.yaml/test-summary.md, `test-summary.md`
# mudando "sob os pés" de uma sessão, e drift de contagem de testes em 3 de 5
# stories porque uma sessão paralela adicionou testes depois do dev escrever as
# notas. O terceiro é fatal para este projeto: a disciplina de baseline
# ("873 passed antes / 891 depois") é a principal evidência de não-regressão que
# o code review consome, e ela não significa nada sob concorrência no mesmo tree.
#
# USO
#   bmad-story-worktree.sh <STORY_KEY>
#
# ENV (todas opcionais exceto BMAD_REPO)
#   BMAD_REPO           repo alvo (obrigatório, path absoluto)
#   BMAD_WORKFLOW       workflow TEA por story
#                       (default: <tea>/examples/workflows/bmad-story-worktree-qa-cycle.yaml)
#   BMAD_TARGET_BRANCH  branch de integração (default: branch atual do repo)
#   BMAD_WORKTREE_BASE  onde criar worktrees (default: $BMAD_REPO/.worktrees)
#   BMAD_IMPL_DIR       dir das stories (default: lido de _bmad/bmm/config.yaml)
#   BMAD_TEA_EXEC       executável TEA (default: tea-python)
#   BMAD_KEEP_WORKTREE  1 = não remove o worktree ao final (debug)
#   BMAD_NO_MERGE       1 = roda a story mas NÃO mergeia (inspeção manual)
#   BMAD_LOCK_TIMEOUT   segundos de espera pelo lock de merge (default: 3600)
#
# EXIT CODES
#   0  story concluída, merge aplicado (ou pulado via BMAD_NO_MERGE)
#   2  erro de configuração/pré-condição (repo ausente, story ambígua, etc.)
#   3  o workflow da story falhou
#   4  merge falhou por conflito — branch preservado para resolução manual
#   5  não conseguiu adquirir o lock de merge dentro do timeout

set -euo pipefail

log()  { printf '[worktree:%s] %s\n' "${STORY_KEY:-?}" "$*" >&2; }
die()  { local code=$1; shift; log "ERRO: $*"; exit "$code"; }

# ---------------------------------------------------------------- args & env
STORY_KEY="${1:-}"
[ -n "$STORY_KEY" ] || { echo "uso: $0 <STORY_KEY>" >&2; exit 2; }

REPO="${BMAD_REPO:-}"
[ -n "$REPO" ] || die 2 "BMAD_REPO não definido (path absoluto do repo alvo)."
[ -d "$REPO/.git" ] || die 2 "BMAD_REPO=$REPO não parece um repo git."

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEA_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
WORKFLOW="${BMAD_WORKFLOW:-$TEA_ROOT/examples/workflows/bmad-story-worktree-qa-cycle.yaml}"
[ -f "$WORKFLOW" ] || die 2 "workflow não encontrado: $WORKFLOW"

TEA_EXEC="${BMAD_TEA_EXEC:-tea-python}"
command -v "$TEA_EXEC" >/dev/null 2>&1 || die 2 "executável TEA não encontrado no PATH: $TEA_EXEC"

TARGET_BRANCH="${BMAD_TARGET_BRANCH:-$(git -C "$REPO" rev-parse --abbrev-ref HEAD)}"
WORKTREE_BASE="${BMAD_WORKTREE_BASE:-$REPO/.worktrees}"
LOCK_TIMEOUT="${BMAD_LOCK_TIMEOUT:-3600}"
LOCK_FILE="$REPO/.git/bmad-merge.lock"

BRANCH="story/$STORY_KEY"
WORKTREE="$WORKTREE_BASE/$STORY_KEY"

# ------------------------------------------------- pré-condição: story única
# O guia DOT alerta que um label que não casa exatamente com o arquivo faz o
# glob falhar SILENCIOSAMENTE. Falhamos cedo e alto: exatamente 1 match ou nada.
IMPL_DIR="${BMAD_IMPL_DIR:-}"
if [ -z "$IMPL_DIR" ]; then
  IMPL_DIR="$(python3 - "$REPO" <<'PY'
import os, sys
repo = sys.argv[1]
default = "_bmad-output/implementation-artifacts"
for rel in ("_bmad/bmm/config.yaml", ".bmad/bmm/config.yaml"):
    path = os.path.join(repo, rel)
    if not os.path.exists(path):
        continue
    try:
        import yaml
        with open(path) as fh:
            cfg = yaml.safe_load(fh) or {}
        default = cfg.get("implementation_artifacts", default)
    except Exception:
        pass
    break
print(default.replace("{project-root}/", "").replace("{project-root}", "").lstrip("/"))
PY
)"
fi
IMPL_ABS="$REPO/$IMPL_DIR"
[ -d "$IMPL_ABS" ] || die 2 "diretório de stories não existe: $IMPL_ABS"

mapfile -t MATCHES < <(find "$IMPL_ABS" -maxdepth 1 -name "*${STORY_KEY}*.md" -type f | sort)
if [ "${#MATCHES[@]}" -eq 0 ]; then
  die 2 "nenhum arquivo de story casa com '*${STORY_KEY}*.md' em $IMPL_ABS.
       Rode bmad-sprint-planning + bmad-create-story antes do DOT."
elif [ "${#MATCHES[@]}" -gt 1 ]; then
  die 2 "label AMBÍGUO: '${STORY_KEY}' casa com ${#MATCHES[@]} arquivos:
       $(printf '%s ' "${MATCHES[@]}")
       Use um label mais específico no DOT (ex.: '10-1-' em vez de '10-1',
       que também casaria com 10-10-*)."
fi
STORY_FILE="${MATCHES[0]}"
STORY_REL="${STORY_FILE#$REPO/}"
log "story resolvida: $STORY_REL"

# ------------------------------------------------------------ setup worktree
cleanup() {
  local rc=$?
  if [ "${BMAD_KEEP_WORKTREE:-0}" = "1" ]; then
    log "BMAD_KEEP_WORKTREE=1 — worktree preservado em $WORKTREE"
  elif [ -d "$WORKTREE" ]; then
    log "removendo worktree $WORKTREE"
    git -C "$REPO" worktree remove --force "$WORKTREE" 2>/dev/null \
      || log "aviso: falha ao remover worktree (remova à mão: git worktree remove --force $WORKTREE)"
  fi
  # O branch NÃO é removido em falha — é onde o trabalho vive até alguém olhar.
  return $rc
}
trap cleanup EXIT

mkdir -p "$WORKTREE_BASE"
if [ -d "$WORKTREE" ]; then
  log "worktree preexistente encontrado — removendo antes de recriar"
  git -C "$REPO" worktree remove --force "$WORKTREE" 2>/dev/null || true
fi
git -C "$REPO" worktree prune

log "criando worktree $WORKTREE (branch $BRANCH a partir de $TARGET_BRANCH)"
git -C "$REPO" worktree add -B "$BRANCH" "$WORKTREE" "$TARGET_BRANCH" >&2 \
  || die 2 "falha ao criar worktree/branch"

# --------------------------------------------------------- roda o workflow
# cwd = worktree. Isso importa: llm_actions._execute_shell_provider chama
# subprocess.Popen SEM cwd=, então o `claude -p` herda o cwd deste processo.
# Rodar o TEA de dentro do worktree é o que garante que o agente trabalhe na
# árvore isolada — não é possível setar cwd por nó no YAML.
log "executando workflow (cwd=$WORKTREE)"
WORKFLOW_RC=0
(
  cd "$WORKTREE"
  BMAD_STORY_KEY="$STORY_KEY" \
  BMAD_TARGET_BRANCH="$TARGET_BRANCH" \
  "$TEA_EXEC" run "$WORKFLOW" --input "{\"arg\": \"$STORY_REL\"}"
) || WORKFLOW_RC=$?

if [ "$WORKFLOW_RC" -ne 0 ]; then
  log "workflow falhou (rc=$WORKFLOW_RC). Branch $BRANCH preservado para inspeção."
  exit 3
fi

# Sem commit no branch = a story não produziu nada. Não é sucesso silencioso.
AHEAD="$(git -C "$REPO" rev-list --count "$TARGET_BRANCH..$BRANCH" 2>/dev/null || echo 0)"
if [ "$AHEAD" -eq 0 ]; then
  log "nenhum commit novo em $BRANCH — a story não produziu mudança commitada."
  exit 3
fi
log "$AHEAD commit(s) em $BRANCH"

if [ "${BMAD_NO_MERGE:-0}" = "1" ]; then
  log "BMAD_NO_MERGE=1 — parando antes do merge. Branch: $BRANCH"
  exit 0
fi

# ------------------------------------------------------- merge sob lock
# Serializar o merge é o que elimina, por topologia, a corrida em arquivos
# compartilhados que 4 retros deste projeto documentam e que o action item
# "[Escalado 5x desde Epic 3]" pede há 5 épicos. Sem isto, N stories de uma
# mesma onda disputariam index.lock e sprint-status.yaml.
log "aguardando lock de merge (timeout ${LOCK_TIMEOUT}s)"
exec 9>"$LOCK_FILE"
if ! flock -w "$LOCK_TIMEOUT" 9; then
  log "não consegui o lock de merge em ${LOCK_TIMEOUT}s"
  exit 5
fi
log "lock adquirido — mergeando $BRANCH em $TARGET_BRANCH"

CURRENT="$(git -C "$REPO" rev-parse --abbrev-ref HEAD)"
[ "$CURRENT" = "$TARGET_BRANCH" ] \
  || die 2 "repo principal está em '$CURRENT', esperado '$TARGET_BRANCH'. Merge abortado."

if ! git -C "$REPO" merge --no-ff --no-edit "$BRANCH" >&2; then
  log "CONFLITO ao mergear $BRANCH — abortando merge e preservando o branch."
  git -C "$REPO" merge --abort 2>/dev/null || true
  exit 4
fi
log "merge aplicado"

# ------------------------------------- sprint-status: escrito aqui, não lá
# A story NÃO escreve sprint-status.yaml dentro do worktree — se escrevesse,
# toda story da onda editaria a mesma região do arquivo e o merge conflitaria
# de forma garantida. A marcação acontece aqui, uma story por vez, sob o lock,
# de forma determinística (sem LLM).
SPRINT="$IMPL_ABS/sprint-status.yaml"
if [ -f "$SPRINT" ]; then
  if python3 - "$SPRINT" "$STORY_KEY" <<'PY'
import re, sys, datetime
path, key = sys.argv[1], sys.argv[2]
with open(path, encoding="utf-8") as fh:
    text = fh.read()
pattern = re.compile(rf"^(\s+{re.escape(key)}:\s*)(\S+)\s*$", re.MULTILINE)
if not pattern.search(text):
    print(f"chave '{key}' não encontrada em sprint-status.yaml", file=sys.stderr)
    sys.exit(1)
text = pattern.sub(lambda m: f"{m.group(1)}done", text, count=1)
today = datetime.date.today().isoformat()
text = re.sub(r"^# last_updated:.*$", f"# last_updated: {today} (Story {key} — worktree merge)",
              text, count=1, flags=re.MULTILINE)
with open(path, "w", encoding="utf-8") as fh:
    fh.write(text)
PY
  then
    git -C "$REPO" add "$IMPL_DIR/sprint-status.yaml"
    git -C "$REPO" commit -m "chore(sprint): ${STORY_KEY} -> done" >&2 || true
    log "sprint-status atualizado: $STORY_KEY -> done"
  else
    log "aviso: não consegui marcar $STORY_KEY em sprint-status.yaml (siga à mão)"
  fi
fi

git -C "$REPO" branch -d "$BRANCH" >/dev/null 2>&1 || log "branch $BRANCH mantido"
log "CONCLUÍDA"
exit 0
