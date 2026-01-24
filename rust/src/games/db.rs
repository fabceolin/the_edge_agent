//! DuckDB persistence layer for game state and word embeddings.
//!
//! This module provides database operations for:
//! - Word embeddings storage and vector similarity search
//! - Game session persistence
//! - Leaderboard management
//!
//! # Example
//!
//! ```rust,ignore
//! use the_edge_agent::games::db::GameDb;
//!
//! let db = GameDb::new(":memory:")?;
//! db.init_schema()?;
//!
//! // Insert a word with embedding
//! db.insert_word("hello", &vec![0.1; 384], Some(1000))?;
//!
//! // Query words
//! let words = db.get_words_with_embeddings()?;
//! ```

use duckdb::{params, Connection, Result as DuckDbResult};
use std::path::Path;

use super::{GameRound, GameSession};

/// Database wrapper for game state and word embeddings.
///
/// Provides methods for:
/// - Schema initialization
/// - Word embedding storage and retrieval
/// - Game session CRUD
/// - Leaderboard operations
pub struct GameDb {
    conn: Connection,
}

impl GameDb {
    /// Create a new GameDb instance.
    ///
    /// # Arguments
    ///
    /// * `path` - Database path (use ":memory:" for in-memory database)
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let db = GameDb::new(":memory:")?;  // In-memory
    /// let db = GameDb::new("game.duckdb")?;  // Persistent
    /// ```
    pub fn new<P: AsRef<Path>>(path: P) -> DuckDbResult<Self> {
        let conn = Connection::open(path)?;
        Ok(Self { conn })
    }

    /// Initialize the database schema.
    ///
    /// Creates all required tables if they don't exist:
    /// - `words`: Word embeddings for similarity search
    /// - `game_sessions`: Session metadata
    /// - `answers`: Individual round details
    /// - `leaderboard`: Best scores per username
    /// - `user_word_knowledge`: Learning analytics
    /// - `user_confusions`: Wrong guess patterns
    pub fn init_schema(&self) -> DuckDbResult<()> {
        // Words table with vector embeddings
        self.conn.execute(
            r#"
            CREATE TABLE IF NOT EXISTS words (
                id VARCHAR PRIMARY KEY,
                text VARCHAR NOT NULL UNIQUE,
                embedding FLOAT[384],
                frequency INTEGER DEFAULT 0
            )
            "#,
            [],
        )?;

        // Game sessions table
        self.conn.execute(
            r#"
            CREATE TABLE IF NOT EXISTS game_sessions (
                id VARCHAR PRIMARY KEY,
                username VARCHAR NOT NULL,
                started_at TIMESTAMP DEFAULT now(),
                total_answers INTEGER DEFAULT 0,
                correct_answers INTEGER DEFAULT 0,
                sum_difficulty DOUBLE DEFAULT 0.0,
                current_difficulty DOUBLE DEFAULT 0.5,
                is_submitted BOOLEAN DEFAULT FALSE
            )
            "#,
            [],
        )?;

        // Answers table
        self.conn.execute(
            r#"
            CREATE TABLE IF NOT EXISTS answers (
                id VARCHAR PRIMARY KEY,
                session_id VARCHAR,
                phrase VARCHAR NOT NULL,
                choices JSON,
                correct_word VARCHAR,
                selected_word VARCHAR,
                is_correct BOOLEAN,
                response_time_ms INTEGER,
                difficulty DOUBLE,
                answered_at TIMESTAMP DEFAULT now()
            )
            "#,
            [],
        )?;

        // Leaderboard table
        self.conn.execute(
            r#"
            CREATE TABLE IF NOT EXISTS leaderboard (
                username VARCHAR PRIMARY KEY,
                score DOUBLE NOT NULL,
                accuracy DOUBLE NOT NULL,
                total_answers INTEGER NOT NULL,
                avg_difficulty DOUBLE NOT NULL,
                submitted_at TIMESTAMP DEFAULT now()
            )
            "#,
            [],
        )?;

        // User word knowledge graph edge table
        self.conn.execute(
            r#"
            CREATE TABLE IF NOT EXISTS user_word_knowledge (
                session_id VARCHAR,
                word_id VARCHAR,
                times_seen INTEGER DEFAULT 0,
                times_correct INTEGER DEFAULT 0,
                times_as_llm INTEGER DEFAULT 0,
                times_guessed INTEGER DEFAULT 0,
                last_seen TIMESTAMP,
                PRIMARY KEY (session_id, word_id)
            )
            "#,
            [],
        )?;

        // User confusion patterns graph edge table
        self.conn.execute(
            r#"
            CREATE TABLE IF NOT EXISTS user_confusions (
                session_id VARCHAR,
                correct_word_id VARCHAR,
                confused_word_id VARCHAR,
                times INTEGER DEFAULT 1,
                PRIMARY KEY (session_id, correct_word_id, confused_word_id)
            )
            "#,
            [],
        )?;

        Ok(())
    }

    /// Load VSS extension for vector similarity search.
    ///
    /// This must be called after `init_schema()` to enable
    /// `array_cosine_similarity` function.
    pub fn load_vss_extension(&self) -> DuckDbResult<()> {
        self.conn.execute("INSTALL vss", [])?;
        self.conn.execute("LOAD vss", [])?;
        Ok(())
    }

    /// Load DuckPGQ extension for graph queries.
    ///
    /// This enables graph pattern matching queries on the
    /// `user_word_knowledge` and `user_confusions` tables.
    pub fn load_duckpgq_extension(&self) -> DuckDbResult<()> {
        self.conn.execute("INSTALL duckpgq FROM community", [])?;
        self.conn.execute("LOAD duckpgq", [])?;
        Ok(())
    }

    /// Load all extensions (VSS and DuckPGQ).
    ///
    /// Convenience method that loads both extensions.
    /// Failures are handled gracefully - if an extension fails to load,
    /// the error is returned but the database remains usable for
    /// operations that don't require that extension.
    pub fn load_extensions(&self) -> DuckDbResult<()> {
        self.load_vss_extension()?;
        self.load_duckpgq_extension()?;
        Ok(())
    }

    /// Insert a word with its embedding into the words table.
    ///
    /// # Arguments
    ///
    /// * `text` - The word text (lowercase)
    /// * `embedding` - 384-dimensional embedding vector
    /// * `frequency` - Optional word frequency (defaults to 0)
    pub fn insert_word(
        &self,
        text: &str,
        embedding: &[f32],
        frequency: Option<i64>,
    ) -> DuckDbResult<()> {
        let id = uuid::Uuid::new_v4().to_string();
        let freq = frequency.unwrap_or(0);

        // Convert embedding to DuckDB array literal
        let embedding_str = format!(
            "[{}]",
            embedding
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );

        self.conn.execute(
            &format!(
                "INSERT INTO words (id, text, embedding, frequency) VALUES (?, ?, {}::FLOAT[384], ?)",
                embedding_str
            ),
            params![id, text, freq],
        )?;

        Ok(())
    }

    /// Insert or update a word (upsert).
    pub fn upsert_word(
        &self,
        text: &str,
        embedding: &[f32],
        frequency: Option<i64>,
    ) -> DuckDbResult<()> {
        let id = uuid::Uuid::new_v4().to_string();
        let freq = frequency.unwrap_or(0);

        let embedding_str = format!(
            "[{}]",
            embedding
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );

        self.conn.execute(
            &format!(
                r#"
                INSERT INTO words (id, text, embedding, frequency) VALUES (?, ?, {}::FLOAT[384], ?)
                ON CONFLICT (text) DO UPDATE SET
                    embedding = EXCLUDED.embedding,
                    frequency = EXCLUDED.frequency
                "#,
                embedding_str
            ),
            params![id, text, freq],
        )?;

        Ok(())
    }

    /// Get the embedding for a word.
    ///
    /// Returns `None` if the word is not in the vocabulary.
    pub fn get_word_embedding(&self, text: &str) -> DuckDbResult<Option<Vec<f32>>> {
        let mut stmt = self
            .conn
            .prepare("SELECT embedding::VARCHAR FROM words WHERE text = ?")?;

        let mut rows = stmt.query(params![text])?;

        if let Some(row) = rows.next()? {
            // DuckDB returns arrays as string like "[0.1, 0.2, ...]"
            // We need to parse it back to Vec<f32>
            let embedding_str: String = row.get(0)?;
            let embedding = parse_embedding_string(&embedding_str);
            Ok(Some(embedding))
        } else {
            Ok(None)
        }
    }

    /// Get the word ID for a given text.
    pub fn get_word_id(&self, text: &str) -> DuckDbResult<Option<String>> {
        let mut stmt = self.conn.prepare("SELECT id FROM words WHERE text = ?")?;
        let mut rows = stmt.query(params![text])?;

        if let Some(row) = rows.next()? {
            let id: String = row.get(0)?;
            Ok(Some(id))
        } else {
            Ok(None)
        }
    }

    /// Check if a word exists in the vocabulary.
    pub fn word_exists(&self, text: &str) -> DuckDbResult<bool> {
        let mut stmt = self
            .conn
            .prepare("SELECT 1 FROM words WHERE text = ? LIMIT 1")?;
        let mut rows = stmt.query(params![text])?;
        Ok(rows.next()?.is_some())
    }

    /// Get the total number of words in the vocabulary.
    pub fn word_count(&self) -> DuckDbResult<usize> {
        let mut stmt = self.conn.prepare("SELECT COUNT(*) FROM words")?;
        let mut rows = stmt.query([])?;
        if let Some(row) = rows.next()? {
            let count: i64 = row.get(0)?;
            Ok(count as usize)
        } else {
            Ok(0)
        }
    }

    /// Find similar words using cosine similarity.
    ///
    /// # Arguments
    ///
    /// * `word` - The target word
    /// * `count` - Maximum number of similar words to return
    /// * `min_similarity` - Minimum cosine similarity (0.0 to 1.0)
    /// * `max_similarity` - Maximum cosine similarity (0.0 to 1.0)
    ///
    /// # Returns
    ///
    /// Vector of (word_text, similarity_score) tuples, randomly ordered
    /// among qualifying words.
    pub fn find_similar_words(
        &self,
        word: &str,
        count: usize,
        min_similarity: f64,
        max_similarity: f64,
    ) -> DuckDbResult<Vec<(String, f64)>> {
        // First get the embedding for the target word
        let target_embedding = match self.get_word_embedding(word)? {
            Some(emb) => emb,
            None => return Ok(vec![]),
        };

        // Build embedding literal for query
        let embedding_str = format!(
            "[{}]",
            target_embedding
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );

        // Query for similar words
        let query = format!(
            r#"
            SELECT text, array_cosine_similarity(embedding, {}::FLOAT[384]) AS similarity
            FROM words
            WHERE text != ?
              AND array_cosine_similarity(embedding, {}::FLOAT[384]) BETWEEN ? AND ?
            ORDER BY RANDOM()
            LIMIT ?
            "#,
            embedding_str, embedding_str
        );

        let mut stmt = self.conn.prepare(&query)?;
        let mut rows = stmt.query(params![word, min_similarity, max_similarity, count as i64])?;

        let mut results = Vec::new();
        while let Some(row) = rows.next()? {
            let text: String = row.get(0)?;
            let similarity: f64 = row.get(1)?;
            results.push((text, similarity));
        }

        Ok(results)
    }

    /// Find similar words by directly providing an embedding vector.
    ///
    /// Use this when the target word is not in the vocabulary but you have
    /// an embedding from another source (e.g., LLM-generated).
    pub fn find_similar_words_by_embedding(
        &self,
        embedding: &[f32],
        exclude_word: Option<&str>,
        count: usize,
        min_similarity: f64,
        max_similarity: f64,
    ) -> DuckDbResult<Vec<(String, f64)>> {
        let embedding_str = format!(
            "[{}]",
            embedding
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );

        let where_clause = match exclude_word {
            Some(word) => format!("WHERE text != '{}'", word.replace('\'', "''")),
            None => String::new(),
        };

        let query = format!(
            r#"
            SELECT text, array_cosine_similarity(embedding, {}::FLOAT[384]) AS similarity
            FROM words
            {}
            {}
            ORDER BY RANDOM()
            LIMIT ?
            "#,
            embedding_str,
            where_clause,
            if where_clause.is_empty() {
                format!(
                    "WHERE array_cosine_similarity(embedding, {}::FLOAT[384]) BETWEEN ? AND ?",
                    embedding_str
                )
            } else {
                format!(
                    "AND array_cosine_similarity(embedding, {}::FLOAT[384]) BETWEEN ? AND ?",
                    embedding_str
                )
            }
        );

        let mut stmt = self.conn.prepare(&query)?;
        let mut rows = stmt.query(params![min_similarity, max_similarity, count as i64])?;

        let mut results = Vec::new();
        while let Some(row) = rows.next()? {
            let text: String = row.get(0)?;
            let similarity: f64 = row.get(1)?;
            results.push((text, similarity));
        }

        Ok(results)
    }

    /// Get all words (for testing/debugging).
    pub fn get_all_words(&self) -> DuckDbResult<Vec<String>> {
        let mut stmt = self.conn.prepare("SELECT text FROM words ORDER BY text")?;
        let mut rows = stmt.query([])?;

        let mut words = Vec::new();
        while let Some(row) = rows.next()? {
            words.push(row.get(0)?);
        }

        Ok(words)
    }

    /// Bulk insert words from a vector of (text, embedding, frequency) tuples.
    ///
    /// This is more efficient than individual inserts for loading large vocabularies.
    pub fn bulk_insert_words(&self, words: &[(String, Vec<f32>, i64)]) -> DuckDbResult<usize> {
        let mut count = 0;

        for (text, embedding, frequency) in words {
            self.upsert_word(text, embedding, Some(*frequency))?;
            count += 1;
        }

        Ok(count)
    }

    /// Load words from a Parquet file into the words table.
    ///
    /// The Parquet file should have the schema:
    /// - id: string (UUID)
    /// - text: string (lowercase word)
    /// - embedding: list<float32>[384]
    /// - frequency: int64 (optional)
    ///
    /// # Arguments
    ///
    /// * `parquet_path` - Path to the Parquet file
    ///
    /// # Returns
    ///
    /// Number of words loaded
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let db = GameDb::new(":memory:")?;
    /// db.init_schema()?;
    /// let count = db.load_words_from_parquet("data/words.parquet")?;
    /// println!("Loaded {} words", count);
    /// ```
    pub fn load_words_from_parquet<P: AsRef<std::path::Path>>(
        &self,
        parquet_path: P,
    ) -> DuckDbResult<usize> {
        let path = parquet_path.as_ref();
        let path_str = path.to_string_lossy();

        log::info!("Loading words from Parquet file: {}", path_str);

        // DuckDB can read Parquet files directly
        // Insert from Parquet with ON CONFLICT for deduplication
        let query = format!(
            r#"
            INSERT INTO words (id, text, embedding, frequency)
            SELECT
                id,
                text,
                embedding::FLOAT[384],
                COALESCE(frequency, 0) as frequency
            FROM read_parquet('{}')
            ON CONFLICT (text) DO UPDATE SET
                embedding = EXCLUDED.embedding,
                frequency = EXCLUDED.frequency
            "#,
            path_str.replace('\'', "''")
        );

        self.conn.execute(&query, [])?;

        // Get count of words loaded
        let count = self.word_count()?;
        log::info!("Loaded {} words from Parquet file", count);

        Ok(count)
    }

    /// Load words from Parquet file with progress logging.
    ///
    /// Wraps `load_words_from_parquet` with detailed progress reporting.
    pub fn load_words_from_parquet_with_progress<P: AsRef<std::path::Path>>(
        &self,
        parquet_path: P,
    ) -> DuckDbResult<usize> {
        let path = parquet_path.as_ref();

        // Check if file exists
        if !path.exists() {
            return Err(duckdb::Error::InvalidPath(path.to_path_buf()));
        }

        // Get initial count
        let initial_count = self.word_count()?;
        log::info!("Initial word count: {}", initial_count);

        // Load words
        let result = self.load_words_from_parquet(path)?;

        // Log statistics
        let final_count = self.word_count()?;
        let new_words = final_count.saturating_sub(initial_count);
        let updated_words = result.saturating_sub(new_words);

        log::info!(
            "Parquet load complete: {} total words ({} new, ~{} updated)",
            final_count,
            new_words,
            updated_words
        );

        Ok(result)
    }

    // =========================================================================
    // Session CRUD Operations (AC-2)
    // =========================================================================

    /// Insert a new game session into the database.
    ///
    /// # Arguments
    ///
    /// * `session` - The GameSession to insert
    pub fn insert_session(&self, session: &GameSession) -> DuckDbResult<()> {
        self.conn.execute(
            r#"
            INSERT INTO game_sessions (
                id, username, total_answers, correct_answers,
                sum_difficulty, current_difficulty, is_submitted
            ) VALUES (?, ?, ?, ?, ?, ?, FALSE)
            "#,
            params![
                session.id,
                session.username,
                session.total_answers as i64,
                session.correct_answers as i64,
                session.sum_difficulty,
                session.current_difficulty,
            ],
        )?;
        Ok(())
    }

    /// Update an existing game session.
    ///
    /// Updates all mutable fields: total_answers, correct_answers,
    /// sum_difficulty, current_difficulty.
    pub fn update_session(&self, session: &GameSession) -> DuckDbResult<()> {
        self.conn.execute(
            r#"
            UPDATE game_sessions SET
                total_answers = ?,
                correct_answers = ?,
                sum_difficulty = ?,
                current_difficulty = ?
            WHERE id = ?
            "#,
            params![
                session.total_answers as i64,
                session.correct_answers as i64,
                session.sum_difficulty,
                session.current_difficulty,
                session.id,
            ],
        )?;
        Ok(())
    }

    /// Retrieve a game session by ID.
    ///
    /// Returns `None` if the session doesn't exist.
    pub fn get_session(&self, id: &str) -> DuckDbResult<Option<GameSession>> {
        let mut stmt = self.conn.prepare(
            r#"
            SELECT id, username, total_answers, correct_answers,
                   sum_difficulty, current_difficulty
            FROM game_sessions WHERE id = ?
            "#,
        )?;

        let mut rows = stmt.query(params![id])?;

        if let Some(row) = rows.next()? {
            Ok(Some(GameSession {
                id: row.get(0)?,
                username: row.get(1)?,
                total_answers: row.get::<_, i64>(2)? as u32,
                correct_answers: row.get::<_, i64>(3)? as u32,
                sum_difficulty: row.get(4)?,
                current_difficulty: row.get(5)?,
                recent_answers: Vec::new(), // Not persisted in DB
            }))
        } else {
            Ok(None)
        }
    }

    /// Mark a session as submitted to the leaderboard.
    pub fn mark_session_submitted(&self, session_id: &str) -> DuckDbResult<()> {
        self.conn.execute(
            "UPDATE game_sessions SET is_submitted = TRUE WHERE id = ?",
            params![session_id],
        )?;
        Ok(())
    }

    // =========================================================================
    // Answer Recording Operations (AC-3, AC-5, AC-6)
    // =========================================================================

    /// Record an answer from a game round.
    ///
    /// This also updates the graph tables:
    /// - `user_word_knowledge`: Updated for all words seen
    /// - `user_confusions`: Updated when an incorrect answer is given
    ///
    /// # Arguments
    ///
    /// * `session_id` - The session this answer belongs to
    /// * `round` - The completed GameRound with answer details
    /// * `difficulty` - The difficulty level when this round was played
    pub fn record_answer(
        &self,
        session_id: &str,
        round: &GameRound,
        difficulty: f64,
    ) -> DuckDbResult<()> {
        let answer_id = uuid::Uuid::new_v4().to_string();

        // Serialize choices to JSON
        let choices_json =
            serde_json::to_string(&round.choices).unwrap_or_else(|_| "[]".to_string());

        // Insert the answer record
        self.conn.execute(
            r#"
            INSERT INTO answers (
                id, session_id, phrase, choices, correct_word,
                selected_word, is_correct, response_time_ms, difficulty
            ) VALUES (?, ?, ?, ?::JSON, ?, ?, ?, ?, ?)
            "#,
            params![
                answer_id,
                session_id,
                round.phrase,
                choices_json,
                round.correct_word,
                round.selected_word.as_deref().unwrap_or(""),
                round.is_correct.unwrap_or(false),
                round.response_time_ms.map(|t| t as i64),
                difficulty,
            ],
        )?;

        // Update user_word_knowledge for the correct word
        self.update_word_knowledge(
            session_id,
            &round.correct_word,
            round.is_correct.unwrap_or(false),
            false, // not as LLM distractor
            false, // not guessed (this was the correct answer)
        )?;

        // Update knowledge for each choice word
        for choice in &round.choices {
            if choice != &round.correct_word {
                let was_guessed = round.selected_word.as_ref() == Some(choice);
                self.update_word_knowledge(
                    session_id,
                    choice,
                    false, // not correct (it's a distractor)
                    true,  // appeared as LLM choice
                    was_guessed,
                )?;

                // If this distractor was incorrectly selected, record the confusion
                if was_guessed {
                    self.record_confusion(session_id, &round.correct_word, choice)?;
                }
            }
        }

        Ok(())
    }

    /// Update user word knowledge for a word.
    fn update_word_knowledge(
        &self,
        session_id: &str,
        word: &str,
        is_correct: bool,
        is_llm_choice: bool,
        was_guessed: bool,
    ) -> DuckDbResult<()> {
        // Get the word_id (or use the word text if not in vocabulary)
        let word_id = self.get_word_id(word)?.unwrap_or_else(|| word.to_string());

        // UPSERT the knowledge record
        self.conn.execute(
            r#"
            INSERT INTO user_word_knowledge (
                session_id, word_id, times_seen, times_correct,
                times_as_llm, times_guessed, last_seen
            ) VALUES (?, ?, 1, ?, ?, ?, now())
            ON CONFLICT (session_id, word_id) DO UPDATE SET
                times_seen = user_word_knowledge.times_seen + 1,
                times_correct = user_word_knowledge.times_correct + EXCLUDED.times_correct,
                times_as_llm = user_word_knowledge.times_as_llm + EXCLUDED.times_as_llm,
                times_guessed = user_word_knowledge.times_guessed + EXCLUDED.times_guessed,
                last_seen = now()
            "#,
            params![
                session_id,
                word_id,
                if is_correct { 1i64 } else { 0i64 },
                if is_llm_choice { 1i64 } else { 0i64 },
                if was_guessed { 1i64 } else { 0i64 },
            ],
        )?;

        Ok(())
    }

    /// Record a confusion between the correct word and a wrong guess.
    fn record_confusion(
        &self,
        session_id: &str,
        correct_word: &str,
        confused_word: &str,
    ) -> DuckDbResult<()> {
        let correct_id = self
            .get_word_id(correct_word)?
            .unwrap_or_else(|| correct_word.to_string());
        let confused_id = self
            .get_word_id(confused_word)?
            .unwrap_or_else(|| confused_word.to_string());

        self.conn.execute(
            r#"
            INSERT INTO user_confusions (
                session_id, correct_word_id, confused_word_id, times
            ) VALUES (?, ?, ?, 1)
            ON CONFLICT (session_id, correct_word_id, confused_word_id) DO UPDATE SET
                times = user_confusions.times + 1
            "#,
            params![session_id, correct_id, confused_id],
        )?;

        Ok(())
    }

    /// Get the knowledge record for a user and word.
    pub fn get_word_knowledge(
        &self,
        session_id: &str,
        word: &str,
    ) -> DuckDbResult<Option<WordKnowledge>> {
        let word_id = self.get_word_id(word)?.unwrap_or_else(|| word.to_string());

        let mut stmt = self.conn.prepare(
            r#"
            SELECT times_seen, times_correct, times_as_llm, times_guessed
            FROM user_word_knowledge
            WHERE session_id = ? AND word_id = ?
            "#,
        )?;

        let mut rows = stmt.query(params![session_id, word_id])?;

        if let Some(row) = rows.next()? {
            Ok(Some(WordKnowledge {
                times_seen: row.get::<_, i64>(0)? as u32,
                times_correct: row.get::<_, i64>(1)? as u32,
                times_as_llm: row.get::<_, i64>(2)? as u32,
                times_guessed: row.get::<_, i64>(3)? as u32,
            }))
        } else {
            Ok(None)
        }
    }

    /// Get confusion count between two words for a session.
    pub fn get_confusion_count(
        &self,
        session_id: &str,
        correct_word: &str,
        confused_word: &str,
    ) -> DuckDbResult<u32> {
        let correct_id = self
            .get_word_id(correct_word)?
            .unwrap_or_else(|| correct_word.to_string());
        let confused_id = self
            .get_word_id(confused_word)?
            .unwrap_or_else(|| confused_word.to_string());

        let mut stmt = self.conn.prepare(
            r#"
            SELECT times FROM user_confusions
            WHERE session_id = ? AND correct_word_id = ? AND confused_word_id = ?
            "#,
        )?;

        let mut rows = stmt.query(params![session_id, correct_id, confused_id])?;

        if let Some(row) = rows.next()? {
            Ok(row.get::<_, i64>(0)? as u32)
        } else {
            Ok(0)
        }
    }

    // =========================================================================
    // Leaderboard Operations (AC-4)
    // =========================================================================

    /// Submit a score to the leaderboard.
    ///
    /// Uses UPSERT logic: only updates if the new score is higher than existing.
    ///
    /// # Arguments
    ///
    /// * `username` - Player's username
    /// * `score` - The score to submit
    /// * `accuracy` - Player's accuracy (0.0 to 1.0)
    /// * `total_answers` - Total number of answers
    /// * `avg_difficulty` - Average difficulty of questions
    ///
    /// # Returns
    ///
    /// `true` if the score was inserted or updated (new high score),
    /// `false` if the existing score was higher.
    pub fn submit_to_leaderboard(
        &self,
        username: &str,
        score: f64,
        accuracy: f64,
        total_answers: u32,
        avg_difficulty: f64,
    ) -> DuckDbResult<bool> {
        // Check current score first
        let current_score = self.get_leaderboard_score(username)?;

        // Only proceed if this is a new entry or a higher score
        if let Some(existing) = current_score {
            if score <= existing {
                return Ok(false);
            }
        }

        // Insert or update the leaderboard entry
        self.conn.execute(
            r#"
            INSERT INTO leaderboard (
                username, score, accuracy, total_answers, avg_difficulty, submitted_at
            ) VALUES (?, ?, ?, ?, ?, now())
            ON CONFLICT (username) DO UPDATE SET
                score = EXCLUDED.score,
                accuracy = EXCLUDED.accuracy,
                total_answers = EXCLUDED.total_answers,
                avg_difficulty = EXCLUDED.avg_difficulty,
                submitted_at = now()
            WHERE EXCLUDED.score > leaderboard.score
            "#,
            params![
                username,
                score,
                accuracy,
                total_answers as i64,
                avg_difficulty,
            ],
        )?;

        Ok(true)
    }

    /// Get a player's current leaderboard score.
    pub fn get_leaderboard_score(&self, username: &str) -> DuckDbResult<Option<f64>> {
        let mut stmt = self
            .conn
            .prepare("SELECT score FROM leaderboard WHERE username = ?")?;

        let mut rows = stmt.query(params![username])?;

        if let Some(row) = rows.next()? {
            Ok(Some(row.get(0)?))
        } else {
            Ok(None)
        }
    }

    /// Get the top entries from the leaderboard.
    ///
    /// # Arguments
    ///
    /// * `limit` - Maximum number of entries to return
    ///
    /// # Returns
    ///
    /// Vector of leaderboard entries sorted by score (descending).
    pub fn get_top_leaderboard(&self, limit: usize) -> DuckDbResult<Vec<LeaderboardEntry>> {
        let mut stmt = self.conn.prepare(
            r#"
            SELECT username, score, accuracy, total_answers, avg_difficulty
            FROM leaderboard
            ORDER BY score DESC
            LIMIT ?
            "#,
        )?;

        let mut rows = stmt.query(params![limit as i64])?;

        let mut entries = Vec::new();
        while let Some(row) = rows.next()? {
            entries.push(LeaderboardEntry {
                username: row.get(0)?,
                score: row.get(1)?,
                accuracy: row.get(2)?,
                total_answers: row.get::<_, i64>(3)? as u32,
                avg_difficulty: row.get(4)?,
            });
        }

        Ok(entries)
    }
}

/// Word knowledge tracking for a user-word pair.
#[derive(Debug, Clone, PartialEq)]
pub struct WordKnowledge {
    /// Number of times the word was seen
    pub times_seen: u32,
    /// Number of times correctly identified
    pub times_correct: u32,
    /// Number of times appeared as LLM distractor
    pub times_as_llm: u32,
    /// Number of times incorrectly guessed
    pub times_guessed: u32,
}

/// A leaderboard entry.
#[derive(Debug, Clone, PartialEq)]
pub struct LeaderboardEntry {
    /// Player's username
    pub username: String,
    /// Player's score
    pub score: f64,
    /// Player's accuracy (0.0 to 1.0)
    pub accuracy: f64,
    /// Total number of answers
    pub total_answers: u32,
    /// Average difficulty of questions
    pub avg_difficulty: f64,
}

/// Parse an embedding string from DuckDB format like "[0.1, 0.2, ...]" into Vec<f32>
fn parse_embedding_string(s: &str) -> Vec<f32> {
    // Remove brackets and split by comma
    let trimmed = s.trim().trim_start_matches('[').trim_end_matches(']');
    trimmed
        .split(',')
        .filter_map(|v| v.trim().parse::<f32>().ok())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_embedding(seed: f32) -> Vec<f32> {
        // Create a simple test embedding - normalized for cosine similarity
        let mut emb: Vec<f32> = (0..384).map(|i| (i as f32 * seed).sin()).collect();
        let norm: f32 = emb.iter().map(|x| x * x).sum::<f32>().sqrt();
        emb.iter_mut().for_each(|x| *x /= norm);
        emb
    }

    #[test]
    fn test_create_in_memory_db() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();
    }

    #[test]
    fn test_insert_and_get_word() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let embedding = create_test_embedding(1.0);
        db.insert_word("hello", &embedding, Some(1000)).unwrap();

        assert!(db.word_exists("hello").unwrap());
        assert!(!db.word_exists("world").unwrap());

        let retrieved = db.get_word_embedding("hello").unwrap();
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().len(), 384);
    }

    #[test]
    fn test_word_count() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        assert_eq!(db.word_count().unwrap(), 0);

        db.insert_word("hello", &create_test_embedding(1.0), None)
            .unwrap();
        assert_eq!(db.word_count().unwrap(), 1);

        db.insert_word("world", &create_test_embedding(2.0), None)
            .unwrap();
        assert_eq!(db.word_count().unwrap(), 2);
    }

    #[test]
    fn test_upsert_word() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let emb1 = create_test_embedding(1.0);
        let emb2 = create_test_embedding(2.0);

        db.upsert_word("hello", &emb1, Some(100)).unwrap();
        assert_eq!(db.word_count().unwrap(), 1);

        // Upsert same word should update, not create duplicate
        db.upsert_word("hello", &emb2, Some(200)).unwrap();
        assert_eq!(db.word_count().unwrap(), 1);
    }

    #[test]
    fn test_get_word_embedding_not_found() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let result = db.get_word_embedding("nonexistent").unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_get_all_words() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        db.insert_word("apple", &create_test_embedding(1.0), None)
            .unwrap();
        db.insert_word("banana", &create_test_embedding(2.0), None)
            .unwrap();
        db.insert_word("cherry", &create_test_embedding(3.0), None)
            .unwrap();

        let words = db.get_all_words().unwrap();
        assert_eq!(words, vec!["apple", "banana", "cherry"]);
    }

    #[test]
    fn test_bulk_insert_words() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let words: Vec<(String, Vec<f32>, i64)> = vec![
            ("word1".into(), create_test_embedding(1.0), 100),
            ("word2".into(), create_test_embedding(2.0), 200),
            ("word3".into(), create_test_embedding(3.0), 300),
        ];

        let count = db.bulk_insert_words(&words).unwrap();
        assert_eq!(count, 3);
        assert_eq!(db.word_count().unwrap(), 3);
    }

    // =========================================================================
    // Session CRUD Tests (AC-2, AC-9)
    // =========================================================================

    #[test]
    fn test_insert_session() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let session = GameSession::with_username("TestPlayer");
        db.insert_session(&session).unwrap();

        let retrieved = db.get_session(&session.id).unwrap();
        assert!(retrieved.is_some());

        let retrieved = retrieved.unwrap();
        assert_eq!(retrieved.username, "TestPlayer");
        assert_eq!(retrieved.total_answers, 0);
        assert_eq!(retrieved.correct_answers, 0);
    }

    #[test]
    fn test_update_session() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let mut session = GameSession::with_username("TestPlayer");
        db.insert_session(&session).unwrap();

        // Simulate game progress
        session.total_answers = 10;
        session.correct_answers = 8;
        session.sum_difficulty = 5.5;
        session.current_difficulty = 0.65;

        db.update_session(&session).unwrap();

        let retrieved = db.get_session(&session.id).unwrap().unwrap();
        assert_eq!(retrieved.total_answers, 10);
        assert_eq!(retrieved.correct_answers, 8);
        assert!((retrieved.sum_difficulty - 5.5).abs() < f64::EPSILON);
        assert!((retrieved.current_difficulty - 0.65).abs() < f64::EPSILON);
    }

    #[test]
    fn test_get_session_not_found() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let result = db.get_session("nonexistent-id").unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_session_with_defaults() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let session = GameSession::with_username("DefaultsTest");
        db.insert_session(&session).unwrap();

        let retrieved = db.get_session(&session.id).unwrap().unwrap();
        assert_eq!(retrieved.total_answers, 0);
        assert_eq!(retrieved.correct_answers, 0);
        assert!((retrieved.sum_difficulty - 0.0).abs() < f64::EPSILON);
        assert!((retrieved.current_difficulty - 0.5).abs() < f64::EPSILON);
    }

    // =========================================================================
    // Leaderboard Tests (AC-4, AC-9)
    // =========================================================================

    #[test]
    fn test_leaderboard_insert_new_entry() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let result = db
            .submit_to_leaderboard("Player1", 0.85, 0.9, 50, 0.6)
            .unwrap();
        assert!(result, "New entry should be inserted");

        let score = db.get_leaderboard_score("Player1").unwrap();
        assert!(score.is_some());
        assert!((score.unwrap() - 0.85).abs() < f64::EPSILON);
    }

    #[test]
    fn test_leaderboard_upsert_higher_score() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        // Insert initial score
        db.submit_to_leaderboard("Player1", 0.50, 0.7, 30, 0.5)
            .unwrap();

        // Submit higher score - should update
        let result = db
            .submit_to_leaderboard("Player1", 0.85, 0.9, 50, 0.6)
            .unwrap();
        assert!(result, "Higher score should update");

        let score = db.get_leaderboard_score("Player1").unwrap().unwrap();
        assert!((score - 0.85).abs() < f64::EPSILON);
    }

    #[test]
    fn test_leaderboard_no_update_lower_score() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        // Insert high score
        db.submit_to_leaderboard("Player1", 0.85, 0.9, 50, 0.6)
            .unwrap();

        // Submit lower score - should NOT update
        let result = db
            .submit_to_leaderboard("Player1", 0.50, 0.7, 30, 0.5)
            .unwrap();
        assert!(!result, "Lower score should not update");

        // Original score should remain
        let score = db.get_leaderboard_score("Player1").unwrap().unwrap();
        assert!((score - 0.85).abs() < f64::EPSILON);
    }

    #[test]
    fn test_leaderboard_no_update_equal_score() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        // Insert score
        db.submit_to_leaderboard("Player1", 0.85, 0.9, 50, 0.6)
            .unwrap();

        // Submit equal score - should NOT update
        let result = db
            .submit_to_leaderboard("Player1", 0.85, 0.8, 40, 0.5)
            .unwrap();
        assert!(!result, "Equal score should not update");
    }

    #[test]
    fn test_get_top_leaderboard() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        // Insert multiple players
        db.submit_to_leaderboard("Player1", 0.50, 0.7, 30, 0.5)
            .unwrap();
        db.submit_to_leaderboard("Player2", 0.85, 0.9, 50, 0.6)
            .unwrap();
        db.submit_to_leaderboard("Player3", 0.65, 0.8, 40, 0.55)
            .unwrap();

        let top = db.get_top_leaderboard(10).unwrap();
        assert_eq!(top.len(), 3);

        // Should be sorted by score descending
        assert_eq!(top[0].username, "Player2");
        assert!((top[0].score - 0.85).abs() < f64::EPSILON);
        assert_eq!(top[1].username, "Player3");
        assert_eq!(top[2].username, "Player1");
    }

    #[test]
    fn test_get_top_leaderboard_with_limit() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        // Insert multiple players
        for i in 0..5 {
            let score = (i as f64) * 0.1;
            db.submit_to_leaderboard(&format!("Player{}", i), score, 0.8, 50, 0.5)
                .unwrap();
        }

        let top = db.get_top_leaderboard(3).unwrap();
        assert_eq!(top.len(), 3);
    }

    // =========================================================================
    // Answer Recording Tests (AC-3, AC-5, AC-6, AC-9)
    // =========================================================================

    #[test]
    fn test_record_answer_correct() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let session = GameSession::with_username("TestPlayer");
        db.insert_session(&session).unwrap();

        let mut round = GameRound::new(
            "The quick brown fox",
            vec![
                "fast".into(),
                "slow".into(),
                "quick".into(),
                "lazy".into(),
                "red".into(),
            ],
            "quick",
        );
        round.record_answer("quick", 1500);

        db.record_answer(&session.id, &round, 0.5).unwrap();

        // Verify word knowledge was updated
        let knowledge = db.get_word_knowledge(&session.id, "quick").unwrap();
        assert!(knowledge.is_some());
        let knowledge = knowledge.unwrap();
        assert_eq!(knowledge.times_seen, 1);
        assert_eq!(knowledge.times_correct, 1);
    }

    #[test]
    fn test_record_answer_incorrect_creates_confusion() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let session = GameSession::with_username("TestPlayer");
        db.insert_session(&session).unwrap();

        let mut round = GameRound::new(
            "The quick brown fox",
            vec![
                "fast".into(),
                "slow".into(),
                "quick".into(),
                "lazy".into(),
                "red".into(),
            ],
            "quick",
        );
        round.record_answer("fast", 2000); // Wrong answer

        db.record_answer(&session.id, &round, 0.5).unwrap();

        // Verify confusion was recorded
        let confusion_count = db
            .get_confusion_count(&session.id, "quick", "fast")
            .unwrap();
        assert_eq!(confusion_count, 1);
    }

    #[test]
    fn test_record_answer_with_json_choices() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let session = GameSession::with_username("TestPlayer");
        db.insert_session(&session).unwrap();

        // Test with various choices including special characters
        let mut round = GameRound::new(
            "Test phrase",
            vec![
                "word1".into(),
                "word2".into(),
                "word3".into(),
                "word4".into(),
                "word5".into(),
            ],
            "word3",
        );
        round.record_answer("word3", 1000);

        // Should not panic - JSON serialization should work
        db.record_answer(&session.id, &round, 0.5).unwrap();
    }

    #[test]
    fn test_word_knowledge_accumulates() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let session = GameSession::with_username("TestPlayer");
        db.insert_session(&session).unwrap();

        // First round - correct answer
        let mut round1 = GameRound::new(
            "Phrase 1",
            vec![
                "target".into(),
                "a".into(),
                "b".into(),
                "c".into(),
                "d".into(),
            ],
            "target",
        );
        round1.record_answer("target", 1000);
        db.record_answer(&session.id, &round1, 0.5).unwrap();

        // Second round - same word, wrong answer
        let mut round2 = GameRound::new(
            "Phrase 2",
            vec![
                "target".into(),
                "x".into(),
                "y".into(),
                "z".into(),
                "w".into(),
            ],
            "target",
        );
        round2.record_answer("x", 1500);
        db.record_answer(&session.id, &round2, 0.6).unwrap();

        let knowledge = db
            .get_word_knowledge(&session.id, "target")
            .unwrap()
            .unwrap();
        assert_eq!(knowledge.times_seen, 2);
        assert_eq!(knowledge.times_correct, 1); // Only first was correct
    }

    #[test]
    fn test_confusion_accumulates() {
        let db = GameDb::new(":memory:").unwrap();
        db.init_schema().unwrap();

        let session = GameSession::with_username("TestPlayer");
        db.insert_session(&session).unwrap();

        // Make same mistake twice
        for _ in 0..2 {
            let mut round = GameRound::new(
                "Test phrase",
                vec![
                    "correct".into(),
                    "wrong".into(),
                    "a".into(),
                    "b".into(),
                    "c".into(),
                ],
                "correct",
            );
            round.record_answer("wrong", 1000);
            db.record_answer(&session.id, &round, 0.5).unwrap();
        }

        let confusion_count = db
            .get_confusion_count(&session.id, "correct", "wrong")
            .unwrap();
        assert_eq!(confusion_count, 2);
    }
}
