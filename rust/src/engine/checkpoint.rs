//! Checkpoint persistence for save/resume capability
//!
//! Provides:
//! - Checkpoint serialization via bincode
//! - File-based persistence
//! - Interrupt handling support

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::fs;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use uuid::Uuid;

use crate::error::{TeaError, TeaResult};

/// A checkpoint representing execution state at a point in time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Checkpoint {
    /// Unique checkpoint ID
    pub id: String,

    /// Current node being executed
    pub current_node: String,

    /// State at checkpoint
    pub state: JsonValue,

    /// Timestamp
    pub created_at: chrono::DateTime<chrono::Utc>,

    /// Graph name
    pub graph_name: Option<String>,

    /// Additional metadata
    pub metadata: JsonValue,
}

impl Checkpoint {
    /// Create a new checkpoint
    pub fn new(current_node: &str, state: JsonValue) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            current_node: current_node.to_string(),
            state,
            created_at: chrono::Utc::now(),
            graph_name: None,
            metadata: JsonValue::Null,
        }
    }

    /// Set the graph name
    pub fn with_graph_name(mut self, name: impl Into<String>) -> Self {
        self.graph_name = Some(name.into());
        self
    }

    /// Set metadata
    pub fn with_metadata(mut self, metadata: JsonValue) -> Self {
        self.metadata = metadata;
        self
    }

    /// Merge state updates into checkpoint
    pub fn merge_state(&mut self, updates: &JsonValue) {
        if let (Some(current), Some(updates)) = (self.state.as_object_mut(), updates.as_object()) {
            for (key, value) in updates {
                current.insert(key.clone(), value.clone());
            }
        }
    }

    /// Serialize to bytes (MessagePack format - like Python's pickle but safe)
    pub fn to_bytes(&self) -> TeaResult<Vec<u8>> {
        rmp_serde::to_vec(self).map_err(|e| TeaError::Serialization(e.to_string()))
    }

    /// Deserialize from bytes (MessagePack format)
    pub fn from_bytes(bytes: &[u8]) -> TeaResult<Self> {
        rmp_serde::from_slice(bytes).map_err(|e| TeaError::Serialization(e.to_string()))
    }

    /// Serialize to JSON string
    pub fn to_json(&self) -> TeaResult<String> {
        serde_json::to_string_pretty(self).map_err(|e| TeaError::Serialization(e.to_string()))
    }

    /// Deserialize from JSON string
    pub fn from_json(json: &str) -> TeaResult<Self> {
        serde_json::from_str(json).map_err(|e| TeaError::Serialization(e.to_string()))
    }
}

/// Checkpointer trait for persistence backends
pub trait Checkpointer: Send + Sync {
    /// Save a checkpoint
    fn save(&self, checkpoint: &Checkpoint) -> TeaResult<String>;

    /// Load a checkpoint by ID or path
    fn load(&self, id_or_path: &str) -> TeaResult<Option<Checkpoint>>;

    /// Delete a checkpoint
    fn delete(&self, id_or_path: &str) -> TeaResult<()>;

    /// List all checkpoints
    fn list(&self) -> TeaResult<Vec<String>>;
}

/// File-based checkpointer
pub struct FileCheckpointer {
    /// Directory for checkpoint files
    dir: PathBuf,

    /// Use binary format (bincode) vs JSON
    binary: bool,
}

impl FileCheckpointer {
    /// Create a new file checkpointer
    pub fn new<P: AsRef<Path>>(dir: P) -> TeaResult<Self> {
        let dir = dir.as_ref().to_path_buf();
        fs::create_dir_all(&dir)?;

        Ok(Self { dir, binary: true })
    }

    /// Create checkpointer with JSON format
    pub fn json<P: AsRef<Path>>(dir: P) -> TeaResult<Self> {
        let dir = dir.as_ref().to_path_buf();
        fs::create_dir_all(&dir)?;

        Ok(Self { dir, binary: false })
    }

    /// Get checkpoint file path
    fn checkpoint_path(&self, id: &str) -> PathBuf {
        let ext = if self.binary { "bin" } else { "json" };
        self.dir.join(format!("{}.{}", id, ext))
    }

    /// Determine if a string is an ID or path
    fn resolve_path(&self, id_or_path: &str) -> PathBuf {
        let path = Path::new(id_or_path);
        if path.exists() || path.is_absolute() {
            path.to_path_buf()
        } else {
            self.checkpoint_path(id_or_path)
        }
    }
}

impl Checkpointer for FileCheckpointer {
    fn save(&self, checkpoint: &Checkpoint) -> TeaResult<String> {
        let path = self.checkpoint_path(&checkpoint.id);

        let mut file = fs::File::create(&path)?;

        if self.binary {
            let bytes = checkpoint.to_bytes()?;
            file.write_all(&bytes)?;
        } else {
            let json = checkpoint.to_json()?;
            file.write_all(json.as_bytes())?;
        }

        Ok(path.to_string_lossy().to_string())
    }

    fn load(&self, id_or_path: &str) -> TeaResult<Option<Checkpoint>> {
        let path = self.resolve_path(id_or_path);

        if !path.exists() {
            return Ok(None);
        }

        let mut file = fs::File::open(&path)?;

        let checkpoint = if self.binary || path.extension().map_or(false, |e| e == "bin") {
            let mut bytes = Vec::new();
            file.read_to_end(&mut bytes)?;
            Checkpoint::from_bytes(&bytes)?
        } else {
            let mut json = String::new();
            file.read_to_string(&mut json)?;
            Checkpoint::from_json(&json)?
        };

        Ok(Some(checkpoint))
    }

    fn delete(&self, id_or_path: &str) -> TeaResult<()> {
        let path = self.resolve_path(id_or_path);

        if path.exists() {
            fs::remove_file(&path)?;
        }

        Ok(())
    }

    fn list(&self) -> TeaResult<Vec<String>> {
        let ext = if self.binary { "bin" } else { "json" };
        let mut ids = Vec::new();

        for entry in fs::read_dir(&self.dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.extension().map_or(false, |e| e == ext) {
                if let Some(stem) = path.file_stem() {
                    ids.push(stem.to_string_lossy().to_string());
                }
            }
        }

        Ok(ids)
    }
}

/// In-memory checkpointer for testing
pub struct MemoryCheckpointer {
    checkpoints: parking_lot::RwLock<std::collections::HashMap<String, Checkpoint>>,
}

impl MemoryCheckpointer {
    /// Create a new in-memory checkpointer
    pub fn new() -> Self {
        Self {
            checkpoints: parking_lot::RwLock::new(std::collections::HashMap::new()),
        }
    }
}

impl Default for MemoryCheckpointer {
    fn default() -> Self {
        Self::new()
    }
}

impl Checkpointer for MemoryCheckpointer {
    fn save(&self, checkpoint: &Checkpoint) -> TeaResult<String> {
        let id = checkpoint.id.clone();
        self.checkpoints
            .write()
            .insert(id.clone(), checkpoint.clone());
        Ok(id)
    }

    fn load(&self, id: &str) -> TeaResult<Option<Checkpoint>> {
        Ok(self.checkpoints.read().get(id).cloned())
    }

    fn delete(&self, id: &str) -> TeaResult<()> {
        self.checkpoints.write().remove(id);
        Ok(())
    }

    fn list(&self) -> TeaResult<Vec<String>> {
        Ok(self.checkpoints.read().keys().cloned().collect())
    }
}

/// Interrupt configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterruptConfig {
    /// Nodes to interrupt before
    pub interrupt_before: Vec<String>,

    /// Nodes to interrupt after
    pub interrupt_after: Vec<String>,
}

impl Default for InterruptConfig {
    fn default() -> Self {
        Self {
            interrupt_before: vec![],
            interrupt_after: vec![],
        }
    }
}

impl InterruptConfig {
    /// Create config to interrupt before specific nodes
    pub fn before(nodes: Vec<String>) -> Self {
        Self {
            interrupt_before: nodes,
            interrupt_after: vec![],
        }
    }

    /// Create config to interrupt after specific nodes
    pub fn after(nodes: Vec<String>) -> Self {
        Self {
            interrupt_before: vec![],
            interrupt_after: nodes,
        }
    }

    /// Add interrupt before node
    pub fn add_before(&mut self, node: impl Into<String>) {
        self.interrupt_before.push(node.into());
    }

    /// Add interrupt after node
    pub fn add_after(&mut self, node: impl Into<String>) {
        self.interrupt_after.push(node.into());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use tempfile::tempdir;

    #[test]
    fn test_checkpoint_creation() {
        let checkpoint = Checkpoint::new("process", json!({"value": 42}));

        assert_eq!(checkpoint.current_node, "process");
        assert_eq!(checkpoint.state["value"], 42);
        assert!(!checkpoint.id.is_empty());
    }

    #[test]
    fn test_checkpoint_serialization_binary() {
        let checkpoint = Checkpoint::new("test", json!({"key": "value"}));

        let bytes = checkpoint.to_bytes().unwrap();
        let restored = Checkpoint::from_bytes(&bytes).unwrap();

        assert_eq!(restored.id, checkpoint.id);
        assert_eq!(restored.current_node, checkpoint.current_node);
        assert_eq!(restored.state, checkpoint.state);
    }

    #[test]
    fn test_checkpoint_serialization_json() {
        let checkpoint = Checkpoint::new("test", json!({"key": "value"}));

        let json = checkpoint.to_json().unwrap();
        let restored = Checkpoint::from_json(&json).unwrap();

        assert_eq!(restored.id, checkpoint.id);
        assert_eq!(restored.current_node, checkpoint.current_node);
        assert_eq!(restored.state, checkpoint.state);
    }

    #[test]
    fn test_checkpoint_merge_state() {
        let mut checkpoint = Checkpoint::new("test", json!({"a": 1, "b": 2}));

        checkpoint.merge_state(&json!({"b": 3, "c": 4}));

        assert_eq!(checkpoint.state["a"], 1);
        assert_eq!(checkpoint.state["b"], 3);
        assert_eq!(checkpoint.state["c"], 4);
    }

    #[test]
    fn test_file_checkpointer_binary() {
        let dir = tempdir().unwrap();
        let checkpointer = FileCheckpointer::new(dir.path()).unwrap();

        let checkpoint = Checkpoint::new("test_node", json!({"x": 123}));
        let id = checkpoint.id.clone();

        // Save
        let path = checkpointer.save(&checkpoint).unwrap();
        assert!(Path::new(&path).exists());

        // Load
        let loaded = checkpointer.load(&id).unwrap().unwrap();
        assert_eq!(loaded.current_node, "test_node");
        assert_eq!(loaded.state["x"], 123);

        // List
        let ids = checkpointer.list().unwrap();
        assert!(ids.contains(&id));

        // Delete
        checkpointer.delete(&id).unwrap();
        assert!(checkpointer.load(&id).unwrap().is_none());
    }

    #[test]
    fn test_file_checkpointer_json() {
        let dir = tempdir().unwrap();
        let checkpointer = FileCheckpointer::json(dir.path()).unwrap();

        let checkpoint = Checkpoint::new("test_node", json!({"y": 456}));
        let id = checkpoint.id.clone();

        // Save
        checkpointer.save(&checkpoint).unwrap();

        // Load
        let loaded = checkpointer.load(&id).unwrap().unwrap();
        assert_eq!(loaded.current_node, "test_node");
        assert_eq!(loaded.state["y"], 456);
    }

    #[test]
    fn test_memory_checkpointer() {
        let checkpointer = MemoryCheckpointer::new();

        let checkpoint = Checkpoint::new("node", json!({"value": "test"}));
        let id = checkpoint.id.clone();

        // Save
        checkpointer.save(&checkpoint).unwrap();

        // Load
        let loaded = checkpointer.load(&id).unwrap().unwrap();
        assert_eq!(loaded.state["value"], "test");

        // List
        let ids = checkpointer.list().unwrap();
        assert_eq!(ids.len(), 1);

        // Delete
        checkpointer.delete(&id).unwrap();
        assert!(checkpointer.load(&id).unwrap().is_none());
    }

    #[test]
    fn test_interrupt_config() {
        let mut config = InterruptConfig::before(vec!["human_review".to_string()]);
        config.add_after("send_email".to_string());

        assert!(config
            .interrupt_before
            .contains(&"human_review".to_string()));
        assert!(config.interrupt_after.contains(&"send_email".to_string()));
    }

    #[test]
    fn test_checkpoint_with_metadata() {
        let checkpoint = Checkpoint::new("node", json!({}))
            .with_graph_name("my-workflow")
            .with_metadata(json!({"user": "test", "version": 1}));

        assert_eq!(checkpoint.graph_name, Some("my-workflow".to_string()));
        assert_eq!(checkpoint.metadata["user"], "test");
    }

    #[test]
    fn test_load_nonexistent() {
        let dir = tempdir().unwrap();
        let checkpointer = FileCheckpointer::new(dir.path()).unwrap();

        let result = checkpointer.load("nonexistent").unwrap();
        assert!(result.is_none());
    }
}
