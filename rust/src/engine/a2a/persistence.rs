//! A2A Message Persistence (TEA-AGENT-001.5-rust)
//!
//! Optional file-backed queue for message persistence using MessagePack serialization.

use super::message::Message;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::PathBuf;
use std::sync::Mutex;

/// Error types for persistence operations.
#[derive(Debug)]
pub enum PersistenceError {
    /// IO error
    Io(std::io::Error),
    /// Serialization error
    Serialization(String),
    /// File size limit exceeded
    SizeLimitExceeded { current: u64, limit: u64 },
    /// Corruption detected
    Corrupted(String),
    /// Lock error
    LockError(String),
}

impl std::fmt::Display for PersistenceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PersistenceError::Io(e) => write!(f, "IO error: {}", e),
            PersistenceError::Serialization(msg) => write!(f, "Serialization error: {}", msg),
            PersistenceError::SizeLimitExceeded { current, limit } => {
                write!(
                    f,
                    "File size limit exceeded: {} bytes (limit: {})",
                    current, limit
                )
            }
            PersistenceError::Corrupted(msg) => write!(f, "Data corrupted: {}", msg),
            PersistenceError::LockError(msg) => write!(f, "Lock error: {}", msg),
        }
    }
}

impl std::error::Error for PersistenceError {}

impl From<std::io::Error> for PersistenceError {
    fn from(err: std::io::Error) -> Self {
        PersistenceError::Io(err)
    }
}

/// Configuration for message persistence.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PersistenceConfig {
    /// Path to persistence file
    pub path: PathBuf,
    /// Maximum file size in bytes (default: 10MB)
    pub max_size: u64,
    /// Enable sync after each write
    pub sync_writes: bool,
}

impl Default for PersistenceConfig {
    fn default() -> Self {
        Self {
            path: PathBuf::from("a2a_queue.bin"),
            max_size: 10 * 1024 * 1024, // 10MB
            sync_writes: false,
        }
    }
}

impl PersistenceConfig {
    /// Create a new config with the given path.
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self {
            path: path.into(),
            ..Default::default()
        }
    }

    /// Set maximum file size.
    pub fn with_max_size(mut self, max_size: u64) -> Self {
        self.max_size = max_size;
        self
    }

    /// Enable sync after each write.
    pub fn with_sync(mut self, sync: bool) -> Self {
        self.sync_writes = sync;
        self
    }
}

/// Header for the persistence file.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct FileHeader {
    /// Magic number for format identification
    magic: [u8; 4],
    /// Format version
    version: u8,
    /// Number of messages in file
    message_count: u32,
}

impl Default for FileHeader {
    fn default() -> Self {
        Self {
            magic: *b"A2AQ",
            version: 1,
            message_count: 0,
        }
    }
}

impl FileHeader {
    fn validate(&self) -> Result<(), PersistenceError> {
        if &self.magic != b"A2AQ" {
            return Err(PersistenceError::Corrupted("Invalid magic number".into()));
        }
        if self.version != 1 {
            return Err(PersistenceError::Corrupted(format!(
                "Unsupported version: {}",
                self.version
            )));
        }
        Ok(())
    }
}

/// Persistent message queue backed by file.
#[derive(Debug)]
pub struct PersistentQueue {
    config: PersistenceConfig,
    /// In-memory cache of messages
    messages: Mutex<VecDeque<Message>>,
    /// Whether the queue has been modified since last persist
    dirty: Mutex<bool>,
}

impl PersistentQueue {
    /// Create a new persistent queue with the given config.
    pub fn new(config: PersistenceConfig) -> Result<Self, PersistenceError> {
        let queue = Self {
            config,
            messages: Mutex::new(VecDeque::new()),
            dirty: Mutex::new(false),
        };

        // Load existing messages if file exists
        if queue.config.path.exists() {
            queue.load()?;
        }

        Ok(queue)
    }

    /// Create a new queue at the given path.
    pub fn at_path(path: impl Into<PathBuf>) -> Result<Self, PersistenceError> {
        Self::new(PersistenceConfig::new(path))
    }

    /// Get the number of messages in the queue.
    pub fn len(&self) -> usize {
        self.messages.lock().unwrap().len()
    }

    /// Check if the queue is empty.
    pub fn is_empty(&self) -> bool {
        self.messages.lock().unwrap().is_empty()
    }

    /// Push a message to the queue.
    pub fn push(&self, message: Message) -> Result<(), PersistenceError> {
        let mut messages = self.messages.lock().unwrap();
        messages.push_back(message);
        *self.dirty.lock().unwrap() = true;
        Ok(())
    }

    /// Pop a message from the queue.
    pub fn pop(&self) -> Option<Message> {
        let mut messages = self.messages.lock().unwrap();
        let msg = messages.pop_front();
        if msg.is_some() {
            *self.dirty.lock().unwrap() = true;
        }
        msg
    }

    /// Peek at the next message without removing it.
    pub fn peek(&self) -> Option<Message> {
        self.messages.lock().unwrap().front().cloned()
    }

    /// Get all messages (for iteration/filtering).
    pub fn all(&self) -> Vec<Message> {
        self.messages.lock().unwrap().iter().cloned().collect()
    }

    /// Clear all messages.
    pub fn clear(&self) {
        let mut messages = self.messages.lock().unwrap();
        if !messages.is_empty() {
            messages.clear();
            *self.dirty.lock().unwrap() = true;
        }
    }

    /// Persist messages to file.
    pub fn persist(&self) -> Result<(), PersistenceError> {
        let messages = self.messages.lock().unwrap();

        // Check if dirty
        let is_dirty = *self.dirty.lock().unwrap();
        if !is_dirty && self.config.path.exists() {
            return Ok(());
        }

        // Serialize to MessagePack (rmp-serde handles chrono::DateTime properly)
        let data = rmp_serde::to_vec(&*messages)
            .map_err(|e| PersistenceError::Serialization(e.to_string()))?;

        // Check size limit
        let header = FileHeader {
            message_count: messages.len() as u32,
            ..Default::default()
        };
        let header_data = rmp_serde::to_vec(&header)
            .map_err(|e| PersistenceError::Serialization(e.to_string()))?;

        let total_size = header_data.len() as u64 + data.len() as u64;
        if total_size > self.config.max_size {
            return Err(PersistenceError::SizeLimitExceeded {
                current: total_size,
                limit: self.config.max_size,
            });
        }

        // Write to file
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&self.config.path)?;

        let mut writer = BufWriter::new(file);
        writer.write_all(&header_data)?;
        writer.write_all(&data)?;

        if self.config.sync_writes {
            writer.flush()?;
            writer.get_ref().sync_all()?;
        }

        *self.dirty.lock().unwrap() = false;
        Ok(())
    }

    /// Load messages from file.
    fn load(&self) -> Result<(), PersistenceError> {
        let file = File::open(&self.config.path)?;
        let mut reader = BufReader::new(file);

        // Read header
        let header: FileHeader = rmp_serde::from_read(&mut reader)
            .map_err(|e| PersistenceError::Corrupted(e.to_string()))?;
        header.validate()?;

        // Read messages
        let messages: VecDeque<Message> = rmp_serde::from_read(&mut reader)
            .map_err(|e| PersistenceError::Corrupted(e.to_string()))?;

        // Validate count
        if messages.len() != header.message_count as usize {
            return Err(PersistenceError::Corrupted(format!(
                "Message count mismatch: header says {}, found {}",
                header.message_count,
                messages.len()
            )));
        }

        *self.messages.lock().unwrap() = messages;
        *self.dirty.lock().unwrap() = false;
        Ok(())
    }

    /// Recover from a corrupted file (returns what could be recovered).
    pub fn recover(&self) -> Result<Vec<Message>, PersistenceError> {
        if !self.config.path.exists() {
            return Ok(Vec::new());
        }

        let file = File::open(&self.config.path)?;
        let mut reader = BufReader::new(file);

        // Try to skip header and read messages directly
        let mut buffer = Vec::new();
        reader.read_to_end(&mut buffer)?;

        // Try different offsets to find valid message data
        for offset in 0..std::cmp::min(buffer.len(), 100) {
            if let Ok(messages) = rmp_serde::from_slice::<VecDeque<Message>>(&buffer[offset..]) {
                return Ok(messages.into_iter().collect());
            }
        }

        Err(PersistenceError::Corrupted(
            "Could not recover any messages".into(),
        ))
    }

    /// Get file size on disk.
    pub fn file_size(&self) -> Result<u64, PersistenceError> {
        if self.config.path.exists() {
            Ok(std::fs::metadata(&self.config.path)?.len())
        } else {
            Ok(0)
        }
    }

    /// Delete the persistence file.
    pub fn delete_file(&self) -> Result<(), PersistenceError> {
        if self.config.path.exists() {
            std::fs::remove_file(&self.config.path)?;
        }
        Ok(())
    }
}

impl Drop for PersistentQueue {
    fn drop(&mut self) {
        // Persist on drop if dirty
        if *self.dirty.lock().unwrap() {
            let _ = self.persist();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use tempfile::tempdir;

    fn temp_queue() -> (PersistentQueue, tempfile::TempDir) {
        let dir = tempdir().unwrap();
        let path = dir.path().join("test_queue.bin");
        let config = PersistenceConfig::new(path);
        let queue = PersistentQueue::new(config).unwrap();
        (queue, dir)
    }

    #[test]
    fn test_persistence_config_default() {
        let config = PersistenceConfig::default();
        assert_eq!(config.max_size, 10 * 1024 * 1024);
        assert!(!config.sync_writes);
    }

    #[test]
    fn test_persistence_config_builder() {
        let config = PersistenceConfig::new("/tmp/test.bin")
            .with_max_size(1024)
            .with_sync(true);

        assert_eq!(config.path, PathBuf::from("/tmp/test.bin"));
        assert_eq!(config.max_size, 1024);
        assert!(config.sync_writes);
    }

    #[test]
    fn test_queue_new_empty() {
        let (queue, _dir) = temp_queue();
        assert!(queue.is_empty());
        assert_eq!(queue.len(), 0);
    }

    #[test]
    fn test_queue_push_pop() {
        let (queue, _dir) = temp_queue();

        let msg1 = Message::new("a", "b", "ns", "type1", json!({"i": 1}));
        let msg2 = Message::new("a", "b", "ns", "type2", json!({"i": 2}));

        queue.push(msg1.clone()).unwrap();
        queue.push(msg2.clone()).unwrap();

        assert_eq!(queue.len(), 2);

        let popped1 = queue.pop().unwrap();
        assert_eq!(popped1.msg_type, "type1");

        let popped2 = queue.pop().unwrap();
        assert_eq!(popped2.msg_type, "type2");

        assert!(queue.pop().is_none());
    }

    #[test]
    fn test_queue_peek() {
        let (queue, _dir) = temp_queue();

        let msg = Message::new("a", "b", "ns", "type", json!({}));
        queue.push(msg.clone()).unwrap();

        let peeked = queue.peek().unwrap();
        assert_eq!(peeked.msg_type, "type");
        assert_eq!(queue.len(), 1); // Still there

        queue.pop();
        assert!(queue.peek().is_none());
    }

    #[test]
    fn test_queue_clear() {
        let (queue, _dir) = temp_queue();

        queue
            .push(Message::new("a", "b", "ns", "t", json!({})))
            .unwrap();
        queue
            .push(Message::new("a", "b", "ns", "t", json!({})))
            .unwrap();

        assert_eq!(queue.len(), 2);
        queue.clear();
        assert_eq!(queue.len(), 0);
    }

    #[test]
    fn test_queue_all() {
        let (queue, _dir) = temp_queue();

        for i in 0..3 {
            queue
                .push(Message::new("a", "b", "ns", "t", json!({"i": i})))
                .unwrap();
        }

        let all = queue.all();
        assert_eq!(all.len(), 3);
    }

    #[test]
    fn test_persist_and_load() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("test_persist.bin");

        // Create and populate queue
        {
            let config = PersistenceConfig::new(path.clone());
            let queue = PersistentQueue::new(config).unwrap();

            queue
                .push(Message::new(
                    "a",
                    "b",
                    "ns",
                    "type1",
                    json!({"data": "hello"}),
                ))
                .unwrap();
            queue
                .push(Message::new(
                    "c",
                    "d",
                    "ns",
                    "type2",
                    json!({"data": "world"}),
                ))
                .unwrap();

            queue.persist().unwrap();
        }

        // Reload and verify
        {
            let config = PersistenceConfig::new(path);
            let queue = PersistentQueue::new(config).unwrap();

            assert_eq!(queue.len(), 2);

            let msg1 = queue.pop().unwrap();
            assert_eq!(msg1.msg_type, "type1");
            assert_eq!(msg1.payload["data"], "hello");

            let msg2 = queue.pop().unwrap();
            assert_eq!(msg2.msg_type, "type2");
            assert_eq!(msg2.payload["data"], "world");
        }
    }

    #[test]
    fn test_persist_size_limit() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("test_limit.bin");
        let config = PersistenceConfig::new(path).with_max_size(100);
        let queue = PersistentQueue::new(config).unwrap();

        // Push many messages to exceed limit
        for i in 0..100 {
            queue
                .push(Message::new(
                    "a",
                    "b",
                    "ns",
                    "t",
                    json!({"data": format!("long data string {}", i)}),
                ))
                .unwrap();
        }

        let result = queue.persist();
        match result {
            Err(PersistenceError::SizeLimitExceeded { .. }) => {}
            _ => panic!("Expected SizeLimitExceeded error"),
        }
    }

    #[test]
    fn test_file_size() {
        let (queue, _dir) = temp_queue();

        // Before persist
        assert_eq!(queue.file_size().unwrap(), 0);

        queue
            .push(Message::new("a", "b", "ns", "t", json!({})))
            .unwrap();
        queue.persist().unwrap();

        // After persist
        assert!(queue.file_size().unwrap() > 0);
    }

    #[test]
    fn test_delete_file() {
        let (queue, dir) = temp_queue();
        let path = dir.path().join("test_queue.bin");

        queue
            .push(Message::new("a", "b", "ns", "t", json!({})))
            .unwrap();
        queue.persist().unwrap();

        assert!(path.exists());
        queue.delete_file().unwrap();
        assert!(!path.exists());
    }

    #[test]
    fn test_persist_empty_queue() {
        let (queue, _dir) = temp_queue();
        // Persisting empty queue should work
        queue.persist().unwrap();
    }

    #[test]
    fn test_drop_persists() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("test_drop.bin");

        // Create, push, and drop
        {
            let config = PersistenceConfig::new(path.clone());
            let queue = PersistentQueue::new(config).unwrap();
            queue
                .push(Message::new("a", "b", "ns", "t", json!({"value": 42})))
                .unwrap();
            // Drop here - should persist
        }

        // Reload and verify
        let config = PersistenceConfig::new(path);
        let queue = PersistentQueue::new(config).unwrap();
        assert_eq!(queue.len(), 1);
        assert_eq!(queue.pop().unwrap().payload["value"], 42);
    }

    #[test]
    fn test_fifo_order() {
        let (queue, _dir) = temp_queue();

        for i in 0..10 {
            queue
                .push(Message::new("a", "b", "ns", "t", json!({"order": i})))
                .unwrap();
        }

        for i in 0..10 {
            let msg = queue.pop().unwrap();
            assert_eq!(msg.payload["order"], i);
        }
    }
}
