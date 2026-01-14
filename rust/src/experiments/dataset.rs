//! Dataset types and loading for experiment framework.
//!
//! Datasets are collections of test items used to evaluate agent behavior.
//! Each item contains an input, expected output, and optional metadata.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::TeaError;

/// A single item in a dataset for evaluation.
///
/// Each item contains:
/// - `input`: The input to pass to the agent
/// - `expected_output`: The expected result for scoring
/// - `metadata`: Optional additional information about the item
///
/// # Example
///
/// ```rust
/// use the_edge_agent::experiments::DatasetItem;
/// use serde_json::json;
///
/// let item = DatasetItem {
///     input: json!({"query": "What is 2+2?"}),
///     expected_output: json!({"answer": "4"}),
///     metadata: Default::default(),
/// };
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatasetItem {
    /// The input to pass to the agent for evaluation
    pub input: Value,

    /// The expected output to compare against
    pub expected_output: Value,

    /// Optional metadata about the item (e.g., category, difficulty)
    #[serde(default)]
    pub metadata: HashMap<String, Value>,
}

impl DatasetItem {
    /// Create a new dataset item with the given input and expected output.
    pub fn new(input: Value, expected_output: Value) -> Self {
        Self {
            input,
            expected_output,
            metadata: HashMap::new(),
        }
    }

    /// Add metadata to the item.
    pub fn with_metadata(mut self, key: impl Into<String>, value: Value) -> Self {
        self.metadata.insert(key.into(), value);
        self
    }
}

/// A collection of dataset items for evaluation.
///
/// Datasets can be loaded from JSON files or created programmatically.
///
/// # Example
///
/// ```rust
/// use the_edge_agent::experiments::Dataset;
///
/// // From JSON string
/// let json = r#"{
///     "name": "math_test",
///     "description": "Basic arithmetic",
///     "items": [
///         {"input": {"x": 2}, "expected_output": {"y": 4}}
///     ]
/// }"#;
/// let dataset = Dataset::from_json_str(json).unwrap();
/// assert_eq!(dataset.name, "math_test");
/// assert_eq!(dataset.items.len(), 1);
/// ```
#[derive(Debug, Clone)]
pub struct Dataset {
    /// Name of the dataset
    pub name: String,

    /// Optional description of the dataset
    pub description: Option<String>,

    /// The items in the dataset
    pub items: Vec<DatasetItem>,
}

/// Internal representation for JSON deserialization
#[derive(Debug, Deserialize)]
struct DatasetJson {
    name: String,
    #[serde(default)]
    description: Option<String>,
    items: Vec<DatasetItem>,
}

impl Dataset {
    /// Create a new empty dataset with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: None,
            items: Vec::new(),
        }
    }

    /// Set the description of the dataset.
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Add an item to the dataset.
    pub fn with_item(mut self, item: DatasetItem) -> Self {
        self.items.push(item);
        self
    }

    /// Add multiple items to the dataset.
    pub fn with_items(mut self, items: impl IntoIterator<Item = DatasetItem>) -> Self {
        self.items.extend(items);
        self
    }

    /// Load a dataset from a JSON file.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the JSON file
    ///
    /// # Returns
    ///
    /// The loaded dataset or an error if loading fails.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// use the_edge_agent::experiments::Dataset;
    /// use std::path::Path;
    ///
    /// let dataset = Dataset::from_json_file(Path::new("tests/fixtures/dataset.json"))?;
    /// ```
    pub fn from_json_file(path: &Path) -> Result<Self, TeaError> {
        let content = fs::read_to_string(path).map_err(|e| {
            TeaError::InvalidConfig(format!("Failed to read dataset file '{}': {}", path.display(), e))
        })?;
        Self::from_json_str(&content)
    }

    /// Load a dataset from a JSON string.
    ///
    /// # Arguments
    ///
    /// * `json` - JSON string containing the dataset
    ///
    /// # Returns
    ///
    /// The loaded dataset or an error if parsing fails.
    ///
    /// # Example
    ///
    /// ```rust
    /// use the_edge_agent::experiments::Dataset;
    ///
    /// let json = r#"{
    ///     "name": "test",
    ///     "items": [
    ///         {"input": {"q": "hello"}, "expected_output": {"a": "world"}}
    ///     ]
    /// }"#;
    /// let dataset = Dataset::from_json_str(json).unwrap();
    /// ```
    pub fn from_json_str(json: &str) -> Result<Self, TeaError> {
        let parsed: DatasetJson = serde_json::from_str(json).map_err(|e| {
            TeaError::InvalidConfig(format!("Failed to parse dataset JSON: {}", e))
        })?;

        Ok(Self {
            name: parsed.name,
            description: parsed.description,
            items: parsed.items,
        })
    }

    /// Get the number of items in the dataset.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Check if the dataset is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Iterate over the items in the dataset.
    pub fn iter(&self) -> impl Iterator<Item = &DatasetItem> {
        self.items.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_dataset_item_new() {
        let item = DatasetItem::new(json!({"x": 1}), json!({"y": 2}));
        assert_eq!(item.input, json!({"x": 1}));
        assert_eq!(item.expected_output, json!({"y": 2}));
        assert!(item.metadata.is_empty());
    }

    #[test]
    fn test_dataset_item_with_metadata() {
        let item = DatasetItem::new(json!({"x": 1}), json!({"y": 2}))
            .with_metadata("category", json!("math"));
        assert_eq!(item.metadata.get("category"), Some(&json!("math")));
    }

    #[test]
    fn test_dataset_new() {
        let dataset = Dataset::new("test");
        assert_eq!(dataset.name, "test");
        assert!(dataset.description.is_none());
        assert!(dataset.items.is_empty());
    }

    #[test]
    fn test_dataset_builder() {
        let dataset = Dataset::new("test")
            .with_description("Test dataset")
            .with_item(DatasetItem::new(json!({"x": 1}), json!({"y": 2})));

        assert_eq!(dataset.name, "test");
        assert_eq!(dataset.description, Some("Test dataset".to_string()));
        assert_eq!(dataset.len(), 1);
    }

    #[test]
    fn test_dataset_from_json_str() {
        let json = r#"{
            "name": "math_test",
            "description": "Basic arithmetic",
            "items": [
                {"input": {"x": 2}, "expected_output": {"y": 4}},
                {"input": {"x": 3}, "expected_output": {"y": 6}, "metadata": {"difficulty": "easy"}}
            ]
        }"#;

        let dataset = Dataset::from_json_str(json).unwrap();
        assert_eq!(dataset.name, "math_test");
        assert_eq!(dataset.description, Some("Basic arithmetic".to_string()));
        assert_eq!(dataset.len(), 2);
        assert_eq!(dataset.items[0].input, json!({"x": 2}));
        assert_eq!(dataset.items[1].metadata.get("difficulty"), Some(&json!("easy")));
    }

    #[test]
    fn test_dataset_from_json_str_minimal() {
        let json = r#"{"name": "minimal", "items": []}"#;
        let dataset = Dataset::from_json_str(json).unwrap();
        assert_eq!(dataset.name, "minimal");
        assert!(dataset.description.is_none());
        assert!(dataset.is_empty());
    }

    #[test]
    fn test_dataset_from_json_str_nested() {
        let json = r#"{
            "name": "nested",
            "items": [
                {
                    "input": {"query": {"text": "hello", "options": [1, 2, 3]}},
                    "expected_output": {"response": {"data": {"nested": true}}}
                }
            ]
        }"#;

        let dataset = Dataset::from_json_str(json).unwrap();
        assert_eq!(dataset.items[0].input["query"]["text"], "hello");
        assert_eq!(dataset.items[0].expected_output["response"]["data"]["nested"], true);
    }

    #[test]
    fn test_dataset_from_json_str_invalid() {
        let result = Dataset::from_json_str("not json");
        assert!(result.is_err());
    }

    #[test]
    fn test_dataset_iter() {
        let dataset = Dataset::new("test")
            .with_item(DatasetItem::new(json!({"x": 1}), json!({"y": 1})))
            .with_item(DatasetItem::new(json!({"x": 2}), json!({"y": 2})));

        let inputs: Vec<_> = dataset.iter().map(|i| i.input.clone()).collect();
        assert_eq!(inputs, vec![json!({"x": 1}), json!({"x": 2})]);
    }
}
