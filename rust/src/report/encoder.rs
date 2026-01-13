//! URL Encoder Module - TEA-REPORT-001b
//!
//! Provides URL encoding for ErrorReport to create shareable bug report URLs.
//! Uses VLQ encoding for numbers, deflate compression, and base64url encoding.
//!
//! # URL Format
//!
//! ```text
//! https://{org}.github.io/the_edge_agent/report/{version}/{runtime}_{encoded_data}
//! ```
//!
//! # Parity
//!
//! This implementation is designed to produce identical output to the Python
//! implementation for the same input, enabling cross-runtime bug report sharing.

use super::ErrorReport;
use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine};
use flate2::read::ZlibDecoder;
use flate2::write::ZlibEncoder;
use flate2::Compression;
use std::io::{Read, Write};

/// Maximum URL length for browser compatibility
pub const MAX_URL_LENGTH: usize = 2000;

/// Default base URL for bug reports
pub const DEFAULT_BASE_URL: &str = "https://example.github.io/the_edge_agent/report";

/// Encode a u64 value to Variable-Length Quantity (VLQ) bytes.
///
/// VLQ encoding uses 7 bits per byte, with the high bit indicating
/// whether more bytes follow.
///
/// # Examples
///
/// ```
/// use the_edge_agent::report::encoder::vlq_encode;
///
/// assert_eq!(vlq_encode(0), vec![0]);
/// assert_eq!(vlq_encode(127), vec![127]);
/// assert_eq!(vlq_encode(128), vec![128, 1]);
/// assert_eq!(vlq_encode(16383), vec![255, 127]);
/// assert_eq!(vlq_encode(16384), vec![128, 128, 1]);
/// ```
pub fn vlq_encode(mut value: u64) -> Vec<u8> {
    let mut result = Vec::new();
    loop {
        let mut byte = (value & 0x7F) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80;
        }
        result.push(byte);
        if value == 0 {
            break;
        }
    }
    result
}

/// Decode VLQ bytes back to a u64 value.
///
/// Returns a tuple of (value, bytes_consumed).
///
/// # Examples
///
/// ```
/// use the_edge_agent::report::encoder::vlq_decode;
///
/// assert_eq!(vlq_decode(&[0]), (0, 1));
/// assert_eq!(vlq_decode(&[127]), (127, 1));
/// assert_eq!(vlq_decode(&[128, 1]), (128, 2));
/// assert_eq!(vlq_decode(&[255, 127]), (16383, 2));
/// ```
pub fn vlq_decode(bytes: &[u8]) -> (u64, usize) {
    let mut value: u64 = 0;
    let mut shift = 0;
    let mut consumed = 0;
    for &byte in bytes {
        consumed += 1;
        value |= ((byte & 0x7F) as u64) << shift;
        if byte & 0x80 == 0 {
            break;
        }
        shift += 7;
    }
    (value, consumed)
}

/// Compress data using deflate (zlib) with maximum compression.
///
/// Uses compression level 9 for maximum compression ratio.
///
/// # Examples
///
/// ```
/// use the_edge_agent::report::encoder::{deflate_compress, deflate_decompress};
///
/// let data = b"Hello, world!";
/// let compressed = deflate_compress(data).unwrap();
/// let decompressed = deflate_decompress(&compressed).unwrap();
/// assert_eq!(data.as_slice(), decompressed.as_slice());
/// ```
pub fn deflate_compress(data: &[u8]) -> Result<Vec<u8>, std::io::Error> {
    let mut encoder = ZlibEncoder::new(Vec::new(), Compression::best());
    encoder.write_all(data)?;
    encoder.finish()
}

/// Decompress deflate (zlib) compressed data.
pub fn deflate_decompress(data: &[u8]) -> Result<Vec<u8>, std::io::Error> {
    let mut decoder = ZlibDecoder::new(data);
    let mut result = Vec::new();
    decoder.read_to_end(&mut result)?;
    Ok(result)
}

/// Encode bytes to base64url (URL-safe base64 without padding).
///
/// Uses the URL-safe alphabet (+ → -, / → _) and strips padding.
///
/// # Examples
///
/// ```
/// use the_edge_agent::report::encoder::base64url_encode;
///
/// assert_eq!(base64url_encode(b"Hello"), "SGVsbG8");
/// assert_eq!(base64url_encode(b"Hello, world!"), "SGVsbG8sIHdvcmxkIQ");
/// ```
pub fn base64url_encode(data: &[u8]) -> String {
    URL_SAFE_NO_PAD.encode(data)
}

/// Decode base64url back to bytes.
///
/// Accepts base64url encoded strings with or without padding.
pub fn base64url_decode(data: &str) -> Result<Vec<u8>, base64::DecodeError> {
    URL_SAFE_NO_PAD.decode(data)
}

/// Truncate an ErrorReport to reduce URL size.
///
/// Progressive truncation strategy:
/// 1. Remove extended context
/// 2. Reduce stack to 10 frames
/// 3. Reduce stack to 5 frames
/// 4. Truncate message to 200 chars
/// 5. Reduce stack to 3 frames (minimum)
fn truncate_report(report: &ErrorReport, level: u8) -> ErrorReport {
    let mut truncated = report.clone();

    match level {
        1 => {
            // Level 1: Remove extended context
            truncated.extended = None;
        }
        2 => {
            // Level 2: Remove extended + reduce stack to 10
            truncated.extended = None;
            if truncated.stack.len() > 10 {
                truncated.stack.truncate(10);
            }
        }
        3 => {
            // Level 3: Remove extended + reduce stack to 5
            truncated.extended = None;
            if truncated.stack.len() > 5 {
                truncated.stack.truncate(5);
            }
        }
        4 => {
            // Level 4: Remove extended + reduce stack to 5 + truncate message
            truncated.extended = None;
            if truncated.stack.len() > 5 {
                truncated.stack.truncate(5);
            }
            if truncated.message.len() > 200 {
                truncated.message = format!("{}...", &truncated.message[..197]);
            }
        }
        _ => {
            // Level 5+: Minimum viable report
            truncated.extended = None;
            truncated.context = None;
            if truncated.stack.len() > 3 {
                truncated.stack.truncate(3);
            }
            if truncated.message.len() > 200 {
                truncated.message = format!("{}...", &truncated.message[..197]);
            }
        }
    }

    truncated
}

/// Encode an ErrorReport to a shareable URL.
///
/// # Arguments
///
/// * `report` - The ErrorReport to encode
/// * `base_url` - Base URL for the report viewer (e.g., `https://example.github.io/the_edge_agent/report`)
///
/// # Returns
///
/// A URL string that can be shared and decoded by the viewer.
///
/// # Examples
///
/// ```
/// use the_edge_agent::report::{ErrorReport, ErrorType};
/// use the_edge_agent::report::encoder::encode_error_report;
///
/// let report = ErrorReport::new(ErrorType::Panic, "Test error");
/// let url = encode_error_report(&report, "https://example.github.io/the_edge_agent/report").unwrap();
/// assert!(url.starts_with("https://example.github.io/the_edge_agent/report/"));
/// assert!(url.contains("/rust_"));
/// ```
pub fn encode_error_report(report: &ErrorReport, base_url: &str) -> Result<String, EncoderError> {
    encode_with_truncation(report, base_url, 0)
}

/// Internal function for recursive truncation
fn encode_with_truncation(
    report: &ErrorReport,
    base_url: &str,
    truncation_level: u8,
) -> Result<String, EncoderError> {
    // Apply truncation if needed
    let report_to_encode = if truncation_level > 0 {
        truncate_report(report, truncation_level)
    } else {
        report.clone()
    };

    // 1. Serialize to JSON (deterministic ordering via serde)
    let json_str = serde_json::to_string(&report_to_encode)?;
    let json_bytes = json_str.as_bytes();

    // 2. Deflate compress
    let compressed = deflate_compress(json_bytes)?;

    // 3. Base64url encode
    let encoded = base64url_encode(&compressed);

    // 4. Build URL
    let url = format!(
        "{}/{}/{}_{encoded}",
        base_url, report_to_encode.version, report_to_encode.runtime
    );

    // 5. Validate length
    if url.len() > MAX_URL_LENGTH {
        if truncation_level < 5 {
            // Truncate and retry
            return encode_with_truncation(report, base_url, truncation_level + 1);
        } else {
            return Err(EncoderError::UrlTooLong(url.len()));
        }
    }

    Ok(url)
}

/// Decode a URL back to an ErrorReport.
///
/// # Arguments
///
/// * `url` - The encoded URL to decode
///
/// # Returns
///
/// The decoded ErrorReport.
///
/// # Examples
///
/// ```
/// use the_edge_agent::report::{ErrorReport, ErrorType};
/// use the_edge_agent::report::encoder::{encode_error_report, decode_error_report};
///
/// let report = ErrorReport::new(ErrorType::Panic, "Test error");
/// let url = encode_error_report(&report, "https://example.github.io/the_edge_agent/report").unwrap();
/// let decoded = decode_error_report(&url).unwrap();
/// assert_eq!(decoded.message, "Test error");
/// ```
pub fn decode_error_report(url: &str) -> Result<ErrorReport, EncoderError> {
    // Parse URL: .../version/runtime_encoded
    let parts: Vec<&str> = url.rsplitn(3, '/').collect();

    if parts.len() < 2 {
        return Err(EncoderError::InvalidUrl("URL too short".to_string()));
    }

    let runtime_encoded = parts[0];

    // Split runtime_encoded on first underscore
    let underscore_pos = runtime_encoded
        .find('_')
        .ok_or_else(|| EncoderError::InvalidUrl("Missing runtime prefix".to_string()))?;

    let encoded = &runtime_encoded[underscore_pos + 1..];

    // Base64url decode
    let compressed = base64url_decode(encoded)?;

    // Inflate decompress
    let json_bytes = deflate_decompress(&compressed)?;

    // Parse JSON
    let json_str =
        std::str::from_utf8(&json_bytes).map_err(|e| EncoderError::Utf8Error(e.to_string()))?;

    let report: ErrorReport = serde_json::from_str(json_str)?;

    Ok(report)
}

/// Errors that can occur during encoding/decoding
#[derive(Debug)]
pub enum EncoderError {
    /// JSON serialization error
    JsonError(serde_json::Error),
    /// Compression error
    CompressionError(std::io::Error),
    /// Base64 decoding error
    Base64Error(base64::DecodeError),
    /// UTF-8 decoding error
    Utf8Error(String),
    /// Invalid URL format
    InvalidUrl(String),
    /// URL exceeds maximum length even after truncation
    UrlTooLong(usize),
}

impl std::fmt::Display for EncoderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EncoderError::JsonError(e) => write!(f, "JSON error: {}", e),
            EncoderError::CompressionError(e) => write!(f, "Compression error: {}", e),
            EncoderError::Base64Error(e) => write!(f, "Base64 error: {}", e),
            EncoderError::Utf8Error(e) => write!(f, "UTF-8 error: {}", e),
            EncoderError::InvalidUrl(e) => write!(f, "Invalid URL: {}", e),
            EncoderError::UrlTooLong(len) => {
                write!(f, "URL too long ({} chars, max {})", len, MAX_URL_LENGTH)
            }
        }
    }
}

impl std::error::Error for EncoderError {}

impl From<serde_json::Error> for EncoderError {
    fn from(e: serde_json::Error) -> Self {
        EncoderError::JsonError(e)
    }
}

impl From<std::io::Error> for EncoderError {
    fn from(e: std::io::Error) -> Self {
        EncoderError::CompressionError(e)
    }
}

impl From<base64::DecodeError> for EncoderError {
    fn from(e: base64::DecodeError) -> Self {
        EncoderError::Base64Error(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::report::{ErrorContext, ErrorType, StackFrame};

    // ========== VLQ Tests (Task 4.1) ==========

    #[test]
    fn test_vlq_encode_zero() {
        assert_eq!(vlq_encode(0), vec![0]);
    }

    #[test]
    fn test_vlq_encode_single_byte_max() {
        // 127 is the max single-byte value (0x7F)
        assert_eq!(vlq_encode(127), vec![127]);
    }

    #[test]
    fn test_vlq_encode_two_byte_min() {
        // 128 requires two bytes: 0x80, 0x01
        assert_eq!(vlq_encode(128), vec![128, 1]);
    }

    #[test]
    fn test_vlq_encode_two_byte_max() {
        // 16383 = 0x3FFF, encoded as 0xFF, 0x7F
        assert_eq!(vlq_encode(16383), vec![255, 127]);
    }

    #[test]
    fn test_vlq_encode_three_byte_min() {
        // 16384 = 0x4000, encoded as 0x80, 0x80, 0x01
        assert_eq!(vlq_encode(16384), vec![128, 128, 1]);
    }

    #[test]
    fn test_vlq_encode_u64_max() {
        // u64::MAX requires 10 bytes
        let encoded = vlq_encode(u64::MAX);
        assert_eq!(encoded.len(), 10);
        // Verify round-trip
        let (decoded, consumed) = vlq_decode(&encoded);
        assert_eq!(decoded, u64::MAX);
        assert_eq!(consumed, 10);
    }

    #[test]
    fn test_vlq_decode_zero() {
        let (value, consumed) = vlq_decode(&[0]);
        assert_eq!(value, 0);
        assert_eq!(consumed, 1);
    }

    #[test]
    fn test_vlq_decode_two_byte() {
        let (value, consumed) = vlq_decode(&[128, 1]);
        assert_eq!(value, 128);
        assert_eq!(consumed, 2);
    }

    #[test]
    fn test_vlq_roundtrip() {
        let test_values = [0, 1, 127, 128, 255, 256, 16383, 16384, 1000000, u64::MAX];
        for &val in &test_values {
            let encoded = vlq_encode(val);
            let (decoded, _) = vlq_decode(&encoded);
            assert_eq!(decoded, val, "VLQ round-trip failed for {}", val);
        }
    }

    // ========== Compression Tests (Task 4.2) ==========

    #[test]
    fn test_deflate_roundtrip() {
        let data = b"Hello, world! This is a test message for compression.";
        let compressed = deflate_compress(data).unwrap();
        let decompressed = deflate_decompress(&compressed).unwrap();
        assert_eq!(data.as_slice(), decompressed.as_slice());
    }

    #[test]
    fn test_deflate_empty() {
        let data = b"";
        let compressed = deflate_compress(data).unwrap();
        let decompressed = deflate_decompress(&compressed).unwrap();
        assert_eq!(data.as_slice(), decompressed.as_slice());
    }

    #[test]
    fn test_deflate_utf8() {
        let data = "Hello, 世界! 🦀 Rust is awesome!".as_bytes();
        let compressed = deflate_compress(data).unwrap();
        let decompressed = deflate_decompress(&compressed).unwrap();
        assert_eq!(data, decompressed.as_slice());
    }

    // ========== Base64url Tests (Task 4.3) ==========

    #[test]
    fn test_base64url_encode() {
        assert_eq!(base64url_encode(b"Hello"), "SGVsbG8");
        assert_eq!(base64url_encode(b"Hello, world!"), "SGVsbG8sIHdvcmxkIQ");
    }

    #[test]
    fn test_base64url_no_plus_or_slash() {
        // Test data that would produce + and / in standard base64
        let data = vec![0xfb, 0xef, 0xbe]; // Would produce ++++ in standard base64
        let encoded = base64url_encode(&data);
        assert!(!encoded.contains('+'), "Base64url should not contain +");
        assert!(!encoded.contains('/'), "Base64url should not contain /");
    }

    #[test]
    fn test_base64url_no_padding() {
        // Test data that would require padding
        let data = b"a"; // Would normally be "YQ==" with padding
        let encoded = base64url_encode(data);
        assert!(
            !encoded.contains('='),
            "Base64url should not contain padding"
        );
    }

    #[test]
    fn test_base64url_roundtrip() {
        let test_data = [
            b"Hello".to_vec(),
            b"Hello, world!".to_vec(),
            vec![0, 1, 2, 3, 255],
            vec![],
        ];
        for data in &test_data {
            let encoded = base64url_encode(data);
            let decoded = base64url_decode(&encoded).unwrap();
            assert_eq!(data, &decoded);
        }
    }

    // ========== Integration Tests (Task 4.4) ==========

    #[test]
    fn test_encode_decode_simple_report() {
        let report = ErrorReport::new(ErrorType::Panic, "Test error message");

        let url = encode_error_report(&report, DEFAULT_BASE_URL).unwrap();

        // Verify URL format
        assert!(url.starts_with(DEFAULT_BASE_URL));
        assert!(url.contains("/rust_"));

        // Decode and verify
        let decoded = decode_error_report(&url).unwrap();
        assert_eq!(decoded.message, "Test error message");
        assert_eq!(decoded.error_type, ErrorType::Panic);
        assert_eq!(decoded.runtime, "rust");
    }

    #[test]
    fn test_encode_decode_with_stack() {
        let report = ErrorReport::new(ErrorType::YamlError, "YAML parse error")
            .with_stack_frame(StackFrame::new(12345).with_symbol("main").with_line(42))
            .with_stack_frame(
                StackFrame::new(67890)
                    .with_symbol("process")
                    .with_file("src/main.rs")
                    .with_line(100),
            );

        let url = encode_error_report(&report, DEFAULT_BASE_URL).unwrap();
        let decoded = decode_error_report(&url).unwrap();

        assert_eq!(decoded.stack.len(), 2);
        assert_eq!(decoded.stack[0].addr, 12345);
        assert_eq!(decoded.stack[0].symbol, Some("main".to_string()));
        assert_eq!(decoded.stack[1].file, Some("src/main.rs".to_string()));
    }

    #[test]
    fn test_encode_decode_with_context() {
        let report = ErrorReport::new(ErrorType::ActionError, "Action failed").with_context(
            ErrorContext::new()
                .with_node_name("process_data")
                .with_action_type("http.get"),
        );

        let url = encode_error_report(&report, DEFAULT_BASE_URL).unwrap();
        let decoded = decode_error_report(&url).unwrap();

        assert!(decoded.context.is_some());
        let ctx = decoded.context.unwrap();
        assert_eq!(ctx.node_name, Some("process_data".to_string()));
        assert_eq!(ctx.action_type, Some("http.get".to_string()));
    }

    #[test]
    fn test_url_length_under_limit() {
        let report = ErrorReport::new(ErrorType::Panic, "Short message");
        let url = encode_error_report(&report, DEFAULT_BASE_URL).unwrap();
        assert!(
            url.len() <= MAX_URL_LENGTH,
            "URL length {} exceeds max {}",
            url.len(),
            MAX_URL_LENGTH
        );
    }

    #[test]
    fn test_truncation_for_long_report() {
        // Create a report with many stack frames
        let mut report = ErrorReport::new(ErrorType::Panic, "A".repeat(500)); // Long message

        // Add 50 stack frames
        for i in 0..50 {
            report = report.with_stack_frame(
                StackFrame::new(i as u64)
                    .with_symbol(format!("function_{}", i))
                    .with_file(format!("src/module{}/file{}.rs", i, i))
                    .with_line((i * 10) as u32),
            );
        }

        let url = encode_error_report(&report, DEFAULT_BASE_URL).unwrap();

        // URL should be within limits after truncation
        assert!(
            url.len() <= MAX_URL_LENGTH,
            "URL length {} exceeds max {}",
            url.len(),
            MAX_URL_LENGTH
        );

        // Decode should still work
        let decoded = decode_error_report(&url).unwrap();
        assert!(!decoded.message.is_empty());
    }

    #[test]
    fn test_invalid_url_error() {
        let result = decode_error_report("not-a-valid-url");
        assert!(result.is_err());
    }

    #[test]
    fn test_missing_runtime_prefix_error() {
        let result = decode_error_report("https://example.com/report/0.9.34/invaliddata");
        assert!(result.is_err());
    }
}
