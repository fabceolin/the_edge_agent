"""URL Encoder Module - TEA-REPORT-001b

Provides URL encoding for ErrorReport to create shareable bug report URLs.
Uses VLQ encoding for numbers, deflate compression, and base64url encoding.

URL Format:
    https://{org}.github.io/the_edge_agent/report/{version}/{runtime}_{encoded_data}

Parity:
    This implementation is designed to produce identical output to the Rust
    implementation for the same input, enabling cross-runtime bug report sharing.

Example:
    >>> from the_edge_agent.report import ErrorReport, ErrorType
    >>> from the_edge_agent.report_encoder import encode_error_report, decode_error_report
    >>> report = ErrorReport(error_type=ErrorType.PANIC, message="Test error")
    >>> url = encode_error_report(report, "https://example.github.io/the_edge_agent/report")
    >>> decoded = decode_error_report(url)
    >>> decoded.message
    'Test error'
"""

import base64
import json
import zlib
from typing import Any, Dict, Tuple

from .report import ErrorReport, ErrorContext, StackFrame, ExtendedContext, ErrorType

# Maximum URL length for browser compatibility
MAX_URL_LENGTH: int = 2000

# Default base URL for bug reports
DEFAULT_BASE_URL: str = "https://example.github.io/the_edge_agent/report"


def vlq_encode(value: int) -> bytes:
    """Encode an integer to Variable-Length Quantity (VLQ) bytes.

    VLQ encoding uses 7 bits per byte, with the high bit indicating
    whether more bytes follow.

    Args:
        value: Non-negative integer to encode

    Returns:
        VLQ encoded bytes

    Examples:
        >>> vlq_encode(0)
        b'\\x00'
        >>> vlq_encode(127)
        b'\\x7f'
        >>> vlq_encode(128)
        b'\\x80\\x01'
        >>> vlq_encode(16383)
        b'\\xff\\x7f'
        >>> vlq_encode(16384)
        b'\\x80\\x80\\x01'
    """
    result = bytearray()
    while True:
        byte = value & 0x7F
        value >>= 7
        if value != 0:
            byte |= 0x80
        result.append(byte)
        if value == 0:
            break
    return bytes(result)


def vlq_decode(data: bytes) -> Tuple[int, int]:
    """Decode VLQ bytes back to an integer.

    Args:
        data: VLQ encoded bytes

    Returns:
        Tuple of (decoded_value, bytes_consumed)

    Examples:
        >>> vlq_decode(b'\\x00')
        (0, 1)
        >>> vlq_decode(b'\\x7f')
        (127, 1)
        >>> vlq_decode(b'\\x80\\x01')
        (128, 2)
        >>> vlq_decode(b'\\xff\\x7f')
        (16383, 2)
    """
    value = 0
    shift = 0
    consumed = 0
    for byte in data:
        consumed += 1
        value |= (byte & 0x7F) << shift
        if byte & 0x80 == 0:
            break
        shift += 7
    return value, consumed


def deflate_compress(data: bytes) -> bytes:
    """Compress data using deflate (zlib) with maximum compression.

    Uses compression level 9 for maximum compression ratio.

    Args:
        data: Bytes to compress

    Returns:
        Compressed bytes

    Examples:
        >>> data = b"Hello, world!"
        >>> compressed = deflate_compress(data)
        >>> deflate_decompress(compressed) == data
        True
    """
    return zlib.compress(data, level=9)


def deflate_decompress(data: bytes) -> bytes:
    """Decompress deflate (zlib) compressed data.

    Args:
        data: Compressed bytes

    Returns:
        Decompressed bytes
    """
    return zlib.decompress(data)


def base64url_encode(data: bytes) -> str:
    """Encode bytes to base64url (URL-safe base64 without padding).

    Uses the URL-safe alphabet (+ -> -, / -> _) and strips padding.

    Args:
        data: Bytes to encode

    Returns:
        Base64url encoded string (no padding)

    Examples:
        >>> base64url_encode(b"Hello")
        'SGVsbG8'
        >>> base64url_encode(b"Hello, world!")
        'SGVsbG8sIHdvcmxkIQ'
    """
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode("ascii")


def base64url_decode(data: str) -> bytes:
    """Decode base64url back to bytes.

    Accepts base64url encoded strings with or without padding.

    Args:
        data: Base64url encoded string

    Returns:
        Decoded bytes
    """
    # Add padding back
    padding = 4 - (len(data) % 4)
    if padding != 4:
        data += "=" * padding
    return base64.urlsafe_b64decode(data)


def _report_to_dict(report: ErrorReport) -> Dict[str, Any]:
    """Convert ErrorReport to a dictionary for JSON serialization.

    Matches Rust's serde JSON output for parity.
    Field order matches Rust struct declaration order for byte-identical JSON.
    """
    from collections import OrderedDict

    def stack_frame_to_dict(frame: StackFrame) -> Dict[str, Any]:
        """Convert StackFrame in Rust's field order."""
        result = OrderedDict()
        result["addr"] = frame.addr
        if frame.symbol is not None:
            result["symbol"] = frame.symbol
        if frame.file is not None:
            result["file"] = frame.file
        if frame.line is not None:
            result["line"] = frame.line
        return result

    def context_to_dict(ctx: ErrorContext) -> Dict[str, Any]:
        """Convert ErrorContext in Rust's field order."""
        result = OrderedDict()
        if ctx.node_name is not None:
            result["node_name"] = ctx.node_name
        if ctx.action_type is not None:
            result["action_type"] = ctx.action_type
        if ctx.checkpoint_id is not None:
            result["checkpoint_id"] = ctx.checkpoint_id
        return result

    def node_info_to_dict(node) -> Dict[str, Any]:
        """Convert NodeInfo in Rust's field order."""
        result = OrderedDict()
        result["name"] = node.name
        if node.action_type is not None:
            result["action_type"] = node.action_type
        return result

    def edge_info_to_dict(edge) -> Dict[str, Any]:
        """Convert EdgeInfo in Rust's field order."""
        result = OrderedDict()
        result["from"] = edge.from_node
        result["to"] = edge.to
        if edge.edge_type is not None:
            result["edge_type"] = edge.edge_type
        return result

    def extended_to_dict(ext: ExtendedContext) -> Dict[str, Any]:
        """Convert ExtendedContext in Rust's field order."""
        result = OrderedDict()
        if ext.workflow_name is not None:
            result["workflow_name"] = ext.workflow_name
        result["nodes"] = [node_info_to_dict(n) for n in ext.nodes]
        result["edges"] = [edge_info_to_dict(e) for e in ext.edges]
        result["schema_fields"] = ext.schema_fields
        if ext.active_node is not None:
            result["active_node"] = ext.active_node
        if ext.active_action is not None:
            result["active_action"] = ext.active_action
        return result

    # Build result in Rust's struct field order
    result = OrderedDict()
    result["version"] = report.version
    result["platform"] = report.platform
    result["runtime"] = report.runtime
    result["error_type"] = report.error_type.value
    result["message"] = report.message
    result["stack"] = [stack_frame_to_dict(f) for f in report.stack]
    if report.context is not None and not report.context.is_empty():
        result["context"] = context_to_dict(report.context)
    if report.extended is not None:
        result["extended"] = extended_to_dict(report.extended)

    return result


def _truncate_report(report: ErrorReport, level: int) -> ErrorReport:
    """Truncate an ErrorReport to reduce URL size.

    Progressive truncation strategy:
    1. Remove extended context
    2. Reduce stack to 10 frames
    3. Reduce stack to 5 frames
    4. Truncate message to 200 chars
    5. Reduce stack to 3 frames (minimum)

    Args:
        report: The ErrorReport to truncate
        level: Truncation level (1-5)

    Returns:
        Truncated ErrorReport
    """
    # Create a new report with the same data
    truncated = ErrorReport(
        version=report.version,
        platform=report.platform,
        runtime=report.runtime,
        error_type=report.error_type,
        message=report.message,
        stack=list(report.stack),  # Copy the list
        context=report.context,
        extended=report.extended,
    )

    if level >= 1:
        # Level 1: Remove extended context
        truncated.extended = None

    if level >= 2:
        # Level 2: Reduce stack to 10
        if len(truncated.stack) > 10:
            truncated.stack = truncated.stack[:10]

    if level >= 3:
        # Level 3: Reduce stack to 5
        if len(truncated.stack) > 5:
            truncated.stack = truncated.stack[:5]

    if level >= 4:
        # Level 4: Truncate message to 200 chars
        if len(truncated.message) > 200:
            truncated.message = truncated.message[:197] + "..."

    if level >= 5:
        # Level 5: Minimum viable report
        truncated.context = None
        if len(truncated.stack) > 3:
            truncated.stack = truncated.stack[:3]

    return truncated


def encode_error_report(report: ErrorReport, base_url: str = DEFAULT_BASE_URL) -> str:
    """Encode an ErrorReport to a shareable URL.

    Args:
        report: The ErrorReport to encode
        base_url: Base URL for the report viewer

    Returns:
        A URL string that can be shared and decoded by the viewer

    Raises:
        EncoderError: If encoding fails or URL is too long even after truncation

    Examples:
        >>> from the_edge_agent.report import ErrorReport, ErrorType
        >>> report = ErrorReport(error_type=ErrorType.PANIC, message="Test")
        >>> url = encode_error_report(report)
        >>> url.startswith("https://example.github.io")
        True
    """
    return _encode_with_truncation(report, base_url, 0)


def _encode_with_truncation(
    report: ErrorReport, base_url: str, truncation_level: int
) -> str:
    """Internal function for recursive truncation."""
    # Apply truncation if needed
    report_to_encode = (
        _truncate_report(report, truncation_level) if truncation_level > 0 else report
    )

    # 1. Serialize to JSON (field order matches Rust struct order for parity)
    report_dict = _report_to_dict(report_to_encode)
    json_str = json.dumps(report_dict, separators=(",", ":"))
    json_bytes = json_str.encode("utf-8")

    # 2. Deflate compress (level 9)
    compressed = deflate_compress(json_bytes)

    # 3. Base64url encode (no padding)
    encoded = base64url_encode(compressed)

    # 4. Build URL
    url = f"{base_url}/{report_to_encode.version}/{report_to_encode.runtime}_{encoded}"

    # 5. Validate length
    if len(url) > MAX_URL_LENGTH:
        if truncation_level < 5:
            # Truncate and retry
            return _encode_with_truncation(report, base_url, truncation_level + 1)
        else:
            raise EncoderError(f"URL too long ({len(url)} chars, max {MAX_URL_LENGTH})")

    return url


def decode_error_report(url: str) -> ErrorReport:
    """Decode a URL back to an ErrorReport.

    Args:
        url: The encoded URL to decode

    Returns:
        The decoded ErrorReport

    Raises:
        EncoderError: If decoding fails

    Examples:
        >>> from the_edge_agent.report import ErrorReport, ErrorType
        >>> report = ErrorReport(error_type=ErrorType.PANIC, message="Test error")
        >>> url = encode_error_report(report)
        >>> decoded = decode_error_report(url)
        >>> decoded.message
        'Test error'
    """
    # Parse URL: .../version/runtime_encoded
    parts = url.rsplit("/", 2)

    if len(parts) < 2:
        raise EncoderError("URL too short")

    runtime_encoded = parts[-1]

    # Split runtime_encoded on first underscore
    underscore_pos = runtime_encoded.find("_")
    if underscore_pos < 0:
        raise EncoderError("Missing runtime prefix")

    encoded = runtime_encoded[underscore_pos + 1 :]

    try:
        # Base64url decode
        compressed = base64url_decode(encoded)

        # Inflate decompress
        json_bytes = deflate_decompress(compressed)

        # Parse JSON
        data = json.loads(json_bytes.decode("utf-8"))

        # Convert dict to ErrorReport
        return _dict_to_report(data)
    except Exception as e:
        raise EncoderError(f"Decoding failed: {e}") from e


def _dict_to_report(data: Dict[str, Any]) -> ErrorReport:
    """Convert a dictionary back to an ErrorReport."""
    # Convert error_type string back to enum
    error_type_str = data.get("error_type", "Panic")
    error_type = ErrorType(error_type_str)

    # Convert stack frames
    stack = []
    for frame_data in data.get("stack", []):
        stack.append(
            StackFrame(
                addr=frame_data.get("addr", 0),
                symbol=frame_data.get("symbol"),
                file=frame_data.get("file"),
                line=frame_data.get("line"),
            )
        )

    # Convert context
    context = None
    if data.get("context"):
        ctx_data = data["context"]
        context = ErrorContext(
            node_name=ctx_data.get("node_name"),
            action_type=ctx_data.get("action_type"),
            checkpoint_id=ctx_data.get("checkpoint_id"),
        )

    # Convert extended context
    extended = None
    if data.get("extended"):
        ext_data = data["extended"]
        from .report import NodeInfo, EdgeInfo

        extended = ExtendedContext(
            workflow_name=ext_data.get("workflow_name"),
            nodes=[
                NodeInfo(name=n.get("name", ""), action_type=n.get("action_type"))
                for n in ext_data.get("nodes", [])
            ],
            edges=[
                EdgeInfo(
                    from_node=e.get("from", ""),
                    to=e.get("to", ""),
                    edge_type=e.get("edge_type"),
                )
                for e in ext_data.get("edges", [])
            ],
            schema_fields=ext_data.get("schema_fields", []),
            active_node=ext_data.get("active_node"),
            active_action=ext_data.get("active_action"),
        )

    return ErrorReport(
        version=data.get("version", ""),
        platform=data.get("platform", ""),
        runtime=data.get("runtime", "python"),
        error_type=error_type,
        message=data.get("message", ""),
        stack=stack,
        context=context,
        extended=extended,
    )


class EncoderError(Exception):
    """Error during encoding or decoding."""

    pass


# CLI entry point for testing parity
if __name__ == "__main__":
    import sys

    if len(sys.argv) < 2:
        print("Usage: python -m the_edge_agent.report_encoder <json>", file=sys.stderr)
        sys.exit(1)

    json_str = sys.argv[1]
    data = json.loads(json_str)
    report = _dict_to_report(data)
    url = encode_error_report(report, DEFAULT_BASE_URL)
    print(url)
