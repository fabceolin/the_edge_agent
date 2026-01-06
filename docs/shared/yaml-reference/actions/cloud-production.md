# Cloud Production Actions

> **Parent document:** [Actions Index](./README.md)
> **Epic:** [TEA-BUILTIN-015 Cloud Production](../../../stories/TEA-BUILTIN-015-cloud-production-epic.md)

## Overview

Cloud production actions provide enterprise-grade capabilities for deploying YAML agents as production services. These actions cover session management, database operations, authentication, validation, response handling, and error management.

**Epic Stories:**
- TEA-BUILTIN-015.1: Session Management
- TEA-BUILTIN-015.2: Firestore CRUD Operations
- TEA-BUILTIN-015.3: Authentication Middleware
- TEA-BUILTIN-015.4: Input Validation
- TEA-BUILTIN-015.5: Response Transformation
- TEA-BUILTIN-015.6: Error Handling

---

## Actions Summary

| Category | Actions |
|----------|---------|
| [Session](#session-actions) | `session.load`, `session.save`, `session.delete`, `session.exists` |
| [Firestore](#firestore-actions) | `firestore.get`, `firestore.set`, `firestore.query`, `firestore.delete`, `firestore.batch` |
| [Authentication](#authentication-actions) | `auth.verify`, `auth.get_user` |
| [Validation](#validation-actions) | `validate.input`, `validate.schema` |
| [HTTP Response](#http-response-actions) | `http.respond` |
| [Error Handling](#error-handling-actions) | `error.is_retryable`, `error.clear`, `error.get`, `error.has`, `error.type`, `error.retry`, `error.respond` |

---

## Session Actions

> **Story:** TEA-BUILTIN-015.1 Session Management

Session actions enable stateful conversations by persisting and loading session data across agent executions.

### Configuration

```yaml
settings:
  session:
    backend: firestore  # firestore | redis | memory
    ttl: 3600           # Session TTL in seconds
    persist_fields:     # Default fields to persist
      - conversation_history
      - user_context
```

### session.load

Load session data by ID into state.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `session_id` | string | No | `state.session_id` | Session ID to load |
| `default` | any | No | `{}` | Default value if not found |

#### Example

```yaml
- name: load_context
  uses: session.load
  with:
    session_id: "{{ state.session_id }}"
    default:
      conversation_history: []
      turn_count: 0
  output: session_data
```

---

### session.save

Persist current state to session backend.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `session_id` | string | No | `state.session_id` | Session ID |
| `fields` | list | No | From settings | Fields to persist |
| `ttl` | int | No | From settings | TTL in seconds |

#### Example

```yaml
- name: save_progress
  uses: session.save
  with:
    session_id: "{{ state.session_id }}"
    fields:
      - conversation_history
      - user_context
      - last_intent
    ttl: 7200
```

---

### session.delete

Delete a session from the backend.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `session_id` | string | No | `state.session_id` | Session ID to delete |

---

### session.exists

Check if a session exists.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `session_id` | string | No | `state.session_id` | Session ID to check |

#### Returns

```yaml
{
  "exists": true  # or false
}
```

---

## Firestore Actions

> **Story:** TEA-BUILTIN-015.2 Firestore CRUD Operations
> **Dependency:** `pip install firebase-admin`

Firestore actions provide CRUD operations for Google Firestore.

### Configuration

```yaml
settings:
  firestore:
    project: "${FIREBASE_PROJECT_ID}"
    emulator_host: "${FIRESTORE_EMULATOR_HOST:-}"
```

### firestore.get

Retrieve a document by ID.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `collection` | string | Yes | - | Collection name (supports nested paths) |
| `document` | string | Yes | - | Document ID |
| `default` | any | No | `null` | Default if not found |

#### Example

```yaml
- name: get_user
  uses: firestore.get
  with:
    collection: "users"
    document: "{{ state.user_id }}"
    default: {name: "Unknown", active: false}
  output: user_data
```

#### Returns

```yaml
{
  "success": true,
  "data": {"name": "John", "email": "john@example.com"},
  "exists": true,
  "doc_id": "user123",
  "path": "users/user123"
}
```

---

### firestore.set

Create or update a document.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `collection` | string | Yes | - | Collection name |
| `data` | object | Yes | - | Document data |
| `document` | string | No | Auto-generated UUID | Document ID |
| `merge` | bool | No | `false` | Merge with existing doc |

#### Example

```yaml
- name: save_result
  uses: firestore.set
  with:
    collection: "results"
    document: "{{ state.session_id }}"
    data:
      answer: "{{ state.answer }}"
      timestamp: "{{ now() }}"
      model: "gpt-4"
    merge: true
  output: doc_ref
```

---

### firestore.query

Query documents with filters.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `collection` | string | Yes | - | Collection name |
| `where` | list | No | - | Filter conditions |
| `order_by` | string/list | No | - | Field(s) to order by |
| `limit` | int | No | `100` | Max documents |
| `offset` | int | No | `0` | Documents to skip |

#### Where Clause Format

```yaml
where:
  - field: user_id
    op: "=="        # ==, !=, <, <=, >, >=, in, not-in, array-contains
    value: "{{ state.user_id }}"
```

#### Example

```yaml
- name: get_history
  uses: firestore.query
  with:
    collection: "conversations"
    where:
      - field: user_id
        op: "=="
        value: "{{ state.user_id }}"
      - field: created_at
        op: ">="
        value: "{{ state.since_date }}"
    order_by: "-created_at"  # Prefix with - for descending
    limit: 10
  output: history
```

---

### firestore.delete

Delete a document.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `collection` | string | Yes | - | Collection name |
| `document` | string | Yes | - | Document ID |

---

### firestore.batch

Execute multiple operations atomically.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `operations` | list | Yes | - | List of operations |

#### Operation Format

```yaml
operations:
  - type: set
    collection: "users"
    document: "{{ state.user_id }}"
    data: {last_active: "{{ now() }}"}
    merge: true
  - type: delete
    collection: "temp"
    document: "{{ state.temp_id }}"
```

---

## Authentication Actions

> **Story:** TEA-BUILTIN-015.3 Auth Middleware

Authentication actions provide explicit token verification within workflows.

### Configuration

See [YAML_REFERENCE.md](../../YAML_REFERENCE.md#settingsauth---authentication-tea-builtin-0153) for full `settings.auth` configuration.

### auth.verify

Verify an authentication token.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `token` | string | No | From headers | Token to verify |
| `headers` | object | No | - | Headers for token extraction |
| `provider` | string | No | From settings | Provider override |

#### Example

```yaml
- name: verify_custom_token
  uses: auth.verify
  with:
    token: "{{ state.custom_token }}"
  output: auth_result

- name: check_auth
  run: |
    if state["auth_result"]["success"]:
        return {"user_id": state["auth_result"]["user"]["uid"]}
    else:
        return {"error": state["auth_result"]["error"]}
```

#### Returns

```yaml
# Success
{
  "success": true,
  "user": {
    "uid": "user123",
    "email": "user@example.com",
    "name": "John Doe"
  },
  "error": null
}

# Failure
{
  "success": false,
  "user": null,
  "error": "Token expired"
}
```

---

### auth.get_user

Get full user profile by UID.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `uid` | string | No | `state.__user__.uid` | User ID |

#### Example

```yaml
- name: get_full_profile
  uses: auth.get_user
  with:
    uid: "{{ state.__user__.uid }}"
  output: full_profile
```

---

## Validation Actions

> **Story:** TEA-BUILTIN-015.4 Input Validation

Validation actions provide explicit mid-flow validation.

### validate.input

Validate data against a schema.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `data` | object | No | Entire state | Data to validate |
| `schema` | object | Yes | - | Validation schema |
| `raise_on_error` | bool | No | `false` | Raise exception on failure |

#### Schema Format

```yaml
schema:
  name:
    type: str
    required: true
    min_length: 1
  email:
    type: str
    pattern: "^[\\w.-]+@[\\w.-]+\\.\\w+$"
  age:
    type: int
    min: 0
    max: 150
    default: 0
```

#### Example

```yaml
- name: validate_user_input
  uses: validate.input
  with:
    data: "{{ state.user_data }}"
    schema:
      name:
        type: str
        required: true
      email:
        type: str
        pattern: "^[\\w.-]+@[\\w.-]+\\.\\w+$"
      age:
        type: int
        min: 0
        max: 150
  output: validation

- name: check_validation
  goto:
    - if: "state['validation']['valid']"
      to: process_input
    - to: handle_validation_error
```

#### Returns

```yaml
# Valid
{
  "valid": true,
  "data": {"name": "John", "email": "john@test.com", "age": 25}
}

# Invalid
{
  "valid": false,
  "errors": [
    {
      "field": "email",
      "error": "pattern",
      "message": "Field 'email' must match pattern..."
    }
  ]
}
```

---

## HTTP Response Actions

> **Story:** TEA-BUILTIN-015.5 Response Transformation

HTTP response actions enable custom responses and early termination.

### http.respond

Send custom HTTP response and terminate workflow.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `status` | int | No | `200` | HTTP status code |
| `body` | any | No | - | Response body |
| `headers` | object | No | `{}` | Response headers |
| `content_type` | string | No | `"application/json"` | Content-Type |

#### Example

```yaml
- name: unauthorized_response
  uses: http.respond
  with:
    status: 401
    body:
      error: "unauthorized"
      message: "Invalid or expired token"
    headers:
      WWW-Authenticate: "Bearer"

- name: rate_limited
  uses: http.respond
  with:
    status: 429
    body:
      error: "rate_limited"
      retry_after: 60
    headers:
      Retry-After: "60"
```

---

## Error Handling Actions

> **Story:** TEA-BUILTIN-015.6 Error Handling

Error handling actions provide fine-grained control over error management.

### error.has

Check if there is an error in state.

```yaml
- name: check_error
  uses: error.has
  output: has_error

- name: route
  goto:
    - if: "state['has_error']"
      to: error_handler
    - to: continue_flow
```

### error.get

Get full error info from state.

```yaml
- name: get_error
  uses: error.get
  output: current_error

- name: log_error
  run: |
    if state.get("current_error"):
        print(f"Error: {state['current_error']['type']}")
        print(f"Message: {state['current_error']['message']}")
```

### error.type

Get just the error type.

```yaml
- name: get_type
  uses: error.type
  output: err_type

- name: route_by_type
  goto:
    - if: "state['err_type'] == 'TimeoutError'"
      to: handle_timeout
    - if: "state['err_type'] == 'RateLimitError'"
      to: handle_rate_limit
    - to: handle_generic
```

### error.is_retryable

Check if current error is retryable.

```yaml
- name: check_retry
  uses: error.is_retryable
  output: can_retry

- name: maybe_retry
  goto:
    - if: "state['can_retry']"
      to: retry_action
    - to: fail_gracefully
```

### error.clear

Clear error from state.

```yaml
- name: clear_error
  uses: error.clear
  # __error__ is now None

- name: continue_flow
  run: |
    return {"status": "recovered"}
```

### error.retry

Retry the last failed action.

#### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `max_attempts` | int | No | `1` | Max retry attempts |

```yaml
- name: retry_failed
  uses: error.retry
  with:
    max_attempts: 2
  output: retry_result
```

### error.respond

Convert error to HTTP response format.

```yaml
- name: error_to_http
  uses: error.respond
  output: http_error

- name: send_error_response
  uses: http.respond
  with:
    status: "{{ state.http_error.status }}"
    body: "{{ state.http_error.body }}"
```

---

## Complete Example: Production Chat API

```yaml
name: production-chat-api
description: Production-ready chat API with full cloud integration

settings:
  auth:
    provider: firebase
    required: true
    inject_user: true

  session:
    backend: firestore
    ttl: 3600
    persist_fields:
      - conversation_history
      - user_context

  firestore:
    project: "${FIREBASE_PROJECT_ID}"

input_schema:
  message:
    type: str
    required: true
    min_length: 1
    max_length: 4000
  session_id:
    type: str
    pattern: "^[a-f0-9-]{36}$"

endpoint:
  path: "/api/v1/chat"
  method: POST

nodes:
  # Load existing session
  - name: load_session
    uses: session.load
    with:
      session_id: "{{ state.session_id }}"
      default:
        conversation_history: []
        turn_count: 0
    output: session

  # Update conversation history
  - name: update_history
    run: |
      history = state["session"].get("conversation_history", [])
      history.append({"role": "user", "content": state["message"]})
      return {"conversation_history": history}

  # Call LLM
  - name: generate_response
    uses: llm.call
    with:
      model: gpt-4
      messages: "{{ state.conversation_history }}"
    output: llm_response

  # Handle LLM errors
  - name: check_llm_error
    uses: error.has
    output: has_llm_error
    goto:
      - if: "state['has_llm_error']"
        to: handle_error
      - to: save_response

  # Save assistant response
  - name: save_response
    run: |
      history = state["conversation_history"]
      history.append({"role": "assistant", "content": state["llm_response"]["content"]})
      return {
          "conversation_history": history,
          "response": state["llm_response"]["content"]
      }

  # Persist to session
  - name: save_session
    uses: session.save
    with:
      session_id: "{{ state.session_id }}"
      fields:
        - conversation_history

  # Log to Firestore
  - name: log_conversation
    uses: firestore.set
    with:
      collection: "conversations"
      data:
        user_id: "{{ state.__user__.uid }}"
        session_id: "{{ state.session_id }}"
        message: "{{ state.message }}"
        response: "{{ state.response }}"
        timestamp: "{{ now() }}"

  # Error handler
  - name: handle_error
    uses: error.is_retryable
    output: can_retry
    goto:
      - if: "state['can_retry']"
        to: retry_llm
      - to: error_response

  - name: retry_llm
    uses: error.retry
    with:
      max_attempts: 2
    goto:
      - if: "state.get('__retry_success__')"
        to: save_response
      - to: error_response

  - name: error_response
    uses: http.respond
    with:
      status: 500
      body:
        error: "processing_error"
        message: "Failed to generate response"
```

---

## See Also

- [YAML Reference - Settings](../../YAML_REFERENCE.md#settings-optional) - Full settings configuration
- [Specialized Actions](./specialized.md) - Rate limiting, checkpoints
- [LLM Actions](./llm.md) - LLM integration
