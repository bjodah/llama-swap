# llama-swap.el

An Emacs client for the [llama-swap](https://github.com/mostlygeek/llama-swap)
transparent proxy server.  Provides a real-time model dashboard, log viewers,
an activity table with request capture inspection, and a streaming chat
playground — all driven by the `/api/events` SSE stream.

## Features

- **Model dashboard** (`llama-swap`) — tabulated list of all configured models
  with real-time state, load/unload controls, alias/peer/description columns
- **Proxy & upstream logs** — live-updating log buffers refreshed by SSE events
- **Activity table** — recent requests with token counts, speed, duration, and
  optional capture viewer (request/response bodies, headers)
- **Chat playground** — streaming chat completions with `reasoning_content`
  support (Qwen3 think-mode)
- **Auth** — Bearer token via explicit `defcustom`, `auth-source`, or none
- **Auto-reconnect** — exponential back-off SSE reconnection (1 s → 5 s cap)

## Requirements

- Emacs 29.1+
- `curl` on `$PATH`
- A running `llama-swap` instance

## Installation

### use-package (Emacs 29+)

```elisp
(use-package llama-swap
  :load-path "~/.emacs.d/site-lisp/emacs-llama-swap"
  :custom
  (llama-swap-base-url "http://localhost:8686")
  ;; optional: (llama-swap-api-key "my-secret-key")
  )
```

### Manual

Clone the repository and add the `emacs-llama-swap/` directory to
`load-path`:

```elisp
(add-to-list 'load-path "/path/to/llama-swap/emacs-llama-swap")
(require 'llama-swap)
```

## Configuration

| Variable | Default | Description |
|---|---|---|
| `llama-swap-base-url` | `"http://localhost:8080"` | llama-swap server URL |
| `llama-swap-api-key` | `nil` | Bearer token (nil = try auth-source) |
| `llama-swap-auth-source-host` | `nil` | Override host for auth-source lookup |
| `llama-swap-curl-program` | `"curl"` | Path to curl executable |
| `llama-swap-request-timeout` | `30` | HTTP request timeout (seconds) |
| `llama-swap-log-max-size` | `102400` | Max bytes per log buffer |
| `llama-swap-max-metrics` | `100` | Max activity entries to keep |
| `llama-swap-chat-temperature` | `1.0` | Default chat temperature |
| `llama-swap-chat-max-tokens` | `nil` | Default max_tokens (nil = omit) |
| `llama-swap-chat-system-prompt` | `nil` | Default system prompt (nil = none) |

### Authentication

Three resolution methods, in order:

1. **Explicit key** — set `llama-swap-api-key` to a string
2. **auth-source** — entry with `:host <server-host> :port "llama-swap"`
3. **No auth** — requests are sent without `Authorization` header

```elisp
;; ~/.authinfo.gpg
machine localhost port llama-swap password my-secret-key
```

## Usage

### Connect and open the dashboard

```
M-x llama-swap
```

Or separately:

```
M-x llama-swap-connect    ; start SSE stream
M-x llama-swap-disconnect ; stop SSE stream
M-x llama-swap-reconnect  ; restart SSE stream
```

### Model dashboard keybindings

| Key | Action |
|-----|--------|
| `g` | Refresh from server |
| `RET` / `l` | Load selected model |
| `u` | Unload selected model |
| `U` | Unload all models |
| `n` | Toggle name / ID column |
| `h` | Toggle visibility of unlisted models |
| `p` | Toggle visibility of peer models |
| `a` | Open activity table |
| `L` | Open log viewer |
| `c` | Open chat for selected model |
| `q` | Quit / bury buffer |

### Log viewer

```
M-x llama-swap-logs-open
```

| Key | Action |
|-----|--------|
| `p` | Show proxy log |
| `u` | Show upstream log |
| `b` | Show combined log |
| `e` | Erase current log buffer |

### Activity table

```
M-x llama-swap-activity-open
```

| Key | Action |
|-----|--------|
| `RET` | View request/response capture |
| `w` | Copy request ID to kill-ring |
| `g` | Refresh |

### Chat playground

```
M-x llama-swap-chat   ; prompts for model
```

| Key | Action |
|-----|--------|
| `C-c C-c` | Send current input |
| `C-c C-k` | Cancel in-progress generation |
| `C-c C-r` | Reset conversation history |
| `C-c C-n` | Open new chat for a different model |

## Running tests

```bash
emacs -Q --batch \
  -L emacs-llama-swap \
  -l emacs-llama-swap/llama-swap-test.el \
  -f ert-run-tests-batch-and-exit
```

## Architecture

```
llama-swap.el          — entry point, defgroup, defcustoms, autoloads
llama-swap-auth.el     — Bearer token resolution and caching
llama-swap-http.el     — async one-shot curl requests
llama-swap-sse.el      — long-lived SSE stream, frame parser, reconnect
llama-swap-state.el    — shared global state with named change hooks
llama-swap-models.el   — tabulated-list model dashboard
llama-swap-logs.el     — proxy/upstream log buffers
llama-swap-activity.el — activity table and capture viewer
llama-swap-chat.el     — streaming chat playground
llama-swap-test.el     — ERT test suite (83 tests)
```
