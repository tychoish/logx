# logx

High-performance, zero-allocation structured logging and line-oriented JSON viewer for GNU Emacs.

## Features

- **Zero-Allocation Disabled Paths**: Fast numeric bitmask gating (`debug`, `info`, `notice`, `warn`, `error`, `critical`, `alert`, `fatal`) with per-category overrides. Disabled logging calls evaluate in ~1.4 µs with zero allocations.
- **Standard Library Ring Buffer**: Uses Emacs built-in `ring.el` for fast, zero-allocation circular retention of recent events.
- **Zero External Dependencies**: Built entirely on GNU Emacs 29+ standard library primitives (`json.c`, `ring.el`, `map.el`, `cl-lib`).
- **Decoupled Architecture**:
  - `logx.el`: Core structured logging engine, macros, circular ring buffer, headless subscriber dispatch, and atomic append-only disk writer.
  - `jsonl-view.el`: Generic, high-performance line-oriented JSON/JSONL viewer with expandable disclosure widgets and full-record detail buffer (`*jsonl-info*`). Contains zero log-specific concepts.
  - `logx-view.el`: Specialized structured log viewer layered on `jsonl-view-mode`. Provides semantic faces (inheriting cleanly from Modus/Emacs standard faces without hardcoded hex colors), interactive level/category filtering, timestamp display cycling, and live throttled streaming.
  - `logx-alert.el`: Optional integration sink routing `alert.el` notifications directly into the `logx` pipeline.
- **Robust Error Handling**: Safe object sanitizer preventing recursive loops on cyclical or complex Elisp structures; robust message intercepter preventing re-entrant loops.

## Installation

Via ELPAish or manual checkout:

```elisp
(use-package logx
  :defer t
  :commands (logx-show-logs logx-fork-filtered-buffer))

(use-package jsonl-view
  :defer t
  :mode ("\\.jsonl\\'" . jsonl-view-mode))
```

## Usage

### Structured Logging

```elisp
(require 'logx)

;; Basic structured event
(logx-info :cat 'auth :msg "User login succeeded" :uid 1001 :ip "127.0.0.1")

;; Warning with arbitrary structured metadata
(logx-warn :cat 'db :msg "Query timeout" :duration-ms 5200 :table "sessions")

;; Higher severities
(logx-critical :cat 'billing :msg "Payment gateway unavailable" :retries 3)
(logx-alert :cat 'security :msg "Brute force attempt detected" :source-ip "10.0.0.99")
```

### Viewing Logs

- `M-x logx-show-logs`: Open the live `*logs*` viewer buffer.
- `M-x logx-fork-filtered-buffer`: Create a specialized live viewer buffer (e.g., `*logs:auth*` or `*logs:error*`).

Within viewer buffers:
- `RET`: Jump to call-site source code.
- `l`: Filter by minimum severity level.
- `c`: Filter by category regex.
- `C`: Clear active filters.
- `t`: Cycle timestamp display format (Time-of-day, Delta, ISO-8601).
- `i`: Open full-record pretty-printed JSON inspection buffer (`*jsonl-info*`).
- `w`: Copy raw line JSON to kill-ring.
- `B`: Fork current view into a new filtered live buffer.
