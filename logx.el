;;; logx.el --- Structured logging framework for Emacs -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (jsonl "0.0.1"))
;; Keywords: tools, convenience, logging
;; URL: https://github.com/tychoish/logx

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; High-performance structured logging framework for Emacs.
;;
;; Features:
;; - Zero-cost disabled logging fast-path via severity gating macros.
;; - Compile-time call-site source tracking via `symbol-with-pos`.
;; - In-memory fixed-capacity circular ring buffer with zero-allocation append.
;; - Multi-buffer filtered dispatching (*logs*, *logs:<filter>*, *logs:<category>*).
;; - Fast C-level JSON Lines (`json-serialize`) disk appender with object sanitization.
;; - Optional message-quarantine mode to silence chatty background subsystems.
;; - Plug-and-play `alert.el` sink integration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'json)

;; Required for viewer rendering
(require 'jsonl-view)

;; Optional low-level dependency
(require 'jsonl nil t)

(defgroup logx nil
  "High-performance structured logging framework."
  :group 'tools
  :prefix "logx-")

;;; ----------------------------------------------------------------------------
;;; Severity Levels and Thresholds
;;; ----------------------------------------------------------------------------

(defconst logx--severity-weights
  '((debug  . 10)
    (info   . 20)
    (notice . 30)
    (warn   . 40)
    (error  . 50)
    (fatal  . 60))
  "Alist mapping severity symbols to numeric priorities.")

(defcustom logx-default-level 'info
  "Default minimum logging severity threshold."
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Notice" notice)
                 (const :tag "Warn" warn)
                 (const :tag "Error" error)
                 (const :tag "Fatal" fatal))
  :group 'logx)

(defcustom logx-category-levels (make-hash-table :test #'equal)
  "Hash table mapping category symbols or strings to severity thresholds."
  :type 'sexp
  :group 'logx)

(defcustom logx-ring-buffer-size 10000
  "Maximum number of structured events retained in the circular ring buffer."
  :type 'integer
  :group 'logx)

(defcustom logx-log-file nil
  "File path for appending JSON Lines log records, or nil to disable disk sink."
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "File Path"))
  :group 'logx)

(defcustom logx-sanitize-max-depth 8
  "Maximum recursion depth when sanitizing complex Lisp objects."
  :type 'integer
  :group 'logx)

(defcustom logx-quarantined-packages '("eglot" "flymake" "magit" "eldoc")
  "List of package prefix strings whose `message' calls are intercepted."
  :type '(repeat string)
  :group 'logx)

(defvar logx-event-hook nil
  "Hook run with one argument (the newly emitted event plist).")

;;;###autoload
(defun logx-level-weight (level)
  "Return numeric priority for LEVEL symbol, or 0 if unknown."
  (or (alist-get level logx--severity-weights) 0))

;;;###autoload
(defun logx-set-category-level (category level)
  "Set threshold for CATEGORY to LEVEL."
  (setf (map-elt logx-category-levels category) level))

;;;###autoload
(defun logx--level-enabled-p (level &optional category)
  "Return non-nil if LEVEL meets or exceeds active threshold for CATEGORY."
  (let* ((weight (logx-level-weight level))
         (cat-thresh (when category (map-elt logx-category-levels category)))
         (threshold (or cat-thresh logx-default-level)))
    (>= weight (logx-level-weight threshold))))

;;; ----------------------------------------------------------------------------
;;; Call-Site Capture
;;; ----------------------------------------------------------------------------

(defun logx--capture-call-site (macro-sym)
  "Extract source coordinates at expansion time for MACRO-SYM.
Returns a plist (:file FILE :path PATH :line LINE :fn FN)."
  (let* ((file (or (macroexp-file-name)
                   (and (boundp 'byte-compile-current-file)
                        byte-compile-current-file)
                   (buffer-file-name)))
         (line nil)
         (fn (when (boundp 'byte-compile-current-form)
               (and (consp byte-compile-current-form)
                    (memq (car byte-compile-current-form) '(defun cl-defun defmacro))
                    (cadr byte-compile-current-form)))))
    ;; Attempt symbol-with-pos extraction in byte-compiled context
    (when (and (fboundp 'symbol-with-pos-p)
               (fboundp 'symbol-with-pos-pos)
               (symbol-with-pos-p macro-sym))
      (let ((pos (symbol-with-pos-pos macro-sym)))
        (when (numberp pos)
          (setq line (line-number-at-pos pos)))))
    ;; Fall back to active buffer position if evaluating in buffer
    (unless line
      (setq line (ignore-errors (line-number-at-pos))))
    (list :file (and file (file-name-nondirectory file))
          :path file
          :line line
          :fn fn)))

;;; ----------------------------------------------------------------------------
;;; Object Sanitization
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun logx--sanitize-value (val &optional depth max-depth seen)
  "Sanitize VAL into JSON-serializable primitives up to MAX-DEPTH.
Prevents crashes on non-serializable Lisp objects (buffers, markers, etc.)
and guards against circular references via SEEN."
  (let ((d (or depth 0))
        (md (or max-depth logx-sanitize-max-depth)))
    (cond
     ((> d md) "[Truncated: max depth reached]")
     ((null val) :null)
     ((eq val t) t)
     ((eq val :false) :false)
     ((numberp val) val)
     ((stringp val) val)
     ((keywordp val) (substring (symbol-name val) 1))
     ((bufferp val) (format "#<buffer %s>" (buffer-name val)))
     ((markerp val) (marker-position val))
     ((processp val) (format "#<process %s>" (process-name val)))
     ((windowp val) (format "#<window %s>" (window-use-time val)))
     ((framep val) (format "#<frame %s>" (frame-parameter val 'name)))
     ((hash-table-p val)
      (if (memq val seen)
          "[Circular Reference]"
        (let ((items nil)
              (new-seen (cons val seen)))
          (map-do (lambda (k v)
                    (push (intern (format ":%s" k)) items)
                    (push (logx--sanitize-value v (1+ d) md new-seen) items))
                  val)
          (nreverse items))))
     ((vectorp val)
      (if (memq val seen)
          "[Circular Reference]"
        (let ((new-seen (cons val seen)))
          (vconcat (mapcar (lambda (elem)
                             (logx--sanitize-value elem (1+ d) md new-seen))
                           val)))))
     ((consp val)
      (if (memq val seen)
          "[Circular Reference]"
        (let ((new-seen (cons val seen)))
          (if (keywordp (car val))
              ;; Plist: preserve keyword symbols as keys
              (let ((res nil)
                    (tail val))
                (while (and (consp tail) (keywordp (car tail)))
                  (push (car tail) res)
                  (push (logx--sanitize-value (cadr tail) (1+ d) md new-seen) res)
                  (setq tail (cddr tail)))
                (nreverse res))
            ;; General list
            (mapcar (lambda (elem)
                      (logx--sanitize-value elem (1+ d) md new-seen))
                    val)))))
     ((symbolp val) (symbol-name val))
     (t (format "%s" val)))))

;;; ----------------------------------------------------------------------------
;;; Circular Ring Buffer
;;; ----------------------------------------------------------------------------

(defvar logx--ring-vector nil
  "Vector backing store for the circular ring buffer.")

(defvar logx--ring-head 0
  "Index where the next event will be written.")

(defvar logx--ring-count 0
  "Total number of active events currently stored.")

(defvar logx--ring-seq 0
  "Monotonically increasing sequence number for log events.")

;;;###autoload
(defun logx--ring-init (&optional size)
  "Initialize or reallocate the ring buffer with capacity SIZE."
  (let ((cap (or size logx-ring-buffer-size 10000)))
    (setq logx--ring-vector (make-vector cap nil)
          logx--ring-head 0
          logx--ring-count 0
          logx--ring-seq 0)))

;;;###autoload
(defun logx--ring-push (event)
  "Push EVENT into the circular ring buffer in O(1) time.
Overwrites the oldest element when the buffer is full."
  (unless logx--ring-vector
    (logx--ring-init))
  (let ((cap (length logx--ring-vector)))
    (aset logx--ring-vector logx--ring-head event)
    (setq logx--ring-head (% (1+ logx--ring-head) cap))
    (when (< logx--ring-count cap)
      (cl-incf logx--ring-count))
    event))

;;;###autoload
(defun logx--ring-clear ()
  "Reset ring buffer pointers without reallocating vector."
  (setq logx--ring-head 0
        logx--ring-count 0))

;;;###autoload
(defun logx--ring-to-list (&optional limit)
  "Return ring buffer events ordered from oldest to newest.
If LIMIT is non-nil, return at most LIMIT entries."
  (unless logx--ring-vector
    (logx--ring-init))
  (let* ((cap (length logx--ring-vector))
         (count (if limit (min limit logx--ring-count) logx--ring-count))
         (start-idx (% (+ (- logx--ring-head logx--ring-count) cap) cap))
         (res nil))
    (dotimes (i count)
      (let ((idx (% (+ start-idx i) cap)))
        (push (aref logx--ring-vector idx) res)))
    (nreverse res)))

;;; ----------------------------------------------------------------------------
;;; Serialization and Disk Sink
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun logx--serialize-jsonl (event)
  "Serialize EVENT plist into a single line-delimited JSON string."
  (let* ((sanitized-data (logx--sanitize-value (plist-get event :data)))
         (payload (list :seq (plist-get event :seq)
                        :ts (plist-get event :ts)
                        :lvl (symbol-name (plist-get event :lvl))
                        :cat (format "%s" (or (plist-get event :cat) "root"))
                        :msg (or (plist-get event :msg) "")
                        :src (logx--sanitize-value (plist-get event :src))
                        :data sanitized-data)))
    (json-serialize payload)))

(defun logx--disk-appender (event)
  "Append serialized EVENT to `logx-log-file' if configured."
  (when (and logx-log-file (stringp logx-log-file))
    (condition-case err
        (let ((json-line (concat (logx--serialize-jsonl event) "\n")))
          (with-temp-buffer
            (insert json-line)
            (append-to-file (point-min) (point-max) logx-log-file)))
      (error
       (message "[logx] Disk appender failed: %s" (error-message-string err))))))

;;; ----------------------------------------------------------------------------
;;; Event Emission Pipeline
;;; ----------------------------------------------------------------------------

(defvar logx--pending-events nil
  "List of events emitted since last batch flush, in reverse chronological order.")

(defvar logx--flush-timer nil
  "Timer driving the throttled viewer buffer flush.")

(defun logx--emit-event (level category msg src data)
  "Construct, store, and dispatch structured event record."
  (let ((event (list :seq (cl-incf logx--ring-seq)
                     :ts (float-time)
                     :lvl level
                     :cat (or category 'root)
                     :msg (or msg "")
                     :src src
                     :data data)))
    (logx--ring-push event)
    (when logx-log-file
      (logx--disk-appender event))
    (push event logx--pending-events)
    (logx--schedule-flush)
    (run-hook-with-args 'logx-event-hook event)
    event))

;;; ----------------------------------------------------------------------------
;;; Live Ring Buffer Renderer & Multi-Buffer Views (*logs*, *logs:<filter>*)
;;; ----------------------------------------------------------------------------

(defcustom logx-render-interval 0.05
  "Interval in seconds between throttled batch flushes to visible viewer buffers."
  :type 'number
  :group 'logx)

(defcustom logx-viewer-buffer-max-lines 10000
  "Maximum number of lines retained in live viewer buffers before truncation."
  :type 'integer
  :group 'logx)

(defvar logx--active-viewers nil
  "Alist of (BUFFER . PREDICATE) active live viewer buffers.")

(defun logx-register-viewer (buffer &optional predicate)
  "Register BUFFER to receive live events matching PREDICATE."
  (setq logx--active-viewers
        (cons (cons buffer (or predicate (lambda (_ev) t)))
              (assq-delete-all buffer logx--active-viewers))))

(defun logx-unregister-viewer (buffer)
  "Unregister BUFFER from live log event delivery."
  (setq logx--active-viewers (assq-delete-all buffer logx--active-viewers)))

(defun logx--flush-active-viewers ()
  "Flush pending events to all visible subscriber viewer buffers in batches."
  (setq logx--flush-timer nil)
  (when logx--pending-events
    (let ((events (nreverse logx--pending-events)))
      (setq logx--pending-events nil)
      ;; Clean dead buffers from registry
      (setq logx--active-viewers
            (seq-filter (lambda (entry) (buffer-live-p (car entry)))
                        logx--active-viewers))
      (dolist (entry logx--active-viewers)
        (let ((buf (car entry))
              (pred (cdr entry)))
          (when (and (buffer-live-p buf)
                     ;; Visibility gating: only insert if displayed in a window
                     (get-buffer-window buf 'visible))
            (with-current-buffer buf
              (let ((inhibit-read-only t)
                    (orig-window (get-buffer-window buf))
                    (should-follow jsonl-view-follow-mode))
                (save-excursion
                  (goto-char (point-max))
                  (dolist (ev events)
                    (when (funcall pred ev)
                      (insert (jsonl-view-render-record ev))))
                  ;; Enforce buffer size limit
                  (when (> (count-lines (point-min) (point-max)) logx-viewer-buffer-max-lines)
                    (let ((excess (- (count-lines (point-min) (point-max)) logx-viewer-buffer-max-lines)))
                      (goto-char (point-min))
                      (forward-line excess)
                      (delete-region (point-min) (point)))))
                ;; Auto-scroll if follow mode is active
                (when (and should-follow orig-window)
                  (with-selected-window orig-window
                    (goto-char (point-max))))))))))))

(defun logx--schedule-flush ()
  "Ensure a throttled flush timer is scheduled."
  (unless logx--flush-timer
    (setq logx--flush-timer
          (run-with-timer logx-render-interval nil #'logx--flush-active-viewers))))

;;;###autoload
(defun logx-show-logs (&optional buffer-name)
  "Display master structured logs viewer buffer, or BUFFER-NAME if specified."
  (interactive)
  (let* ((name (or buffer-name "*logs*"))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (eq major-mode 'jsonl-view-mode)
        (jsonl-view-mode)
        (setq-local jsonl-view-follow-mode t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (ev (logx--ring-to-list))
            (insert (jsonl-view-render-record ev))))
        (goto-char (point-max))))
    (logx-register-viewer buf)
    (pop-to-buffer buf)))

;;;###autoload
(defun logx-fork-filtered-buffer (name filter-pred)
  "Create and display a specialized live viewer buffer.
Creates *logs:NAME* containing events matching FILTER-PRED."
  (interactive
   (let* ((n (read-string "Filtered buffer name suffix (e.g. error, asq): "))
          (min-lvl (intern (completing-read "Minimum severity: "
                                            '("debug" "info" "notice" "warn" "error" "fatal")
                                            nil t nil nil "info")))
          (cat-rx (read-string "Category regex (empty for all): ")))
     (list n
           (lambda (ev)
             (let ((l (plist-get ev :lvl))
                   (c (format "%s" (or (plist-get ev :cat) ""))))
               (and (>= (logx-level-weight l) (logx-level-weight min-lvl))
                    (if (string-empty-p cat-rx) t (string-match-p cat-rx c))))))))
  (let* ((buf-name (format "*logs:%s*" name))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (jsonl-view-mode)
      (setq-local jsonl-view-follow-mode t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (ev (logx--ring-to-list))
          (when (funcall filter-pred ev)
            (insert (jsonl-view-render-record ev))))
        (goto-char (point-max))))
    (logx-register-viewer buf filter-pred)
    (pop-to-buffer buf)))

;;; ----------------------------------------------------------------------------
;;; Macro Expansion & Public Logging API
;;; ----------------------------------------------------------------------------

(defun logx--parse-macro-args (args)
  "Extract :cat, :msg, and remaining key-values from ARGS."
  (let ((cat nil)
        (msg nil)
        (rest nil))
    (while args
      (let ((key (car args)))
        (cond
         ((eq key :cat)
          (setq cat (cadr args)
                args (cddr args)))
         ((eq key :msg)
          (setq msg (cadr args)
                args (cddr args)))
         ((keywordp key)
          (push key rest)
          (push (cadr args) rest)
          (setq args (cddr args)))
         (t
          (push key rest)
          (setq args (cdr args))))))
    (list :cat cat :msg msg :data (nreverse rest))))

(defun logx--expand-macro (level args macro-sym)
  "Expand logging macro for LEVEL with ARGS and MACRO-SYM."
  (let* ((parsed (logx--parse-macro-args args))
         (cat-form (plist-get parsed :cat))
         (msg-form (plist-get parsed :msg))
         (data-forms (plist-get parsed :data))
         (src (logx--capture-call-site macro-sym)))
    `(when (logx--level-enabled-p ',level ,cat-form)
       (logx--emit-event ',level
                         ,cat-form
                         ,msg-form
                         ',src
                         (list ,@data-forms)))))

;;;###autoload
(defmacro logx-debug (&rest args)
  "Emit a DEBUG level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'debug args 'logx-debug))

;;;###autoload
(defmacro logx-info (&rest args)
  "Emit an INFO level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'info args 'logx-info))

;;;###autoload
(defmacro logx-notice (&rest args)
  "Emit a NOTICE level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'notice args 'logx-notice))

;;;###autoload
(defmacro logx-warn (&rest args)
  "Emit a WARN level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'warn args 'logx-warn))

;;;###autoload
(defmacro logx-error (&rest args)
  "Emit an ERROR level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'error args 'logx-error))

;;;###autoload
(defmacro logx-fatal (&rest args)
  "Emit a FATAL level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'fatal args 'logx-fatal))

;;; ----------------------------------------------------------------------------
;;; Legacy Message Interception Quarantine Bridge
;;; ----------------------------------------------------------------------------

(defvar logx--in-quarantine-p nil
  "Reentrancy guard flag preventing infinite recursion in message advice.")

(defun logx--caller-matches-quarantine-p (pkgs)
  "Return non-nil if caller stack matches any package prefix in PKGS."
  (let ((matched nil)
        (frame-idx 3)) ;; Skip advice internal frames
    (while (and (not matched) (< frame-idx 20))
      (let ((frame (backtrace-frame frame-idx)))
        (when frame
          (let ((fn (cadr frame)))
            (when (symbolp fn)
              (let ((fn-name (symbol-name fn)))
                (dolist (pkg pkgs)
                  (when (string-prefix-p pkg fn-name)
                    (setq matched pkg))))))))
      (cl-incf frame-idx))
    matched))

(defun logx--caller-call-site ()
  "Inspect call stack for originating caller coordinates."
  (let ((frame (backtrace-frame 4)))
    (when frame
      (list :fn (cadr frame)))))

(defun logx--message-advice (orig-fun &rest args)
  "Advice intercepting chatty background `message' invocations."
  (if (or logx--in-quarantine-p
          (null (car args))
          inhibit-message)
      (apply orig-fun args)
    (let ((logx--in-quarantine-p t))
      (condition-case _err
          (let ((pkg (and (not this-command)
                          (logx--caller-matches-quarantine-p logx-quarantined-packages))))
            (if pkg
                (let ((formatted (apply #'format-message args)))
                  (logx--emit-event 'info
                                    (intern (format "legacy.%s" pkg))
                                    formatted
                                    (logx--caller-call-site)
                                    (list :raw-args (mapcar #'logx--sanitize-value (cdr args)))))
              (apply orig-fun args)))
        (error
         (apply orig-fun args))))))

;;;###autoload
(define-minor-mode logx-quarantine-mode
  "Global minor mode diverting background package messages to logx."
  :global t
  :group 'logx
  (if logx-quarantine-mode
      (advice-add 'message :around #'logx--message-advice)
    (advice-remove 'message #'logx--message-advice)))

;;; ----------------------------------------------------------------------------
;;; Alert.el Integration Sink
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun logx-alert-sink (info)
  "Route `alert' notification INFO plist into `logx' structured logging."
  (let* ((sev (or (plist-get info :severity) 'normal))
         (level (pcase sev
                  ('urgent 'error)
                  ('high 'warn)
                  ('low 'debug)
                  (_ 'info)))
         (cat (or (plist-get info :category) 'alert))
         (title (plist-get info :title))
         (msg (plist-get info :message))
         (combined-msg (if title (format "[%s] %s" title msg) msg)))
    (logx--emit-event level (intern (format "%s" cat)) combined-msg nil info)))

(with-eval-after-load 'alert
  (declare-function alert-define-style "alert" (name &rest args))
  (when (fboundp 'alert-define-style)
    (alert-define-style 'logx
                        :title "Logx Structured Logger"
                        :notifier #'logx-alert-sink)))

(provide 'logx)
;;; logx.el ends here
