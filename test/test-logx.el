;;; test-logx.el --- Tests for logx structured logging -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'ert)
(require 'logx)

(ert-deftest test-logx-level-weights ()
  "Verify numeric weights and progression of all 8 severity levels."
  (should (< (logx-level-weight 'debug)
             (logx-level-weight 'info)))
  (should (< (logx-level-weight 'info)
             (logx-level-weight 'notice)))
  (should (< (logx-level-weight 'notice)
             (logx-level-weight 'warn)))
  (should (< (logx-level-weight 'warn)
             (logx-level-weight 'error)))
  (should (< (logx-level-weight 'error)
             (logx-level-weight 'critical)))
  (should (< (logx-level-weight 'critical)
             (logx-level-weight 'alert)))
  (should (< (logx-level-weight 'alert)
             (logx-level-weight 'fatal)))
  ;; Verify alias resolution in logx-level-weight
  (should (= (logx-level-weight 'warning) (logx-level-weight 'warn)))
  (should (= (logx-level-weight 'trace) (logx-level-weight 'debug)))
  (should (= (logx-level-weight :info) (logx-level-weight 'info)))
  (should (= (logx-level-weight "warn") (logx-level-weight 'warn))))

(ert-deftest test-logx-level-gating ()
  "Verify global and category-specific severity thresholds."
  (let ((logx-default-level 'info)
        (logx-category-levels nil))
    ;; Global threshold checks
    (should-not (logx--level-enabled-p 'debug))
    (should (logx--level-enabled-p 'info))
    (should (logx--level-enabled-p 'warn))
    (should (logx--level-enabled-p 'error))
    (should (logx--level-enabled-p 'critical))
    (should (logx--level-enabled-p 'alert))
    (should (logx--level-enabled-p 'fatal))

    ;; Category-specific override
    (logx-set-category-level 'mcp.proxy 'debug)
    (should (logx--level-enabled-p 'debug 'mcp.proxy))
    (should-not (logx--level-enabled-p 'debug 'other.cat))

    ;; Category-specific threshold elevation
    (logx-set-category-level 'chatty.lib 'error)
    (should-not (logx--level-enabled-p 'warn 'chatty.lib))
    (should (logx--level-enabled-p 'error 'chatty.lib))

    ;; Keyword category matching
    (logx-set-category-level 'auth 'error)
    (should-not (logx--level-enabled-p 'info :auth))
    (should (logx--level-enabled-p 'error :auth))))

(ert-deftest test-logx-macro-zero-cost-disabled-path ()
  "Verify arguments are not evaluated when log level is inactive."
  (let ((logx-default-level 'warn)
        (side-effect-flag nil))
    ;; Level is info < warn -> side-effect expression must not run
    (logx-info :cat 'test :msg (setq side-effect-flag t))
    (should-not side-effect-flag)

    ;; Level is error >= warn -> side-effect expression evaluates
    (logx-error :cat 'test :msg (setq side-effect-flag t))
    (should side-effect-flag)

    ;; Test critical and alert macros
    (setq side-effect-flag nil)
    (logx-critical :cat 'test :msg (setq side-effect-flag 'critical))
    (should (eq side-effect-flag 'critical))

    (setq side-effect-flag nil)
    (logx-alert :cat 'test :msg (setq side-effect-flag 'alert))
    (should (eq side-effect-flag 'alert))))

(ert-deftest test-logx-ring-buffer-operations ()
  "Verify standard library ring.el push, circular wrap-around, and FIFO retrieval."
  (let ((logx-ring-buffer-size 3))
    (logx--ring-init 3)
    (should (= (logx--ring-length) 0))

    ;; Push 2 items
    (logx--ring-push '(:id 1 :msg "first"))
    (logx--ring-push '(:id 2 :msg "second"))
    (should (= (logx--ring-length) 2))
    (let ((items (logx--ring-to-list)))
      (should (= (length items) 2))
      (should (= (plist-get (car items) :id) 1))
      (should (= (plist-get (cadr items) :id) 2)))

    ;; Push 3rd item (full)
    (logx--ring-push '(:id 3 :msg "third"))
    (should (= (logx--ring-length) 3))

    ;; Push 4th item (wrap around: 1 is discarded)
    (logx--ring-push '(:id 4 :msg "fourth"))
    (let ((items (logx--ring-to-list)))
      (should (= (length items) 3))
      (should (= (plist-get (car items) :id) 2))
      (should (= (plist-get (nth 1 items) :id) 3))
      (should (= (plist-get (nth 2 items) :id) 4)))

    ;; Clear
    (logx--ring-clear)
    (should (= (logx--ring-length) 0))))

(ert-deftest test-logx-subscriber-dispatch ()
  "Verify event delivery to registered subscribers."
  (let ((received nil))
    (logx-register-subscriber 'test-sub (lambda (ev) (push ev received)))
    (unwind-protect
        (progn
          (logx-info :cat 'dispatch :msg "Test message 1")
          (should (= (length received) 1))
          (should (equal (plist-get (car received) :msg) "Test message 1")))
      (logx-unregister-subscriber 'test-sub))))

(ert-deftest test-logx-object-sanitizer ()
  "Verify non-JSON Lisp primitives and cycles are safely converted."
  ;; Buffers
  (with-temp-buffer
    (rename-buffer "*logx-test-buf*" t)
    (let ((sanitized (logx--sanitize-value (current-buffer))))
      (should (stringp sanitized))
      (should (string-match-p "\\*logx-test-buf\\*" sanitized))))

  ;; Markers
  (with-temp-buffer
    (insert "hello world")
    (let* ((m (copy-marker 4))
           (sanitized (logx--sanitize-value m)))
      (should (integerp sanitized))
      (should (= sanitized 4))))

  ;; Symbols and keywords
  (should (equal (logx--sanitize-value :my-key) "my-key"))
  (should (equal (logx--sanitize-value 'my-sym) "my-sym"))

  ;; Circular lists
  (let ((cyclic-list (list :a 1 :b nil)))
    (setf (cadr (cddr cyclic-list)) cyclic-list)
    (let ((sanitized (logx--sanitize-value cyclic-list)))
      (should (listp sanitized))
      (should (member "[Circular Reference]" sanitized))))

  ;; Deep recursion
  (let ((deep-tree '(:l1 (:l2 (:l3 (:l4 (:l5 (:l6 (:l7 (:l8 (:l9 "too deep")))))))))))
    (let ((sanitized (logx--sanitize-value deep-tree 0 5)))
      (should (string-match-p "Truncated" (format "%S" sanitized))))))

(ert-deftest test-logx-jsonl-serialization ()
  "Verify events serialize cleanly to JSON Lines strings."
  (let ((event '(:seq 42
                 :ts 1700000000.123
                 :lvl info
                 :cat agent.asq
                 :msg "Processing item"
                 :src (:file "test.el" :line 10)
                 :data (:item-id "it-123" :count 5))))
    (let ((json-str (logx--serialize-jsonl event)))
      (should (stringp json-str))
      (let ((parsed (json-parse-string json-str :object-type 'plist)))
        (should (= (plist-get parsed :seq) 42))
        (should (equal (plist-get parsed :lvl) "info"))
        (should (equal (plist-get parsed :cat) "agent.asq"))
        (should (equal (plist-get parsed :msg) "Processing item")))))

  ;; List in data and non-string symbol in :msg
  (let ((event '(:seq 43
                 :ts 1700000001.0
                 :lvl warn
                 :cat test
                 :msg failed-status
                 :src nil
                 :data (:tags ("backend" "auth") :counts (10 20 30)))))
    (let ((json-str (logx--serialize-jsonl event)))
      (should (stringp json-str))
      (let ((parsed (json-parse-string json-str :object-type 'plist)))
        (should (equal (plist-get parsed :msg) "failed-status"))
        (should (equal (plist-get (plist-get parsed :data) :tags) ["backend" "auth"]))
        (should (equal (plist-get (plist-get parsed :data) :counts) [10 20 30]))))))

(ert-deftest test-logx-disk-appender ()
  "Verify log events append to target file."
  (let* ((tmp-file (make-temp-file "logx-test-" nil ".jsonl"))
         (logx-log-file tmp-file)
         (logx-default-level 'info))
    (unwind-protect
        (progn
          (logx-info :cat 'fs :msg "Disk append event 1" :field "val1")
          (logx-info :cat 'fs :msg "Disk append event 2" :field "val2")
          (logx-flush)
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (let ((lines (split-string (buffer-string) "\n" t)))
              (should (= (length lines) 2))
              (should (string-match-p "Disk append event 1" (car lines)))
              (should (string-match-p "Disk append event 2" (cadr lines))))))
      (when (file-exists-p tmp-file)
        (delete-file tmp-file)))))

(ert-deftest test-logx-pluggable-backends ()
  "Verify backend registration, enable/disable, and batch dispatch."
  (let ((received nil)
        (logx-default-level 'info)
        (logx-async-mode nil))
    (logx-register-backend 'mock-custom (lambda (evs) (setq received (append received evs))) t)
    (should (logx-backend-enabled-p 'mock-custom))

    (logx-info :cat 'test :msg "Custom backend event 1")
    (should (= (length received) 1))
    (should (equal (plist-get (car received) :msg) "Custom backend event 1"))

    (logx-disable-backend 'mock-custom)
    (should-not (logx-backend-enabled-p 'mock-custom))
    (logx-info :cat 'test :msg "Custom backend event 2")
    (should (= (length received) 1)) ; did not receive event 2

    (logx-enable-backend 'mock-custom)
    (should (logx-backend-enabled-p 'mock-custom))
    (logx-info :cat 'test :msg "Custom backend event 3")
    (should (= (length received) 2))

    (logx-unregister-backend 'mock-custom)
    (should-not (logx-backend-enabled-p 'mock-custom))))

(ert-deftest test-logx-journald-format ()
  "Verify formatting of systemd-journald socket datagrams."
  (let* ((ev (list :seq 42
                   :ts 1700000000.0
                   :lvl 'error
                   :cat 'service.auth
                   :msg "Authentication failure"
                   :src '(:file "auth.el" :line 88)
                   :data '(:uid 1002 :ip "10.0.0.1")))
         (datagram (logx--journald-format-event ev)))
    (should (string-match-p "^MESSAGE=Authentication failure\n" datagram))
    (should (string-match-p "^PRIORITY=3\n" datagram))
    (should (string-match-p "^SYSLOG_IDENTIFIER=emacs\n" datagram))
    (should (string-match-p "^EMACS_CATEGORY=service.auth\n" datagram))
    (should (string-match-p "^EMACS_SEQ=42\n" datagram))
    (should (string-match-p "^CODE_FILE=auth.el\n" datagram))
    (should (string-match-p "^CODE_LINE=88\n" datagram))
    (should (string-match-p "\"uid\":1002" datagram))))

(ert-deftest test-logx-syslog-format ()
  "Verify formatting of RFC 3164 syslog datagrams."
  (let* ((ev (list :seq 1
                   :ts 1700000000.0
                   :lvl 'warn
                   :cat 'disk
                   :msg "Space low"
                   :src nil
                   :data nil))
         (datagram (logx--syslog-format-event ev)))
    ;; Priority for warn (4) with user facility (8) = 12
    (should (string-prefix-p "<12>" datagram))
    (should (string-match-p (format "emacs\\[%d\\]: \\[disk\\] Space low\n" (emacs-pid)) datagram))))

(ert-deftest test-logx-async-backpressure ()
  "Verify backpressure queue bounding and drop policy."
  (let ((logx-async-mode t)
        (logx-async-transport 'sync)
        (logx-async-max-queue 5)
        (logx-async-backpressure-policy :drop-oldest)
        (logx--async-queue nil)
        (logx--async-queue-tail nil)
        (logx--async-queue-len 0)
        (logx--dropped-count 0))
    ;; Simulate enqueueing 10 events into a max 5 queue without flushing
    (cl-letf (((symbol-function 'logx--async-schedule-flush) #'ignore))
      (dotimes (i 10)
        (logx--async-enqueue (list :seq i :msg (format "ev-%d" i)))))
    (should (= logx--async-queue-len 5))
    (should (= logx--dropped-count 5))
    ;; Oldest remaining event should be ev-5
    (should (equal (plist-get (car logx--async-queue) :msg) "ev-5"))))

(ert-deftest test-logx-category-string-symbol-interop ()
  "Verify category levels match interchangeably across strings and symbols."
  (let ((logx-default-level 'info)
        (logx-category-levels nil))
    ;; Set as symbol, check as string
    (logx-set-category-level 'sym.cat 'debug)
    (should (logx--level-enabled-p 'debug 'sym.cat))
    (should (logx--level-enabled-p 'debug "sym.cat"))

    ;; Set as string, check as symbol
    (logx-set-category-level "str.cat" 'debug)
    (should (logx--level-enabled-p 'debug 'str.cat))
    (should (logx--level-enabled-p 'debug "str.cat"))))

(ert-deftest test-logx-socket-error-recovery ()
  "Verify backend process handles errors and cleans up socket reference."
  (let ((logx--journald-process 'dummy-proc)
        (logx--syslog-process 'dummy-proc))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc _str) (error "Simulated socket error (ECONNRESET)")))
              ((symbol-function 'delete-process) #'ignore))
      ;; journald handler error cleans up process
      (logx--journald-backend-handler (list (list :lvl 'info :msg "test")))
      (should-not logx--journald-process)

      ;; syslog handler error cleans up process
      (logx--syslog-backend-handler (list (list :lvl 'info :msg "test")))
      (should-not logx--syslog-process))))

(ert-deftest test-logx-flush-return-count ()
  "Verify logx-flush returns number of flushed events."
  (let ((logx-async-mode t)
        (logx-async-transport 'sync)
        (logx--async-queue nil)
        (logx--async-queue-tail nil)
        (logx--async-queue-len 0))
    ;; Mock schedule-flush to prevent immediate flush
    (cl-letf (((symbol-function 'logx--async-schedule-flush) #'ignore))
      (logx--async-enqueue (list :msg "ev1"))
      (logx--async-enqueue (list :msg "ev2"))
      (logx--async-enqueue (list :msg "ev3"))
      (should (= logx--async-queue-len 3))
      (let ((flushed (logx-flush)))
        (should (= flushed 3))
        (should (= logx--async-queue-len 0))
        (should-not logx--async-queue)))))

(ert-deftest test-logx-async-thread-transport ()
  "Verify background thread transport flushes pending events."
  (let ((flushed-events nil)
        (orig-mode logx-async-mode)
        (orig-transport logx-async-transport)
        (orig-interval logx-async-flush-interval))
    (unwind-protect
        (progn
          (setq logx-async-mode t
                logx-async-transport 'thread
                logx-async-flush-interval 0.02
                logx--async-queue nil
                logx--async-queue-tail nil
                logx--async-queue-len 0)
          (logx-register-backend 'test-thread-backend
                                 (lambda (evs) (setq flushed-events (append flushed-events evs)))
                                 t)
          (logx--async-enqueue (list :msg "Thread event 1"))
          (logx--async-enqueue (list :msg "Thread event 2"))
          ;; Allow background thread to wake up and flush
          (sleep-for 0.08)
          (should (>= (length flushed-events) 2))
          (should (equal (plist-get (car flushed-events) :msg) "Thread event 1")))
      (logx-stop-async-worker)
      (logx-unregister-backend 'test-thread-backend)
      (setq logx-async-mode orig-mode
            logx-async-transport orig-transport
            logx-async-flush-interval orig-interval))))

(ert-deftest test-logx-quarantine-mode ()
  "Verify logx-quarantine-mode intercepts whitelisted package messages."
  (let ((logx-quarantined-packages '("mock-pkg"))
        (logx--in-quarantine-p nil)
        (intercepted nil)
        (this-command nil)
        (inhibit-message nil))
    (unwind-protect
        (progn
          (logx-quarantine-mode 1)
          (cl-letf (((symbol-function 'logx--caller-matches-quarantine-p)
                     (lambda (_pkgs) "mock-pkg"))
                    ((symbol-function 'logx--emit-event)
                     (lambda (lvl cat msg src data)
                       (push (list :lvl lvl :cat cat :msg msg :src src :data data) intercepted))))
            (message "Hello from mock background package %d" 42)
            (should (= (length intercepted) 1))
            (should (equal (plist-get (car intercepted) :cat) 'legacy.mock-pkg))
            (should (equal (plist-get (car intercepted) :msg) "Hello from mock background package 42"))))
      (logx-quarantine-mode -1))))

(ert-deftest test-logx-unified-macro ()
  "Verify the unified (logx ...) macro with explicit, aliased, and default levels."
  (let ((logx-default-level 'debug)
        (logx-default-event-level 'debug)
        (logx-category-levels nil)
        (events nil))
    (cl-letf (((symbol-function 'logx--ring-push)
               (lambda (ev) (push ev events) ev)))
      ;; 1. Explicit quoted levels
      (logx 'info "Hello info" :key1 1)
      (should (eq (plist-get (car events) :lvl) 'info))
      (should (equal (plist-get (car events) :msg) "Hello info"))
      (should (equal (plist-get (car events) :data) '(:key1 1)))

      ;; 2. Canonical level aliases: 'warning -> 'warn, 'trace -> 'debug
      (logx 'warning "Warning message" :cat 'sys)
      (should (eq (plist-get (car events) :lvl) 'warn))
      (should (equal (plist-get (car events) :cat) 'sys))

      (logx 'trace "Trace message")
      (should (eq (plist-get (car events) :lvl) 'debug))
      (should (equal (plist-get (car events) :msg) "Trace message"))

      ;; 3. Keyword levels and variable preservation
      (logx :warn "Keyword warn")
      (should (eq (plist-get (car events) :lvl) 'warn))

      ;; User variable named after a level is treated as message, not level
      (let ((info "Message from info variable"))
        (logx info)
        (should (eq (plist-get (car events) :lvl) 'debug))
        (should (equal (plist-get (car events) :msg) "Message from info variable")))

      ;; 4. Default level when omitted
      (logx "Untagged message" :count 5)
      (should (eq (plist-get (car events) :lvl) 'debug))
      (should (equal (plist-get (car events) :msg) "Untagged message"))

      ;; 5. Configurable default level override
      (let ((logx-default-event-level 'warn))
        (logx "Warning default" :count 10)
        (should (eq (plist-get (car events) :lvl) 'warn))))))

(ert-deftest test-logx-aliases-cat-msg ()
  "Verify :category, :domain, and :message argument aliases."
  (let ((logx-default-level 'debug)
        (logx-default-event-level 'debug)
        (events nil))
    (cl-letf (((symbol-function 'logx--ring-push)
               (lambda (ev) (push ev events) ev)))
      ;; :category alias
      (logx-info :category 'auth :message "User logged in")
      (should (equal (plist-get (car events) :cat) 'auth))
      (should (equal (plist-get (car events) :msg) "User logged in"))

      ;; :domain alias
      (logx 'notice :domain 'network :msg "Socket connected")
      (should (equal (plist-get (car events) :cat) 'network))
      (should (equal (plist-get (car events) :msg) "Socket connected"))

      ;; Mixed positional message with :domain
      (logx 'error "Database offline" :domain 'storage :code 500)
      (should (equal (plist-get (car events) :cat) 'storage))
      (should (equal (plist-get (car events) :msg) "Database offline"))
      (should (equal (plist-get (car events) :data) '(:code 500))))))

(ert-deftest test-logx-positional-strings-and-expressions ()
  "Verify positional strings and dynamic expressions across all logging macros."
  (let ((logx-default-level 'info)
        (events nil))
    (cl-letf (((symbol-function 'logx--ring-push)
               (lambda (ev) (push ev events) ev)))
      ;; Positional string in logx-info
      (logx-info "Service started" :port 8080)
      (should (equal (plist-get (car events) :msg) "Service started"))
      (should (equal (plist-get (car events) :data) '(:port 8080)))

      ;; Positional expression in logx-warn
      (logx-warn (format "Disk %s near capacity" "/dev/sda1") :free-pct 5)
      (should (equal (plist-get (car events) :msg) "Disk /dev/sda1 near capacity"))
      (should (equal (plist-get (car events) :data) '(:free-pct 5)))

      ;; Zero-cost disabled path for dynamic positional expressions
      (let ((side-effect-flag nil))
        (logx-debug (setq side-effect-flag t) :extra "val")
        (should-not side-effect-flag))

      (let ((logx-default-level 'error)
            (logx-default-event-level 'debug)
            (side-effect-flag nil))
        (logx (setq side-effect-flag t) :extra "val")
        (should-not side-effect-flag)))))

(ert-deftest test-logx-macro-hygiene-and-once-eval ()
  "Verify macro hygiene (no `lvl' variable capture) and single evaluation of expressions."
  (let ((logx-default-level 'debug)
        (logx-default-event-level 'debug)
        (events nil))
    (cl-letf (((symbol-function 'logx--ring-push)
               (lambda (ev) (push ev events) ev)))
      ;; 1. Variable capture test: user variable named `lvl`
      (let ((lvl :user-token-value))
        (logx "Hygiene test" :token lvl)
        (should (equal (plist-get (plist-get (car events) :data) :token)
                       :user-token-value)))

      ;; 2. Once-evaluation test: dynamic category form evaluated only once
      (let ((cat-eval-count 0))
        (logx-info "Category once eval" :cat (cl-incf cat-eval-count))
        (should (= cat-eval-count 1)))

      ;; 3. Non-loss of data when both positional string and :msg keyword are given
      (logx-info "Positional string" :msg "Explicit keyword message")
      (should (equal (plist-get (car events) :msg) "Positional string"))
      (should (equal (plist-get (car events) :data) '(:msg "Explicit keyword message"))))))

(ert-deftest test-logx-call-site-capture ()
  "Verify call-site extraction when byte-compiling functions."
  (require 'bytecomp)
  (let ((byte-compile-current-form 'my-worker-function))
    (let ((site (logx--capture-call-site 'logx-info)))
      (should (eq (plist-get site :fn) 'my-worker-function))))
  (let ((byte-compile-current-form '(defun my-other-function () nil)))
    (let ((site (logx--capture-call-site 'logx-info)))
      (should (eq (plist-get site :fn) 'my-other-function)))))

(provide 'test-logx)
;;; test-logx.el ends here
