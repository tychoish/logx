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
             (logx-level-weight 'fatal))))

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
    (should (logx--level-enabled-p 'error 'chatty.lib))))

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
        (should (equal (plist-get parsed :msg) "Processing item"))))))

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

(provide 'test-logx)
;;; test-logx.el ends here
