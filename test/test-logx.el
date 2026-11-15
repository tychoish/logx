;;; test-logx.el --- Tests for logx structured logging -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'ert)
(require 'logx (expand-file-name "../logx.el" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest test-logx-level-weights ()
  "Verify numeric weights and progression of severity levels."
  (should (< (logx-level-weight 'debug)
             (logx-level-weight 'info)))
  (should (< (logx-level-weight 'info)
             (logx-level-weight 'notice)))
  (should (< (logx-level-weight 'notice)
             (logx-level-weight 'warn)))
  (should (< (logx-level-weight 'warn)
             (logx-level-weight 'error)))
  (should (< (logx-level-weight 'error)
             (logx-level-weight 'fatal))))

(ert-deftest test-logx-level-gating ()
  "Verify global and category-specific severity thresholds."
  (let ((logx-default-level 'info)
        (logx-category-levels (make-hash-table :test #'equal)))
    ;; Global threshold checks
    (should-not (logx--level-enabled-p 'debug))
    (should (logx--level-enabled-p 'info))
    (should (logx--level-enabled-p 'warn))
    (should (logx--level-enabled-p 'error))

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
    (should side-effect-flag)))

(ert-deftest test-logx-ring-buffer-operations ()
  "Verify O(1) push, circular wrap-around, and FIFO retrieval."
  (let ((logx-ring-buffer-size 3))
    (logx--ring-init 3)
    (should (= (length (logx--ring-to-list)) 0))

    ;; Push 2 items
    (logx--ring-push '(:id 1 :msg "first"))
    (logx--ring-push '(:id 2 :msg "second"))
    (let ((items (logx--ring-to-list)))
      (should (= (length items) 2))
      (should (= (plist-get (car items) :id) 1))
      (should (= (plist-get (cadr items) :id) 2)))

    ;; Push 3rd item (full)
    (logx--ring-push '(:id 3 :msg "third"))
    (should (= (length (logx--ring-to-list)) 3))

    ;; Push 4th item (wrap around: 1 is discarded)
    (logx--ring-push '(:id 4 :msg "fourth"))
    (let ((items (logx--ring-to-list)))
      (should (= (length items) 3))
      (should (= (plist-get (car items) :id) 2))
      (should (= (plist-get (nth 1 items) :id) 3))
      (should (= (plist-get (nth 2 items) :id) 4)))

    ;; Clear
    (logx--ring-clear)
    (should (= (length (logx--ring-to-list)) 0))))

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
      ;; Must not hang or blow stack
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
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (let ((lines (split-string (buffer-string) "\n" t)))
              (should (= (length lines) 2))
              (should (string-match-p "Disk append event 1" (car lines)))
              (should (string-match-p "Disk append event 2" (cadr lines))))))
      (when (file-exists-p tmp-file)
        (delete-file tmp-file)))))

(ert-deftest test-logx-live-viewer-flush ()
  "Verify live viewer receives batched events."
  (let* ((buf (generate-new-buffer "*logx-test-viewer*"))
         (logx-default-level 'info))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (jsonl-view-mode))
          ;; Display buffer in a window so visibility check passes
          (set-window-buffer (selected-window) buf)
          (logx-register-viewer buf)

          ;; Emit events
          (logx-info :cat 'live :msg "Live event 1")
          (logx-info :cat 'live :msg "Live event 2")

          ;; Run synchronous flush
          (logx--flush-active-viewers)

          (with-current-buffer buf
            (let ((content (buffer-string)))
              (should (string-match-p "Live event 1" content))
              (should (string-match-p "Live event 2" content)))))
      (logx-unregister-viewer buf)
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest test-logx-multi-buffer-filtered-views ()
  "Verify specialized filtered buffers receive only matching events."
  (let* ((all-buf (generate-new-buffer "*logx-all*"))
         (err-buf (generate-new-buffer "*logx-errors*"))
         (logx-default-level 'debug))
    (unwind-protect
        (progn
          (with-current-buffer all-buf (jsonl-view-mode))
          (with-current-buffer err-buf (jsonl-view-mode))
          (set-window-buffer (selected-window) all-buf)

          ;; Register all-buf for all events, err-buf for only error/fatal
          (logx-register-viewer all-buf (lambda (_ev) t))
          (logx-register-viewer err-buf (lambda (ev) (eq (plist-get ev :lvl) 'error)))

          ;; Pretend err-buf is also visible by mocking get-buffer-window or setting it
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (b &optional _v) (and (memq b (list all-buf err-buf)) (selected-window)))))
            (logx-info :cat 't1 :msg "An informational message")
            (logx-error :cat 't1 :msg "A critical failure message")
            (logx--flush-active-viewers))

          ;; Check all-buf has both
          (with-current-buffer all-buf
            (should (string-match-p "An informational message" (buffer-string)))
            (should (string-match-p "A critical failure message" (buffer-string))))

          ;; Check err-buf has only error
          (with-current-buffer err-buf
            (should-not (string-match-p "An informational message" (buffer-string)))
            (should (string-match-p "A critical failure message" (buffer-string)))))
      (logx-unregister-viewer all-buf)
      (logx-unregister-viewer err-buf)
      (when (buffer-live-p all-buf) (kill-buffer all-buf))
      (when (buffer-live-p err-buf) (kill-buffer err-buf)))))

(ert-deftest test-logx-alert-sink ()
  "Verify alert.el notifications are routed to logx ring buffer."
  (logx--ring-clear)
  (logx-alert-sink '(:message "Alert test notification"
                     :severity urgent
                     :category test-alert))
  (let ((items (logx--ring-to-list)))
    (should (= (length items) 1))
    (let ((ev (car items)))
      (should (eq (plist-get ev :lvl) 'error))
      (should (string-match-p "Alert test notification" (plist-get ev :msg))))))

(provide 'test-logx)
;;; test-logx.el ends here
