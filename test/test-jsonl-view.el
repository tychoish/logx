;;; test-jsonl-view.el --- Tests for jsonl-view mode -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'ert)
(require 'jsonl-view (expand-file-name "../jsonl-view.el" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest test-jsonl-view-parse-line ()
  "Verify parsing of valid JSON lines and empty string handling."
  (should-not (jsonl-view--parse-line ""))
  (should-not (jsonl-view--parse-line "   "))
  (should-not (jsonl-view--parse-line "not valid json {"))

  (let ((parsed (jsonl-view--parse-line "{\"ts\":1700000000,\"msg\":\"hello\"}")))
    (should parsed)
    (should (= (plist-get parsed :ts) 1700000000))
    (should (equal (plist-get parsed :msg) "hello"))))

(ert-deftest test-jsonl-view-schema-projection ()
  "Verify recognition of standard log keys and generic dataset fallback."
  ;; Standard log vocabulary (ts, level, cat, msg, src)
  (let* ((rec '(:timestamp "2026-11-14T12:00:00Z"
                :severity "WARN"
                :component "db.pool"
                :summary "Connection timeout"
                :caller (:file "pool.el" :line 45)
                :extra-count 3))
         (proj (jsonl-view--project-record rec)))
    (should (equal (plist-get proj :ts) "2026-11-14T12:00:00Z"))
    (should (eq (plist-get proj :lvl) 'warn))
    (should (equal (plist-get proj :cat) "db.pool"))
    (should (equal (plist-get proj :msg) "Connection timeout"))
    (should (equal (plist-get (plist-get proj :src) :file) "pool.el"))
    (should (= (plist-get (plist-get proj :data) :extra-count) 3)))

  ;; Generic non-log dataset (arbitrary keys)
  (let* ((rec '(:id 101 :name "Alice" :role "admin"))
         (proj (jsonl-view--project-record rec)))
    (should (eq (plist-get proj :lvl) 'info))
    (should (= (plist-get (plist-get proj :data) :id) 101))
    (should (equal (plist-get (plist-get proj :data) :name) "Alice"))))

(ert-deftest test-jsonl-view-timestamp-formatting ()
  "Verify timestamp transformations across time, delta, and ISO modes."
  (let ((ts 1700000000.0)
        (next-ts 1700000000.045))
    ;; Format time mode returns non-empty string
    (let ((formatted (jsonl-view--format-timestamp ts :time)))
      (should (stringp formatted))
      (should (> (length formatted) 0)))

    ;; Delta mode calculation
    (let ((delta-str (jsonl-view--format-timestamp next-ts :delta ts)))
      (should (string-match-p "\\+45ms" delta-str)))

    ;; ISO mode formatting
    (let ((iso-str (jsonl-view--format-timestamp ts :iso)))
      (should (string-match-p "T" iso-str)))))

(ert-deftest test-jsonl-view-render-record ()
  "Verify rendered lines contain expected text properties and metadata."
  (let* ((rec '(:ts 1700000000.0
                :lvl "error"
                :cat "worker"
                :msg "Job failed"
                :src (:file "worker.el" :line 88)
                :retry 2))
         (rendered (jsonl-view-render-record rec)))
    (should (stringp rendered))
    (should (string-match-p "\\[ERROR\\]" rendered))
    (should (string-match-p "worker:" rendered))
    (should (string-match-p "Job failed" rendered))
    (should (string-match-p "worker.el:88" rendered))
    ;; Metadata properties on string
    (should (equal (get-text-property 0 'jsonl-lvl rendered) 'error))
    (should (equal (get-text-property 0 'jsonl-cat rendered) "worker"))
    (should (plist-get (get-text-property 0 'jsonl-source rendered) :file))))

(ert-deftest test-jsonl-view-invisibility-filtering ()
  "Verify level and category filters set invisible text property without deleting lines."
  (with-temp-buffer
    (jsonl-view-mode)
    (let ((inhibit-read-only t))
      (insert (jsonl-view-render-record '(:ts 100 :lvl "debug" :cat "asq" :msg "debug message")))
      (insert (jsonl-view-render-record '(:ts 101 :lvl "error" :cat "asq" :msg "error message")))
      (insert (jsonl-view-render-record '(:ts 102 :lvl "info" :cat "other" :msg "info message"))))

    ;; Filter minimum severity: error
    (jsonl-view-filter-level 'error)
    (save-excursion
      ;; Line 1 (debug) must be hidden
      (goto-char (point-min))
      (should (eq (get-text-property (point) 'invisible) 'jsonl-filter-hidden))
      ;; Line 2 (error) must be visible
      (forward-line 1)
      (should-not (get-text-property (point) 'invisible))
      ;; Line 3 (info) must be hidden
      (forward-line 1)
      (should (eq (get-text-property (point) 'invisible) 'jsonl-filter-hidden)))

    ;; Clear filters restores all lines
    (jsonl-view-clear-filters)
    (save-excursion
      (goto-char (point-min))
      (should-not (get-text-property (point) 'invisible))
      (forward-line 1)
      (should-not (get-text-property (point) 'invisible))
      (forward-line 1)
      (should-not (get-text-property (point) 'invisible)))

    ;; Filter by category: asq
    (jsonl-view-filter-category "asq")
    (save-excursion
      (goto-char (point-min))
      (should-not (get-text-property (point) 'invisible))
      (forward-line 1)
      (should-not (get-text-property (point) 'invisible))
      (forward-line 1)
      (should (eq (get-text-property (point) 'invisible) 'jsonl-filter-hidden)))))

(ert-deftest test-jsonl-view-copy-as-json ()
  "Verify copying record at point to kill-ring."
  (with-temp-buffer
    (jsonl-view-mode)
    (let ((inhibit-read-only t))
      (insert (jsonl-view-render-record '(:ts 100 :msg "copy me" :count 5))))
    (goto-char (point-min))
    (jsonl-view-copy-as-json)
    (should (string-match-p "copy me" (current-kill 0)))))

(provide 'test-jsonl-view)
;;; test-jsonl-view.el ends here
