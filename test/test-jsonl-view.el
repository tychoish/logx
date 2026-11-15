;;; test-jsonl-view.el --- Tests for generic jsonl-view mode -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'ert)
(require 'jsonl-view)

(ert-deftest test-jsonl-view-parse-line ()
  "Verify parsing of valid JSON lines and empty string handling."
  (should-not (jsonl-view-parse-line ""))
  (should-not (jsonl-view-parse-line "   "))
  (should-not (jsonl-view-parse-line "not valid json {"))

  (let ((parsed (jsonl-view-parse-line "{\"id\":101,\"name\":\"Alice\"}")))
    (should parsed)
    (should (= (plist-get parsed :id) 101))
    (should (equal (plist-get parsed :name) "Alice"))))

(ert-deftest test-jsonl-view-default-formatter ()
  "Verify generic formatting of arbitrary JSON record."
  (let* ((rec '(:user "alice" :role "admin" :score 95))
         (rendered (jsonl-view-default-formatter rec)))
    (should (stringp rendered))
    (should (string-match-p "user:.*alice" rendered))
    (should (string-match-p "role:.*admin" rendered))
    (should (string-match-p "score:.*95" rendered))
    (should (equal (get-text-property 0 'jsonl-record rendered) rec))))

(ert-deftest test-jsonl-view-generic-filtering ()
  "Verify predicate filtering hides non-matching lines using invisibility."
  (with-temp-buffer
    (jsonl-view-mode)
    (let ((inhibit-read-only t))
      (insert (jsonl-view-default-formatter '(:id 1 :role "admin")))
      (insert (jsonl-view-default-formatter '(:id 2 :role "guest")))
      (insert (jsonl-view-default-formatter '(:id 3 :role "admin"))))

    ;; Filter by predicate: role == admin
    (jsonl-view-filter (lambda (rec) (equal (plist-get rec :role) "admin")))
    (save-excursion
      (goto-char (point-min))
      ;; Line 1 visible
      (should-not (get-text-property (point) 'invisible))
      ;; Line 2 hidden
      (forward-line 1)
      (should (eq (get-text-property (point) 'invisible) 'jsonl-filter-hidden))
      ;; Line 3 visible
      (forward-line 1)
      (should-not (get-text-property (point) 'invisible)))

    ;; Clear filters restores line 2
    (jsonl-view-clear-filters)
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (should-not (get-text-property (point) 'invisible)))))

(ert-deftest test-jsonl-view-copy-as-json ()
  "Verify copying record at point to kill-ring."
  (with-temp-buffer
    (jsonl-view-mode)
    (let ((inhibit-read-only t))
      (insert (jsonl-view-default-formatter '(:user "bob" :count 5))))
    (goto-char (point-min))
    (jsonl-view-copy-as-json)
    (should (string-match-p "bob" (current-kill 0)))))

(provide 'test-jsonl-view)
;;; test-jsonl-view.el ends here
