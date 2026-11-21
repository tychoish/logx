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

(ert-deftest test-jsonl-view-truncation-preview ()
  "Verify long strings render truncated prefix and ellipsis button."
  (let* ((long-val (make-string 100 ?x))
         (rec (list :msg long-val))
         (rendered (jsonl-view-default-formatter rec)))
    (should (string-match-p (make-string jsonl-view-truncate-length ?x) rendered))
    (should (string-match-p "…" rendered))))

(ert-deftest test-jsonl-view-collapse-all-midline ()
  "Verify collapse-all collapses buttons located mid-line."
  (with-temp-buffer
    (jsonl-view-mode)
    (let ((inhibit-read-only t))
      ;; Insert records with nested complex objects (buttons are mid-line after 'data:')
      (insert (jsonl-view-default-formatter '(:id 1 :data (:sub "val1"))))
      (insert (jsonl-view-default-formatter '(:id 2 :data (:sub "val2")))))
    (goto-char (point-min))
    ;; Expand line 1
    (jsonl-view-toggle-disclosure)
    ;; Line 1 should have inserted an expansion line
    (should (> (count-lines (point-min) (point-max)) 2))
    ;; Collapse all
    (jsonl-view-collapse-all)
    ;; Lines should return to 2
    (should (= (count-lines (point-min) (point-max)) 2))))

(ert-deftest test-jsonl-view-filter-hides-expansions ()
  "Verify filtering hides multi-line expansions alongside parent records."
  (with-temp-buffer
    (jsonl-view-mode)
    (let ((inhibit-read-only t))
      (insert (jsonl-view-default-formatter '(:id 1 :role "guest" :details (:note "secret"))))
      (insert (jsonl-view-default-formatter '(:id 2 :role "admin" :details (:note "public")))))
    (goto-char (point-min))
    ;; Expand guest record
    (jsonl-view-toggle-disclosure)
    ;; Filter to show only admin
    (jsonl-view-filter (lambda (rec) (equal (plist-get rec :role) "admin")))
    ;; Guest line and all its expansion lines must be hidden
    (save-excursion
      (goto-char (point-min))
      (should (eq (get-text-property (point) 'invisible) 'jsonl-filter-hidden))
      (forward-line 1)
      (while (get-text-property (point) 'jsonl-expansion)
        (should (eq (get-text-property (point) 'invisible) 'jsonl-filter-hidden))
        (forward-line 1))
      ;; Admin line must be visible
      (should-not (get-text-property (point) 'invisible)))))

(ert-deftest test-jsonl-view-omit-nil-fields ()
  "Verify default formatter omits fields with nil or :null values."
  (let* ((rec '(:id 10 :name "test" :missing nil :none :null :extra "present"))
         (rendered (jsonl-view-default-formatter rec)))
    (should (string-match-p "id: 10" rendered))
    (should (string-match-p "name: test" rendered))
    (should (string-match-p "extra: present" rendered))
    (should-not (string-match-p "missing" rendered))
    (should-not (string-match-p "none" rendered))))

(ert-deftest test-jsonl-view-disclosure-arrow-not-underlined ()
  "Verify disclosure button face specifies :underline nil."
  (should-not (face-attribute 'jsonl-view-disclosure-button :underline)))

(ert-deftest test-jsonl-view-expansion-styles ()
  "Verify 4 payload expansion styles: json-multiline, json-oneline, sexp-multiline, sexp-oneline."
  (let ((val '(:method "tools/call" :params (:name "my_tool" :arguments (:x 1)))))
    ;; 1. json-multiline (pretty formatted across multiple lines)
    (let ((fmt (jsonl-view--format-expansion val 'json-multiline)))
      (should (> (length (split-string fmt "\n")) 1))
      (should (string-match-p "\"method\": \"tools/call\"" fmt)))

    ;; 2. json-oneline (compact single line JSON)
    (let ((fmt (jsonl-view--format-expansion val 'json-oneline)))
      (should (= (length (split-string fmt "\n")) 1))
      (should (string-match-p "\"method\":\"tools/call\"" fmt)))

    ;; 3. sexp-multiline (pretty formatted across multiple lines)
    (let ((fmt (jsonl-view--format-expansion val 'sexp-multiline)))
      (should (> (length (split-string fmt "\n")) 1))
      (should (string-match-p ":method \"tools/call\"" fmt)))

    ;; 4. sexp-oneline (compact single line sexp)
    (let ((fmt (jsonl-view--format-expansion val 'sexp-oneline)))
      (should (= (length (split-string fmt "\n")) 1))
      (should (string-match-p ":method \"tools/call\"" fmt))))

  ;; Verify style cycling and buffer-local setters
  (with-temp-buffer
    (jsonl-view-mode)
    (should (eq jsonl-view-expansion-style 'json-multiline))
    (jsonl-view-set-style-json-oneline)
    (should (eq jsonl-view-expansion-style 'json-oneline))
    (jsonl-view-set-style-sexp-multiline)
    (should (eq jsonl-view-expansion-style 'sexp-multiline))
    (jsonl-view-set-style-sexp-oneline)
    (should (eq jsonl-view-expansion-style 'sexp-oneline))
    (jsonl-view-cycle-expansion-style)
    (should (eq jsonl-view-expansion-style 'json-multiline))))

(provide 'test-jsonl-view)
;;; test-jsonl-view.el ends here
