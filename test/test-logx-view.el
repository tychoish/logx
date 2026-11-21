;;; test-logx-view.el --- Tests for logx-view -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'ert)
(require 'logx)
(require 'jsonl-view)
(require 'logx-view)

(ert-deftest test-logx-view-project-record ()
  "Verify record projection and field canonicalization."
  (let* ((raw '(:timestamp "2026-11-14T12:00:00Z"
                :severity "CRIT"
                :component "auth"
                :message "User login failed"
                :caller (:file "auth.el" :line 42)
                :uid 1001
                :ip "127.0.0.1"))
         (proj (logx-view-project-record raw)))
    (should (equal (plist-get proj :ts) "2026-11-14T12:00:00Z"))
    (should (equal (plist-get proj :lvl) 'critical))
    (should (equal (plist-get proj :cat) "auth"))
    (should (equal (plist-get proj :msg) "User login failed"))
    (should (equal (plist-get proj :src) '(:file "auth.el" :line 42)))
    (should (equal (plist-get (plist-get proj :data) :uid) 1001))
    (should (equal (plist-get (plist-get proj :data) :ip) "127.0.0.1"))))

(ert-deftest test-logx-view-project-record-aliases ()
  "Verify alias resolution for different logger schemas."
  (let* ((rec1 '(:level "alert" :logger "db" :msg "Out of memory"))
         (p1 (logx-view-project-record rec1)))
    (should (equal (plist-get p1 :lvl) 'alert))
    (should (equal (plist-get p1 :cat) "db"))
    (should (equal (plist-get p1 :msg) "Out of memory")))

  (let* ((rec2 '(:log_level "emerg" :subsystem "kernel" :event "Panic"))
         (p2 (logx-view-project-record rec2)))
    (should (equal (plist-get p2 :lvl) 'fatal))
    (should (equal (plist-get p2 :cat) "kernel"))
    (should (equal (plist-get p2 :msg) "Panic"))))

(ert-deftest test-logx-view-render-record-properties ()
  "Verify rendered line has proper text properties."
  (let* ((rec '(:ts 1700000000.0 :lvl warn :cat test :msg "Warning test" :code 404))
         (rendered (logx-view-render-record rec)))
    (should (stringp rendered))
    (should (string-match-p "WARN" rendered))
    (should (string-match-p "test:" rendered))
    (should (string-match-p "Warning test" rendered))
    (should (string-match-p "code: 404" rendered))
    ;; Verify metadata text properties preserved on the string
    (should (equal (get-text-property 0 'logx-lvl rendered) 'warn))
    (should (equal (get-text-property 0 'logx-cat rendered) 'test))
    (should (equal (get-text-property 0 'logx-ts rendered) 1700000000.0))))

(ert-deftest test-logx-view-filtering ()
  "Verify interactive severity and category filtering."
  (with-temp-buffer
    (logx-view-mode)
    (let ((inhibit-read-only t))
      (insert (logx-view-render-record '(:ts 100.0 :lvl debug :cat app :msg "d1")))
      (insert (logx-view-render-record '(:ts 101.0 :lvl info :cat sys :msg "i1")))
      (insert (logx-view-render-record '(:ts 102.0 :lvl warn :cat app :msg "w1")))
      (insert (logx-view-render-record '(:ts 103.0 :lvl error :cat sys :msg "e1"))))

    ;; Filter by level: error (should hide debug, info, warn)
    (logx-view-filter-level 'error)
    (goto-char (point-min))
    (should (invisible-p (point))) ;; debug is hidden
    (forward-line 1)
    (should (invisible-p (point))) ;; info is hidden
    (forward-line 1)
    (should (invisible-p (point))) ;; warn is hidden
    (forward-line 1)
    (should-not (invisible-p (point))) ;; error is visible

    ;; Clear filters
    (logx-view-clear-filters)
    (goto-char (point-min))
    (should-not (invisible-p (point)))
    (forward-line 1)
    (should-not (invisible-p (point)))

    ;; Filter by category regex: sys
    (logx-view-filter-category "sys")
    (goto-char (point-min))
    (should (invisible-p (point))) ;; app debug hidden
    (forward-line 1)
    (should-not (invisible-p (point))) ;; sys info visible
    (forward-line 1)
    (should (invisible-p (point))) ;; app warn hidden
    (forward-line 1)
    (should-not (invisible-p (point))) ;; sys error visible
    ))

(ert-deftest test-logx-view-cycle-timestamp ()
  "Verify cycling timestamp display formats."
  (with-temp-buffer
    (logx-view-mode)
    (setq logx-view-timestamp-mode :time)
    (let ((inhibit-read-only t))
      (insert (logx-view-render-record '(:ts 1700000000.0 :lvl info :cat app :msg "hello"))))
    (should (eq logx-view-timestamp-mode :time))

    (logx-view-cycle-timestamp-mode)
    (should (eq logx-view-timestamp-mode :delta))

    (logx-view-cycle-timestamp-mode)
    (should (eq logx-view-timestamp-mode :iso))

    (logx-view-cycle-timestamp-mode)
    (should (eq logx-view-timestamp-mode :time))))

(ert-deftest test-logx-view-live-batch-flushing ()
  "Verify batch delivery to live viewer buffers."
  (let ((buf (get-buffer-create "*test-logx-view-batch*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (logx-view-mode)
            (erase-buffer))
          (logx-view-register-viewer buf)

          ;; Mock window visibility for test buffer
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (b &optional _all)
                       (if (eq b buf) 'pseudo-window nil))))
            ;; Emit events via logx
            (logx--emit-event 'info 'batch "Event 1" nil '(:val 1))
            (logx--emit-event 'warn 'batch "Event 2" nil '(:val 2))

            ;; Trigger flush synchronously
            (logx-view--flush-active-viewers)

            (with-current-buffer buf
              (let ((content (buffer-string)))
                (should (string-match-p "Event 1" content))
                (should (string-match-p "Event 2" content))))))
      (logx-view-unregister-viewer buf)
      (kill-buffer buf))))

(ert-deftest test-logx-view-combined-filters ()
  "Verify level, category, and predicate filters compose cleanly."
  (with-temp-buffer
    (logx-view-mode)
    (let ((inhibit-read-only t))
      (insert (logx-view-render-record '(:ts 100.0 :lvl info :cat sys :msg "sys info" :tag "prod")))
      (insert (logx-view-render-record '(:ts 101.0 :lvl error :cat sys :msg "sys err" :tag "staging")))
      (insert (logx-view-render-record '(:ts 102.0 :lvl error :cat sys :msg "sys err prod" :tag "prod")))
      (insert (logx-view-render-record '(:ts 103.0 :lvl error :cat app :msg "app err prod" :tag "prod"))))

    ;; Apply level error + category sys + custom predicate (tag == prod)
    (setq logx-view-min-level 'error
          logx-view-category-filter "sys"
          jsonl-view-filter-predicate (lambda (rec) (equal (plist-get rec :tag) "prod")))
    (logx-view--apply-filters)

    ;; Line 1: info (hidden by level)
    (goto-char (point-min))
    (should (invisible-p (point)))

    ;; Line 2: staging (hidden by tag predicate)
    (forward-line 1)
    (should (invisible-p (point)))

    ;; Line 3: error + sys + prod (VISIBLE)
    (forward-line 1)
    (should-not (invisible-p (point)))

    ;; Line 4: app (hidden by category filter)
    (forward-line 1)
    (should (invisible-p (point)))))

(ert-deftest test-logx-view-cleanup-on-kill ()
  "Verify killing last viewer cancels timer and clears pending events."
  (let ((buf (get-buffer-create "*test-logx-view-kill*")))
    (with-current-buffer buf
      (logx-view-mode))
    (logx-view-register-viewer buf)
    (logx--emit-event 'info 'test "Pending ev" nil nil)
    (should logx-view--pending-events)
    ;; Kill the buffer
    (kill-buffer buf)
    ;; active viewers should be empty, timer nil, pending events nil
    (should-not logx-view--active-viewers)
    (should-not logx-view--flush-timer)
    (should-not logx-view--pending-events)))

(ert-deftest test-logx-view-fork-buffer ()
  "Verify logx-fork-filtered-buffer creates a filtered live buffer."
  (let ((logx--ring (make-ring 10)))
    (logx--ring-push '(:seq 1 :lvl info :cat "net" :msg "Net info"))
    (logx--ring-push '(:seq 2 :lvl error :cat "net" :msg "Net error"))
    (logx--ring-push '(:seq 3 :lvl error :cat "fs" :msg "FS error"))
    (let ((forked-buf (logx-fork-filtered-buffer "test-net-err"
                                                 (lambda (ev)
                                                   (and (eq (plist-get ev :lvl) 'error)
                                                        (equal (plist-get ev :cat) "net"))))))
      (unwind-protect
          (with-current-buffer forked-buf
            (let ((content (buffer-string)))
              (should-not (string-match-p "Net info" content))
              (should (string-match-p "Net error" content))
              (should-not (string-match-p "FS error" content))))
        (kill-buffer forked-buf)))))

(ert-deftest test-logx-view-omit-nil-fields ()
  "Verify nil and :null fields are omitted from projected data and rendered output."
  (let* ((rec '(:seq 5012 :ts 100.0 :lvl info :cat root :msg "hello"
                :src (:file "test.el" :line 10) :data nil :extra_nil nil :other :null :valid 42))
         (proj (logx-view-project-record rec))
         (rendered (logx-view-render-record rec)))
    ;; In projection, data should only contain non-nil fields
    (should (equal (plist-get proj :data) '(:seq 5012 :valid 42)))
    ;; Rendered text should not contain "data: nil" or "extra_nil:" or "other:"
    (should-not (string-match-p "data: nil" rendered))
    (should-not (string-match-p "extra_nil" rendered))
    (should-not (string-match-p "other" rendered))
    (should (string-match-p "seq: 5012" rendered))
    (should (string-match-p "valid: 42" rendered))))

(ert-deftest test-logx-view-source-whitespace-not-underlined ()
  "Verify whitespace before source button is not part of the button."
  (let* ((rec '(:seq 1 :ts 100.0 :lvl info :cat root :msg "hi" :src (:file "test.el" :line 10)))
         (rendered (logx-view-render-record rec))
         (src-pos (string-match "test\\.el:10" rendered)))
    (should src-pos)
    ;; The character right before the button is a space and should NOT have the button property or link face
    (let ((space-pos (1- src-pos)))
      (should-not (get-text-property space-pos 'button rendered))
      (should-not (eq (get-text-property space-pos 'face rendered) 'logx-view-source-link)))))

(ert-deftest test-logx-view-background-buffer-flush ()
  "Verify events are delivered to active viewer buffers even when not visible in a window."
  (let ((buf (get-buffer-create "*test-logx-view-bg*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (logx-view-mode))
          (logx-view-register-viewer buf)
          ;; Ensure buffer has no visible window
          (should-not (get-buffer-window buf 'visible))
          ;; Push event
          (logx--emit-event 'info 'bg "Background message" nil '(:val 99))
          ;; Flush
          (logx-view--flush-active-viewers)
          ;; Buffer should contain the rendered event
          (with-current-buffer buf
            (should (string-match-p "Background message" (buffer-string)))
            (should (string-match-p "data:" (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest test-logx-view-refresh ()
  "Verify logx-view-refresh restores ring buffer contents."
  (let ((buf (get-buffer-create "*test-logx-refresh*"))
        (logx--ring (make-ring 10)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (logx-view-mode))
          (logx--ring-push '(:seq 1 :lvl info :cat "r" :msg "Msg 1"))
          (logx--ring-push '(:seq 2 :lvl info :cat "r" :msg "Msg 2"))
          (with-current-buffer buf
            (logx-view-refresh)
            (should (string-match-p "Msg 1" (buffer-string)))
            (should (string-match-p "Msg 2" (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest test-logx-view-fn-source-label ()
  "Verify that source link formats file:fn when line is nil, and does not format file:1."
  (let* ((rec1 '(:seq 1 :ts 100.0 :lvl info :cat root :msg "test"
                 :src (:file "test.el" :line nil :fn my-func)))
         (rec2 '(:seq 2 :ts 100.0 :lvl info :cat root :msg "test"
                 :src (:file "test.el" :line nil :fn nil)))
         (rendered1 (logx-view-render-record rec1))
         (rendered2 (logx-view-render-record rec2)))
    (should (string-match-p "test\\.el:my-func" rendered1))
    (should-not (string-match-p "test\\.el:1" rendered1))
    (should (string-match-p "test\\.el\n" rendered2))))

(provide 'test-logx-view)
;;; test-logx-view.el ends here
