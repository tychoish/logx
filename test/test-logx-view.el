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

(provide 'test-logx-view)
;;; test-logx-view.el ends here
