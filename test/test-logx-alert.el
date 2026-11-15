;;; test-logx-alert.el --- Tests for logx-alert -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'ert)
(require 'logx)
(require 'logx-alert)

(ert-deftest test-logx-alert-sink-severities ()
  "Verify alert severity mapping to logx levels."
  (let ((received nil))
    (cl-letf (((symbol-function 'logx--emit-event)
               (lambda (lvl cat msg src data)
                 (push (list :lvl lvl :cat cat :msg msg :src src :data data) received))))
      ;; emergency -> fatal
      (logx-alert-sink '(:message "System dying" :severity emergency :category sys))
      (should (equal (plist-get (car received) :lvl) 'fatal))
      (should (equal (plist-get (car received) :cat) 'sys))
      (should (equal (plist-get (car received) :msg) "System dying"))

      ;; urgent -> alert
      (logx-alert-sink '(:message "Action needed" :severity urgent :category ops))
      (should (equal (plist-get (car received) :lvl) 'alert))

      ;; high -> critical
      (logx-alert-sink '(:message "Service impaired" :severity high :category db))
      (should (equal (plist-get (car received) :lvl) 'critical))

      ;; moderate -> warn
      (logx-alert-sink '(:message "Disk space low" :severity moderate :category disk))
      (should (equal (plist-get (car received) :lvl) 'warn))

      ;; low -> debug
      (logx-alert-sink '(:message "Minor detail" :severity low :category debug))
      (should (equal (plist-get (car received) :lvl) 'debug))

      ;; default/normal -> info
      (logx-alert-sink '(:message "Task done" :severity normal :category task))
      (should (equal (plist-get (car received) :lvl) 'info)))))

(ert-deftest test-logx-alert-sink-title-formatting ()
  "Verify that title is prefixed when present."
  (let ((captured nil))
    (cl-letf (((symbol-function 'logx--emit-event)
               (lambda (_lvl _cat msg _src _data)
                 (setq captured msg))))
      (logx-alert-sink '(:title "AlertTitle" :message "AlertMessage"))
      (should (equal captured "[AlertTitle] AlertMessage"))

      (logx-alert-sink '(:message "AlertMessageOnly"))
      (should (equal captured "AlertMessageOnly"))

      ;; Title only (no message)
      (logx-alert-sink '(:title "AlertTitleOnly"))
      (should (equal captured "[AlertTitleOnly]"))

      ;; Neither title nor message
      (logx-alert-sink '(:severity high))
      (should (equal captured "Alert notification")))))

(ert-deftest test-logx-alert-register ()
  "Verify registration with alert-define-style when alert is available."
  (let ((registered nil))
    (cl-letf (((symbol-function 'alert-define-style)
               (lambda (name &rest args)
                 (setq registered (cons name args)))))
      (logx-alert-register)
      (should (equal (car registered) 'logx))
      (should (equal (plist-get (cdr registered) :title) "Logx Structured Logger"))
      (should (equal (plist-get (cdr registered) :notifier) #'logx-alert-sink)))))

(provide 'test-logx-alert)
;;; test-logx-alert.el ends here
