;;; -*- mode: lisp-data -*-
emacs-lisp-mode

;; Logx Structured Logging
(logxi "(logx-info :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-info")
(logx-info "(logx-info :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-info")
(logxd "(logx-debug :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-debug")
(logx-debug "(logx-debug :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-debug")
(logxw "(logx-warn :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-warn")
(logx-warn "(logx-warn :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-warn")
(logxe "(logx-error :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-error")
(logx-error "(logx-error :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-error")
(logxn "(logx-notice :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-notice")
(logx-notice "(logx-notice :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-notice")
(logxf "(logx-fatal :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-fatal")
(logx-fatal "(logx-fatal :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx-fatal")
(logx "(logx " (p ":info" lvl) " :cat '" (p "cat") " :msg \"" (p "message") "\"" q ")" :doc "logx generic level")
(logxm "(logx-" (p "info" lvl) " :cat '" (p "cat") n> ":msg \"" (p "message") "\"" n> ":" (p "key") " " (p "val") q ")" :doc "logx multiline with key-value data")
(logx-data "(logx-" (p "info" lvl) " :cat '" (p "cat") n> ":msg \"" (p "message") "\"" n> ":" (p "key") " " (p "val") q ")" :doc "logx multiline with key-value data")
(logx-try "(condition-case err" n> r> n> "(error" n> "(logx-error :cat '" (p "cat") n> ":msg (format \"" (p "operation") " failed: %s\" (error-message-string err))" n> ":error (error-message-string err)" q ")))" :doc "condition-case with logx-error")
(logx-time "(let ((start-time (float-time)))" n> "(unwind-protect" n> "(progn" n> r> ")" n> "(let ((dur (* (- (float-time) start-time) 1000.0)))" n> "(logx-info :cat '" (p "cat") n> ":msg (format \"" (p "operation") " completed in %.2fms\" dur)" n> ":duration_ms dur" q "))))" :doc "timed execution block with logx-info")
(logx-set "(logx-set-category-level '" (p "cat") " '" (p "level") ")" q :doc "logx-set-category-level")
