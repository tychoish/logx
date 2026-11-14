;;; logx.el --- Structured logging framework for Emacs -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, logging
;; URL: https://github.com/tychoish/logx

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; High-performance structured logging framework for Emacs.
;;
;; Features:
;; - Zero-cost disabled logging fast-path via severity gating macros.
;; - Compile-time call-site source tracking via `symbols-with-pos`.
;; - In-memory fixed-capacity circular ring buffer.
;; - Fast C-level JSON Lines (`json-serialize`) disk appender with object sanitization.
;; - Optional message-quarantine mode to silence chatty background subsystems.

;;; Code:

(provide 'logx)
;;; logx.el ends here
