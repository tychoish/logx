;;; jsonl-view.el --- General-purpose line-oriented JSON viewer -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, files
;; URL: https://github.com/tychoish/logx

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Interactive line-oriented JSON (JSON Lines / NDJSON) viewer mode.
;;
;; Features:
;; - Interactive inspection for arbitrary `.jsonl` and `.ndjson` files.
;; - In-place collapsible disclosure widgets (`TAB` / `S-TAB`) for nested objects and long strings.
;; - Dynamic timestamp format cycling (wall-clock, time-of-day, relative delta).
;; - High-performance zero-copy severity and category filtering using `buffer-invisibility-spec`.
;; - Pluggable schema projection heuristics for logs and generic JSON datasets.
;; - Live streaming and in-memory ring buffer viewer support.

;;; Code:

(provide 'jsonl-view)
;;; jsonl-view.el ends here
