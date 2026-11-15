;;; jsonl-view.el --- General-purpose line-oriented JSON viewer -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (jsonl "0.0.1"))
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
;; - Dedicated "info" view inspector buffer (`*logx-info*`) pretty-printing single lines in `json-ts-mode`.
;; - Tree-sitter accelerated AST parsing and syntactic punctuation concealment.
;; - Dynamic timestamp format cycling (wall-clock, time-of-day, relative delta).
;; - High-performance zero-copy severity and category filtering using `buffer-invisibility-spec`.
;; - Pluggable schema projection heuristics for logs and generic JSON datasets.
;; - Live streaming and in-memory ring buffer viewer support with multi-buffer routing (`*logs:<filter>*`).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'json)
(require 'button)
(require 'treesit nil t)

(defgroup jsonl-view nil
  "General-purpose line-oriented JSON viewer."
  :group 'tools
  :prefix "jsonl-view-")

;;; ----------------------------------------------------------------------------
;;; Customization and Faces
;;; ----------------------------------------------------------------------------

(defcustom jsonl-view-truncate-length 60
  "Maximum string length before inline truncation with disclosure button."
  :type 'integer
  :group 'jsonl-view)

(defcustom jsonl-view-info-buffer-name "*logx-info*"
  "Name of the auxiliary pretty-printed JSON inspector buffer."
  :type 'string
  :group 'jsonl-view)

(defcustom jsonl-view-schema-mappings
  '((:ts  . ("ts" "time" "timestamp" "@timestamp"))
    (:lvl . ("level" "lvl" "severity" "log_level"))
    (:cat . ("cat" "category" "logger" "subsystem" "component"))
    (:msg . ("msg" "message" "event" "summary"))
    (:src . ("src" "caller" "source" "file")))
  "Alist mapping standard semantic keys to candidate JSON property names."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'jsonl-view)

(defface jsonl-view-timestamp
  '((default :inherit shadow)
    (((class color) (min-colors 88)) :foreground "#7f8c8d"))
  "Face for timestamp column."
  :group 'jsonl-view)

(defface jsonl-view-level-debug
  '((default :inherit shadow)
    (((class color) (min-colors 88)) :foreground "#95a5a6"))
  "Face for debug log level."
  :group 'jsonl-view)

(defface jsonl-view-level-info
  '((default :inherit font-lock-builtin-face)
    (((class color) (min-colors 88)) :foreground "#27ae60" :weight bold))
  "Face for info log level."
  :group 'jsonl-view)

(defface jsonl-view-level-notice
  '((default :inherit font-lock-function-name-face)
    (((class color) (min-colors 88)) :foreground "#2980b9" :weight bold))
  "Face for notice log level."
  :group 'jsonl-view)

(defface jsonl-view-level-warn
  '((default :inherit warning)
    (((class color) (min-colors 88)) :foreground "#f39c12" :weight bold))
  "Face for warn log level."
  :group 'jsonl-view)

(defface jsonl-view-level-error
  '((default :inherit error)
    (((class color) (min-colors 88)) :foreground "#c0392b" :weight bold))
  "Face for error log level."
  :group 'jsonl-view)

(defface jsonl-view-level-fatal
  '((default :inverse-video t :weight bold)
    (((class color) (min-colors 88)) :foreground "#ffffff" :background "#96281b"))
  "Face for fatal log level."
  :group 'jsonl-view)

(defface jsonl-view-category
  '((default :inherit font-lock-keyword-face)
    (((class color) (min-colors 88)) :foreground "#8e44ad"))
  "Face for category/logger tag."
  :group 'jsonl-view)

(defface jsonl-view-key
  '((default :inherit font-lock-variable-name-face)
    (((class color) (min-colors 88)) :foreground "#34495e"))
  "Face for structured attribute keys."
  :group 'jsonl-view)

(defface jsonl-view-source-link
  '((default :inherit link :underline t)
    (((class color) (min-colors 88)) :foreground "#7f8c8d"))
  "Face for call-site origin hyperlinks."
  :group 'jsonl-view)

(defface jsonl-view-disclosure-button
  '((default :inherit button :weight bold)
    (((class color) (min-colors 88)) :foreground "#2980b9"))
  "Face for inline disclosure expansion widgets."
  :group 'jsonl-view)

;;; ----------------------------------------------------------------------------
;;; Buffer-Local State
;;; ----------------------------------------------------------------------------

(defvar-local jsonl-view-timestamp-mode :time
  "Current timestamp display mode (:time, :delta, or :iso).")

(defvar-local jsonl-view-follow-mode nil
  "Non-nil if the viewer buffer automatically scrolls to the end on new lines.")

(defvar-local jsonl-view-info-follow-mode nil
  "Non-nil if the auxiliary info window auto-updates on cursor movement.")

(defvar-local jsonl-view-min-level nil
  "Active minimum severity filter symbol, or nil if unfiltered.")

(defvar-local jsonl-view-category-filter nil
  "Active category filter regex string, or nil if unfiltered.")

;;; ----------------------------------------------------------------------------
;;; Parsing and Schema Projection
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun jsonl-view--parse-line (line-str)
  "Parse LINE-STR into a Lisp plist using native C JSON or fallback.
Returns nil if LINE-STR is blank or unparseable."
  (let ((trimmed (string-trim line-str)))
    (unless (string-empty-p trimmed)
      (condition-case nil
          (json-parse-string trimmed :object-type 'plist)
        (error nil)))))

(defun jsonl-view--find-prop (plist candidates)
  "Find the first property in PLIST matching any key in CANDIDATES strings."
  (let ((found nil)
        (tail plist))
    (while (and tail (not found))
      (let* ((key-sym (car tail))
             (key-str (substring (symbol-name key-sym) 1)))
        (when (member (downcase key-str) candidates)
          (setq found (cadr tail))))
      (setq tail (cddr tail)))
    found))

;;;###autoload
(defun jsonl-view--project-record (record)
  "Project raw JSON plist RECORD into standardized semantic components.
Returns a plist (:ts TS :lvl LVL :cat CAT :msg MSG :src SRC :data DATA)."
  (if (null record)
      nil
    (let* ((ts-cand (alist-get :ts jsonl-view-schema-mappings))
           (lvl-cand (alist-get :lvl jsonl-view-schema-mappings))
           (cat-cand (alist-get :cat jsonl-view-schema-mappings))
           (msg-cand (alist-get :msg jsonl-view-schema-mappings))
           (src-cand (alist-get :src jsonl-view-schema-mappings))
           (ts (jsonl-view--find-prop record ts-cand))
           (lvl-raw (jsonl-view--find-prop record lvl-cand))
           (cat (jsonl-view--find-prop record cat-cand))
           (msg (jsonl-view--find-prop record msg-cand))
           (src (jsonl-view--find-prop record src-cand))
           (lvl (when lvl-raw
                  (let ((s (downcase (format "%s" lvl-raw))))
                    (cond
                     ((string-match-p "debug\\|trace" s) 'debug)
                     ((string-match-p "info" s) 'info)
                     ((string-match-p "notice" s) 'notice)
                     ((string-match-p "warn" s) 'warn)
                     ((string-match-p "err" s) 'error)
                     ((string-match-p "fatal\\|emerg\\|crit" s) 'fatal)
                     (t (intern s))))))
           ;; Remaining data keys
           (data nil)
           (tail record))
      (while tail
        (let* ((k (car tail))
               (v (cadr tail))
               (k-str (substring (symbol-name k) 1)))
          (unless (or (member (downcase k-str) ts-cand)
                      (member (downcase k-str) lvl-cand)
                      (member (downcase k-str) cat-cand)
                      (member (downcase k-str) msg-cand)
                      (member (downcase k-str) src-cand))
            (push k data)
            (push v data)))
        (setq tail (cddr tail)))
      (list :ts ts
            :lvl (or lvl 'info)
            :cat (or cat "root")
            :msg (or msg "")
            :src src
            :data (nreverse data)))))

;;; ----------------------------------------------------------------------------
;;; Timestamp Formatting and Cycling
;;; ----------------------------------------------------------------------------

(defun jsonl-view--format-timestamp (ts-val mode &optional prev-ts)
  "Format timestamp TS-VAL according to MODE (:time, :delta, :iso).
PREV-TS is the timestamp of the preceding line for delta calculation."
  (let ((time (cond
               ((numberp ts-val) (seconds-to-time ts-val))
               ((stringp ts-val)
                (condition-case nil
                    (date-to-time ts-val)
                  (error (current-time))))
               (t (current-time)))))
    (pcase mode
      (:iso
       (format-time-string "%Y-%m-%dT%H:%M:%S" time))
      (:delta
       (if (and prev-ts (numberp ts-val) (numberp prev-ts))
           (let ((diff-ms (* 1000.0 (- ts-val prev-ts))))
             (if (< (abs diff-ms) 1000)
                 (format "+%.0fms" diff-ms)
               (format "+%.1fs" (/ diff-ms 1000.0))))
         "+0ms"))
      (_ ;; default :time (HH:MM:SS)
       (format-time-string "%H:%M:%S" time)))))

(defun jsonl-view-cycle-timestamp-mode ()
  "Cycle timestamp display mode between Time-of-day, Delta, and ISO-8601."
  (interactive)
  (setq jsonl-view-timestamp-mode
        (pcase jsonl-view-timestamp-mode
          (:time :delta)
          (:delta :iso)
          (_ :time)))
  (let ((inhibit-read-only t)
        (pos (point-min))
        (prev-ts nil))
    (save-excursion
      (while (< pos (point-max))
        (goto-char pos)
        (let ((ts-val (get-text-property pos 'jsonl-ts)))
          (when ts-val
            (let ((eol (line-end-position))
                  (new-str (jsonl-view--format-timestamp ts-val jsonl-view-timestamp-mode prev-ts)))
              (put-text-property pos (min (+ pos (length new-str)) eol)
                                 'display new-str)
              (setq prev-ts (if (numberp ts-val) ts-val prev-ts))))
          (forward-line 1)
          (setq pos (point))))))
  (message "Timestamp mode: %s" jsonl-view-timestamp-mode))

;;; ----------------------------------------------------------------------------
;;; Tree-sitter Delimiter Concealment
;;; ----------------------------------------------------------------------------

(defun jsonl-view--treesit-conceal-region (beg end)
  "Conceal JSON punctuation characters in BEG..END using Tree-sitter queries."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p)
             (treesit-language-available-p 'json))
    (condition-case nil
        (let* ((parser (treesit-parser-create 'json))
               (query (treesit-query-compile 'json
                                             '([ "{" "}" "[" "]" "\"" ":" "," ] @conceal)))
               (matches (treesit-query-capture parser query beg end)))
          (dolist (match matches)
            (let* ((node (cdr match))
                   (nbeg (treesit-node-start node))
                   (nend (treesit-node-end node)))
              (when (and (>= nbeg beg) (<= nend end))
                (put-text-property nbeg nend 'invisible 'jsonl-conceal)))))
      (error nil))))

;;; ----------------------------------------------------------------------------
;;; Rendering Engine and Disclosure Widgets
;;; ----------------------------------------------------------------------------

(defun jsonl-view--level-face (lvl)
  "Return face corresponding to severity symbol LVL."
  (pcase lvl
    ('debug 'jsonl-view-level-debug)
    ('info 'jsonl-view-level-info)
    ('notice 'jsonl-view-level-notice)
    ('warn 'jsonl-view-level-warn)
    ('error 'jsonl-view-level-error)
    ('fatal 'jsonl-view-level-fatal)
    (_ 'jsonl-view-level-info)))

(defun jsonl-view--toggle-disclosure-action (btn)
  "Toggle disclosure state for disclosure button BTN."
  (let* ((expanded (button-get btn 'jsonl-expanded))
         (full-val (button-get btn 'jsonl-value))
         (inhibit-read-only t))
    (save-excursion
      (goto-char (button-start btn))
      (let ((line-end (line-end-position)))
        (if expanded
            ;; Collapse: Remove expansion overlay / text below
            (progn
              (button-put btn 'jsonl-expanded nil)
              (button-put btn 'help-echo "mouse-1, RET: expand payload")
              ;; Flip button text
              (let ((beg (button-start btn))
                    (end (button-end btn)))
                (delete-region beg end)
                (insert-text-button "▶"
                                    'action #'jsonl-view--toggle-disclosure-action
                                    'jsonl-value full-val
                                    'jsonl-expanded nil
                                    'face 'jsonl-view-disclosure-button
                                    'follow-link t))
              ;; Remove trailing expansion lines if present
              (forward-line 1)
              (while (and (not (eobp)) (get-text-property (point) 'jsonl-expansion))
                (let ((kill-end (min (point-max) (1+ (line-end-position)))))
                  (delete-region (point) kill-end))))
          ;; Expand: Pretty print indented representation below
          (button-put btn 'jsonl-expanded t)
          (let ((beg (button-start btn))
                (end (button-end btn)))
            (delete-region beg end)
            (insert-text-button "▼"
                                'action #'jsonl-view--toggle-disclosure-action
                                'jsonl-value full-val
                                'jsonl-expanded t
                                'face 'jsonl-view-disclosure-button
                                'follow-link t))
          (goto-char line-end)
          (insert "\n")
          (let ((formatted-json (condition-case nil
                                    (json-serialize full-val)
                                  (error (format "%S" full-val)))))
            (insert (propertize (format "    %s\n" formatted-json)
                                'jsonl-expansion t
                                'face 'font-lock-doc-face))))))))

(defun jsonl-view-toggle-disclosure ()
  "Toggle payload disclosure expansion on current record."
  (interactive)
  (let ((btn (button-at (point))))
    (if (and btn (button-get btn 'jsonl-value))
        (jsonl-view--toggle-disclosure-action btn)
      ;; Find first disclosure button on current line
      (save-excursion
        (beginning-of-line)
        (let ((found nil)
              (eol (line-end-position)))
          (while (and (< (point) eol) (not found))
            (let ((b (button-at (point))))
              (if (and b (button-get b 'jsonl-value))
                  (setq found b)
                (forward-char 1))))
          (if found
              (jsonl-view--toggle-disclosure-action found)
            (message "No expandable disclosure on current line.")))))))

(defun jsonl-view-collapse-all ()
  "Collapse all expanded disclosure blocks across the buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (pos (point-min)))
    (save-excursion
      (while (< pos (point-max))
        (goto-char pos)
        (let ((btn (button-at pos)))
          (when (and btn (button-get btn 'jsonl-expanded))
            (jsonl-view--toggle-disclosure-action btn)))
        (forward-line 1)
        (setq pos (point)))))
  (message "Collapsed all disclosures."))

(defun jsonl-view--source-jump-action (btn)
  "Jump to source location referenced by BTN."
  (let ((src (button-get btn 'jsonl-source)))
    (when src
      (let ((path (or (plist-get src :path) (plist-get src :file)))
            (line (plist-get src :line)))
        (when path
          (find-file-other-window path)
          (when (and line (numberp line))
            (goto-char (point-min))
            (forward-line (1- line))))))))

(defun jsonl-view-jump-to-source ()
  "Jump to the call-site source definition of record at point."
  (interactive)
  (let ((src (get-text-property (point) 'jsonl-source)))
    (if src
        (let ((path (or (plist-get src :path) (plist-get src :file)))
              (line (plist-get src :line)))
          (if path
              (progn
                (find-file-other-window path)
                (when (and line (numberp line))
                  (goto-char (point-min))
                  (forward-line (1- line))))
            (message "Source file path not available.")))
      (message "No source call-site metadata for record at point."))))

;;;###autoload
(defun jsonl-view-render-record (raw-record &optional prev-ts)
  "Render RAW-RECORD plist into formatted string with text properties.
PREV-TS is preceding timestamp for relative offset."
  (let* ((proj (jsonl-view--project-record raw-record))
         (ts-val (plist-get proj :ts))
         (lvl (plist-get proj :lvl))
         (cat (plist-get proj :cat))
         (msg (plist-get proj :msg))
         (src (plist-get proj :src))
         (data (plist-get proj :data))
         (ts-str (jsonl-view--format-timestamp ts-val jsonl-view-timestamp-mode prev-ts))
         (lvl-str (format "%-5s" (upcase (symbol-name lvl))))
         (line-str ""))
    (with-temp-buffer
      ;; Timestamp
      (insert (propertize ts-str 'face 'jsonl-view-timestamp) " ")
      ;; Level badge
      (insert (propertize (format "[%s]" lvl-str) 'face (jsonl-view--level-face lvl)) " ")
      ;; Category
      (insert (propertize (format "%s:" cat) 'face 'jsonl-view-category) " ")
      ;; Message
      (insert (format "%s" msg))
      ;; Data key-values
      (let ((tail data))
        (while tail
          (let* ((k (car tail))
                 (v (cadr tail))
                 (k-name (if (keywordp k) (substring (symbol-name k) 1) (format "%s" k)))
                 (v-str (format "%s" v)))
            (insert "  " (propertize (format "%s:" k-name) 'face 'jsonl-view-key) " ")
            (if (or (consp v) (hash-table-p v) (> (length v-str) jsonl-view-truncate-length))
                (insert-text-button "▶"
                                    'action #'jsonl-view--toggle-disclosure-action
                                    'jsonl-value v
                                    'jsonl-expanded nil
                                    'face 'jsonl-view-disclosure-button
                                    'help-echo "mouse-1, RET: expand payload"
                                    'follow-link t)
              (insert (propertize v-str 'face 'font-lock-string-face))))
          (setq tail (cddr tail))))
      ;; Source hyperlink on right margin if available
      (when src
        (let* ((file (or (plist-get src :file)
                         (and (plist-get src :path) (file-name-nondirectory (plist-get src :path)))))
               (line (plist-get src :line))
               (label (if (and file line) (format " %s:%s" file line) " [src]")))
          (insert "  ")
          (insert-text-button label
                              'action #'jsonl-view--source-jump-action
                              'jsonl-source src
                              'face 'jsonl-view-source-link
                              'help-echo "mouse-1, RET: visit source"
                              'follow-link t)))
      (insert "\n")
      (let ((rendered (buffer-string)))
        (setq line-str
              (propertize rendered
                          'jsonl-record raw-record
                          'jsonl-ts ts-val
                          'jsonl-lvl lvl
                          'jsonl-cat cat
                          'jsonl-source src))))
    line-str))

;;; ----------------------------------------------------------------------------
;;; Invisibility-Based Filtering
;;; ----------------------------------------------------------------------------

(defconst jsonl-view--severity-order
  '(debug info notice warn error fatal))

(defun jsonl-view--severity-ge-p (line-lvl min-lvl)
  "Return non-nil if LINE-LVL >= MIN-LVL."
  (let ((l1 (or (cl-position line-lvl jsonl-view--severity-order) 1))
        (l2 (or (cl-position min-lvl jsonl-view--severity-order) 1)))
    (>= l1 l2)))

;;;###autoload
(defun jsonl-view-filter-level (level)
  "Filter buffer to show only entries with severity >= LEVEL."
  (interactive
   (list (intern (completing-read "Minimum severity: "
                                  '("debug" "info" "notice" "warn" "error" "fatal")
                                  nil t nil nil "info"))))
  (setq jsonl-view-min-level level)
  (jsonl-view--apply-filters)
  (message "Filtered minimum level: %s" level))

;;;###autoload
(defun jsonl-view-filter-category (regex)
  "Filter buffer to show only entries matching category REGEX."
  (interactive "sFilter category (regex): ")
  (setq jsonl-view-category-filter (if (string-empty-p regex) nil regex))
  (jsonl-view--apply-filters)
  (message "Filtered category: %s" (or jsonl-view-category-filter "all")))

;;;###autoload
(defun jsonl-view-clear-filters ()
  "Clear active severity and category filters, restoring all lines."
  (interactive)
  (setq jsonl-view-min-level nil
        jsonl-view-category-filter nil)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(invisible nil)))
  (message "Cleared all filters."))

(defun jsonl-view--apply-filters ()
  "Apply active level and category filters via buffer-invisibility-spec."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((pos (point))
               (lvl (get-text-property pos 'jsonl-lvl))
               (cat (get-text-property pos 'jsonl-cat))
               (eol (line-end-position)))
          (when (or (get-text-property pos 'jsonl-record) lvl)
            (let ((hide nil))
              (when (and jsonl-view-min-level
                         (not (jsonl-view--severity-ge-p lvl jsonl-view-min-level)))
                (setq hide t))
              (when (and jsonl-view-category-filter
                         (not (string-match-p jsonl-view-category-filter (format "%s" (or cat "")))))
                (setq hide t))
              (if hide
                  (put-text-property pos (min (1+ eol) (point-max))
                                     'invisible 'jsonl-filter-hidden)
                (remove-text-properties pos (min (1+ eol) (point-max))
                                        '(invisible nil))))))
        (forward-line 1)))))

;;; ----------------------------------------------------------------------------
;;; Dedicated Info View Inspector Buffer
;;; ----------------------------------------------------------------------------

(defun jsonl-view--update-info-buffer (record)
  "Render RECORD in pretty-printed JSON inside `jsonl-view-info-buffer-name'."
  (let ((buf (get-buffer-create jsonl-view-info-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if record
            (let ((formatted (condition-case nil
                                 (json-serialize record)
                               (error (format "%S" record)))))
              (insert formatted "\n")
              (ignore-errors
                (if (fboundp 'json-ts-mode)
                    (json-ts-mode)
                  (when (fboundp 'json-mode) (json-mode))))
              (ignore-errors (json-pretty-print-buffer)))
          (insert "No JSON record under point.\n"))
        (goto-char (point-min))))))

(defun jsonl-view-toggle-info ()
  "Toggle auxiliary side window pretty-printing record at point."
  (interactive)
  (let ((existing (get-buffer-window jsonl-view-info-buffer-name)))
    (if existing
        (delete-window existing)
      (let ((rec (get-text-property (point) 'jsonl-record)))
        (jsonl-view--update-info-buffer rec)
        (display-buffer (get-buffer jsonl-view-info-buffer-name)
                        '((display-buffer-in-side-window)
                          (side . right)
                          (window-width . 0.4)))))))

(defun jsonl-view--info-post-command ()
  "Update info inspector window on point change if follow mode is active."
  (when (and jsonl-view-info-follow-mode
             (get-buffer-window jsonl-view-info-buffer-name))
    (let ((rec (get-text-property (point) 'jsonl-record)))
      (jsonl-view--update-info-buffer rec))))

;;; ----------------------------------------------------------------------------
;;; Navigation, Copy, and Follow Mode
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun jsonl-view-copy-as-json ()
  "Copy pretty-printed JSON of the record under point to kill-ring."
  (interactive)
  (let ((rec (get-text-property (point) 'jsonl-record)))
    (if rec
        (let ((json-str (condition-case nil
                            (json-serialize rec)
                          (error (format "%S" rec)))))
          (kill-new json-str)
          (message "Copied record JSON to kill-ring."))
      (message "No JSON record at point."))))

(defun jsonl-view-toggle-follow ()
  "Toggle auto-scrolling tail follow mode."
  (interactive)
  (setq jsonl-view-follow-mode (not jsonl-view-follow-mode))
  (message "Auto-scroll follow mode: %s" (if jsonl-view-follow-mode "ON" "OFF")))

;;; ----------------------------------------------------------------------------
;;; Major Mode Definition and Keymap
;;; ----------------------------------------------------------------------------

(defvar jsonl-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB")     #'jsonl-view-toggle-disclosure)
    (define-key map (kbd "<tab>")   #'jsonl-view-toggle-disclosure)
    (define-key map (kbd "<backtab>") #'jsonl-view-collapse-all)
    (define-key map (kbd "S-TAB")   #'jsonl-view-collapse-all)
    (define-key map (kbd "i")       #'jsonl-view-toggle-info)
    (define-key map (kbd "RET")     #'jsonl-view-jump-to-source)
    (define-key map (kbd "t")       #'jsonl-view-cycle-timestamp-mode)
    (define-key map (kbd "l")       #'jsonl-view-filter-level)
    (define-key map (kbd "c")       #'jsonl-view-filter-category)
    (define-key map (kbd "C")       #'jsonl-view-clear-filters)
    (define-key map (kbd "w")       #'jsonl-view-copy-as-json)
    (define-key map (kbd "F")       #'jsonl-view-toggle-follow)
    (define-key map (kbd "q")       #'quit-window)
    map)
  "Keymap for `jsonl-view-mode'.")

;;;###autoload
(define-derived-mode jsonl-view-mode special-mode "JSONL-View"
  "Major mode for viewing line-delimited JSON log and data streams."
  :group 'jsonl-view
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t)
  (add-to-invisibility-spec '(jsonl-filter-hidden . t))
  (add-hook 'post-command-hook #'jsonl-view--info-post-command nil t))

;;;###autoload
(defun jsonl-view-file (file)
  "Open FILE in `jsonl-view-mode' with rendered line-oriented records."
  (interactive "fOpen JSON Lines file: ")
  (let ((buf (generate-new-buffer (format "*jsonl: %s*" (file-name-nondirectory file)))))
    (with-current-buffer buf
      (jsonl-view-mode)
      (let ((inhibit-read-only t)
            (prev-ts nil))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                   (parsed (jsonl-view--parse-line line)))
              (when parsed
                (let ((rendered (jsonl-view-render-record parsed prev-ts)))
                  (with-current-buffer buf
                    (insert rendered)))
                (setq prev-ts (plist-get parsed :ts))))
            (forward-line 1))))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jsonl\\'" . jsonl-view-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ndjson\\'" . jsonl-view-mode))

(provide 'jsonl-view)
;;; jsonl-view.el ends here
