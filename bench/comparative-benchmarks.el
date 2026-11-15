;;; comparative-benchmarks.el --- Comprehensive comparative logging benchmarks -*- lexical-binding: t; -*-

;; Compares:
;; 1. logx (in-memory ring buffer, async timer disk flush, async thread disk flush)
;; 2. log4e (common Emacs Lisp logging library)
;; 3. elog (EIEIO logging library)
;; 4. emacs-lgr / lgr (EIEIO hierarchical logging framework)
;; 5. core `message` with format specs
;; 6. core `(format ...)`

(require 'benchmark)
(require 'logx)

;; Setup modern alias for elog if needed
(eval-and-compile
  (unless (fboundp 'call-next-method)
    (defalias 'call-next-method #'cl-call-next-method))
  (let ((log4e-dir "/home/tychoish/src/agent-shell-queue/.deps/elpa/log4e-20240123.1313")
        (elog-dir "/home/tychoish/src/elog")
        (lgr-dir "/home/tychoish/src/emacs-lgr"))
    (add-to-list 'load-path log4e-dir)
    (add-to-list 'load-path elog-dir)
    (add-to-list 'load-path lgr-dir)
    (require 'log4e)
    (require 'elog)
    (require 'lgr)
    ;; Fix elog 2015 EIEIO slot names in modern Emacs
    (cl-defmethod elog-should-log-p ((log elog-object) serverity)
      (let ((l (oref log serverity)))
        (and (integerp l)
             (<= serverity l))))
    (cl-defmethod elog-log ((log elog-object) serverity ident string &rest objects)
      (when (elog-should-log-p log serverity)
        (mapc (lambda (func) (funcall func log)) (oref log prelog-functions))
        (let ((fmt (oref log fmt)))
          (setq fmt (replace-regexp-in-string "%I" (format "%s" ident) fmt t))
          (setq fmt (replace-regexp-in-string "%T" (current-time-string) fmt t))
          (setq fmt (replace-regexp-in-string "%L" (format "%s" serverity) fmt t))
          (setq fmt (replace-regexp-in-string "%P" (format "%s" (emacs-pid)) fmt t))
          (setq fmt (replace-regexp-in-string "%M" string fmt t))
          (apply #'elog-insert-log log serverity fmt objects))
        (mapc (lambda (func) (funcall func log)) (oref log postlog-functions))))))

;; Setup log4e logger
(log4e:deflogger "benchlog" "%t [%l] %m" "%H:%M:%S")

;; Setup elog logger (message appender)
(defvar benchelog--obj (make-instance 'elog-message-object))
(oset benchelog--obj serverity elog-info)
(defun benchelog-log (serverity format-string &rest objects)
  (apply #'elog-log benchelog--obj serverity 'benchelog format-string objects))

;; Setup emacs-lgr logger
(defvar bench-lgr-logger (lgr-get-logger "bench"))
(lgr-set-threshold bench-lgr-logger lgr-level-info)

(defun bench--format-row (label n res)
  "Format benchmark row RES for N iterations of LABEL."
  (let* ((total-time (nth 0 res))
         (gcs (nth 1 res))
         (gc-time (nth 2 res))
         (ops-sec (/ (float n) (max total-time 0.000001)))
         (us-per-op (* (/ total-time (float n)) 1000000.0)))
    (format "| %-42s | %10d | %8.4f s | %10d | %8.3f µs | %4d (%.3fs) |"
            label n total-time (round ops-sec) us-per-op gcs gc-time)))

(defun run-comparative-benchmarks ()
  "Run the full comparative benchmark suite across all libraries and backends."
  (message "==========================================================================================")
  (message "Comparative Logging Benchmarks in Isolated Emacs (%s)" emacs-version)
  (message "==========================================================================================")

  ;; ------------------------------------------------------------------
  ;; SUITE 1: DISABLED / GATED PATH (Zero-Cost Logging)
  ;; ------------------------------------------------------------------
  (message "\n### Suite 1: Disabled / Gated Logging (Below Active Threshold)\n")
  (message "| Implementation / Test Case                 | Iterations | Total Time |  Ops / sec | Latency/op | GC (Pause)   |")
  (message "|:-------------------------------------------|-----------:|-----------:|-----------:|-----------:|:-------------|")

  ;; 1. logx disabled macro
  (let ((logx-default-level 'info)
        (logx-category-levels nil)
        (n 1000000))
    (let ((res (benchmark-run n
                 (logx-debug :cat 'test :msg "Ignored message" :arg1 42 :arg2 "val"))))
      (message "%s" (bench--format-row "logx-debug (level=info)" n res))))

  ;; 2. log4e disabled macro
  (benchlog--log-enable-logging)
  (benchlog--log-set-level 'info)
  (let ((n 1000000))
    (let ((res (benchmark-run n
                 (benchlog--log-debug* "Ignored message %s %s" 42 "val"))))
      (message "%s" (bench--format-row "log4e benchlog--log-debug*" n res))))

  ;; 3. elog disabled call
  (let ((n 200000))
    (let ((res (benchmark-run n
                 (benchelog-log elog-debug "Ignored message %s %s" 42 "val"))))
      (message "%s" (bench--format-row "elog (severity=info, log debug)" n res))))

  ;; 4. emacs-lgr disabled macro
  (let ((n 500000))
    (let ((res (benchmark-run n
                 (lgr-debug bench-lgr-logger "Ignored message" :arg1 42 :arg2 "val"))))
      (message "%s" (bench--format-row "emacs-lgr lgr-debug (threshold=info)" n res))))

  ;; 5. Raw (when nil ...) baseline
  (let ((n 1000000))
    (let ((res (benchmark-run n
                 (when nil (format "Ignored message %s %s" 42 "val")))))
      (message "%s" (bench--format-row "Baseline: (when nil ...)" n res))))

  ;; ------------------------------------------------------------------
  ;; SUITE 2: ACTIVE IN-MEMORY LOGGING (No Disk I/O)
  ;; ------------------------------------------------------------------
  (message "\n### Suite 2: Active In-Memory Logging\n")
  (message "| Implementation / Storage Destination       | Iterations | Total Time |  Ops / sec | Latency/op | GC (Pause)   |")
  (message "|:-------------------------------------------|-----------:|-----------:|-----------:|-----------:|:-------------|")

  ;; 1. logx -> ring.el buffer
  (let ((logx-default-level 'info)
        (logx-category-levels nil)
        (logx-log-file nil)
        (logx-async-mode nil)
        (logx--active-subscribers nil)
        (n 100000))
    (logx--ring-init 10000)
    (let ((res (benchmark-run n
                 (logx-info :cat 'bench :msg "User logged in" :user "tycho" :code 200))))
      (message "%s" (bench--format-row "logx -> ring.el (structured plist)" n res))))

  ;; 2. log4e -> buffer
  (benchlog--log-enable-logging)
  (benchlog--log-set-level 'info)
  (let ((n 10000))
    (let ((res (benchmark-run n
                 (benchlog--log-info* "User logged in: user=%s code=%d" "tycho" 200))))
      (message "%s" (bench--format-row "log4e -> *benchlog* buffer" n res))))

  ;; 3. elog -> message
  (let ((n 5000))
    (let ((res (benchmark-run n
                 (let ((inhibit-message t))
                   (benchelog-log elog-info "User logged in: user=%s code=%d" "tycho" 200)))))
      (message "%s" (bench--format-row "elog -> message appender" n res))))

  ;; 4. emacs-lgr -> message
  (let ((n 5000))
    (let ((res (benchmark-run n
                 (let ((inhibit-message t))
                   (lgr-info bench-lgr-logger "User logged in" :user "tycho" :code 200)))))
      (message "%s" (bench--format-row "emacs-lgr -> message appender" n res))))

  ;; 5. core message to *Messages*
  (let ((n 50000))
    (let ((res (benchmark-run n
                 (let ((inhibit-message t))
                   (message "User logged in: user=%s code=%d" "tycho" 200)))))
      (message "%s" (bench--format-row "core message (inhibit-message t)" n res))))

  ;; 6. pure format string
  (let ((n 100000))
    (let ((res (benchmark-run n
                 (format "User logged in: user=%s code=%d" "tycho" 200))))
      (message "%s" (bench--format-row "core format (pure string formatting)" n res))))

  ;; ------------------------------------------------------------------
  ;; SUITE 3: ASYNC FLUSH & PERSISTENT DISK PRESSURE (50,000 events)
  ;; ------------------------------------------------------------------
  (message "\n### Suite 3: Async Flushing & Disk I/O Under Pressure (50,000 Events)\n")
  (message "| Pipeline Strategy / Transport              | Iterations | Total Time |  Ops / sec | Latency/op | GC (Pause)   |")
  (message "|:-------------------------------------------|-----------:|-----------:|-----------:|-----------:|:-------------|")

  ;; 1. logx Primary Ingress + Async Timer Flush
  (let* ((tmp-file (make-temp-file "logx-async-timer-" nil ".jsonl"))
         (logx-default-level 'info)
         (logx-log-file tmp-file)
         (logx-async-mode t)
         (logx-async-transport 'timer)
         (logx-async-flush-interval 0.05)
         (logx--active-subscribers nil)
         (n 50000))
    (logx-enable-backend 'file)
    (logx--ring-init 10000)
    (let ((res (benchmark-run n
                 (logx-info :cat 'fs :msg "Async timer event" :count 1 :status :ok))))
      (logx-flush)
      (delete-file tmp-file)
      (message "%s" (bench--format-row "logx: Primary Ring + Async Timer Flush" n res))))

  ;; 2. logx Primary Ingress + Async Thread Flush (make-thread)
  (let* ((tmp-file (make-temp-file "logx-async-thread-" nil ".jsonl"))
         (logx-default-level 'info)
         (logx-log-file tmp-file)
         (logx-async-mode t)
         (logx-async-transport 'thread)
         (logx-async-flush-interval 0.02)
         (logx--active-subscribers nil)
         (n 50000))
    (logx-enable-backend 'file)
    (logx--ring-init 10000)
    (let ((res (benchmark-run n
                 (logx-info :cat 'fs :msg "Async thread event" :count 1 :status :ok))))
      (logx-flush)
      (delete-file tmp-file)
      (message "%s" (bench--format-row "logx: Primary Ring + Async Thread Flush" n res))))

  ;; 3. logx Synchronous Disk Append per Event
  (let* ((tmp-file (make-temp-file "logx-sync-disk-" nil ".jsonl"))
         (logx-default-level 'info)
         (logx-log-file tmp-file)
         (logx-async-mode nil)
         (logx-async-transport 'sync)
         (logx--active-subscribers nil)
         (n 2000))
    (logx-enable-backend 'file)
    (logx--ring-init 10000)
    (let ((res (benchmark-run n
                 (logx-info :cat 'fs :msg "Sync disk event" :count 1 :status :ok))))
      (delete-file tmp-file)
      (message "%s" (bench--format-row "logx: Synchronous Disk Write per Event" n res))))

  ;; ------------------------------------------------------------------
  ;; SUITE 4: SERIALIZATION PIPELINE BREAKDOWN
  ;; ------------------------------------------------------------------
  (message "\n### Suite 4: Serialization Pipeline Breakdown\n")
  (message "| Operation                                  | Iterations | Total Time |  Ops / sec | Latency/op | GC (Pause)   |")
  (message "|:-------------------------------------------|-----------:|-----------:|-----------:|-----------:|:-------------|")

  (let* ((sample-event (list :seq 1
                             :ts 1700000000.123
                             :lvl 'info
                             :cat 'bench
                             :msg "User logged in"
                             :src '(:file "auth.el" :line 42)
                             :data '(:user "tycho" :code 200 :ip "127.0.0.1")))
         (n 50000)
         (res (benchmark-run n
                (logx--serialize-jsonl sample-event))))
    (message "%s" (bench--format-row "logx--serialize-jsonl (sanitization + C JSON)" n res)))

  (let* ((ready-payload '(:seq 1 :ts 1700000000.123 :lvl "info" :cat "bench"
                          :msg "User logged in" :user "tycho" :code 200))
         (n 50000)
         (res (benchmark-run n
                (json-serialize ready-payload))))
    (message "%s" (bench--format-row "C json-serialize (flat plist directly)" n res)))

  (let ((n 50000))
    (let ((res (benchmark-run n
                 (format "{\"seq\":%d,\"ts\":%.3f,\"lvl\":\"%s\",\"cat\":\"%s\",\"msg\":\"%s\",\"user\":\"%s\",\"code\":%d}"
                         1 1700000000.123 "info" "bench" "User logged in" "tycho" 200))))
      (message "%s" (bench--format-row "core manual JSON formatting via (format ...)" n res))))

  (message "\n==========================================================================================")
  (message "Comparative benchmark suite finished.")
  (message "==========================================================================================\n"))

(provide 'comparative-benchmarks)
;;; comparative-benchmarks.el ends here
