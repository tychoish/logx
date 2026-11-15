;;; comparative-benchmarks.el --- Comparative logging benchmarks -*- lexical-binding: t; -*-

;; Compares logx against:
;; 1. log4e (common Emacs Lisp logging library)
;; 2. core `message` with format specs (with inhibit-message t to measure *Messages* insertion)
;; 3. pure string `format`
;; 4. native C `json-serialize`

(require 'benchmark)
(require 'logx)

;; Load log4e from local dependencies
(let ((log4e-dir "/home/tychoish/src/agent-shell-queue/.deps/elpa/log4e-20240123.1313"))
  (add-to-list 'load-path log4e-dir)
  (require 'log4e))

;; Setup log4e logger
(log4e:deflogger "benchlog" "%t [%l] %m" "%H:%M:%S")

(defun bench--format (title n res)
  "Format benchmark result RES for N iterations of TITLE."
  (let* ((total-time (nth 0 res))
         (gcs (nth 1 res))
         (gc-time (nth 2 res))
         (ops-sec (/ (float n) (max total-time 0.000001)))
         (us-per-op (* (/ total-time (float n)) 1000000.0)))
    (message "----------------------------------------------------------------------")
    (message "%s" title)
    (message "  Iterations: %d" n)
    (message "  Total Time: %.4f s" total-time)
    (message "  Throughput: %d ops/sec" (round ops-sec))
    (message "  Latency:    %.3f µs/op" us-per-op)
    (message "  GC Runs:    %d (GC time: %.4f s)" gcs gc-time)))

(defun run-comparative-benchmarks ()
  "Run the comparative benchmark suite."
  (message "======================================================================")
  (message "Comparative Logging Benchmarks in Isolated Emacs (%s)" emacs-version)
  (message "======================================================================")

  ;; ------------------------------------------------------------------
  ;; SUITE 1: DISABLED / GATED PATH (Zero-Cost Logging)
  ;; Measuring overhead when the logging statement is below active threshold
  ;; ------------------------------------------------------------------
  (message "\n>>> SUITE 1: DISABLED / GATED LOG CALLS (Severity below threshold)")

  ;; logx disabled macro
  (let ((logx-default-level 'info)
        (logx-category-levels nil)
        (n 1000000))
    (let ((res (benchmark-run n
                 (logx-debug :cat 'test :msg "Ignored message" :arg1 42 :arg2 "val"))))
      (bench--format "1.1 logx: Disabled Macro (logx-debug when lvl=info)" n res)))

  ;; log4e disabled macro
  (benchlog--log-enable-logging)
  (benchlog--log-set-level 'info)
  (let ((n 1000000))
    (let ((res (benchmark-run n
                 (benchlog--log-debug* "Ignored message %s %s" 42 "val"))))
      (bench--format "1.2 log4e: Disabled Macro (benchlog--log-debug* when lvl=info)" n res)))

  ;; Baseline: simple (when nil ...)
  (let ((n 1000000))
    (let ((res (benchmark-run n
                 (when nil (format "Ignored message %s %s" 42 "val")))))
      (bench--format "1.3 Baseline: raw (when nil ...)" n res)))

  ;; ------------------------------------------------------------------
  ;; SUITE 2: ACTIVE / ENABLED IN-MEMORY LOGGING
  ;; Measuring overhead of an enabled log statement delivering into memory
  ;; ------------------------------------------------------------------
  (message "\n>>> SUITE 2: ACTIVE / ENABLED IN-MEMORY LOGGING")

  ;; logx enabled macro -> ring.el buffer
  (let ((logx-default-level 'info)
        (logx-category-levels nil)
        (logx-log-file nil)
        (logx--active-subscribers nil)
        (n 100000))
    (logx--ring-init 10000)
    (let ((res (benchmark-run n
                 (logx-info :cat 'bench :msg "User logged in" :user "tycho" :code 200))))
      (bench--format "2.1 logx: Active Macro -> ring.el buffer (structured plist)" n res)))

  ;; log4e active macro -> log buffer
  (benchlog--log-set-level 'info)
  (let ((n 10000))
    (let ((res (benchmark-run n
                 (benchlog--log-info* "User logged in: user=%s code=%d" "tycho" 200))))
      (bench--format "2.2 log4e: Active Macro -> *benchlog* buffer (regex template formatting)" n res)))

  ;; core message with format specs (writing to *Messages*)
  (let ((n 50000))
    (let ((res (benchmark-run n
                 (let ((inhibit-message t))
                   (message "User logged in: user=%s code=%d" "tycho" 200)))))
      (bench--format "2.3 core: message with format specs (to *Messages* with inhibit-message)" n res)))

  ;; pure string format (no storage)
  (let ((n 100000))
    (let ((res (benchmark-run n
                 (format "User logged in: user=%s code=%d" "tycho" 200))))
      (bench--format "2.4 core: pure (format \"...\" ...) string formatting (no storage)" n res)))

  ;; ------------------------------------------------------------------
  ;; SUITE 3: SERIALIZATION (JSONL vs Format String)
  ;; ------------------------------------------------------------------
  (message "\n>>> SUITE 3: SERIALIZATION DETAILS (What happens per op)")

  ;; logx--serialize-jsonl with structured data payload
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
    (bench--format "3.1 logx--serialize-jsonl (sanitize + C json-serialize with :data)" n res))

  ;; Pure C json-serialize on ready plist
  (let* ((ready-payload '(:seq 1 :ts 1700000000.123 :lvl "info" :cat "bench"
                          :msg "User logged in" :user "tycho" :code 200))
         (n 50000)
         (res (benchmark-run n
                (json-serialize ready-payload))))
    (bench--format "3.2 core: C json-serialize on flat plist directly" n res))

  ;; Pure format-spec string rendering
  (let ((n 50000))
    (let ((res (benchmark-run n
                 (format "{\"seq\":%d,\"ts\":%.3f,\"lvl\":\"%s\",\"cat\":\"%s\",\"msg\":\"%s\",\"user\":\"%s\",\"code\":%d}"
                         1 1700000000.123 "info" "bench" "User logged in" "tycho" 200))))
      (bench--format "3.3 core: manual JSON formatting via (format ...)" n res)))

  (message "\n======================================================================")
  (message "Comparative benchmark suite finished.")
  (message "======================================================================\n"))

(provide 'comparative-benchmarks)
;;; comparative-benchmarks.el ends here
