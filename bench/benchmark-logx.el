;;; benchmark-logx.el --- Benchmarks for logx structured logging -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'benchmark)
(require 'logx)
(require 'jsonl-view)
(require 'logx-view)

(defun logx-bench--format-result (title n res)
  "Format benchmark result RES (total-time gcs gc-time) for N iterations of TITLE."
  (let* ((total-time (nth 0 res))
         (gcs (nth 1 res))
         (gc-time (nth 2 res))
         (ops-sec (/ (float n) (max total-time 0.000001)))
         (us-per-op (* (/ total-time (float n)) 1000000.0)))
    (message "------------------------------------------------------------")
    (message "Benchmark: %s" title)
    (message "  Iterations: %d" n)
    (message "  Total Time: %.4f s" total-time)
    (message "  Throughput: %d ops/sec" (round ops-sec))
    (message "  Latency:    %.3f µs/op" us-per-op)
    (message "  GC Runs:    %d (GC time: %.4f s)" gcs gc-time)))

(defun logx-bench-run-all ()
  "Run complete benchmark suite across hot paths."
  (message "============================================================")
  (message "Starting Logx Performance Benchmarks (Emacs %s)" emacs-version)
  (message "============================================================")

  ;; 1. Disabled macro path (zero-cost gating check)
  (let ((logx-default-level 'info)
        (logx-category-levels nil)
        (n 1000000))
    (let ((res (benchmark-run n
                 (logx-debug :cat 'test :msg "Ignored message" :arg1 42 :arg2 "value"))))
      (logx-bench--format-result "1. Disabled Macro Path (logx-debug when lvl=info)" n res)))

  ;; 2. Enabled macro path + in-memory ring buffer insert
  (let ((logx-default-level 'info)
        (logx-category-levels nil)
        (logx-log-file nil)
        (logx--active-subscribers nil)
        (n 50000))
    (logx--ring-init 1000)
    (let ((res (benchmark-run n
                 (logx-info :cat 'bench :msg "Active message" :count 123 :status :ok))))
      (logx-bench--format-result "2. Enabled Macro + Ring Buffer Insert (50k events)" n res)))

  ;; 3. Object sanitizer on nested data
  (let* ((sample '(:id 101 :tags [web api v2] :meta (:active t :scores [98.5 99.1]) :nested (:a 1 :b 2)))
         (n 20000)
         (res (benchmark-run n
                (logx--sanitize-value sample))))
    (logx-bench--format-result "3. Object Sanitizer (deep plists/vectors)" n res))

  ;; 4. JSONL Serializer throughput
  (let* ((sample-event '(:ts 1700000000.123 :lvl info :cat bench :msg "Hello World" :user "tycho" :code 200))
         (n 20000)
         (res (benchmark-run n
                (logx--serialize-jsonl sample-event))))
    (logx-bench--format-result "4. JSONL Serializer (direct string formatting)" n res))

  ;; 5. Viewer Record Renderer
  (let* ((sample-event '(:ts 1700000000.123 :lvl warn :cat sys :msg "Memory threshold" :percent 85 :host "app-01"))
         (n 20000)
         (res (benchmark-run n
                (logx-view-render-record sample-event))))
    (logx-bench--format-result "5. Logx-View Record Renderer (propertized string)" n res))

  (message "============================================================")
  (message "Benchmark suite completed successfully.")
  (message "============================================================"))

(when noninteractive
  (logx-bench-run-all))

(provide 'benchmark-logx)
;;; benchmark-logx.el ends here
