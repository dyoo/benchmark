#lang racket/base
(require "../../measurement-struct.rkt"
         "../../get-host-info.rkt")


(provide make-run)

;; make-run: suite-path module-name -> measurement 
(define ((make-run) suite-directory module-name)
  (let ([op (open-output-string)]
        [start-time #f])
    (parameterize ([current-directory suite-directory]
                   [current-namespace (make-base-namespace)]
                   [current-output-port op]
                   [eval-jit-enabled #f]
                   [read-accept-reader #t])
      (eval (read-syntax module-name (open-input-file (format "~a.rkt" module-name))))
      (set! start-time (current-inexact-milliseconds))
      (dynamic-require `',module-name #f))
    (let ([end-time (current-inexact-milliseconds)])
      (make-measurement (current-seconds)
                        "racket-no-jit"
                        module-name
                        (- end-time start-time)
                        (get-output-string op)))))
