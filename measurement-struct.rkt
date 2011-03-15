#lang racket/base
(provide (struct-out measurement))

(define-struct measurement (current-seconds  ;; the date in which the measurement was taken
                            host
                            platform
                            program
                            time
                            output)
  #:transparent)