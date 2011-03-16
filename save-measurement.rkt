#lang racket/base

(require "measurement-struct.rkt"
	 "get-host-info.rkt"
	 racket/match
	 racket/runtime-path)

(provide save-measurement!)


(define-runtime-path measurements-path "data/measurements")
  

(define (measurement->sexp a-measurement)
  (match a-measurement
    [(struct measurement (current-seconds
			  platform
			  program
			  time
			  output))
     `((current-date ,current-seconds)
       (host-name ,(lookup-host-name))
       (platform ,platform)
       (time ,time)
       (output , output))]))



(define (save-measurement! a-measurement)
  (call-with-output-file measurements-path
    (lambda (op)
      (write (measurement->sexp a-measurement) op)
      (newline op))
    #:exists 'append))