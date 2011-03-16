#lang racket/base

(require "measurement-struct.rkt"
	 "get-host-info.rkt"
	 racket/match
	 racket/runtime-path
	 racket/list
	 racket/date)

(provide save-measurement!)


(define-runtime-path measurements-path "data/measurements")
  

(define (measurement->sexp a-measurement)
  (match a-measurement
    [(struct measurement (current-seconds
			  platform
			  program
			  time
			  output))
     `((current-date ,current-seconds 
                     ,(date->string (seconds->date current-seconds)))
       (host-name ,(second (lookup-host-name)))
       (program ,program)
       (platform ,platform)
       (time ,time)
       (output , output))]))



(define (save-measurement! a-measurement)
  (call-with-output-file measurements-path
    (lambda (op)
      (write (measurement->sexp a-measurement) op)
      (newline op))
    #:exists 'append))