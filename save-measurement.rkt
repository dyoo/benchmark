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
     (let ([date (seconds->date current-seconds)])
       `((current-date ,current-seconds 
                       ,(date->string (seconds->date current-seconds))
                       (year ,(date-year date))
                       (month ,(date-month date))
                       (day ,(date-day date))
                       (second ,(date-second date))
                       (minute ,(date-minute date))
                       (hour ,(date-hour date))
                       (week-day ,(date-week-day date))
                       (year-day ,(date-year-day date))
                       (dst? ,(day-dst date))
                       (time-zone-offset ,(date-time-zone-offset date)))
         (host-name ,(second (lookup-host-name)))
         (program ,program)
         (platform ,platform)
         (time ,time)
         (output , output)))]))



(define (save-measurement! a-measurement)
  (call-with-output-file measurements-path
    (lambda (op)
      (write (measurement->sexp a-measurement) op)
      (newline op))
    #:exists 'append))