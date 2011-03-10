#lang racket/base

(require racket/port)

(provide get-host-info)

(define (get-host-info)
  (call-with-input-file* "/proc/cpuinfo"
    (lambda (ip)
      (port->string ip))))