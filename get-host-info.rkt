#lang racket/base

(require racket/port
	 racket/runtime-path)


(provide get-host-info
	 read-known-hosts
	 write-known-hosts)


(define (get-host-info)
  (call-with-input-file* "/proc/cpuinfo"
    (lambda (ip)
      (port->string ip))))


(define-runtime-path known-hosts "known-hosts")

;; read-known-hosts: -> (listof (list [host-info string] [host-name string]))
(define (read-known-hosts)
  (call-with-input-file known-hosts
    read))

(define (write-known-hosts new-hosts)
  (call-with-output-file known-hosts
    (lambda (op)
      (write new-hosts op))
    #:exists 'replace))
