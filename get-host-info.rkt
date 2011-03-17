#lang racket/base

(require racket/port
	 racket/runtime-path
	 racket/list

	 mzlib/os)


(provide get-host-info
	 read-known-hosts
	 write-known-hosts!
	 lookup-host-name
	 register-new-host!)


(define (get-host-info)
  (call-with-input-file* "/proc/cpuinfo"
    (lambda (ip)
      (port->string ip))))


(define-runtime-path known-hosts "data/known-hosts")
(define-runtime-path known-hosts "data/this-host")



(define (get-host-name)
  (cond
   [(file-exists? this-host)
    (call-with-input-file this-host read-line)]
   [else
    (gethostname)]))



(define (lookup-host-name)
  (list (get-host-info) (get-host-name)))


;; read-known-hosts: -> (listof (list [host-info string] [host-name string]))
(define (read-known-hosts)
  (cond
   [(file-exists? known-hosts)
    (call-with-input-file known-hosts
      read)]
   [else
    '()]))


(define (write-known-hosts! new-hosts)
  (call-with-output-file known-hosts
    (lambda (op)
      (write new-hosts op))
    #:exists 'truncate))


(define (register-new-host! name)
  (write-known-hosts!
   (cons (list (get-host-info)
	       name)
	 (read-known-hosts))))


(let loop ([hosts (read-known-hosts)])
  (cond
   [(empty? hosts)
    (printf "Registering new host in ~a.\n" (path->string known-hosts))
    (register-new-host! (get-host-name))]
   [(string=? (second (first hosts))
	      (get-host-name))
    (void)]
   [else
    (loop (rest hosts))]))



;; At module invokation time, this will ensure that we give a name to a host.
(let [(host-pair (lookup-host-name))]
  (cond
   [host-pair
    (void) #;(printf "Current host: ~s\n\n" (second host-pair))]
   [else
    (printf "Unknown host.\n\nCPU info: ~a\n\nWhat do you want to name this?  \n"
	    (get-host-info))
    (let ([name (read-line)])
      (register-new-host! name))
    (newline)]))
