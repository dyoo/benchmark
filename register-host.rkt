#lang racket

(require racket/runtime-path
	 "get-host-info.rkt")


(define-runtime-path known-hosts "known-hosts")



(define host-pair (assoc (get-host-info) (get-known-hosts)))

(cond
 [host-pair
  (printf "Current host: ~s" (second host-pair))]
 [else
  (printf "Unknown host.  What do you want to name this?  \n")
  (let ([name (read-line)])
    (write-known-hosts
     (cons (list (get-host-info) name)
	   (get-known-hosts)))])
