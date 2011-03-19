#lang racket/base

(require racket/file)

(provide with-temporary-directory)


;; Evaluate some function that uses a directory, and put it in the context of a temporary
;; directory that'll be deleted on exit.
(define (with-temporary-directory f)
  (let ([dir #f])
    (dynamic-wind 
     (lambda ()
       (set! dir (make-temporary-file))
       (delete-file dir)
       (make-directory dir))     
     (lambda ()
       (f dir))
     (lambda ()
       (delete-directory/files dir)))))

