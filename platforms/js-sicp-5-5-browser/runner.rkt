#lang racket/base
(require "../../measurement-struct.rkt"
         "../../get-host-info.rkt"
         "private/browser-evaluate.rkt"
         "private/package.rkt")

(define evaluate (make-evaluate package-anonymous))

(provide run)

(define (read* inp)
  (parameterize ([read-accept-reader #t])
    (let loop ()
      (let ([next (read inp)])
        (cond
          [(eof-object? next)
           '()]
          [else
           (cons next (loop))])))))


;; run: suite-path module-name -> measurement 
(define (run suite-directory module-name)
  (let ([program
         (let ([desugared-path 
                (build-path suite-directory (format "~a-desugared.sch" module-name))])
           (cond [(file-exists? desugared-path)
                  (cons 'begin (call-with-input-file* desugared-path read*))]
                 [else
                  (cons 'begin (call-with-input-file* 
                                   (build-path suite-directory (format "~a.sch" module-name))
                                 read*))]))])
    (parameterize ([current-directory suite-directory])
      (let* ([result (evaluate program)])
        (make-measurement (current-seconds)
                          (get-host-info)
                          "racket"
                          module-name
                          (evaluated-t result)
                          (evaluated-stdout result))))))