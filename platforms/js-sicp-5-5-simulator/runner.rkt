#lang racket/base
(require "../../measurement-struct.rkt"
         "../../get-host-info.rkt"
         "../../externals/js-sicp-5-5/simulator.rkt"
         "../../externals/js-sicp-5-5/parse.rkt"
         "../../externals/js-sicp-5-5/compile.rkt"
         "../../externals/js-sicp-5-5/simulator-structs.rkt")


(provide run)


(define (evaluate program)
  (let ([op (open-output-string)])
    (parameterize ([current-simulated-output-port op])
      (values (machine-val (step-to-completion 
			    (new-machine 
			     (compile (parse program) 'val 'next))))
	      (get-output-string op)))))
  

;; Run the machine to completion.
(define (step-to-completion m)
  (cond
    [(can-step? m)
     (step! m)
     (step-to-completion m)]
    [else
     m]))


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
      (let-values ([(result stdout) (evaluate program)])
        (make-measurement (current-seconds)
                          (get-host-info)
                          "racket"
                          module-name
                          result
                          stdout)))))