#lang racket/base
(require "../../measurement-struct.rkt"
         "../../get-host-info.rkt"
         "../../externals/js-sicp-5-5/simulator.rkt"
         "../../externals/js-sicp-5-5/parse.rkt"
         "../../externals/js-sicp-5-5/compiler.rkt"
         "../../externals/js-sicp-5-5/simulator-structs.rkt")


(provide make-run)



;; Run the machine to completion.
(define (step-to-completion! m)
  (cond
    [(can-step? m)
     (step! m)
     (step-to-completion! m)]
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
(define (make-run) 

(define (evaluate program)
  (let ([op (open-output-string)])
    (parameterize ([current-simulated-output-port op])
      (let* ([code (compile (parse program) 'val 'next)]
	     [machine (new-machine code)])
	(let ([start-time (current-inexact-milliseconds)])
	  (step-to-completion! machine)
	  (values (- (current-inexact-milliseconds) start-time)
		  (get-output-string op)))))))
  
  (lambda (suite-directory module-name)
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
      (let-values ([(time stdout) (evaluate program)])
        (make-measurement (current-seconds)
                          "simulator"
                          module-name
                          time
                          stdout))))))