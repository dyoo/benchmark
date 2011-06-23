#lang racket/base
(require "../../measurement-struct.rkt"
         "../../get-host-info.rkt"
         "../../externals/whalesong/tests/browser-evaluate.rkt"
         "../../externals/whalesong/make/make-structs.rkt"
         "../../externals/whalesong/js-assembler/package.rkt"
         racket/runtime-path
         racket/port)



(provide make-run)

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
  (define evaluate (make-evaluate 
                    (lambda (program op)
                      
                      (fprintf op "(function () {")
                      
                      ;; The runtime code
                      (write-runtime op)
                      
                      (newline op)
                      
                      (fprintf op "var innerInvoke = ")
                      (package-anonymous program
                                         #:should-follow? (lambda (p) #t)
                                         #:output-port op)
                      (fprintf op "();\n")
                      
                      (fprintf op #<<EOF
return (function(succ, fail, params) {
            return innerInvoke(new plt.runtime.Machine(), succ, fail, params);
        });
});
EOF
                               ))))
  (lambda (suite-directory module-name)
    (let ([program
           (let ([desugared-path 
                  (build-path suite-directory (format "~a-desugared.sch" module-name))])
             (cond [(file-exists? desugared-path)
                    (make-SexpSource
                     (cons 'begin (call-with-input-file* desugared-path read*)))]
                   [else
                    (make-SexpSource (cons 'begin
                                           (call-with-input-file* 
                                               (build-path suite-directory
                                                           (format "~a.sch" module-name))
                                             read*)))]))])
      (parameterize ([current-directory suite-directory])
        (let* ([result (evaluate program)])
          (make-measurement (current-seconds)
                            (format "whalesong:~a" (evaluated-browser result))
                            module-name
                            (evaluated-t result)
                            (evaluated-stdout result)))))))
