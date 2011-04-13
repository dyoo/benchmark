#lang racket/base
(require "../../measurement-struct.rkt"
         "../../get-host-info.rkt"
         "../../externals/js-sicp-5-5/browser-evaluate.rkt"
         "../../externals/js-sicp-5-5/package.rkt"
	 racket/runtime-path
	 racket/port)

(define-runtime-path runtime.js "../../externals/js-sicp-5-5/runtime.js")



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
                    (call-with-input-file* runtime.js
                      (lambda (ip)
                        (copy-port ip op)))
                    
                    (newline op)
                    
                    (fprintf op "var innerInvoke = ")
                    (package-anonymous program op)
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
                  (cons 'begin (call-with-input-file* desugared-path read*))]
                 [else
                  (cons 'begin (call-with-input-file* 
                                   (build-path suite-directory (format "~a.sch" module-name))
                                 read*))]))])
    (parameterize ([current-directory suite-directory])
      (let* ([result (evaluate program)])
        (make-measurement (current-seconds)
                          (format "browser:~a" (evaluated-browser result))
                          module-name
                          (evaluated-t result)
                          (evaluated-stdout result)))))))
