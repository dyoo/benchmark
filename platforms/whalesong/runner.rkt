#lang racket/base
(require "../../measurement-struct.rkt"
         "../../get-host-info.rkt"
         (planet dyoo/whalesong:1:15/make/make-structs)
         (planet dyoo/whalesong:1:15/js-assembler/package)
         (planet dyoo/browser-evaluate)
         racket/runtime-path
         racket/path
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
                      (fprintf op "var machine = new plt.runtime.Machine();")
                      (fprintf op "plt.runtime.currentMachine = machine;");
                      (package program
                               #:should-follow-children? (lambda (src) #t)
                               #:output-port op)
                      (fprintf op #<<EOF
return (function(succ, fail, params) {
            plt.runtime.ready(function() {
                                     machine.params.currentDisplayer = function(MACHINE, v) {
                                         params.currentDisplayer(v);
                                     };
                                     plt.runtime.invokeMains(machine,
                                                             succ,
                                                             function(MACHINE, e) {
                                                                fail(e);
                                                             });
                              });
        });
});
EOF
                               ))))
  (lambda (suite-directory module-name)
    (let ([program
           (let ([path 
                  (build-path suite-directory (format "~a-whalesong.rkt" module-name))])
             (cond [(file-exists? path)
                    (make-MainModuleSource (normalize-path path))]
                   [else
                    (error 'whalesong-runner "Couldn't find ~a" path)]))])
      (parameterize ([current-directory suite-directory])
        (let* ([result (evaluate program)])
          (make-measurement (current-seconds)
                            (format "whalesong:~a" (evaluated-browser result))
                            module-name
                            (evaluated-t result)
                            (evaluated-stdout result)))))))
