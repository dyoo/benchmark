#lang racket/base
(require "private/private/write-runtime.rkt"
         "private/private/write-module-records.rkt"
         "private/private/compile-moby-module.rkt"
         "private/private/module-record.rkt"
         "../../browser-evaluate.rkt"
         "../../measurement-struct.rkt"
         racket/port
         racket/runtime-path)

(provide make-run)

(define-runtime-path self-path "runner.rkt")

(define-runtime-path wescheme-language-module 
  "private/lang/base.rkt"
  #;"private/lang/wescheme.rkt")



(define extra-head-code #<<EOF
    <!-- Under IE8, excanvas doesn't work unless we get into IE7 standards
         mode. -->
    
    <!--[if IE]>
        <meta http-equiv="X-UA-Compatible" content="IE=7" />
        <![endif]-->
    
    <!--[if lt IE 8]>
        <script src="IE8.js" type="text/javascript"></script>
        <script src="ie7-squish.js" type="text/javascript"></script>
        <![endif]-->
    
    <!--[if IE]>
        <script src="excanvas.js" type="text/javascript"></script>
        <script src="ie-fixes.js" type="text/javascript"></script>
        <![endif]-->
    
    <script src="canvas-text/canvas.text.js"></script>
    <script src="canvas-text/faces/optimer-normal-normal.js"></script>
        
    <!-- Compatibility for XMLHttpRequest. -->
    <script src="XMLHttpRequest.js"></script>
  
    <script src="runtime.js"></script>
    <script src="evaluator.js"></script>
    <script src="modules.js"></script>
EOF
  )


(define (for-each/lazy f l)
  (cond
    [(null? l)
     (void)]
    [else
     (f (car l))
     (for-each/lazy f ((cdr l)))]))



;; write-supporting-files
(define (write-supporting-files output-directory)
    ;; Write out the support runtime.
    (call-with-output-file (build-path output-directory "runtime.js")
      (lambda (op)
        (write-runtime "browser" op))
      #:exists 'replace)
    (copy-support-files output-directory)
    (copy-js-compatibility-libraries output-directory)
  
  
  (let ([collection-roots
       (list wescheme-language-module)])

  (call-with-output-file (build-path tmp-htdocs "modules.js")
    (lambda (op)
      (fprintf op "var MODULES = {};\n")
      (let loop ([module-records/lazy
                  (compile-moby-modules/lazy collection-roots self-path)])
        (for-each/lazy (lambda (module-record)
                         (let ([code (encode-module-record module-record)])
                           (fprintf op
                                    "MODULES[~s]=~a;\n"
                                    (symbol->string (module-record-name 
                                                     module-record))
                                    code)))
                       module-records/lazy)))
    #:exists 'replace)))


(define-runtime-path tmp-htdocs "tmp-htdocs")

(unless (directory-exists? tmp-htdocs)
  (make-directory tmp-htdocs))
(write-supporting-files tmp-htdocs)







(define (generate-javascript-and-run program-text op)
  (let ([template #<<EOF
(function() {
    var mainModule = ~a;
    var evaluator = new Evaluator( {write: function(v) {} });
    
    return function(success, fail, params) {

        evaluator.write = function(v) {
                              params.currentDisplayer(String(v.textContent)); 
                          };
        evaluator.executeCompiledProgram(
            mainModule.bytecode,
            function(prefix) {
                evaluator.absorbPrefixIntoNamespace(prefix);
                success();
            },
            function(err) {
                console.log(err);
                fail(err.message);
            });
    }
})
EOF
                  ])
    (fprintf op template 
             (get-module-code program-text))))




(define (get-module-code program-text)
  (let* ([lang-line (format "#lang s-exp (file ~s)\n" (path->string wescheme-language-module))]
         [bytecode-ip (get-module-bytecode/port
                       program-text
                       (make-module-input-port lang-line program-text))]
         [module-record 
          (compile-plain-racket-module self-path
                                       self-path
                                       bytecode-ip)]
         [code (encode-module-record module-record)])
    code))


 


(define (make-module-input-port lang-line text)
  (let ([ip (open-input-string (string-append lang-line text))])
    (port-count-lines! ip)
    (let ([ip2
           (transplant-input-port 
            ip
            (lambda ()
              (let-values ([(l c p)
                            (port-next-location ip)])
                (cond
                  [(<= p (string-length lang-line))
                   (values l c p)]
                  [else
                   (values (sub1 l)
                           c
                           (- p (string-length lang-line)))])))
            1)])
      (port-count-lines! ip2)
      ip2)))  


  
 


(define (read-program suite-directory module-name)
  (let ([desugared-path 
         (build-path suite-directory (format "~a-desugared.sch" module-name))])
    (cond [(file-exists? desugared-path)
           (call-with-input-file* desugared-path port->string)]
          [else
           (call-with-input-file* 
               (build-path suite-directory (format "~a.sch" module-name))
             port->string)])))


(define (make-run)
  (define evaluate (make-evaluate generate-javascript-and-run
                                  #:extra-head-code extra-head-code
                                  #:extra-files-paths (list tmp-htdocs)))
    (lambda (suite-directory module-name)
  (let ([result (evaluate (read-program suite-directory module-name))])
    (make-measurement (current-seconds)
		      (format "js-vm:~a" (evaluated-browser result))
                      module-name
                      (evaluated-t result)
                      (evaluated-stdout result)))))