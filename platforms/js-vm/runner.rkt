#lang racket/base
(require "private/private/write-runtime.rkt"
         "private/private/write-module-records.rkt"
         "private/private/module-record.rkt"
         "private/private/compile-moby-module.rkt"
         "../../utils.rkt"
         "../../browser-evaluate.rkt"
         "../../measurement-struct.rkt"
         racket/port
         racket/path
         racket/runtime-path)

(provide run)

(define-runtime-path self-path "runner.rkt")

(define-runtime-path wescheme-language-module 
  "private/lang/wescheme.rkt")

(define-runtime-path wescheme-interaction-language-module 
  "private/lang/wescheme-interaction.rkt")


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
EOF
  )



;; write-supporting-files
(define (write-supporting-files output-directory)
    ;; Write out the support runtime.
    (call-with-output-file (build-path output-directory "runtime.js")
      (lambda (op)
        (write-runtime "browser" op))
      #:exists 'replace)
    (copy-support-files output-directory)
    (copy-js-compatibility-libraries output-directory))


(define-runtime-path tmp-htdocs "tmp-htdocs")

(unless (directory-exists? tmp-htdocs)
  (make-directory tmp-htdocs))
(write-supporting-files tmp-htdocs)



(define (generate-javascript-and-run program op)
  (void)
  #;(let* ([empty-module-record
          (compile-plain-racket-module )]
         
         [interaction-record 
          (compile-interaction `(file ,(path->string wescheme-interaction-language-module))
                               (datum->syntax #f program))]
         [code
          (encode-interaction-record interaction-record)])
      
  
  (printf "I see ~a" program)
  
  (fprintf op "(function() {")
  
  (write-module-records module-records op)
  
  
  (fprintf op "})")))



(define (for-module)
  (let* ([lang-line (format "#lang s-exp (file ~s)\n" (path->string wescheme-language-module))]
         [bytecode-ip (get-module-bytecode/port
                       "empty-name"
                       (make-module-input-port lang-line ""))]
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


  
 
#;(define evaluate (make-evaluator generate-javascript-and-run
                                  #:extra-head-code extra-head-code
                                  #:extra-files-path (list tmp-htdocs)))




(define (read* inp)
  (parameterize ([read-accept-reader #t])
    (datum->syntax `(begin ,@
                           (let loop ()
                             (let ([next (read-syntax #f inp)])
                               (cond
                                 [(eof-object? next)
                                  '()]
                                 [else
                                  (cons next (loop))])))))))

(define (read-program suite-directory module-name)
  (let ([desugared-path 
         (build-path suite-directory (format "~a-desugared.sch" module-name))])
    (cond [(file-exists? desugared-path)
           (cons 'begin (call-with-input-file* desugared-path read*))]
          [else
           (cons 'begin (call-with-input-file* 
                            (build-path suite-directory (format "~a.sch" module-name))
                          read*))])))

(define (run suite-directory module-name)
  (void)
  #;(let ([result (evaluate (read-program suite-directory module-name))])
    (make-measurement "current-seconds"
                      "js-vm" 
                      module-name
                      (evaluated-t result)
                      (evaluated-stdout result))))
