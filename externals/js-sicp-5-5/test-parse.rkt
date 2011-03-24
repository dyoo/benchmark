#lang racket/base

(require "parse.rkt"
         "lexical-structs.rkt"
         "expression-structs.rkt"
         (for-syntax racket/base))

; Test out the compiler, using the simulator.
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ expr expt)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running ~s ...\n" (syntax->datum #'expr))
           (let ([expected expt]
                 [actual 
                  (with-handlers ([void
                                   (lambda (exn)
                                     (raise-syntax-error #f (format "Runtime error: got ~s" exn)
                                                         #'stx))])
                    expr)])
             (unless (equal? actual expected)
               (raise-syntax-error #f (format "Expected ~s, got ~s" expected actual)
                                   #'stx))
             (printf "ok.\n\n")))))]))



(test (parse '1) 
      (make-Top (make-Prefix '())
                (make-Constant 1)))

(test (parse ''hello) 
      (make-Top (make-Prefix '()) 
                (make-Constant 'hello)))

(test (parse 'hello) 
      (make-Top (make-Prefix '(hello)) 
                (make-ToplevelRef 0 0)))

(test (parse '(begin hello world))
      (make-Top (make-Prefix '(hello world)) 
                (make-Seq (list (make-ToplevelRef 0 0)
                                (make-ToplevelRef 0 1)))))

(test (parse '(define x y))
      (make-Top (make-Prefix '(x y))
                (make-ToplevelSet 0 0 'x (make-ToplevelRef 0 1))))

(test (parse '(begin (define x 42)
                     (define y x)))
      (make-Top (make-Prefix '(x y))
                (make-Seq (list (make-ToplevelSet 0 0 'x (make-Constant 42))
                                (make-ToplevelSet 0 1 'y (make-ToplevelRef 0 0))))))

(test (parse '(if x y z))
      (make-Top (make-Prefix '(x y z))
                (make-Branch (make-ToplevelRef 0 0)
                             (make-ToplevelRef 0 1)
                             (make-ToplevelRef 0 2))))

(test (parse '(if x (if y z 1) #t))
      (make-Top (make-Prefix '(x y z))
                (make-Branch (make-ToplevelRef 0 0)
                             (make-Branch (make-ToplevelRef 0 1)
                                          (make-ToplevelRef 0 2)
                                          (make-Constant 1))
                             (make-Constant #t))))

(test (parse '(if x y))
      (make-Top (make-Prefix '(x y))
                (make-Branch (make-ToplevelRef 0 0)
                             (make-ToplevelRef 0 1)
                             (make-Constant (void)))))

(test (parse '(cond [x y]))
      (make-Top (make-Prefix '(x y))
                (make-Branch (make-ToplevelRef 0 0)
                             (make-ToplevelRef 0 1)
                             (make-Constant (void)))))

(test (parse '(cond [x y] [else "ok"]))
      (make-Top (make-Prefix '(x y))
                (make-Branch (make-ToplevelRef 0 0)
                             (make-ToplevelRef 0 1)
                             (make-Constant "ok"))))

(test (parse '(lambda (x y z) x))
      (make-Top (make-Prefix '())
                (make-Lam #f 3 (make-LocalRef 0 #f) '())))

(test (parse '(lambda (x y z) y))
      (make-Top (make-Prefix '())
                (make-Lam #f 3 (make-LocalRef 1 #f) '())))

(test (parse '(lambda (x y z) z))
      (make-Top (make-Prefix '())
                (make-Lam #f 3 (make-LocalRef 2 #f) '())))


(test (parse '(lambda (x y z) x y z))
      (make-Top (make-Prefix '())
                (make-Lam #f 3 (make-Seq (list (make-LocalRef 0 #f)
                                            (make-LocalRef 1 #f)
                                            (make-LocalRef 2 #f)))
                          '())))

(test (parse '(lambda (x y z) k))
      (make-Top (make-Prefix '(k))
                (make-Lam #f 
                          3 
                          (make-ToplevelRef 0 0 )
                          '(0))))

(test (parse '(lambda (x y z) k x y z))
      (make-Top (make-Prefix '(k))
                (make-Lam #f
                          3 (make-Seq (list (make-ToplevelRef 0 0 )
                                            (make-LocalRef 1 #f)
                                            (make-LocalRef 2 #f)
                                            (make-LocalRef 3 #f)))
                          '(0))))

(test (parse '(lambda (x)
                (lambda (y)
                  (lambda (z)
                    x 
                    y 
                    z
                    w))))
      (make-Top (make-Prefix '(w))
                (make-Lam #f 1 
                          (make-Lam #f 1
                                    (make-Lam #f 1
                                              (make-Seq (list 
                                                         (make-LocalRef 1 #f)
                                                         (make-LocalRef 2 #f)
                                                         (make-LocalRef 3 #f)
                                                         (make-ToplevelRef 0 0)))
                                              '(0 1 2) ;; w x y
                                              )
                                    
                                    '(0 1) ;; w x
                                    )
                          '(0))))

(test (parse '(lambda (x)
                (lambda (y)
                  x)))
      (make-Top (make-Prefix '())
                (make-Lam #f 1
                          (make-Lam #f 1
                                    (make-LocalRef 0 #f)
                                    '(0))
                          (list))))

(test (parse '(lambda (x)
                (lambda (y)
                  y)))
      (make-Top (make-Prefix '())
                (make-Lam #f 1
                          (make-Lam #f 1
                                    (make-LocalRef 0 #f)
                                    (list))
                          (list))))

(test (parse '(+ x x))
      (make-Top (make-Prefix '(+ x))
                (make-App (make-ToplevelRef 2 0)
                          (list (make-ToplevelRef 2 1)
                                (make-ToplevelRef 2 1)))))
  

(test (parse '(lambda (x) (+ x x)))
      (make-Top (make-Prefix '(+))
                (make-Lam #f 1
                          (make-App (make-ToplevelRef 2 0)
                                    (list (make-LocalRef 3 #f)
                                          (make-LocalRef 3 #f)))
                          '(0))))

(test (parse '(lambda (x) 
                (+ (* x x) x)))
      (make-Top (make-Prefix '(* +))
                (make-Lam #f 1
                          ;; stack layout: [???, ???, prefix, x]
                          (make-App (make-ToplevelRef 2 1)
                                    (list
                                     ;; stack layout: [???, ???, ???, ???, prefix, x]
                                     (make-App (make-ToplevelRef 4 0)
                                               (list (make-LocalRef 5 #f)
                                                     (make-LocalRef 5 #f)))
                                     (make-LocalRef 3 #f)))
                          '(0))))

(test (parse '(let ()
                x))
      (make-Top (make-Prefix '(x))
                (make-ToplevelRef 0 0)))

(test (parse '(let ([x 3])
                x))
      (make-Top (make-Prefix '())
                (make-Let1 (make-Constant 3)
                           (make-LocalRef 0 #f))))

(test (parse '(let ([x 3]
                    [y 4])
                x
                y))
      (make-Top (make-Prefix '())
                (make-LetVoid 2
                              (make-Seq (list (make-InstallValue 0 (make-Constant 3) #f)
                                              (make-InstallValue 1 (make-Constant 4) #f)
                                              (make-Seq (list (make-LocalRef 0 #f)
                                                              (make-LocalRef 1 #f)))))
                              #f)))

(test (parse '(let ([x 3]
                    [y 4])
                (let ([x y]
                      [y x])
                  x
                  y)))
      (make-Top (make-Prefix '())
                (make-LetVoid 2
                              (make-Seq (list (make-InstallValue 0 (make-Constant 3) #f)
                                              (make-InstallValue 1 (make-Constant 4) #f)
                                              (make-LetVoid 2
                                                            (make-Seq (list (make-InstallValue 0 (make-LocalRef 3 #f) #f)
                                                                            (make-InstallValue 1 (make-LocalRef 2 #f) #f)
                                                                            (make-Seq (list (make-LocalRef 0 #f)
                                                                                            (make-LocalRef 1 #f)))))
                                                            #f)))
                              #f)))



(test (parse '(let* ([x 3]
                     [x (add1 x)])
                (add1 x)))
      (make-Top (make-Prefix '(add1))
                
                ;; stack layout: [prefix]
                
                (make-Let1 (make-Constant 3)
                           
                           ;; stack layout: [x_0, prefix]

                           (make-Let1
                            
                            ;; stack layout: [???, x_0, prefix]

                            (make-App 
                             
                             ;; stack layout: [???, ???, x_0, prefix]
                             (make-ToplevelRef 3 0) (list (make-LocalRef 2 #f)))
                            
                            ;; stack layout [???, x_1, x_0, prefix]
                            (make-App (make-ToplevelRef 3 0)
                                      (list (make-LocalRef 1 #f)))))))


(test (parse '(let* ()
                42))
      (make-Top (make-Prefix '()) (make-Constant 42)))


(test (parse '(letrec ([x (lambda (x) x)]
                       [y (lambda (x) x)])
                (x y)))
      (make-Top (make-Prefix '())
                (make-LetVoid 2
                              (make-Seq 
                               (list 
                                (make-InstallValue 0
                                                   (make-Lam 'x 1 (make-LocalRef 0 #f) '())
                                                   #t)
                                (make-InstallValue 1 
                                                   (make-Lam 'y 1 (make-LocalRef 0 #f) '())
                                                   #t)
                                ;; stack layout: ??? x y
                                (make-App (make-LocalRef 1 #t)
                                          (list (make-LocalRef 2 #t)))))
                              #t)))


(test (parse '(letrec ([x (lambda (x) (y x))]
                       [y (lambda (x) (x y))])
                (x y)))
      (make-Top (make-Prefix '())
                (make-LetVoid 2
                              (make-Seq 
                               (list 
                                (make-InstallValue 0
                                                   (make-Lam 'x 1
                                                             (make-App (make-LocalRef 1 #t) 
                                                                       (list (make-LocalRef 2 #f)))
                                                             '(1))
                                                   #t)
                                (make-InstallValue 1 
                                                   (make-Lam 'y 1 
                                                             (make-App (make-LocalRef 2 #f)
                                                                       (list (make-LocalRef 1 #t)))
                                                             '(1))
                                                   #t)
                                ;; stack layout: ??? x y
                                (make-App (make-LocalRef 1 #t)
                                          (list (make-LocalRef 2 #t)))))
                              #t)))

(test (parse '(let ([x 0])
                (lambda ()
                  (set! x (add1 x)))))
      (make-Top (make-Prefix '(add1))
                (make-Let1 (make-Constant 0)
                           (make-BoxEnv 0
                                        (make-Lam #f 0 
                                                  (make-Seq (list (make-InstallValue 
                                                                   1 
                                                                   (make-App (make-ToplevelRef 1 0)
                                                                             (list (make-LocalRef 2 #t)))
                                                                   #t)
                                                                  (make-Constant (void))))
                                                  '(1 0)))))) ;; x is 0, prefix is 1



(test (parse '(let ([x 0]
                    [y 1])
                (lambda ()
                  (set! x (add1 x)))))
      (make-Top (make-Prefix '(add1))
                (make-LetVoid 2
                              (make-Seq (list
                                         (make-InstallValue 0 (make-Constant 0) #t)
                                         (make-InstallValue 1 (make-Constant 1) #t)
                                         (make-Lam #f 0 
                                                   (make-Seq
                                                    (list (make-InstallValue 
                                                           1 
                                                           (make-App (make-ToplevelRef 1 0)
                                                                     (list (make-LocalRef 2 #t)))
                                                           #t)
                                                          (make-Constant (void))))
                                                   '(2 0))))
                              #t)))



(test (parse '(begin (define a '(hello))
                     (define b '(world))
                     (define reset!
                       (lambda ()
                         (set! a '())
                         (set! b '())))
                     (reset!)
                     (list a b)))
      (make-Top
       (make-Prefix '(a b list reset!))
       (make-Seq
        (list
         (make-ToplevelSet 0 0 'a (make-Constant '(hello)))
         (make-ToplevelSet 0 1 'b (make-Constant '(world)))
         (make-ToplevelSet
          0
          3
          'reset!
          (make-Lam
           'reset!
           0
           (make-Seq
             (list
              (make-Seq (list (make-ToplevelSet 0 0 'a (make-Constant '())) (make-Constant (void))))
              (make-Seq (list (make-ToplevelSet 0 1 'b (make-Constant '())) (make-Constant (void))))))
           '(0)))
         (make-App (make-ToplevelRef 0 3) '())
         (make-App (make-ToplevelRef 2 2) (list (make-ToplevelRef 2 0) (make-ToplevelRef 2 1)))))))