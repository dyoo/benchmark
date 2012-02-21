#lang racket/base

(require plot
         racket/list
         "make-plots.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (histogram-with-points data)
  (define-values (histogram-points label-points)
    (for/fold ([histogram-points '()]
               [label-points '()])
              ([data-point data]
               [i (in-naturals)])
       (define floored-val (inexact->exact (floor (second data-point))))
       (values (cons (discrete-histogram (list (vector (first data-point) (second data-point)))
                                         #:x-min i)
                     histogram-points)
               (cons (point-label (vector (+ i 0.5) (second data-point)) 
                                  (string-append (number->string floored-val) "X"))
                     label-points))))
  (append histogram-points label-points))


(define latest-points 
  (apply append
         (for/list ([class+data analyzed-points])
           (define the-class (first class+data))
           (define the-data (second class+data))
           (define the-filtered-data
             (let ([max-date (apply max (map analyzed-point-day the-data))])
               (filter (lambda (a) (= (analyzed-point-day a) max-date))
                       the-data)))
           (for/list ([d the-filtered-data])
             (list the-class (analyzed-point-platform d) (analyzed-point-times-slower d))))))


;; whalesong-points: (listof (list [test-type string] [times-racket number]))
(define whalesong-points (for/list ([p latest-points]
                                    #:when (and (regexp-match #px"whalesong" (second p))
                                                (not (regexp-match #px"^test$" (first p)))))
                           (list (first p) (third p))))


(plot (histogram-with-points whalesong-points)
      #:y-max (+ (apply max (map second whalesong-points)) 10)
      #:title "Performance of the JS-based evaluator relative to Racket"
      #:x-label "Programs"
      #:y-label "X times slower than native Racket"
      #:width 1200)

                             
                             
#;(plot (histogram-with-points '((gauss 50)
                               (gauss-iter 60)
                               (cpstack 40)))
      #:y-max 70
      
      #:title "Performance of the JS-based evaluator relative to Racket"
      #:x-label "Programs"
      #:y-label "X times slower than native Racket"
      )

#;(plot (list (discrete-histogram (list (vector 'gauss (/ 9930. 129))
                                      (vector 'gauss-iter (/ 11091 82.36))
                                      (vector 'cpstack (/ 282492 4805.))))
            (point-label (vector 0.5 (/ 9930. 129)) 
                         (number->string (inexact->exact (floor (/ 9930. 129))))))
      #:title "Performance of the JS-based evaluator relative to Racket"
      #:x-label "Programs"
      #:y-label "* times slower than native Racket")

