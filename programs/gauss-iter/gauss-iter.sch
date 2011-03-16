(define (gauss n)
  (gauss-iter n 0))

(define (gauss-iter n acc)
  (if (= n 0)
      acc
      (gauss-iter (- n 1) (+ n acc))))
 
(letrec ([loop (lambda (n v)
		 (if (zero? n)
		     (begin (display v) (newline))
		     (loop (- n 1) (gauss 10000))))])
  (loop 1500 0))