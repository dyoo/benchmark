(define (gauss n)
  (if (= n 0)
      0
      (+ n (gauss (- n 1)))))
 
(letrec ([loop (lambda (n v)
		 (if (zero? n)
		     (begin (display v) (newline))
		     (loop (- n 1) (gauss 1000))))])
  (loop 1500 0))