#lang racket/base


;; http://code.google.com/apis/chart/docs/gallery/line_charts.html


(require racket/runtime-path
         racket/match
         racket/date
         racket/list
         "measurement-struct.rkt")

(define plots-path "data/plots")


(define template #<<EOF
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  </head>
  <body onload="loadAllGraphs()">
    <h1>Benchmarks</h1>
    ~a
  </body>
</html>
EOF
  )

(define (make-graph-link name)
  (let ([graph-text #<<EOF
    <div>
    <h1>~a</h1>
    <iframe src="~a.html" width="300" height="200"></iframe>
    </div>
EOF
                    ])
    (format graph-text name name)))


(define-runtime-path measurements "data/measurements")


(define SECONDS-IN-A-DAY
  (* 24 ;; hours
     60 ;; minutes per hour
     60 ;; seconds per minute
     ))


(define-struct data-point (date hostname program platform time)
  #:transparent)

(define data-points 
  (let ()
    (define (switch-to-day-number points)
      (let ([first-second 
             (apply min (map date->seconds (map data-point-date points)))])
        (map (lambda (point)
               (match point
                 [(struct data-point (date hostname program platform time))
                  (make-data-point (quotient (- (date->seconds date) first-second)
                                             SECONDS-IN-A-DAY)
                   hostname 
                   program
                   platform 
                   time)]))
             points)))
     
    
    (define (sexp->data-point sexp)
      (match sexp
        [(list (list 'current-date seconds readable-date date-attrs ...)
               (list 'host-name host-name)
               (list 'program program)
               (list 'platform platform)
               (list 'time time)
               (list 'output output))
         (cond [(empty? date-attrs)
                (make-data-point (seconds->date seconds)
                                 host-name 
                                 (format "~a" program)
                                 platform 
                                 time)]
               [else
                (make-data-point (apply make-date
                                        (map (lambda (attr)
                                               (second (assoc attr date-attrs)))
                                             '(second 
                                               minute hour 
                                               day month year
                                               week-day year-day 
                                               dst? time-zone-offset)))
                                 host-name
                                 (format "~a" program) 
                                 platform 
                                 time)])]))    
    (define (read* ip)
      (let ([next (read ip)])
        (cond
          [(eof-object? next)
           '()]
          [else
           (cons (sexp->data-point next) 
                 (read* ip))])))
    
    (switch-to-day-number
     (call-with-input-file measurements read*))))


;; unique: (listof string) -> (listof string)
(define (unique-strings strs)
  (define ht (make-hash))
  (for ([s strs]) (hash-set! ht s #t))
  (sort (for/list ([k (in-hash-keys ht)]) k)
        string<?))




(define all-program-names (unique-strings (map data-point-program data-points)))
(define all-platform-names (unique-strings (map data-point-platform data-points)))


;; cluster: (listof X) (X -> Y) -> (hashof Y X)
(define (cluster lst f)
  (define ht (make-hash))
  (for ([x lst])
    (hash-set! ht (f x) (cons x (hash-ref ht (f x) '()))))
  ht)


;; filter-points-by-program: (listof data-point) string -> (listof points)
(define (filter-points-by-program pts program-name)
  (filter (lambda (p) (string=? (data-point-program p)
                                program-name))
          pts))


;;(define cluster-datapoints-by-day (cluster data-points data-point-date))




(define (make-plot name)
  (let* ([points (filter-points-by-program data-points name)]
         [text #<<EOF
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <script type='application/javascript'>

    var loadAllGraphs = function() {
      var i;
      var children = document.body.children;
      for(i = 0; i < children.length; i++) {
          if (typeof(children[i]) === 'object' && children[i].tagName === 'FORM') {
              children[i].submit();
          }
      }
    }
  </script>
  </head>
  <body onload="loadAllGraphs()">
    <form action='https://chart.googleapis.com/chart' method='POST' id='post_form'
          onsubmit="this.action = 'https://chart.googleapis.com/chart?chid=' + (new Date()).getMilliseconds(); return true;">
      <input type='hidden' name='cht' value='lxy'/>
      <input type='hidden' name='chtt' value=~s/>
      <input type='hidden' name='chs' value='300x200'/>
      <input type='hidden' name='chxt' value='x,y'/>
      <input type='hidden' name='chxr' value='0,0,6|1,0,1000'>
      <input type='hidden' name='chco' value="FF0000,00FF00,0000FF">
      <input type='hidden' name='chdl' value="racket|browser|js-vm">
      <input type='hidden' name='chd' value='t:1,2,3,4,5,6|40,20,50,20,3,17|1,2,3,4,5,6|50,100,50,20,10|1,2,3,4,5,6|1,2,3,4,5,6'/>
      <input type='submit'/>
    </form>
  </body>
</html>
EOF
              ])
    (format text name)))




(define (normalize numbers)
  (let* ([min-val (apply min numbers)]
         [max-val (apply max numbers)]
         [width (- max-val min-val)])
    (map (lambda (n)
           (exact->inexact (* (/ (- n min-val) width) 100)))
         numbers)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(call-with-output-file (build-path plots-path "index.html")
  (lambda (op)
    (display (format template 
                     (apply string-append
                            (map make-graph-link
                                 (take all-program-names 1))))
             op))
  #:exists 'replace)

(for ([program-name all-program-names])
  (call-with-output-file (build-path plots-path (format "~a.html" program-name))
    (lambda (op)
      (display (make-plot program-name) op))
    #:exists 'replace))