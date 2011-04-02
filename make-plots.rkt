#lang racket/base


;; http://code.google.com/apis/chart/docs/gallery/line_charts.html
;;
;; This program takes the data/measurements and plots the performance of js-sicp and browser
;; with regards to raw Racket.
;;

(require racket/runtime-path
         racket/match
         racket/date
         racket/list
         racket/string
         racket/pretty
         "measurement-struct.rkt")


(define plot-width 400)
(define plot-height 400)

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
  (let* ([program-data (second (assoc name analyzed-points))]
         [op (open-output-string)]
         [platforms (unique-strings (map analyzed-point-platform program-data))]
         [template #<<EOF
    <div>
    <h1>~a</h1>
    <iframe src="~a.html" width="~a" height="~a"></iframe>
    <p><small>(smaller is better)</small></p>
    <pre>
EOF
                   ])
    (fprintf op template
             name name
             plot-width
             plot-height)
    
    (for ([platform platforms])
      (let ([filtered-points (filter (lambda (p) (string=? (analyzed-point-platform p) platform))
                                     program-data)])
        (define previous-p #f)
        (for ([p filtered-points])
          (fprintf op "Day ~a: ~a took ~a times longer than racket~a\n"
                   (analyzed-point-day p)
                   (analyzed-point-platform p)
                   (analyzed-point-times-slower p)
                   (if previous-p 
                       (format " (~a)" (add-sign (- (analyzed-point-times-slower p)
                                                    (analyzed-point-times-slower previous-p))))
                       ""))
          (set! previous-p p)))
      (fprintf op "\n"))
    (fprintf op "</pre></div>")
    (get-output-string op)))

(define (add-sign n)
  (if (> n 0)
      (format "+~a" n)
      (format "~a" n)))


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
  (filter-points-by-key pts data-point-program program-name))


(define (filter-points-by-key pts key-f name)
  (filter (lambda (p) (string=? (key-f p)
                                name))
          pts))
  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This analysis computes time relative to Racket raw performance, for each test,
;; on each day.
;;

;; (listof number) -> number
(define (average nums)
  (/ (apply + nums)
     (length nums)))


;; (listof data-point) -> (listof (list [test-name string] (listof analyzed-point)))
(define (analyze)
  (for/list ([test (unique-strings (map data-point-program data-points))])
    (list test 
          (sort (analyze-by-test (filter-points-by-program data-points test))
                analyzed-point<?))))


;; (listof data-point) -> (listof ...)
(define (analyze-by-test points)
  (let* ([clustered-by-day (cluster points data-point-date)])
    (apply append
           (for/list ([day (in-hash-keys clustered-by-day)])
             (analyze-single-day day (hash-ref clustered-by-day day))))))

  


;; number points -> (listof analyzed-point)
(define (analyze-single-day day points)
  (let* ([ht (make-hash)]
         [hosts (unique-strings (map data-point-hostname points))]
         [data-per-host
          (for/list ([host hosts])
            (analyze-single-day-and-host day (filter-points-by-key points data-point-hostname host)))])
    ;; we need to normalize the data per host.
    (define (normalize pts)
      (let ([ht (make-hash)])
        (for ([p pts])
          (hash-set! ht (analyzed-point-platform p) 
                     (cons (analyzed-point-times-slower p)
                           (hash-ref ht (analyzed-point-platform p) '()))))
        (for/list ([key (in-hash-keys ht)])
          (make-analyzed-point (add1 day) key (average (hash-ref ht key))))))
    (normalize (apply append data-per-host))))

;; number points -> (listof analyzed-point)
(define (analyze-single-day-and-host day points)
  ;; First, find how long it took for racket.
  (let* ([racket-times
          (map data-point-time 
               (filter-points-by-key points data-point-platform "racket"))])
    (cond
      [(empty? racket-times)
       '()]
      [else
       (let ([average-racket-time
              (average racket-times)])
         #;(printf "Time for racket: ~s\n" time-for-racket)
         ;; Then, for every other platform, restate the data in terms of times slower than racket. 
         (for/list ([platform (remove "racket" (unique-strings (map data-point-platform points)))])
           (let ([platform-points (filter-points-by-key points data-point-platform platform)])
             (let ([times-slower
                    (/ (average (map data-point-time platform-points))
                       average-racket-time)])
               #;(printf "~s: ~s times slower than racket\n" 
                         platform
                         times-slower)
               (make-analyzed-point (add1 day) platform times-slower)))))])))

(define-struct analyzed-point (day platform times-slower)
  #:transparent)


(define (analyzed-point<? p1 p2)
  (cond
    [(< (analyzed-point-day p1)
        (analyzed-point-day p2))
     #t]
    [(> (analyzed-point-day p1)
        (analyzed-point-day p2))
     #f]
    [else
     (string<? (analyzed-point-platform p1)
               (analyzed-point-platform p2))]))


                                                     

(define analyzed-points (analyze))

(define (clone-first l)
  (cons
   (car l) l))

(define (make-plot name)
  (let* ([points (second (assoc name analyzed-points))]
         [platform-names (remove "racket" (unique-strings (map analyzed-point-platform points)))]
         [colors (map platform-name->color platform-names)]
         [min-days 0 #;(apply min (map analyzed-point-day points))]
         [max-days (apply max (map analyzed-point-day points))]
         [min-times-slower (apply min (map analyzed-point-times-slower points))]
         [max-times-slower (apply max (map analyzed-point-times-slower points))]
         [point-text (string-join 
                      (map (lambda (platform-name)
                             (let* ([platform-points
                                     (filter (lambda (p) (string=? (analyzed-point-platform p) platform-name))
                                             points)]
                                    [normalized-platform-days
                                     (cons 0
                                           (map (lambda (x)
                                                  (inexact->exact (floor (* 100.0
                                                                            (/ (analyzed-point-day x)
                                                                               max-days)))))
                                                platform-points))]
                                    [normalized-times-slower
                                     (clone-first (map (lambda (x)
                                                         (inexact->exact (floor (* 100.0
                                                                                   (/ (analyzed-point-times-slower x)
                                                                                      max-times-slower)))))
                                                       platform-points))])
                               (string-append (string-join
                                               (map number->string normalized-platform-days)
                                               ",")
                                              "|"
                                              (string-join
                                               (map number->string normalized-times-slower)
                                               ","))))
                           platform-names)
                      "|")]
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
      <input type='hidden' name='chs' value='~ax~a'/>
      <input type='hidden' name='chxt' value='x,y'/>
      <input type='hidden' name='chxr' value='0,~a,~a|1,~a,~a'>
      <input type='hidden' name='chco' value="~a">
      <input type='hidden' name='chdl' value="~a">
      <input type='hidden' name='chd' value='t:~a'/>
      <input type='submit'/>
    </form>
  </body>
</html>
EOF
              ])
    (format text
            name
            plot-width
            plot-height
            min-days
            max-days
            min-times-slower
            max-times-slower
            (string-join colors ",")
            (string-join platform-names "|")
            point-text
            )))


(define WEB-COLORS '(;"FFFFFF" ;; white
                     ;"C0C0C0" ;; silver
                     ;"808080" ;; gray
                     "FF0000" ;; red
                     ;"FFFF00" ;; yellow
                     "808000" ;; olive
                     ;"008000" ;; green
                     "000000" ;; black
                     "0000FF" ;; blue
                     "800000" ;; maroon
                     ;"00FF00" ;; lime
                     ;"00FFFF" ;; aqua
                     "008080" ;; teal

                     "000080" ;; navy
                     "FF00FF" ;; fushia
                     "800080" ;; purple
                     ))

(define platform-name->color 
  (let ([counter 0]
        [ht (make-hash)])
  (lambda (name)
    (cond
      [(hash-has-key? ht name)
       (hash-ref ht name)]
      [else
       (hash-set! ht name (list-ref WEB-COLORS counter))
       (set! counter (add1 counter))
       (hash-ref ht name)]))))



(define (normalize numbers)
  (printf "I see numbers: ~s" numbers)
  (let* ([min-val (apply min numbers)]
         [max-val (apply max numbers)]
         [width (- max-val min-val)])
    (map (lambda (n)
           (exact->inexact (* (/ (- n min-val) width) 100)))
         numbers)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (directory-exists? plots-path)
  (make-directory plots-path))

(call-with-output-file (build-path plots-path "index.html")
  (lambda (op)
    (display (format template 
                     (apply string-append
                            (map make-graph-link
                                 all-program-names)))
             op))
  #:exists 'replace)

(for ([program-name all-program-names])
  (call-with-output-file (build-path plots-path (format "~a.html" program-name))
    (lambda (op)
      (display (make-plot program-name) op))
    #:exists 'replace))