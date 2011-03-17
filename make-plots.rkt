#lang racket/base

(require racket/runtime-path)

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
    <iframe src="plots/~a.html"></iframe>
EOF
                    ])
    (format graph-text name)))


(define (make-plot name)
  (let ([text #<<EOF
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
    <h2>~a</h2>
    <form action='https://chart.googleapis.com/chart' method='POST' id='post_form'
          onsubmit="this.action = 'https://chart.googleapis.com/chart?chid=' + (new Date()).getMilliseconds(); return true;">
      <input type='hidden' name='cht' value='lc'/>
      <input type='hidden' name='chtt' value='This is | my chart'/>
      <input type='hidden' name='chs' value='300x200'/>
      <input type='hidden' name='chxt' value='x'/>
      <input type='hidden' name='chd' value='t:40,20,50,20,100'/>
      <input type='submit'/>
    </form>
  </body>
</html>
EOF
              ])
    (format text name)))





(call-with-output-file (build-path plots-path "index.html")
  (lambda (op)
    (display (format template 
                     (apply string-append
                            (map make-graph-link
                                 '("test"))))
             op))
  #:exists 'replace)



(for ([program-name '("test")])
  (call-with-output-file (build-path plots-path (format "~a.html" program-name))
    (lambda (op)
      (display (make-plot program-name) op))
    #:exists 'replace))