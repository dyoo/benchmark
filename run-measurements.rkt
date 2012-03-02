#!/usr/bin/env racket
#lang racket/base

;; Command line program.  Run benchmarks on different platforms.
;;
;; If called with no command line arguments, applies the entire suite.
;;
;; Otherwise:
;;
;;     The first arugment is the platform [racket, browser, simulator, js-vm]
;;     The rest of the arguments are the tests to run.
;;
;; The tests are: [test, gauss, gauss-iter, cpstack, tak, conform] 
;; with more tests to come.
;;
;; Measurements are written to data/measurements.


(require "save-measurement.rkt"
	 "measurement-struct.rkt"
	 racket/promise
	 racket/runtime-path
	 racket/list)


(require (prefix-in racket: "platforms/racket/runner.rkt"))
(require (prefix-in racket-no-jit: "platforms/racket-no-jit/runner.rkt"))
(require (prefix-in browser: "platforms/js-sicp-5-5-browser/runner.rkt"))
(require (prefix-in simulator: "platforms/js-sicp-5-5-simulator/runner.rkt"))
(require (prefix-in whalesong: "platforms/whalesong/runner.rkt"))

(define-runtime-path this-path ".")


(define-struct platform (name runner))
(define all-platforms (list (make-platform "racket"
                                           (delay (racket:make-run)))
                            (make-platform "racket-no-jit"
                                           (delay (racket-no-jit:make-run)))
                            #;(make-platform "simulator" (delay (simulator:make-run)))
                            (make-platform 
			     "js-vm" 
			     (delay 
			       (let ([make-run 
				      (dynamic-require "platforms/js-vm/runner.rkt"
						       'make-run)])
				 (make-run))))
                            (make-platform "browser" (delay (browser:make-run)))
                            (make-platform "whalesong" (delay (whalesong:make-run)))))

(define (find-platform name)
  (let loop ([platforms all-platforms])
    (cond
      [(empty? platforms)
       (error 'find-platform "unknown platform ~s" name)]
      [(string=? name (platform-name (first platforms)))
       (first platforms)]
      [else
       (loop (rest platforms))])))

(define-struct program (dir name))

;; new-program: symbol -> program
(define (new-program name)
  (make-program (string-append "programs/" (symbol->string name))
                name))

;; These are all programs under the "programs/..." directory
(define all-programs (map new-program '(test
                                        gauss
                                        gauss-iter
                                        cpstack
                                        tak
                                        takl
                                        conform
                                        earley
                                        dderiv
                                        deriv
                                        graphs
                                        kanren
                                        lattice
                                        lattice2
                                        mazefun
                                        nboyer
                                        nestedloop
                                        nfa
                                        nqueens
                                        nucleic2
                                        paraffins
                                        peval
                                        puzzle
                                        sboyer
                                        scheme
                                        triangle
                                        div
                                        ackermann
                                        )))

(define (find-program name)
  (let loop ([programs all-programs])
    (cond
      [(empty? programs)
       (error 'find-program "unknown program ~s" name)]
      [(string=? name (symbol->string (program-name (first programs))))
       (first programs)]
      [else
       (loop (rest programs))])))


(define-values (programs platforms)
  (cond
    [(> (vector-length (current-command-line-arguments)) 0)
     (let ([platform (find-platform 
                      (vector-ref (current-command-line-arguments) 0))])
       (cond [(> (vector-length (current-command-line-arguments)) 1)
              (let ([programs
                     (map find-program 
                          (rest (vector->list (current-command-line-arguments))))])
                (values programs (list platform)))]
             [else
              (values all-programs (list platform))]))]
    [else 
     (values all-programs all-platforms)]))



(for ([program programs])
  (printf "Running ~a benchmark...\n" (program-name program))
  (parameterize ([current-directory this-path])
    (for ([platform platforms])
      (printf "    ~a... " (platform-name platform))
      (flush-output)
      (with-handlers ([void (lambda (err)
					 (printf "        Error occurred: ~s.\n" err))])
	 (let ([measurement ((force (platform-runner platform))
			     (program-dir program) (program-name program))]) 
	   (printf "~a milliseconds\n" (measurement-time measurement))
	   (save-measurement! measurement))))))