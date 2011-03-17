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
	 racket/runtime-path
	 racket/list)


(require (prefix-in racket: "platforms/racket/runner.rkt"))
(require (prefix-in browser: "platforms/js-sicp-5-5-browser/runner.rkt"))
(require (prefix-in simulator: "platforms/js-sicp-5-5-simulator/runner.rkt"))
(require (prefix-in js-vm: "platforms/js-vm/runner.rkt"))

(define-runtime-path this-path ".")


(define-struct platform (name runner))
(define all-platforms (list (make-platform "racket" racket:run)
                            (make-platform "simulator" simulator:run)
                            (make-platform "js-vm" js-vm:run)
                            (make-platform "browser" browser:run)))

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

(define all-programs (list (make-program "programs/test" 'test)
                           (make-program "programs/gauss" 'gauss)
                           (make-program "programs/gauss-iter" 'gauss-iter)
                           (make-program "programs/cpstack" 'cpstack)
                           (make-program "programs/tak" 'tak)
                           (make-program "programs/conform" 'conform)))

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
      (printf "    ~a...\n" (platform-name platform))
      (save-measurement! ((platform-runner platform) (program-dir program) (program-name program))))))