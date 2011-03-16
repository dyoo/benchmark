#lang racket/base

(require "save-measurement.rkt"
	 racket/runtime-path
	 racket/list)


(require (prefix-in racket: "platforms/racket/runner.rkt"))
(require (prefix-in browser: "platforms/js-sicp-5-5-browser/runner.rkt"))
(require (prefix-in simulator: "platforms/js-sicp-5-5-simulator/runner.rkt"))


(define-runtime-path this-path ".")

(define programs '(("programs/gauss" gauss)
		   ("programs/gauss-iter" gauss-iter)
		   ("programs/cpstack" cpstack)
		   ("programs/tak" tak)
		   ))


(for ([p programs])
  (let ([dir (first p)]
	[module-name (second p)])
    (printf "Running ~a benchmark...\n" module-name)
    (parameterize ([current-directory this-path])
      (printf "    racket...\n")
      (save-measurement! (racket:run dir module-name))
      (printf "    simulator...\n")
      (save-measurement! (simulator:run dir module-name))
      (printf "    browser...\n")
      (save-measurement! (browser:run dir module-name)))))
