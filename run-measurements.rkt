#lang racket/base

(require (prefix-in racket: "platforms/racket/runner.rkt"))
(require (prefix-in browser: "platforms/js-sicp-5-5-browser/runner.rkt"))
(require (prefix-in simulator: "platforms/js-sicp-5-5-simulator/runner.rkt"))



(racket:run "programs/cpstack" 'cpstack)
(simulator:run "programs/cpstack" 'cpstack)
(browser:run "programs/cpstack" 'cpstack)


(racket:run "programs/tak" 'tak)
(simulator:run "programs/tak" 'tak)
(browser:run "programs/tak" 'tak)
