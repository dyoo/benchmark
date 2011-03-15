#lang racket/base

(require (prefix-in racket: "platforms/racket/runner.rkt"))
(require (prefix-in browser: "platforms/js-sicp-5-5-browser/runner.rkt"))

(racket:run "programs/tak" 'tak)
(browser:run "programs/tak" 'tak)