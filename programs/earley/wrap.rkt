#lang racket
(provide (rename-out (module-begin #%module-begin)))

(require racket/include
         (for-syntax racket/list))

(define-syntax (module-begin stx)
  (define name (syntax-property stx 'enclosing-module-name))
  (define tokens (rest (syntax->datum stx)))
  (define r5rs? (memq 'r5rs tokens))
  #`(#%module-begin
     #,@(if r5rs? #'((require r5rs)) #'())
     (include #,(format "~a.sch" name))))
