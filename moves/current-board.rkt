#lang racket

(provide current on-board)

(require chess)

(define current (make-parameter #f))

(define-syntax-rule (on-board b expr ...)
  (parameterize ([current b])
     expr ...))
