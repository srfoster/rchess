#lang racket

(provide all-squares
         all-occupied-squares)

(require chess
         "./current-board.rkt")

(define (all-squares)
  (for*/list ([rank (in-chess-ranks)]
              [file (in-chess-files)])
    (chess-square #:rank rank #:file file)))

(define (all-occupied-squares)
  (filter
    occupied-chess-square?
    (map
      (curry chess-board-ref-square (current)) 
      (all-squares))))
