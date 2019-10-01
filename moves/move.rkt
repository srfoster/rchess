#lang racket

(provide move)

(require chess 
         pict
         "./util.rkt"
         "./all-squares.rkt"
         "./current-board.rkt")

(require rebellion/streaming/reducer)

(define (move s1 s2)
  (define piece (chess-board-ref (current) s1))

  (on-board (clear-square s1)
   (fill-square s2 piece)))

(define (fill-square s piece)
  (define curr (current)) 

  (reduce-all 
    into-chess-board
    (cons
     (chess-square-occupy s piece)
     (all-occupied-squares))))

(define (clear-square s)
  (define curr (current)) 

  (reduce-all 
    into-chess-board
    (filter-not 
      (curry square=? s)
      (all-occupied-squares))))

(define (square=? s1 s2)
  (equal?
    (square->square s1)
    (square->square s2)))


(module+ test
  (chess-board-pict
    (on-board start
              (clear-square f2)))

  (chess-board-pict
    (on-board start
              (fill-square f5 black-pawn)))


  (chess-board-pict
    (on-board start
              (move e2 e4)))) 
