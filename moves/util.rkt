#lang racket

(provide square->piece
         square->square
         (rename-out [starting-chess-board start]))

(require chess
         "./current-board.rkt")

(define (square->piece s)
  (if (occupied-chess-square? s)  
    (occupied-chess-square-piece s) 
    (chess-board-ref (current) s)))

(define/contract (square->square s)
  (-> (or/c chess-square? occupied-chess-square?) chess-square?)

  (if (occupied-chess-square? s)  
    (chess-square-remove-occupant s) 
    s))
