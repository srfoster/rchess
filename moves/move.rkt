#lang racket

(provide move
         castle-kingside
         castle-queenside
         moves)

(require chess 
         pict
         "./util.rkt"
         "./all-squares.rkt"
         "./current-board.rkt")

(require rebellion/streaming/reducer)

(define-syntax-rule (moves [from to] ...)
  (foldl 
    (lambda (pair board)
      (on-board board
                (apply move pair)))
    (current) 
    (list 
      (list from to)
      ...)))

(define (move s1 s2)
  (define piece (chess-board-ref (current) s1))

  (on-board (clear-square s2)
    (on-board (clear-square s1)
      (fill-square s2 piece))))

(define (castle-kingside color)
  (if (eq? white color)
    (moves 
      [e1 g1]
      [h1 f1])
    (moves 
      [e8 g8]
      [h8 f8])))

(define (castle-queenside color)
  (if (eq? white color)
    (moves 
      [e1 c1]
      [a1 d1])
    (moves 
      [e8 c8]
      [a8 d8])))

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
              (move e2 e4)))
  
  (chess-board-pict
    (on-board start
              (moves 
                [e2 e4]
                [e7 e5]
                [f1 c4] 
                [b8 c6] 
                [c4 f7])))

  (define before-castle
     (on-board start
              (moves 
                [e2 e4]
                [e7 e5]
                [f1 c4] 
                [b8 c6]
                [g1 g3]
                [g8 f6])))

  
  (chess-board-pict
   (on-board before-castle
             (castle-kingside white))
   )

  ) 
