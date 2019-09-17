#lang racket

(require chess
         rebellion/streaming/reducer
         pict)

(provide start-position
         image-chess)

(define (symbol->piece s)
  (cond
    [(eq? s 'K) black-king]
    [(eq? s 'Q) black-queen]
    [(eq? s 'R) black-rook]
    [(eq? s 'B) black-bishop]
    [(eq? s 'N) black-knight]
    [(eq? s 'P) black-pawn]
    [(eq? s 'k) white-king]
    [(eq? s 'q) white-queen]
    [(eq? s 'r) white-rook]
    [(eq? s 'b) white-bishop]
    [(eq? s 'n) white-knight]
    [(eq? s 'p) white-pawn]
    [else #f]))

(define (image-chess l)
  (define i 0)
  (define occupied-squares
    (filter identity
            (for*/list
                ([rank (reverse (stream->list (in-chess-ranks)))]
                 [file (in-chess-files)])
              (define s (list-ref l i))
              (set! i (add1 i))
              (define square (chess-square #:rank rank #:file file))
              (define piece (symbol->piece s))
              (if piece
                  (chess-square-occupy square piece)
                  #f))))
  
  (pict->bitmap
   (scale 
    (chess-board-pict
     (apply reduce into-chess-board occupied-squares))
    2)))


(define start-position
  '(R N B Q K B N R
      P P P P P P P P
      _ _ _ _ _ _ _ _
      _ _ _ _ _ _ _ _
      _ _ _ _ _ _ _ _ 
      _ _ _ _ _ _ _ _
      p p p p p p p p
      r n b q k b n r))

