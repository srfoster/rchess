#lang racket

(provide (except-out 
           (all-from-out racket) 
           #%module-begin)
         (rename-out 
           [my-module-begin #%module-begin]))

(require syntax/parse/define
         chess)

(define-syntax (my-module-begin stx)
  (syntax-parse stx
    [(_ exprs ...)
     #'(#%module-begin 
        (map chess-board-pict (convert-to-position-list '(exprs ...)))       
        )]))


(define (move-number? sym)
  (string->number (~a sym)))

(define (convert-to-position-list raw-moves)
  (define moves (filter-not move-number? raw-moves))

  (define parsed-moves (map (compose parse-move ~a) moves))

  (current-player #f) ;Restarting...
  
  (foldl add-next-position 
    (list starting-chess-board)
    parsed-moves))

(define current-player (make-parameter #f))

(define (switch-players!)
  (if (or 
        (not (current-player))
        (equal? black (current-player)))
    (current-player white)
    (current-player black)))

(define (parse-move m)
  (switch-players!)
  (define parse
    (match (move-type m)
      ['normal parse-normal]
      ['castle parse-castle]
      ['result parse-result]))
  
  (parse m))

(define (move-type m)
  (cond
    [(get-square m) 'normal]
    [(regexp-match #px"^O" m) 'castle]
    [(regexp-match #px"^[01]" m) 'result]))

(struct parsed-move (type original piece target) #:transparent)

(define (normal? m)
  (eq? 'normal (parsed-move-type m)))
(define (castle? m)
  (eq? 'castle (parsed-move-type m)))
(define (result? m)
  (eq? 'result (parsed-move-type m)))

(define (parse-castle m)
  (parsed-move 'castle m #f #f))

(define (parse-result m)
  (parsed-move 'result m #f #f))

(define (parse-normal m)
  (define square (get-square m))
  (define piece (get-piece m))
  
  (parsed-move 'normal m piece square))

(define (get-square m)
  (define match (regexp-match #px"[a-z][1-8]" m))
  (if match 
    (string->square (first match)) 
    #f))

(define (string->square s)
  (define f
    (string->symbol (substring s 0 1)))

  (define r
    (string->number (substring s 1 2)))

  (define fi
    (index-of '(a b c d e f g h) f eq?))  

  (define ri
    (index-of '(1 2 3 4 5 6 7 8) r eq?))

  (chess-square
    #:rank (chess-rank ri)
    #:file (chess-file fi)))

(define (get-piece m)
  (define big-piece-match
    (regexp-match #px"([A-Z])[a-z]" m))
  (define piece 
    (if big-piece-match
      (symbol->chess-piece (string->symbol (second big-piece-match)))
      (colored-chess-piece #:type pawn
                           #:owner (current-player))))

  piece)

(define (symbol->chess-piece s)
  (colored-chess-piece 
    #:type (match s
             ['Q queen]
             ['K king]
             ['B bishop]
             ['N knight]
             ['R rook])
    #:owner (current-player)))  


(define (add-next-position move positions)
  (define prev (last positions))
  (define next (next-position move prev))
  (append positions
          (list next)))


(define (next-position move p)
  (switch-players!)
  (cond
    [(normal? move)
     (make-normal-move move p)]
    [(castle? move)
     (on-board p
               (if (string=? "O-O" (parsed-move-original move))
                 (m:castle-kingside (current-player))
                 (m:castle-queenside (current-player))))    ]
    [(result? move)
     p]))


(require "../moves/main.rkt"
         chess/patch
         )


 (require rebellion/base/result
          (only-in pict pict->bitmap)
          2htdp/image

          (prefix-in m: "../moves/move.rkt")
          )

(define (make-normal-move move p)

  (define target (parsed-move-target move))
  (define piece  (parsed-move-piece move))    
  (define piece-type (colored-chess-piece-type piece))    


  (displayln move)

  (define source
    (first 
        (on-board p
           (which 
             (current-player)
             piece-type 
             #:to target))))

  (define source-square 
    (chess-square-remove-occupant 
      source))

  (define source-piece
    (occupied-chess-square-piece source))

  (on-board p
    (m:move source-square target)))



