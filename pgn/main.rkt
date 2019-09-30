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
  
  (foldl add-next-position 
    (list starting-chess-board)
    parsed-moves))

(define (parse-move m)
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

(define (parse-castle m)
  (parsed-move 'CASTLE!!! m #f #f))

(define (parse-result m)
  (parsed-move 'SOMEONE-WON!!! m #f #f))

(define (parse-normal m)
  (define square (get-square m))
  (define piece (get-piece m))
  
  (parsed-move 'normal m piece square))

(define (get-square m)
  (define match (regexp-match #px"[a-z][1-8]" m))
  (if match 
    (string->symbol (first match)) 
    #f))

(define (get-piece m)
  (define big-piece-match
    (regexp-match #px"([A-Z])[a-z]" m))
  (define piece 
    (if big-piece-match
      (string->symbol (second big-piece-match))
      'P))

  piece)



(define (add-next-position move positions)
  (define prev (last positions))
  (define next (next-position move prev))
  (append positions
          (list next)))


(define (next-position move p)
  (displayln move)
  (displayln (chess-board-pict p))
  
  p)









