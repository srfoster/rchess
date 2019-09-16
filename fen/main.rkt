#lang racket

(provide fen->list)

(define (explode-number s)
  (if (string->number s)
    (map (const "_") (range (string->number s))) 
    s))

(define (upper? s)
  (member s '("K" "Q" "R" "B" "N" "P")))

(define (lower? s)
  (member s '("k" "q" "r" "b" "n" "p")))

(define (flip-case s)
  (cond
    [(upper? s) (string-downcase s)]
    [(lower? s) (string-upcase s)]
    [else s]))

(define (fen->pieces pieces)
  (define ranks (string-split pieces "/"))

  (map
    (compose string->symbol flip-case)
    (flatten
      (map
        explode-number
        (filter 
          (negate (curry string=? ""))
          (flatten
            (map (curryr string-split "") ranks)))))))

(define (fen->list s)
  
  (define parts
    (string-split s " ")) 

  (define pieces (fen->pieces (first parts)))

  pieces)

