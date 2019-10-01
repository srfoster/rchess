#lang racket

(provide on-board
         can-move?
         which)

(require chess 
         "./util.rkt"
         "./all-squares.rkt"
         "./current-board.rkt"
         )


;TODO: Add optional arguments later for other board state that is known when you are in the context of an actual game -- e.g. castling, en passant. 
; For now, we'll just do the stuff that doesn't require context other than the board itself, and the current player to move (implied by whatever color piece is on the from square).
(define (can-move? from to)
  (define from-piece (square->piece from))
  (cond 
    [(not from-piece) #f]
    [else (not (not (try-move from to)))]) )


(define (which color-of-piece
               type-of-piece  
               #:to to)

  (define squares          (all-squares))
  (define incoming-squares (filter (curryr can-move? to) squares))   

  (filter
    (lambda (s) 
      (and
        (equal? color-of-piece 
                (colored-chess-piece-owner 
                  (square->piece s)))  
        (equal? type-of-piece 
                (colored-chess-piece-type 
                  (square->piece s)))))
    (map 
      (curry chess-board-ref-square (current))
      incoming-squares)))

(define (same-colors? a b)
  (define piece-a (square->piece a)) 
  (define piece-b (square->piece b)) 

  (and piece-a
       piece-b
       (equal? (colored-chess-piece-owner piece-a)
               (colored-chess-piece-owner piece-b))))

;Returns false if the movement does not succceed
;Otherwise, returns the new chessboard -- updated from (current)
(define (try-move from to)
  (define curr (current)) 
  (define from-square (chess-board-ref-square curr from))
  (define possible (valid-squares from-square))

  (member to possible))

(define (color-of p)
  (colored-chess-piece-owner p))

(define (valid-squares from-square)
  (define curr (current))

  (define p (occupied-chess-square-piece from-square))

  (define empty-from-square (square->square from-square))

  (define t (colored-chess-piece-type p)) 

  (cond
    [(eq? pawn t) (pawn-squares empty-from-square)]
    [(eq? knight t) (knight-squares empty-from-square)]
    [(eq? bishop t) (bishop-squares empty-from-square)]
    [(eq? rook t) (rook-squares empty-from-square)]
    [(eq? king t) (king-squares empty-from-square)]
    [(eq? queen t) (queen-squares empty-from-square)]))

;List of squares in order from from-square to to-square, inclusive of to-square
; Returns empty if they do not share a file, rank, diagonal
;Is chopped short if it hits a piece
(define (ray from-square to-square)
  (chop
    from-square
    (cond
      [(same-file? from-square to-square) (file-ray from-square to-square)]
      [(same-rank? from-square to-square) (rank-ray from-square to-square)]
      [(same-diag? from-square to-square) (diag-ray from-square to-square)]
      [else '()])))

(define (chop from-square r)
  (define (obstruction? s)
    (chess-board-ref (current) s))

  (define first-obstruction
    (index-where r obstruction?))

  

  (cond
    [(not first-obstruction) r] 
    [(same-colors? 
       (list-ref r first-obstruction)
       from-square)
     (take r first-obstruction) ]
    [else 
      (take r (add1 first-obstruction))])

  )

;from a to b, not including a
;  descending or not, depending on whether a is bigger than b or not.
(define (between a b)
  (define dir (if (< a b) 1 -1))
  (stream->list 
    (in-range (+ a dir) (+ dir b) 
              dir)))

(define (same-rank? f t)
  (equal? 
    (chess-rank-index (chess-square-rank f))  
    (chess-rank-index (chess-square-rank t))))

(define (same-file? f t)
  (equal? 
    (chess-file-index (chess-square-file f))  
    (chess-file-index (chess-square-file t))))

(define (same-diag? f t)
  (define f-rank (chess-rank-index (chess-square-rank f)))   
  (define t-rank (chess-rank-index (chess-square-rank t)))   
  (define f-file (chess-file-index (chess-square-file f)))   
  (define t-file (chess-file-index (chess-square-file t)))   
  (= (abs (- f-rank t-rank))
     (abs (- f-file t-file))))

(define (file-ray f t)
  (define f-rank (chess-rank-index (chess-square-rank f)))  
  (define t-rank (chess-rank-index (chess-square-rank t)))  
  (map (lambda (rank) 
         (chess-square #:rank (chess-rank rank) 
                       #:file (chess-square-file f))) 
       (stream->list (between f-rank t-rank))))

(define (rank-ray f t)
  (define f-file (chess-file-index (chess-square-file f)))  
  (define t-file (chess-file-index (chess-square-file t)))  
  (map 
    (lambda (file) 
      (chess-square #:file (chess-file file)
                    #:rank (chess-square-rank f)))
    (stream->list (between f-file t-file))))

(define (diag-ray f t)
  (define f-rank (chess-rank-index (chess-square-rank f)))  
  (define t-rank (chess-rank-index (chess-square-rank t)))  
  (define f-file (chess-file-index (chess-square-file f)))  
  (define t-file (chess-file-index (chess-square-file t)))  

  (for/list ([rank (between f-rank t-rank)]
             [file (between f-file t-file)]) 
    (chess-square #:rank (chess-rank rank) 
                  #:file (chess-file file))))

(define (north-end s)
  (chess-square
    #:rank (chess-rank 7)
    #:file (chess-square-file s)))

(define (south-end s)
  (chess-square
    #:rank (chess-rank 0)
    #:file (chess-square-file s)))

(define (east-end s)
  (chess-square
    #:file (chess-file 7)
    #:rank (chess-square-rank s)))

(define (west-end s)
  (chess-square
    #:file (chess-file 0)
    #:rank (chess-square-rank s)))


(define (seven? n)
  (= 7 n))

(define (diag-until-end s rank-adj file-adj)
  (define s-rank (chess-rank-index (chess-square-rank s)))  
  (define s-file (chess-file-index (chess-square-file s)))  

  (define (on-edge x y)
    (or (zero? x)
        (zero? y)
        (seven? x)
        (seven? y)))

  (if (on-edge s-rank s-file)
    s
    (north-east-end (chess-square
                      #:rank (rank-adj s-rank)
                      #:file (file-adj s-file)))))

(define (north-east-end s)
  (diag-until-end s add1 add1))

(define (south-east-end s)
  (diag-until-end s add1 sub1))

(define (south-west-end s)
  (diag-until-end s sub1 sub1))

(define (north-west-end s)
  (diag-until-end s sub1 add1))


(define (pawn-squares s)
  (define p (chess-board-ref (current) s))


  (define squares
    (cond 
      ;Unlike other pieces, we actually need to know what kind of pawn is here 
      [(or (not p) 
           (not (eq? pawn (colored-chess-piece-type p))))
       '()] 
      [(equal? white (colored-chess-piece-owner p))
       (colored-pawn-squares s 1 north-end add1)]
      [else (colored-pawn-squares s 6 south-end sub1)]))
  
  squares)

;Just a helper...
(define (colored-pawn-squares s home-rank file-end forward-dir)
  (define rank-index (chess-rank-index (chess-square-rank s)))  
  (define file-index (chess-file-index (chess-square-file s)))  

  (define dist
    (if (= home-rank rank-index)
      2 1))

  (define forward-squares
    (take (ray s (file-end s))
          dist)) 

  (when 
    (and
      (= 2 (length forward-squares))
      (occupied-chess-square? 
        (chess-board-ref-square 
          (current)
          (second forward-squares))))
    (set! forward-squares (list (first forward-squares))))

  (when (occupied-chess-square? 
        (chess-board-ref-square 
          (current)
          (first forward-squares)))
    (set! forward-squares '()))

  (define capture-right-square
    (maybe-chess-square 
      #:file (add1 file-index) 
      #:rank (forward-dir rank-index)))

  (define capture-left-square
    (maybe-chess-square 
      #:file (sub1 file-index) 
      #:rank (forward-dir rank-index)))

  (define capture-right-piece
    (and capture-right-square 
         (chess-board-ref (current) capture-right-square)))

  (define capture-left-piece
    (and capture-left-square
         (chess-board-ref (current) capture-left-square)))

  (define capture-squares
    (list
      (and capture-right-piece capture-right-square)       
      (and capture-left-piece capture-left-square)))

  (filter identity
          (append
            forward-squares
            capture-squares)))


(define (maybe-chess-square #:rank r #:file f)
  (with-handlers ([exn:fail? (thunk* #f)])
    (chess-square #:rank (chess-rank r) 
                  #:file (chess-file f))))

(define (knight-squares s)


  (define add2 (compose add1 add1))
  (define sub2 (compose sub1 sub1))
  (define s-rank (chess-rank-index (chess-square-rank s)))  
  (define s-file (chess-file-index (chess-square-file s)))  

  (define up-up-right 
    (maybe-chess-square 
      #:file (add1 s-file)
      #:rank (add2 s-rank)))

  (define up-right-right
    (maybe-chess-square 
      #:file (add2 s-file)
      #:rank (add1 s-rank)))

  (define down-right-right
    (maybe-chess-square 
      #:file (add2 s-file)
      #:rank (sub1 s-rank)))

  (define down-down-right
    (maybe-chess-square 
      #:file (add1 s-file)
      #:rank (sub2 s-rank)))

  (define down-down-left
    (maybe-chess-square 
      #:file (sub1 s-file)
      #:rank (sub2 s-rank)))

  (define down-left-left
    (maybe-chess-square 
      #:file (sub2 s-file)
      #:rank (sub1 s-rank)))

  (define up-left-left
    (maybe-chess-square 
      #:file (sub2 s-file)
      #:rank (add1 s-rank)))

  (define up-up-left
    (maybe-chess-square 
      #:file (sub1 s-file)
      #:rank (add2 s-rank)))

  (filter identity
          (list
            up-up-right
            up-right-right
            down-right-right
            down-down-right
            down-down-left
            down-left-left
            up-left-left
            up-up-left)))

(define (bishop-squares s)
  ;Along each diagonal until opposite color (inclusive) or same color (exclusive)
  (append
    (ray s (north-east-end s))
    (ray s (south-east-end s))
    (ray s (south-west-end s))
    (ray s (north-west-end s))))

(define (rook-squares s)
  ;Along each file until opposite color (inclusive) or same color (exclusive)
  (append
    (ray s (north-end s))
    (ray s (east-end s))
    (ray s (south-end s))
    (ray s (west-end s))))

(define (safe-take l n)
  (if (<= (length l) n)
    l
    (take l n)))

(define (king-squares s)
  (append
    (safe-take (ray s (north-end s)) 1)
    (safe-take (ray s (east-end s)) 1)
    (safe-take (ray s (south-end s)) 1)
    (safe-take (ray s (west-end s)) 1)))

(define (queen-squares s)
  (append (rook-squares s)
          (bishop-squares s)))



(module+ test
  (require rackunit)
  (define start starting-chess-board)
  (define empty empty-chess-board))


(module+ test
  (check-true
    (on-board start
       (can-move? g1 f3))
    "Nf3 is valid"))

(module+ test
  (check-equal?
    (on-board
      (chess-board a5 black-pawn)
      (ray a1 a8))
    (list a2 a3 a4 a5))

  (check-equal?
    (on-board
      (chess-board g7 black-pawn)
      (ray a1 h8))
    (list b2 c3 d4 e5 f6 g7)))

(module+ test

  (define test-board-1
    (chess-board e4 white-pawn
                 e5 black-pawn
                 f5 black-knight
                 h7 black-pawn
                 a3 white-pawn
                 c4 white-pawn))

  (check-true
    (on-board start
      (can-move? e2 e3))
    "e3 is valid")

  (check-true
    (on-board start
      (can-move? g1 f3))
    "Nf3 is valid")


  (check-false
    (on-board start
      (can-move? a1 b1))
    "Can't move a piece onto its own color")

  (check-false
    (on-board start
      (can-move? e3 e5))
    "Can't move from a square with no pieces")

  (check-false
    (on-board start
      (can-move? e2 e5))
    "Can't move a pawn more than two squares")

  (check-pred
    (lambda (squares)
      (not (member e6 squares )))
    (on-board (chess-board e4 white-rook
                           e6 white-knight)
      (rook-squares e4))
    "Should not be able to move onto your own pieces")

  (check-false
    (on-board start
      (can-move? a1 a3))
    "Can't move a Rook through other pieces")


  (check-equal?
    (on-board empty-chess-board
      (rook-squares e4))
    (list e5 e6 e7 e8 f4 g4 h4 e3 e2 e1 d4 c4 b4 a4))


  (check-equal?
    (on-board empty-chess-board
      (knight-squares e4))
    (list f6 g5 g3 f2 d2 c3 c5 d6))

  (check-equal?
    (on-board empty-chess-board
      (knight-squares h5))
    (list g3 f4 f6 g7))

  (check-equal?
    (on-board starting-chess-board
      (pawn-squares e2))
    (list e3 e4))

  (check-equal?
    (on-board starting-chess-board
      (pawn-squares e7))
    (list e6 e5))



  (check-equal?
    (on-board test-board-1
              (pawn-squares e4))
 
    (list f5))

  (check-equal?
    (on-board test-board-1
              (pawn-squares e5))
 
    (list ))

  (check-equal?
    (on-board test-board-1
              (pawn-squares h7))
 
    (list h6 h5))

  (check-equal?
    (on-board test-board-1
              (pawn-squares a3))
 
    (list a4)))

(module+ test
  (check-equal?
    (length (on-board starting-chess-board
                      (which white knight #:to f3)))
    1))


(module+ test
  (check-equal?
    (length (on-board starting-chess-board
                      (which white pawn #:to e4)))
    1))



