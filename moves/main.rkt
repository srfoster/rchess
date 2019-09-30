#lang racket

(require chess)


;TODO: Change (ray ...) to stop when it hits something 
;      Take params - normal: end point inclusive if opposite color, exclusive if same color
;                    pawn: exclusive regardless of color


;TODO: Add optional arguments later for other board state that is known when you are in the context of an actual game -- e.g. castling, en passant. 
; For now, we'll just do the stuff that doesn't require context other than the board itself, and the current player to move (implied by whatever color piece is on the from square).
(define/contract (can-move? from to on)
  (-> chess-square? chess-square? chess-board? boolean?)
  (parameterize ([current on]
                 ;Add params here for other board state: Castling state, en passant opportunities, etc.
                 )
    (define from-piece (square->piece from))
    (cond 
      [(not from-piece) #f]
      [else (not (empty? (try-move from to)))])))

(define current (make-parameter #f))

(define (square->piece s)
  (chess-board-ref (current) s))

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

  (member to (valid-squares from-square)))

(define (color-of p)
  (colored-chess-piece-owner p))

(define (valid-squares from-square)
  (define curr (current))

  (define p (occupied-chess-square-piece from-square))

  (define empty-from-square (chess-square-remove-occupant from-square))

  (match (colored-chess-piece-type p)
    [pawn (pawn-squares empty-from-square)]
    [knight (knight-squares empty-from-square)]
    [bishop (bishop-squares empty-from-square)]
    [rook (rook-squares empty-from-square)]
    [king (king-squares empty-from-square)]
    [queen (queen-squares empty-from-square)]))

;List of squares in order from from-square to to-square, inclusive of to-square
; Returns empty if they do not share a file, rank, diagonal
(define (ray from-square to-square)
  (cond
    [(same-file? from-square to-square) (file-ray from-square to-square)]
    [(same-rank? from-square to-square) (rank-ray from-square to-square)]
    [(same-diag? from-square to-square) (diag-ray from-square to-square)]
    [else '()]))

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


;Express below in terms of "rays" (except knight... hardcode)

(define (pawn-squares s)
  ;One in color dir if not occupied
  ;On home row?
  ;  Two in color dir, if not occupied and can reach on file 
  ;diag-left in color dir if occupied by opposite color
  ;diag-right in color dir if occupied by opposite color
  (define p (chess-board-ref (current) s))

  (define squares
    (if (equal? white (colored-chess-piece-owner p))
      (colored-pawn-squares s 1 north-end add1)
      (colored-pawn-squares s 6 south-end sub1)))
  
  squares)

(define (colored-pawn-squares s home-rank file-end forward-dir)
  (define rank-index (chess-rank-index (chess-square-rank s)))  
  (define file-index (chess-file-index (chess-square-file s)))  

  (define dist
    (if (= home-rank rank-index)
      2 1))

  (displayln dist)

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
      #:rank (sub2 s-rank)))

  (define up-right-right
    (maybe-chess-square 
      #:file (add2 s-file)
      #:rank (sub1 s-rank)))

  (define down-right-right
    (maybe-chess-square 
      #:file (add2 s-file)
      #:rank (add1 s-rank)))

  (define down-down-right
    (maybe-chess-square 
      #:file (add1 s-file)
      #:rank (add2 s-rank)))

  (define down-down-left
    (maybe-chess-square 
      #:file (sub1 s-file)
      #:rank (add2 s-rank)))

  (define down-left-left
    (maybe-chess-square 
      #:file (sub2 s-file)
      #:rank (add1 s-rank)))

  (define up-left-left
    (maybe-chess-square 
      #:file (sub2 s-file)
      #:rank (sub1 s-rank)))

  (define up-up-left
    (maybe-chess-square 
      #:file (sub1 s-file)
      #:rank (sub2 s-rank)))

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

(define (king-squares s)
  (append
    (take (ray s (north-end s)) 1)
    (take (ray s (east-end s)) 1)
    (take (ray s (south-end s)) 1)
    (take (ray s (west-end s)))))

(define (queen-squares s)
  (append (rook-squares s)
          (bishop-squares s)))



(module+ test
  (require rackunit)
  (define start starting-chess-board)


  (check-true
    (can-move? e2 e3 start)
    "e3 is valid")

  (check-true
    (can-move? g1 f3 start)
    "Nf3 is valid")


  (check-false
    (can-move? a1 b1 start)
    "Can't move a piece onto its own color")

  (check-false
    (can-move? e3 e5 start)
    "Can't move from a square with no pieces")

  (check-false
    (can-move? e2 e5 start)
    "Can't move a pawn more than two squares")

  (check-false
    (can-move? a1 a3 start)
    "Can't move a Rook through other pieces")

  ;TODO: Tests....

  #;
  (displayln
    (ray a1 a8))

  #;
  (displayln
    (ray a1 h8))

  #;
  (displayln
    (ray a1 h7))

  #;
  (displayln
    (ray a1 h1))


  ;OBO
  (displayln
    (ray e4 e1))

  (displayln
    (ray e4 e8))

  (displayln
    (ray e4 h4))

  (displayln
    (ray e4 a4))

  (displayln
    (ray e4 h7))

  (displayln
    (ray e4 b1))

  (displayln
    (parameterize ([current empty-chess-board])
      (rook-squares e4)))

  (displayln
    (parameterize ([current empty-chess-board])
      (knight-squares e4)))

  (displayln
    (parameterize ([current empty-chess-board])
      (knight-squares h5)))

  (displayln
    (parameterize ([current starting-chess-board])
      (pawn-squares e2)) )

  (displayln
    (parameterize ([current starting-chess-board])
      (pawn-squares e7)) )


  (define test-board-1
    (chess-board e4 white-pawn
                 e5 black-pawn
                 f5 black-knight))

  (displayln
    (parameterize ([current test-board-1])
      (pawn-squares e4)))

  )
