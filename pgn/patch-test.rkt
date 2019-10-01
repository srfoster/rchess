#lang racket

(require chess chess/patch)
(require rebellion/base/result)

(define s starting-chess-board)

(define p
  (chess-patch
   #:placements
   (hash e3 white-knight)
   #:removals (hash a5 white-rook)	 
   #:captures (hash)	 
   #:obstruction-checks (set)	 
   #:safety-checks (hash)))


(chess-board-pict
 (success-value (chess-patch-apply s p)))