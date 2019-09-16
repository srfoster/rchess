#lang racket

(require "../main.rkt" 
         rackunit
         (only-in 2htdp/image image?))

;Turn lists into images
(check-pred image?
            (image-chess 
              (shuffle start-position)))   

;Turn FENs into images,
; + fetch FENs from chessblunders.com
(check-pred image?
            (chessblunder->image 
              (random-chessblunder)))
