#lang racket

(provide (all-from-out "./pgn/main.rkt"))
(require "./pgn/main.rkt")

(module reader syntax/module-reader
  rchess/pgn)
