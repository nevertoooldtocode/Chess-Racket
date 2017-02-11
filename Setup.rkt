#lang racket

(require "Mechanics.rkt")

(provide (all-defined-out))
(define FEN-startposition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(define STARTPOSITION (FEN->position FEN-startposition))

