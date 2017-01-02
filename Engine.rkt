#lang racket

(require "Mechanics.rkt")

(provide (all-defined-out))

(define (bestmove pos)
  (sleep 0.5)
  (random-ref (send pos legal-moves)))