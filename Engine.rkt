#lang racket

(provide (all-defined-out))

(define (random-ref sequence)
  (list-ref sequence (random (length sequence))))

(define engine%
  (class object%
    (super-new)

    (define/public (get-move pos)
      (sleep 0.5)
      (random-ref (send pos legal-moves)))
    ))