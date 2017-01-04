#lang racket

(provide (all-defined-out))

(module+ test
  (require rackunit)
  (define e5 (new square% [col 5] [row 6]))
  (define e2 (new square% [col 5] [row 3]))
  (define e7 (new square% [col 5] [row 8]))
  (define dummysquare (make-object square% (sub1 FIRSTCOLUMN) (sub1 FIRSTROW)))
  (define EMPTYPOSITION (make-object position% '() 'white 'KQkq dummysquare 0 1))
  )

; General helper functions

(define (flatmap proc sequence)
  (foldr append '() (map proc sequence)))

(define (find-first predicate? proc sequence)
  (cond ((null? sequence) (proc sequence))
        ((predicate? (car sequence)) (proc (car sequence)))
        (else (find-first predicate? proc (cdr sequence)))))

(define (enumerate-interval m n)
  (if (> m n)
      '()
      (cons m (enumerate-interval (+ m 1) n))))

(define (random-ref sequence)
  (list-ref sequence (random (length sequence))))

(define (print-list sequence)
  (define (iter result remaining-elements)
    (if (null? remaining-elements)
        result
        (if (string=? result "")
            (iter (send (car remaining-elements) print)
                  (cdr remaining-elements))
            (iter (string-append result ", "
                                 (send (car remaining-elements) print))
                  (cdr remaining-elements)))))
  (iter "" sequence))

(module+ test
  (check-equal? (print-list (list e5 e5)) "e5, e5"))

; Constants

(define FIRSTROW 2)
(define LASTROW 9)
(define FIRSTCOLUMN 1)
(define LASTCOLUMN 8)


; Classes

(define square%
  (class object%
    (super-new)
    (init-field col row)
    
    (define ASCIICODE-A 96)
    (define ASCIICODE-1 47)
    
    (define/public (square-eq?  square)
      (and (= row (get-field row square))
           (= col (get-field col square))))
    
    (define/public (oob?) ; returns true if square is not on the playing area
      (or (> row LASTROW)
          (< row FIRSTROW)
          (< col FIRSTCOLUMN)
          (> col LASTCOLUMN)))
    
    (define/public (step direction) ; returns destination square
      (make-object square%
        (+ col (car direction))
        (+ row (cdr direction))))
    
    (define/public (print)
      (~a (integer->char (+ ASCIICODE-A col))
          (integer->char (+ ASCIICODE-1 row))))   
    ))


(define move%
  (class object%
    (super-new)
    
    (init-field source-square dest-square)
    
    (define/public (reaches-last-rank?)
      (let ((dest-row (get-field row dest-square)))
        (or (= dest-row FIRSTROW) (= dest-row LASTROW))))
    
    (define/public (move-eq? move)
      (and (send source-square square-eq? (get-field source-square move))
           (send dest-square square-eq? (get-field dest-square move))))
    
    (define/public (move-memq? move-list)
      (cond ((null? move-list) #f)
            ((move-eq? (car move-list)) #t)
            (else (move-memq? (cdr move-list)))))
    
    (define/public (print)
      (format "~a-~a"
              (send source-square print)
              (send dest-square print)))    
    ))

(define piece%
  (class object%
    (super-new)
    
    (init-field color square)
    
    (define/public (possible-moves position)
      (eprintf "possible-moves was directed to the piece% class instead of to ~a\n" (print))
      void)
    
    (define/public (possible-attacking-moves position)
      (possible-moves position))
    
    (define/public (make-copy new-square)
      (make-object this% color new-square))
    
    (define/public (ascii-representation)
      (eprintf "ascii-representation was directed to the piece% class instead of to ~a\n" (print))
      void)
    
    (define/public (print)
      (format "~a~a" (ascii-representation) (send square print)))
    ))

(define rook%
  (class piece%
    (super-new)
    
    (inherit-field color square)
    
    (define ROOKDIRECTIONS '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))    
    
    (define/override (possible-moves position)
      
      (define (squares-until-no-further source-square direction)
        (let ((dest-square (send source-square step direction)))
          (cond ((send dest-square oob?) '())
                ((send position friendly? dest-square) '())
                ((send position enemy? dest-square) (list dest-square))
                (else (cons dest-square
                            (squares-until-no-further dest-square direction))))))
      
      (map (lambda (destsquare) (make-object move% square destsquare))
           (flatmap
            (lambda (direction)
              (squares-until-no-further square direction))
            ROOKDIRECTIONS)))  
    
    (define/override (ascii-representation)
      (cond ((eq? color 'white) "R")
            ((eq? color 'black) "r")
            (else (error "ascii-representation -- unknown type"))))
    ))

(module+ test
  (define testrook (make-object rook% 'white e5))
  (check-equal?
   (print-list (send testrook possible-moves EMPTYPOSITION))
   "e5-f5, e5-g5, e5-h5, e5-d5, e5-c5, e5-b5, e5-a5, e5-e6, e5-e7, e5-e8, e5-e4, e5-e3, e5-e2, e5-e1")
  (check-equal?
   (print-list (send testrook possible-attacking-moves EMPTYPOSITION))
   "e5-f5, e5-g5, e5-h5, e5-d5, e5-c5, e5-b5, e5-a5, e5-e6, e5-e7, e5-e8, e5-e4, e5-e3, e5-e2, e5-e1")
  )

(define bishop%
  (class piece%
    (super-new)
    
    (inherit-field color square)
    
    (define BISHOPDIRECTIONS '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1)))  
    
    (define/override (possible-moves position)
      
      (define (squares-until-no-further source-square direction)
        (let ((dest-square (send source-square step direction)))
          (cond ((send dest-square oob?) '())
                ((send position friendly? dest-square) '())
                ((send position enemy? dest-square) (list dest-square))
                (else (cons dest-square
                            (squares-until-no-further dest-square direction))))))
      
      (map (lambda (destsquare) (make-object move% square destsquare))
           (flatmap
            (lambda (direction)
              (squares-until-no-further square direction))
            BISHOPDIRECTIONS)))  
    
    (define/override (ascii-representation)
      (cond ((eq? color 'white) "B")
            ((eq? color 'black) "b")
            (else (error "ascii-representation -- unknown type"))))
    ))

(module+ test
  (define testbishop (make-object bishop% 'white e5))
  (check-equal?
   (print-list (send testbishop possible-moves EMPTYPOSITION))
   "e5-f6, e5-g7, e5-h8, e5-f4, e5-g3, e5-h2, e5-d6, e5-c7, e5-b8, e5-d4, e5-c3, e5-b2, e5-a1")
  (check-equal?
   (print-list (send testbishop possible-attacking-moves EMPTYPOSITION))
   "e5-f6, e5-g7, e5-h8, e5-f4, e5-g3, e5-h2, e5-d6, e5-c7, e5-b8, e5-d4, e5-c3, e5-b2, e5-a1")
  )

(define queen%
  (class piece%
    (super-new)
    
    (inherit-field color square)
    
    (define QUEENDIRECTIONS '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1) (1 . 0) (-1 . 0) (0 . 1) (0 . -1)))    
    
    (define/override (possible-moves position)
      
      (define (squares-until-no-further source-square direction)
        (let ((dest-square (send source-square step direction)))
          (cond ((send dest-square oob?) '())
                ((send position friendly? dest-square) '())
                ((send position enemy? dest-square) (list dest-square))
                (else (cons dest-square
                            (squares-until-no-further dest-square direction))))))
      
      (map (lambda (destsquare) (make-object move% square destsquare))
           (flatmap
            (lambda (direction)
              (squares-until-no-further square direction))
            QUEENDIRECTIONS)))  
    
    (define/override (ascii-representation)
      (cond ((eq? color 'white) "Q")
            ((eq? color 'black) "q")
            (else (error "ascii-representation -- unknown type"))))
    ))

(module+ test
  (define testqueen (make-object queen% 'white e5))
  (check-equal?
   (print-list (send testqueen possible-moves EMPTYPOSITION))
   "e5-f6, e5-g7, e5-h8, e5-f4, e5-g3, e5-h2, e5-d6, e5-c7, e5-b8, e5-d4, e5-c3, e5-b2, e5-a1, e5-f5, e5-g5, e5-h5, e5-d5, e5-c5, e5-b5, e5-a5, e5-e6, e5-e7, e5-e8, e5-e4, e5-e3, e5-e2, e5-e1")
  (check-equal?
   (print-list (send testqueen possible-attacking-moves EMPTYPOSITION))
   "e5-f6, e5-g7, e5-h8, e5-f4, e5-g3, e5-h2, e5-d6, e5-c7, e5-b8, e5-d4, e5-c3, e5-b2, e5-a1, e5-f5, e5-g5, e5-h5, e5-d5, e5-c5, e5-b5, e5-a5, e5-e6, e5-e7, e5-e8, e5-e4, e5-e3, e5-e2, e5-e1")
  )

(define king%
  (class piece%
    (super-new)
    
    (inherit-field color square)
    
    (define KINGDIRECTIONS '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1) (1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
    
    (define/public (kingsteps)
      (map (lambda (direction) (send square step direction))
           KINGDIRECTIONS))
    
    (define/override (possible-moves position)
      (map (lambda (destsquare) (make-object move% square destsquare))
           (filter (lambda (filtersquare)
                     (and (not (send filtersquare oob?))
                          (not (send position friendly? filtersquare))))
                   (kingsteps))))  
    
    (define/override (ascii-representation)
      (cond ((eq? color 'white) "K")
            ((eq? color 'black) "k")
            (else (error "ascii-representation -- unknown type"))))
    ))

(module+ test
  (define testking (make-object king% 'white e5))
  (check-equal?
   (print-list (send testking possible-moves EMPTYPOSITION))
   "e5-f6, e5-f4, e5-d6, e5-d4, e5-f5, e5-d5, e5-e6, e5-e4")
  (check-equal?
   (print-list (send testking possible-attacking-moves EMPTYPOSITION))
   "e5-f6, e5-f4, e5-d6, e5-d4, e5-f5, e5-d5, e5-e6, e5-e4")
  )

(define knight%
  (class piece%
    (super-new)
    
    (inherit-field color square)
    
    (define KNIGHTDIRECTIONS '((1 . 2) (1 . -2) (-1 . 2) (-1 . -2) (2 . 1) (2 . -1) (-2 . 1) (-2 . -1)))
    
    (define/public (knightsteps)
      (map (lambda (direction) (send square step direction))
           KNIGHTDIRECTIONS))
    
    (define/override (possible-moves position)
      (map (lambda (destsquare) (make-object move% square destsquare))
           (filter (lambda (filtersquare)
                     (and (not (send filtersquare oob?))
                          (not (send position friendly? filtersquare))))
                   (knightsteps))))  
    
    (define/override (ascii-representation)
      (cond ((eq? color 'white) "N")
            ((eq? color 'black) "n")
            (else (error "ascii-representation -- unknown type"))))
    
    ))

(module+ test
  (define testknight (make-object knight% 'white e5))
  (check-equal?
   (print-list (send testknight possible-moves EMPTYPOSITION))
   "e5-f7, e5-f3, e5-d7, e5-d3, e5-g6, e5-g4, e5-c6, e5-c4")
  )

(define pawn%
  (class piece%
    (super-new)
    
    (inherit-field color square)
    
    (define DIRECTION (if (eq? color 'white) 1 -1))
    
    (define BEGINROWWHITEPAWN (+ FIRSTROW 1))
    (define BEGINROWBLACKPAWN (+ FIRSTROW 6))
    (define BEGINROWPAWN (if (eq? color 'white)
                             BEGINROWWHITEPAWN
                             BEGINROWBLACKPAWN))
    
    (define PAWNDIRECTION (cons 0 (* 1 DIRECTION)))
    (define PAWNDOUBLEDIRECTION (cons 0 (* 2 DIRECTION)))
    (define PAWNCAPTUREDIRECTIONS (list (cons 1 (* 1 DIRECTION)) (cons -1 (* 1 DIRECTION))))
    
    (define pawnstep (send square step PAWNDIRECTION))   
    (define pawndoublestep (send square step PAWNDOUBLEDIRECTION))   
    (define pawncapturesteps
      (map (lambda (direction) (send square step direction))
           PAWNCAPTUREDIRECTIONS))
    
    (define/override (possible-moves position)
      (map (lambda (destsquare) (make-object move% square destsquare))
           (append
            (if (send position free? pawnstep)
                (list pawnstep)
                '())
            (if (and (eq? (get-field row square) BEGINROWPAWN)
                     (send position free? pawnstep)
                     (send position free? pawndoublestep))
                (list pawndoublestep)
                '())                         
            (filter (lambda (filtersquare)
                      (and (not (send filtersquare oob?))
                           (send position enemy? filtersquare)))
                    pawncapturesteps))
           ))
    
    (define/override (possible-attacking-moves source-square)
      (map (lambda (destsquare) (make-object move% square destsquare))                    
           (filter (lambda (filtersquare)
                     (not (send filtersquare oob?)))
                   pawncapturesteps))
      ) 
    
    (define/override (ascii-representation)
      (cond ((eq? color 'white) "P")
            ((eq? color 'black) "P")
            (else (error "ascii-representation -- unknown type"))))    
    ))

(module+ test
  (define white-testpawn (make-object pawn% 'white e2))
  (check-equal?
   (print-list (send white-testpawn possible-moves EMPTYPOSITION))
   "e2-e3, e2-e4")
  (check-equal?
   (print-list (send white-testpawn possible-attacking-moves EMPTYPOSITION))
   "e2-f3, e2-d3")
  (define black-testpawn (make-object pawn% 'black e7))
  (check-equal?
   (print-list (send black-testpawn possible-moves EMPTYPOSITION))
   "e7-e6, e7-e5")
  (check-equal?
   (print-list (send black-testpawn possible-attacking-moves EMPTYPOSITION))
   "e7-f6, e7-d6")
  )

(define position%
  (class object%
    (super-new)
    
    (init-field piece-list colortomove castling-ability ep-target-square halfmove-clock fullmove-counter)
    
    (define/public (white-pieces)
      (filter (lambda (piece) (eq? (get-field color piece) 'white))
              piece-list))
    
    (define/public (black-pieces)
      (filter (lambda (piece) (eq? (get-field color piece) 'black))
              piece-list))
    
    (define (friendly-pieces)
      (filter (lambda (piece) (eq? (get-field color piece) colortomove))
              piece-list))
    
    (define/public (enemy-pieces)
      (filter (lambda (piece) (not (eq? (get-field color piece) colortomove)))
              piece-list))
    
    (define/public (occupied? square)   ; returns the piece on the square, or #f if free
      (find-first
       (lambda (piece) (send square square-eq? (get-field square piece)))
       (lambda (piece) (if (null? piece) #f piece))
       piece-list))
    
    (define/public (free? square)
      (not (occupied? square)))
    
    (define/public (friendly? square)
      (find-first
       (lambda (piece) (send square square-eq? (get-field square piece)))
       (lambda (piece) (if (null? piece) #f piece))
       (friendly-pieces)))
    
    (define/public (enemy? square)
      (find-first
       (lambda (piece) (send square square-eq? (get-field square piece)))
       (lambda (piece) (if (null? piece) #f piece))
       (enemy-pieces)))
    
    (define (switch-colortomove)
      (if (eq? colortomove 'white)
          'black
          'white))
    
    (define/public (changed-pos move); returns a position after move is made
      (let ((move-source-square (get-field source-square move))
            (move-dest-square (get-field dest-square move)))
        
        (define (changepiece piece)
          (cond ((send move-source-square square-eq? (get-field square piece))
                 (if (and (is-a? piece pawn%) (send move reaches-last-rank?))
                     (make-object queen% (get-field color piece) move-dest-square)
                     (send piece make-copy move-dest-square)))
                (else piece)))
        
        (define (notcaptured? piece)
          (not (send move-dest-square square-eq? (get-field square piece))))
        
        (make-object position%
          (map changepiece (filter notcaptured? piece-list))
          (switch-colortomove)
          castling-ability
          ep-target-square
          halfmove-clock
          fullmove-counter
          )))
    
    (define/public (possible-moves)
      (flatmap (lambda (piece)
                 (send piece possible-moves this))
               (friendly-pieces)))
    
    (define/public (possible-attacking-moves)
      (flatmap (lambda (piece)
                 (send piece possible-attacking-moves this))
               (friendly-pieces)))
    
    (define/public (legal-moves)
      (filter (lambda (move)
                (not (send (changed-pos move) giving-check?)))
              (possible-moves)))
    
    (define/public (legal-move? move)
      (send move move-memq? (legal-moves)))
    
    (define/public (giving-check?)
      (attacking? (find-first
                   (lambda (piece) (is-a? piece king%))
                   (lambda (piece) (get-field square piece))
                   (enemy-pieces))))
    
    (define (attacking? square)
      (find-first (lambda (move) (send square square-eq? (get-field dest-square move)))
                  (lambda (move) (if (null? move) #f #t))
                  (possible-attacking-moves)))
    
    (define (under-attack? square)
      (send (make-object position%
              piece-list
              (switch-colortomove)
              castling-ability
              ep-target-square
              halfmove-clock
              fullmove-counter)
            attacking? square))
    
    (define/public (endofgame?)
      #f)
    
    (define/public (print)
      (format "~a, ~a to move" (print-list piece-list) (~a colortomove)))    
    ))



