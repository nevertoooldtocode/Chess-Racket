#lang racket

(provide (all-defined-out))

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
  (define (iter remaining-elements)
    (if (null? remaining-elements)
        (void)    
        (begin (printf (send (car remaining-elements) print))
               (if (not (null? (cdr remaining-elements)))
                   (printf ", ")
                   (void))
               (iter (cdr remaining-elements)))))
  (iter sequence))

; Constants

(define BISHOPDIRECTIONS '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1)))
(define ROOKDIRECTIONS '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
(define QUEENDIRECTIONS (append BISHOPDIRECTIONS ROOKDIRECTIONS))
(define KINGDIRECTIONS QUEENDIRECTIONS)
(define KNIGHTDIRECTIONS '((1 . 2) (1 . -2) (-1 . 2) (-1 . -2) (2 . 1) (2 . -1) (-2 . 1) (-2 . -1)))
(define PAWNDIRECTION '(0 . 1))
(define PAWNDOUBLEDIRECTION '(0 . 2))
(define WHITEPAWNDIRECTION '(0 . 1))
(define WHITEPAWNDOUBLEDIRECTION '(0 . 2))
(define WHITEPAWNCAPTUREDIRECTIONS '((1 . 1) (-1 . 1)))
(define BLACKPAWNDIRECTION '(0 . -1))
(define BLACKPAWNDOUBLEDIRECTION '(0 . -2))
(define BLACKPAWNCAPTUREDIRECTIONS '((1 . -1) (-1 . -1)))
(define BEGINROWWHITEPAWN 3)
(define BEGINROWBLACKPAWN 8)

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
      (or (> row 9)
          (< row 2)
          (< col 1)
          (> col 8)))
    
    (define/public (step direction) ; returns destination square
      (make-object square%
        (+ col (car direction))
        (+ row (cdr direction))))
    
    (define/public (kingsteps)
      (map (lambda (direction) (step direction))
           KINGDIRECTIONS))
    
    (define/public (knightsteps)
      (map (lambda (direction) (step direction))
           KNIGHTDIRECTIONS))
    
    (define/public (whitepawnstep)
      (step WHITEPAWNDIRECTION))
    
    (define/public (blackpawnstep)
      (step BLACKPAWNDIRECTION))
    
    (define/public (whitepawndoublestep)
      (step WHITEPAWNDOUBLEDIRECTION))
    
    (define/public (blackpawndoublestep)
      (step BLACKPAWNDOUBLEDIRECTION))
    
    (define/public (whitepawncapturesteps)
      (map (lambda (direction) (step direction))
           WHITEPAWNCAPTUREDIRECTIONS))
    
    (define/public (blackpawncapturesteps)
      (map (lambda (direction) (step direction))
           BLACKPAWNCAPTUREDIRECTIONS))
    
    (define/public (string-representation)
      (~a (integer->char (+ ASCIICODE-A col))
          (integer->char (+ ASCIICODE-1 row))))
    
    (define/public (print) (send this string-representation))
    ))


(define move%
  (class object%
    (super-new)
    
    (init-field source-square dest-square)
    
    (define WHITELASTROW 9)
    (define BLACKLASTROW 2)
    
    (define/public (reaches-last-rank?)
      (let ((dest-row (get-field row dest-square)))
        (or (= dest-row WHITELASTROW) (= dest-row BLACKLASTROW))))
    
    (define/public (move-eq? move)
      (and (send source-square square-eq? (get-field source-square move))
           (send dest-square square-eq? (get-field dest-square move))))
    
    (define/public (move-memq? move-list)
      (cond ((null? move-list) #f)
            ((move-eq? (car move-list)) #t)
            (else (move-memq? (cdr move-list)))))
    
    (define/public (string-representation)
      (format "~a-~a"
              (send source-square string-representation)
              (send dest-square string-representation)))
    
    (define/public (print)
      (send this string-representation))
    ))


(define piece%
  (class object%
    (super-new)
    
    (init-field type color square)
    
    (define/public (ascii-representation)
      (cond ((eq? color 'white)
             (cond ((eq? type 'pawn) "p")
                   ((eq? type 'knight) "n")
                   ((eq? type 'bishop) "b")
                   ((eq? type 'rook) "r")
                   ((eq? type 'queen) "q")
                   ((eq? type 'king) "k")
                   (else (error "ascii-representation -- unknown type"))))
            ((eq? color 'black)
             (cond ((eq? type 'pawn) "P")
                   ((eq? type 'knight) "N")
                   ((eq? type 'bishop) "B")
                   ((eq? type 'rook) "R")
                   ((eq? type 'queen) "Q")
                   ((eq? type 'king) "K")
                   (else (error "ascii-representation -- unknown type"))))
            (else (error "ascii-representation -- unknown color"))))
    
    (define/public (print)
      (format "~a~a" (send this ascii-representation) (send square print)))
    ))

(define position%
  (class object%
    (super-new)
    
    (init-field pieces colortomove)
    
    (define/public (white-pieces)
      (filter (lambda (piece) (eq? (get-field color piece) 'white))
              pieces))
    
    (define/public (black-pieces)
      (filter (lambda (piece) (eq? (get-field color piece) 'black))
              pieces))
    
    (define (friendly-pieces)
      (filter (lambda (piece) (eq? (get-field color piece) colortomove))
              pieces))
    
    (define (enemy-pieces)
      (filter (lambda (piece) (not (eq? (get-field color piece) colortomove)))
              pieces))
    
    (define/public (occupied? square)   ; returns the piece on the square, or #f if free
      (find-first
       (lambda (piece) (send square square-eq? (get-field square piece)))
       (lambda (piece) (if (null? piece) #f piece))
       pieces))
    
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
    
    (define (squares-until-no-further source-square direction)
      (let ((dest-square (send source-square step direction)))
        (cond ((send dest-square oob?) '())
              ((friendly? dest-square) '())
              ((enemy? dest-square) (list dest-square))
              (else (cons dest-square
                          (squares-until-no-further dest-square direction))))))
    
    (define (qbr-moves source-square directionlist)
      (map (lambda (destsquare) (make-object move% source-square destsquare))
           (flatmap
            (lambda (direction)
              (squares-until-no-further source-square direction))
            directionlist)))
    
    (define (knightmoves source-square)
      (map (lambda (destsquare) (make-object move% source-square destsquare))
           (filter (lambda (filtersquare)
                     (and (not (send filtersquare oob?))
                          (not (friendly? filtersquare))))
                   (send source-square knightsteps))))
    
    (define (kingmoves source-square)
      (map (lambda (destsquare) (make-object move% source-square destsquare))
           (filter (lambda (filtersquare)
                     (and (not (send filtersquare oob?))
                          (not (friendly? filtersquare))))
                   (send source-square kingsteps))))
    
    (define (pawnmoves source-square)
      (let ((pawnstep (if (eq? colortomove 'white)
                          (send source-square whitepawnstep)
                          (send source-square blackpawnstep)))
            (pawndoublestep (if (eq? colortomove 'white)
                                (send source-square whitepawndoublestep)
                                (send source-square blackpawndoublestep)))
            (pawncapturesteps (if (eq? colortomove 'white)
                                  (send source-square whitepawncapturesteps)
                                  (send source-square blackpawncapturesteps)))
            (BEGINROWPAWN (if (eq? colortomove 'white)
                              BEGINROWWHITEPAWN
                              BEGINROWBLACKPAWN)))
        (map (lambda (destsquare) (make-object move% source-square destsquare))
             (append
              (if (free? pawnstep)
                  (list pawnstep)
                  '())
              (if (and (eq? (get-field row source-square) BEGINROWPAWN)
                       (free? pawnstep)
                       (free? pawndoublestep))
                  (list pawndoublestep)
                  '())                         
              (filter (lambda (filtersquare)
                        (and (not (send filtersquare oob?))
                             (enemy? filtersquare)))
                      pawncapturesteps))
             )))
    
    (define (pawn-attacking-moves source-square)
      (let* ((pawncapturesteps (if (eq? colortomove 'white)
                                   (send source-square whitepawncapturesteps)
                                   (send source-square blackpawncapturesteps))))
        (map (lambda (destsquare) (make-object move% source-square destsquare))
             (append                      
              (filter (lambda (filtersquare)
                        (not (send filtersquare oob?)))
                      pawncapturesteps))
             )))
    
    (define (switch-colortomove)
      (if (eq? colortomove 'white)
          'black
          'white))
    
    (define/public (changed-pos move); returns a position after move is made
      (let ((move-source-square (get-field source-square move))
            (move-dest-square (get-field dest-square move)))
        
        (define (changepiece piece)
          (cond ((send move-source-square square-eq? (get-field square piece))
                 (if (and (eq? (get-field type piece) 'pawn) (send move reaches-last-rank?))
                     (make-object piece% 'queen (get-field color piece) move-dest-square)
                     (make-object piece% (get-field type piece) (get-field color piece) move-dest-square)))
                (else piece)))
        
        (define (notcaptured? piece)
          (not (send move-dest-square square-eq? (get-field square piece))))
        
        (make-object position%
          (map changepiece (filter notcaptured? pieces))
          (switch-colortomove))))
    
    (define (possible-moves)
      (flatmap (lambda (piece)
                 (cond ((eq? (get-field type piece) 'rook) (qbr-moves (get-field square piece) ROOKDIRECTIONS))
                       ((eq? (get-field type piece) 'bishop) (qbr-moves (get-field square piece) BISHOPDIRECTIONS))
                       ((eq? (get-field type piece) 'knight) (knightmoves (get-field square piece)))
                       ((eq? (get-field type piece) 'queen) (qbr-moves (get-field square piece) QUEENDIRECTIONS))
                       ((eq? (get-field type piece) 'king) (kingmoves (get-field square piece)))
                       ((eq? (get-field type piece) 'pawn) (pawnmoves (get-field square piece)))
                       (else (error "listmoves -- unknown piece type"))))
               (friendly-pieces)))
    
    (define (possible-attacking-moves)
      (flatmap (lambda (piece)
                 (cond ((eq? (get-field type piece) 'rook) (qbr-moves (get-field square piece) ROOKDIRECTIONS))
                       ((eq? (get-field type piece) 'bishop) (qbr-moves (get-field square piece) BISHOPDIRECTIONS))
                       ((eq? (get-field type piece) 'knight) (knightmoves (get-field square piece)))
                       ((eq? (get-field type piece) 'queen) (qbr-moves (get-field square piece) QUEENDIRECTIONS))
                       ((eq? (get-field type piece) 'king) (kingmoves (get-field square piece)))
                       ((eq? (get-field type piece) 'pawn) (pawn-attacking-moves (get-field square piece)))
                       (else (error "listmoves -- unknown piece type"))))
               (friendly-pieces)))
    
    (define/public (legal-moves)
      (filter (lambda (move)
                (not (send (changed-pos move) giving-check?)))
              (possible-moves)))

    (define/public (legal-move? move)
      (send move move-memq? (legal-moves)))
    
    (define/public (giving-check?)
      (attacking? (find-first
                   (lambda (piece) (eq? (get-field type piece) 'king))
                   (lambda (piece) (get-field square piece))
                   (enemy-pieces))))
    
    (define (attacking? square)
      (find-first (lambda (move) (send square square-eq? (get-field dest-square move)))
                  (lambda (move) (if (null? move) #f #t))
                  (possible-attacking-moves)))
    
    (define (under-attack? square)
      (send (make-object position% pieces (switch-colortomove)) attacking? square))
    
    (define/public (endofgame?)
      #f)
    
    (define/public (print)
      (print-list (append pieces (list colortomove))))
    
    ))



