#lang racket/gui

(require "Mechanics.rkt" "Setup.rkt" "Engine.rkt")

(define ascii-interface%
  (class object%
    (super-new)
    
    (define (make-board pos) ; creates a list of 8 row-lists representing the playing area
      (map (lambda (row)
             (map (lambda (column)
                    (send pos occupied? (make-object square% column row)))
                  (range FIRSTCOLUMN (add1 LASTCOLUMN))))
           (range FIRSTROW (add1 LASTROW))))
    
    (define (display-board pos)
      (for-each (lambda (row)
                  (for-each (lambda (piece)
                              (if (eq? piece #f)
                                  (display " ")
                                  (display (send piece ascii-representation))))
                            row)
                  (newline))
                (reverse (make-board pos))))
    
    (define (inputmove)
      (read))
    
    (define (game startpos)
      (display-board startpos)(newline)
      (define (do-move pos)
        
        (define engineturn?
          (eq? (get-field colortomove pos) (get-field colortomove startpos)))
        
        (cond ((send pos endofgame?) true)
              (engineturn? (let ((newpos (send pos changed-pos (bestmove pos))))
                             (display-board newpos)(newline)
                             (do-move newpos)))
              (else (let ((newpos (send pos changed-pos (inputmove))))
                      (display-board newpos)(newline)
                      (do-move newpos)))))
      
      (do-move startpos))
    
    (game STARTPOSITION)
    ))


(define gui-interface%
  (class object%
    (super-new)
    
    (define engine-state
      (new
       (class object%
         (init [begin-state 'waiting-for-input])
         (define state begin-state)
         (super-new)
         
         (define/public (set-state! input-state)
           (cond ((or (eq? input-state 'waiting-for-input) 
                      (eq? input-state 'waiting-for-destsquare)
                      (eq? input-state 'finding-bestmove)
                      (eq? input-state 'end-of-game))
                  (set! state input-state))
                 (else (eprintf "engine-state --- ~v unknown state" input-state))))
         
         (define/public (get-state) state))))
    
    (define board-canvas% ; Standard graph coordinates; 0-8, origin upper left. So e4 is (4.5 4.5)
      (class canvas%
        (inherit get-dc get-width get-height refresh-now)
        
        (define LINEWIDTH 0.01)
        (define position STARTPOSITION)
        (define sourcesquare (make-object square% 0 0))
        
        (define (draw-white-pieces pos dc)
          (define text-foreground (send dc get-text-foreground))
          (send dc set-text-foreground "DarkGoldenrod")
          (send dc set-font (make-object font% 0.45 'default))
          (for-each
           (lambda (piece)
             (let ((c (get-field col (get-field square piece)))
                   (r (get-field row (get-field square piece))))
               (send dc draw-text (send piece ascii-representation)
                     (- c .65)
                     (- LASTROW r -0.1))))
           (send pos white-pieces))
          (send dc set-text-foreground text-foreground))
        
        (define (draw-black-pieces pos dc)
          (send dc set-font (make-object font% 0.45 'default))
          (for-each
           (lambda (piece)
             (let ((c (get-field col (get-field square piece)))
                   (r (get-field row (get-field square piece))))
               (send dc draw-text (send piece ascii-representation)
                     (- c .65)
                     (- LASTROW r -0.1))))
           (send pos black-pieces)))
        
        (define (draw-board dc)
          (send dc set-pen "black" LINEWIDTH 'solid)
          (send dc set-brush (make-object color% 10 10 10 .3) 'solid)
          (for-each
           (lambda (row)
             (for-each
              (lambda (col)
                (send dc draw-rectangle (sub1 (+ (remainder row 2) (* 2 col))) row 1.02 1.02))
              (range 0 5)))
           (range 0 8)))
        
        (define (mouse-x->column mouse-x)
          (floor (add1 (* (/ mouse-x (get-width)) 8))))
        
        (define (mouse-y->row mouse-y)
          (floor (- 10 (* (/ mouse-y (get-height)) 8))))
        

        (define highlighted-squares
          (new
           (class object%
             (super-new)
             
             (define highlight-square-list '())
                     
             (define/public (highlight-square col row)
               (add-highlight-square (make-object square% col row)))
        
             (define (add-highlight-square square)
               (set! highlight-square-list (cons square highlight-square-list))
               )
        
             (define/public (clear-highlight-square-list)
               (set! highlight-square-list '()))
        
             (define/public (draw-highlight-square-list dc)
               (send dc set-pen "black" LINEWIDTH 'solid)
               (send dc set-brush "LemonChiffon" 'solid)
               (for-each (lambda (square)
                           (send dc draw-rectangle
                                 (- (get-field col square) FIRSTCOLUMN)
                                 (- LASTROW (get-field row square))
                                 1 1))
                         highlight-square-list
                         ))

             )))
        
        (define (select-source-square clicked-square col row)
          (if (send position friendly? clicked-square)
              (begin
                (set! sourcesquare clicked-square)
                (send highlighted-squares highlight-square col row) 
                (send engine-state set-state! 'waiting-for-destsquare))
              (eprintf "click friendly square")))
        
        (define (select-destination-square clicked-square col row)
          
          (define (start-over)
            (send highlighted-squares clear-highlight-square-list)
            (send engine-state set-state! 'waiting-for-input))
          
          (define (execute-input-move clicked-move)
            (send highlighted-squares highlight-square col row)
            (set! position (send position changed-pos clicked-move))
            (send engine-state set-state! 'finding-bestmove)
            (refresh-now))
          
          (define (execute-engine-best-move)
            (send highlighted-squares clear-highlight-square-list)
            (set! position (send position changed-pos (bestmove position)))
            (send engine-state set-state! 'waiting-for-input))
          
          (if (send clicked-square square-eq? sourcesquare)
              (start-over)
              (let ((clicked-move (make-object move% sourcesquare clicked-square)))
                (if (send position legal-move? clicked-move)
                    (begin
                      (execute-input-move clicked-move)
                      (execute-engine-best-move))
                    (eprintf "click legal square")))))
        
        (define/override (on-event mouse-event)
          (cond ((send mouse-event button-down?)           
                 (let* ((col (mouse-x->column (send mouse-event get-x)))
                        (row (mouse-y->row (send mouse-event get-y)))
                        (clicked-square (make-object square% col row))
                        (state (send engine-state get-state)))
                   (cond ((eq? state 'waiting-for-input)
                          (select-source-square clicked-square col row))
                         ((eq? state 'waiting-for-destsquare)
                          (select-destination-square clicked-square col row))
                         (else void))))
                (else void))
          (refresh-now))
        
        (define/override (on-paint)
          (define dc (get-dc))
          (define x-scale (/ (get-width) 8))
          (define y-scale (/ (get-height) 8))
          (send dc set-scale x-scale y-scale)
          (draw-board dc)
          (send highlighted-squares draw-highlight-square-list dc)
          (draw-white-pieces position dc)
          (draw-black-pieces position dc)
          (send main-window set-status-text
                (format "status: ~a" (send engine-state get-state)))
          )
        
        (super-new)))  
    
    (define chess-menu%
      (class object%
        (super-new)
        (define chess-menu-bar
          (new menu-bar%
               [parent main-window]))
        
        (define file-menu
          (new menu%
               [label "&File"]
               [parent chess-menu-bar]))
        
        (new menu-item%
             [parent file-menu]
             [label "First &Entry"]
             [shortcut #\e]
             [callback (lambda (item event)
                         (eprintf "First Entry, ~a, ~a\n"
                                  (send item get-shortcut-prefix)
                                  (send event get-event-type)))])
        (new menu-item%
             [parent file-menu]
             [label "&Quit"]
             [shortcut #\q]
             [callback (lambda (i e) (send main-window show #f))])
        ))
    
    (define main-window
      (new frame%
           [label "Chess GUI"]))
    
    (new board-canvas%
         [parent main-window]
         [min-width 400]
         [min-height 400])
    
    (new chess-menu%)
    (send main-window create-status-line)
    (send main-window set-status-text
          (format "status: ~a" (send engine-state get-state)))
    
    (send main-window show #t)
    ))

(new gui-interface%)
;(new ascii-interface%)

