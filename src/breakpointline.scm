

(def-area-subclass (<breakpointline> parent width height :key (first-val 0.5) (last-val 0.5) (breakpoint-width 20))
  
  (def-var breakpoints (list (list 0.0 first-val)(list 0.5 0.2)(list 0.7 0.7)(list 1.0 last-val)))

  (def-var breakpoint-width)

  (def-var line-color black-color)
  (def-var breakpoint-color blue-color)
  
  (define (->x b)
    (c-scale (car b) 0.0 1.0 x x2))
  (define (->y b)
    (c-scale (cadr b) 0.0 1.0 y y2))
  (define (x-> x*)
    (c-scale x* x x2 0.0 1.0))
  (define (y-> y*)
    (c-scale y* y y2 0.0 1.0))

  (def-method (paint g x* y* x2* y2*)
    (define last-x -1)
    (define last-y -1)
    (define w/2 (/ breakpoint-width 2))
    (let loop ((last-x -1)
	       (last-y -1)
	       (breakpoints breakpoints))
      (when (not (null? breakpoints))
	(define breakpoint (car breakpoints))
	(define x* (->x breakpoint))
	(define y* (->y breakpoint))
	;;(c-display breakpoint x* y*)
	(-> g draw-rect breakpoint-color (- x* w/2) (- y* w/2) breakpoint-width breakpoint-width)
	(if (not (= -1 last-x))
	    (-> g draw-line line-color last-x last-y x* y*))
	(loop x* y* (cdr breakpoints)))))

  (define (find-breakpoint func cont)
    (define w/2 (/ breakpoint-width 2))
    (let loop ((before #f)
	       (breakpoints breakpoints))
      (when (not (null? breakpoints))
	(define breakpoint (car breakpoints))
	(define x* (->x breakpoint))
	(define y* (->y breakpoint))
	(if (func (- x* w/2) (- y* w/2)
		  (+ x* w/2) (+ y* w/2))
	    (cont before breakpoint (if (null? (cdr breakpoints)) #f (cadr breakpoints)))
	    (loop breakpoint (cdr breakpoints))))))

  (define (square x)
    (* x x))
  (define (distance2 x1 y1 x2 y2)
    (+ (square (- x1 x2)) (square (- y1 y2))))

  (define (on-line? x* y*)
    (call/cc
     (lambda (return)
       (let loop ((breakpoints breakpoints))
	 (when (not (null? (cdr breakpoints)))
	   (define a (car breakpoints))
	   (define b (cadr breakpoints))
	   (define x1 (->x a))
	   (define x2 (->x b))
	   (define y1 (->y a))
	   (define y2 (->y b))
	   (if (and (>= x* x1)
		    (<  x* x2))
	       (let* ((y-hit (c-scale x* x1 x2 y1 y2))
		      (dist (abs (- y* y-hit))))
		 (< dist breakpoint-width))
	       (loop (cdr breakpoints))))))))
  

  (def-method (add-breakpoint x* y*)
    (define new-breakpoint (list (x-> x*) (y-> y*)))
    (set! breakpoints
	  (let loop ((breakpoints breakpoints))
	    (cond ((null? (cdr breakpoints))
		   (cons new-breakpoint
			 breakpoints))
		  (else
		   (define breakpoint (car breakpoints))
		   (define x** (->x breakpoint))
		   (if (> x** x*)
		       (cons new-breakpoint
			     breakpoints)
		       (cons breakpoint
			     (loop (cdr breakpoints)))))))))


  (let ((before #f)
	(breakpoint #f)
	(after #f))


    (define (set-new-pos button x* y*)
      (if (or (not before)
	      (not after))
	  (set! x* (->x breakpoint)))
      (define bx (if before (->x before) x*))
      (define by (if before (->y before) y*))
      (define lx (->x breakpoint))
      (define ly (->y breakpoint))
      (define ax (if after (->x after) x*))
      (define ay (if after (->y after) y*))
      (if (and before after)
	  (set-car! breakpoint (x-> (boundaries bx x* ax))))
      (set-car! (cdr breakpoint) (boundaries 0.0 (y-> y*) 1.0))
      ;;(c-display "y:" y "by:" by "ly:" ly "y*:" y* "ay:" ay "y2:" y2)
      ;;(c-display (- bx breakpoint-width) (- (min by ly y* ay) breakpoint-width)
	;;	 (+ ax breakpoint-width) (+ (max by ly y* ay) breakpoint-width))

      (-> parent repaint
	  (- bx breakpoint-width) (- (min by ly y* ay) breakpoint-width)
	  (+ ax breakpoint-width) (+ (max by ly y* ay) breakpoint-width))
      )
      
    (define (button-down button x* y*)
      (find-breakpoint (lambda (x y x2 y2)
			 ;;(c-display x x* x2 "-" y y* y2)
			 (and (= button *left-mouse-button*)
			      (>= x*  x)
			      (>= y*  y)
			      (<  x* x2)
			      (<  y* y2)))
		       (lambda (before* breakpoint* after*)
			 (set! before before*)
			 (set! breakpoint breakpoint*)
			 (set! after after*)
			 'grab)))

    (add-mouse-cycle button-down
		     set-new-pos
		     set-new-pos)

    (add-mouse-cycle (lambda (button x* y*)
		       (when (and (= button *left-mouse-button*)
				  (on-line? x* y*))
			 (define breakpoint (add-breakpoint x* y*))
			 (c-display "breakpoints" breakpoints)
			 (repaint-me!)
			 (button-down button x* y*)))
		     set-new-pos
		     set-new-pos)
    )
  
  )

