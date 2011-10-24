
(c-import colors)

;; Important: All x and y values (including x2, y2, x*, y*, etc.)
;; are absolute values according to the underlying Graphics object!

(define debug-area #f)

;; Subclasses of def-area must have width and height as two of their parameters. In addition, subclasses must also provide "parent", which is the object
;; providing the "repaint", control-pressed?, add-self-blitting-area and paint-background methods.

;; Subclasses can implement the following functions or methods: ismoved, key-pressed, blit, paint and post-paint.



;; Drawing happens like this:
;; 1. Paint to the image. (paint-internal)
;; 2. Blit the image to display. (blit-internal)

;; Areas that do buffered painting overrides "paint" and/or "post-paint", while areas that does
;; unbuffered painting overrides "blit" instead. Or, in case both buffered and unbuffered painting is used,
;; both "paint"/"post-paint" and "blit" can be overridden.



(c-define-macro (def-area-subclass def . body)

  `(def-class ,def
     (set! width (c-integer width))
     (set! height (c-integer height))

     (def-var x 0)
     (def-var y 0)
     (def-var x2 (1- width))
     (def-var y2 (1- height))

     (def-var width)
     (def-var height)

     (def-var do-paint #t)


     ;; Position
     (def-method (get-position callback)
       (callback x y x2 y2 width height))

     (define ismoved #f)

     (def-method (move! dx dy)
       (set! dx (c-integer dx))
       (set! dy (c-integer dy))
       (inc! x dx)
       (inc! y dy)
       (inc! x2 dx)
       (inc! y2 dy)
       (for-each (lambda (sub-area)
		   (-> sub-area move! dx dy))
		 sub-areas)
       (if ismoved
	   (ismoved)))

     (def-method (repaint-me!)
       (-> parent repaint x y x2 y2))
      
     (def-method (set-position! x* y*)
       (set! x* (c-integer x*))
       (set! y* (c-integer y*))
       (let ((dx (- x* x))
	     (dy (- y* y)))
	 (move! dx dy)))     


     (def-var parent-area #f)

     ;; Sub areas
     (def-var sub-areas '())
     (define top-area #f)

     (def-method (add-sub-area sub-area x y)
       (-> sub-area set-position! x y)
       (push-back! sub-area sub-areas)
       (set! top-area sub-area)
       (-> sub-area parent-area this)

       (-> sub-area get-position
	   (lambda (x* y* x2* y2* with* height*)
	     (if (and (< x* x2)
		      (>= x2* x)
		      (< y* y2)
		      (>= y2* y))
		 (-> parent repaint x* y* x2* y2*))))
       ;;(c-display "sub-area added to" ',(car def) ". New sub-area length:" (length sub-areas))
       )

     (def-method (add-sub-area-after before-sub-area sub-area)
       (-> before-sub-area get-position
	   (lambda (x y x2 y2 width height)
	     (add-sub-area sub-area x2 y))))

     (def-method (add-sub-area-below above-sub-area sub-area)
       (-> above-sub-area get-position
	   (lambda (x y x2 y2 width height)
	     (add-sub-area sub-area x y2))))

     (def-method (remove-sub-area sub-area)
       (set! sub-areas (delete! sub-area sub-areas eq?))
       (set! top-area
	     (if (null? sub-areas)
		 #f
		 (last sub-areas)))
       )

     (def-method (lift-sub-area sub-area)
       (when (not (eq? sub-area top-area))
	 (set! sub-areas (append! (delete! sub-area sub-areas eq?) (list sub-area)))
	 (set! top-area sub-area)))

     (def-method (lift-me!)
       (when parent-area
	 (-> parent-area lift-sub-area this)
	 (-> parent repaint x y x2 y2)))



     ;; Keyboard listener
     (define key-pressed #f)
     (def-method (key-pressed-internal key-event)
       (call/cc
	(lambda (return)
	  (if key-pressed
	      (let ((ret (key-pressed key-event)))
		(if ret
		    (return #t)))
	      (for-each (lambda (sub-area)
			  (if (-> sub-area key-pressed-internal key-event)
			      (return #t)))
			sub-areas))
	  #f)))

     (def-method (key-released-internal key-event)
       ;;(c-display "released something")
       #t
       )

     ;; Mouse cycles
     (def-var mouse-cycles '())
       
     (def-method (add-mouse-cycle press-func :optional (drag-func (lambda x #f)) (release-func (lambda x #f)))
       (push-back! (list press-func drag-func release-func)
		   mouse-cycles)
       )

     (def-method (get-mouse-cycle button x* y*)
       (and do-paint
	    (>= x* x)
	    (< x* x2)
	    (>= y* y)
	    (< y* y2)
	    (or (call/cc (lambda (return)
			   (for-each (lambda (sub-area)
				       (and-let* ((res (-> sub-area get-mouse-cycle button x* y*)))
					  (return res)))
				     (reverse sub-areas))
			   #f))
		(call/cc (lambda (return)
			   (for-each (lambda (mouse-cycle)
				       (and-let* ((res ((car mouse-cycle) button x* y*)))
					  (return mouse-cycle)))
				     mouse-cycles)
			   #f)))))

     ;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;
     ;; Drawing


     ;; Overlap check for both paint-internal and blit-internal. Bad name for the function.
     (def-method (paint? x* y* x2* y2*)
       (and (> x2* x)
	    (< x* x2)
	    (> y2* y)
	    (< y* y2)))



     ;; Blitting
     ;;;;;;;;;;;;;

     (define blit #f)
     (def-method (get-blit-function)
       blit)

     (def-method (blit-sub-areas g image x* y* x2* y2*)
       (for-each (lambda (sub-area)
		   (-> sub-area blit-internal g image x* y* x2* y2*))
		 sub-areas))

     (def-method (blit-internal g image x* y* x2* y2*)
       ;;(if debug-area
       ;;	   (c-display "blit-internal called" class-name))
       (if (paint? x* y* x2* y2*)
	   (let ((clipit (lambda (cont)
			   (let ((cx1 (max x x*))
				 (cy1 (max y y*))
				 (cx2 (min x2 x2*))
				 (cy2 (min y2 y2*)))
			     (-> g setClip cx1 cy1 (- cx2 cx1) (- cy2 cy1))
			     (cont cx1 cy1 cx2 cy2)))))
	     (cond (blit
		    (clipit (lambda (cx1 cy1 cx2 cy2)
			      (blit g image cx1 cy1 cx2 cy2))))
		   ((call/cc (lambda (return)
			       (for-each (lambda (sub-area)
					   (if (-> sub-area get-blit-function)
					       (return #t)))
					 sub-areas)
			       #f))
		    (blit-sub-areas g image x* y* x2* y2*))
		   (else
		    (clipit (lambda (cx1 cy1 cx2 cy2)
			      (-> g blit-sub-image image cx1 cy1 cx2 cy2)))
		    ))))
       )

     ;; Painting
     ;;;;;;;;;;;;;;;

     (def-method (paint-sub-areas g x* y* x2* y2*)
       (for-each (lambda (sub-area)
		   (-> sub-area paint-internal g x* y* x2* y2* x y x2 y2))
		 sub-areas))


     ;; Subclasses must override one or two of these ones to show anything on screen.

     (define paint #f)      ;; Called before painting the current area's sub-areas
     (define post-paint #f) ;; Called after painting the current area's sub-areas


     (def-method (paint-internal g x* y* x2* y2* px1 py1 px2 py2)
       ;;(c-display "paint-internal called" ',(car def) x* y* x2* y2* "(" x y x2 y2 ")")
       (when (and do-paint
		  (paint? x* y* x2* y2*))
	     ;;(c-display "paint-internal hepp" ',(car def) paint "sub-areas" sub-areas)
	     (let ((cx1 (max x x* px1))
		   (cy1 (max y y* py1))
		   (cx2 (min x2 x2* px2))
		   (cy2 (min y2 y2* py2)))
	       (when paint
                 (-> g setClip cx1 cy1 (- cx2 cx1) (- cy2 cy1))
                 (paint g x* y* x2* y2*))
	       (paint-sub-areas g x* y* x2* y2*)
	       (when post-paint
                 (-> g setClip cx1 cy1 (- cx2 cx1) (- cy2 cy1))
                 (post-paint g x* y* x2* y2*)))))





     (set! class-name ',(car def))

     ,@body))




(def-area-subclass (<area> parent width height)
  )





