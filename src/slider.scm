

;; Externally:
;;  0 <= x-1 <= x-2 <= 1
;;
;; Internally:
;;  0 <= x-1 <= x-2 <= width



(def-area-subclass (<slider> parent width height x-1 x-2 report-func :key timeslider vertical (paint-x-1 #t) (paint-borders #t) background-color)

  (def-var threshold (c-integer (/ width 16))) ;;20)
  (def-var threshold2 (max 5 (c-integer (/ threshold 5))))

  (def-var border-color black-color)
  (def-var slider-border-color black-color)
  (def-var slider-nonpressed-color (-> Various getColor 0.4 0.0 1.0))
  (def-var slider-pressed-color slider-nonpressed-color)
  (def-var slider-color slider-nonpressed-color)
  (def-var threshold-color red-color)
  (def-var background-color)

  (def-var alpha-val alpha-0.05)

  (define is-solid #f)

  (def-method (set-solid! color)
    (set! is-solid #t)
    (set! threshold 0)
    (set! threshold2 0)
    (set! border-color color)
    (set! slider-border-color color)
    (set! slider-pressed-color color)
    (set! slider-nonpressed-color color)
    (set! slider-color color)
    (set! threshold-color color))

  (define (get-slider-width)
    (if is-solid
	(if vertical height width)
	(- (if vertical height width) 4)))

  (def-method (set-values! x-1* x-2* :optional (repaint #t))
    (set! x-1 (i-scale x-1* 0 1 0 (get-slider-width)))
    (set! x-2 (i-scale x-2* 0 1 0 (get-slider-width)))
    (if repaint
	(-> parent repaint x y (1- x2) y2)))

  ;; Internally, x-1 and x-2 are scaled to the size of the slider, while outside its between 0 and 1.

  (set-values! x-1 x-2 #f)

  (def-method (report! :optional (callback report-func))
    (callback (c-scale x-1 0 (get-slider-width) 0 1)
	      (c-scale x-2 0 (get-slider-width) 0 1)))



  (def-method (paint g x* y* x2* y2*)
    ;;(c-display "slider:paint called with" g (<-o background-color) x y x2 y2 x-1 x-2);; width height)

    (if background-color
	(begin
	  (-> parent paint-background g x y x2 y2)
	  (-> g fill-rect background-color (+ 2 x) (+ 2 y) (- x2 x 4) (- y2 y 4)))
	(-> parent paint-background g x y x2 y2))

    (if paint-borders
	(-> g draw-rect border-color x y (1- width) (1- height)))

    (define x-1* (if paint-x-1 x-1 0))
    (define x-2* x-2)

    (when (and vertical (not paint-x-1))
	  (set! x-1* x-1)
	  (set! x-2* (1- height)))

    (if is-solid
	(let ()
	  (if vertical
	      (-> g fill-rect slider-color
		  x (+ y x-1*)
		  width (- x-2* x-1*))
	      (-> g fill-rect slider-color
		  (+ x x-1*) y
		  (- x-2* x-1*) height)))

	(let ()
	  (-> g do-alpha alpha-val
	      (lambda ()
		(if vertical
		    (-> g fill-rect slider-color
			(+ x 2) (+ y x-1* 2)
			(- width 5) (- x-2* x-1* 1))
		    (-> g fill-rect slider-color
			(+ x x-1* 2) (+ y 2)
			(- x-2* x-1* 2) (- height 5)))))
	  
	  (if paint-borders
	      (if vertical
		  (-> g draw-rect slider-border-color
		      (+ x 2) (+ y x-1* 2)
		      (- width 5) (- x-2* x-1* 1))
		  (-> g draw-rect slider-border-color
		      (+ x x-1* 2) (+ y 2)
		      (- x-2* x-1* 2) (- height 5))))
	  )
	)
    )

  
  (def-method (post-paint g x* y* x2* y2*)
    (when timeslider
	  (-> g setColor threshold-color)
	  (let* ((x1 (max x (- (+ x x-1) threshold)))
		 (x2 (+ (+ x x-1) threshold)))
	    (-> g drawRect
		x1 (+ y 4)
		(- x2 x1 2) (- height 10)))
	  (let* ((x1 (- (+ x x-2) threshold))
		 (x2 (min x2 (+ x1 (* 2 threshold)))))
	    (-> g drawRect
		x1 (+ y 4)
		(- x2 x1 2) (- height 10)))))


  ;; Changing width of slider.  
    
  (define (mouse-change-x1 x-1*)
    (set! x-1 x-1*)
    
    (if (< x-1 0)
	(set! x-1 0))
    (if (> x-1 (- x-2 threshold2))
	(set! x-1 (- x-2 threshold2)))
    
    (-> parent repaint x y (1- x2) (1- y2))
    
    (report!)
    
    ;;(-> parent repaint x y (1- x2) y2))
    )

    
  (define (mouse-change-x2 x-2*)
    (set! x-2 x-2*)

    (if (> x-2 (get-slider-width))
	(set! x-2 (get-slider-width)))
    (if (< x-2 (+ x-1 threshold2))
	(set! x-2 (+ x-1 threshold2)))
 
    (-> parent repaint x y (1- x2) (1- y2))

    (report!)
    )

  (def-var timeslider-mouse-cycle-left #f)
  (def-var timeslider-mouse-cycle-right #f)

  (when timeslider
	(let* ((startpos 0)
	       (mouse-cycle (list (lambda (button x* y*)
				    (if (and (or #t (= button 1))
					     (> x* (- (+ x x-1) threshold))
					     (< x* (+ (+ x x-1) threshold))
					     (< x* (/ (+ (+ x x-1 threshold) (- (+ x x-2) threshold)) 2)))
					(begin
					  (set! startpos (- (+ x x-1) x*))
					  'grab)
					#f))
				  (lambda (button x* y*)
				    (mouse-change-x1 (+ (- x* x) startpos)))
				  (lambda (button x* y*)
				    (mouse-change-x1 (+ (- x* x) startpos))))))
	  (apply add-mouse-cycle mouse-cycle)
	  (set! timeslider-mouse-cycle-left mouse-cycle))

	(let* ((startpos 0)
	       (mouse-cycle (list (lambda (button x* y*)
				    (if (and (or #t (= button 1))
					     (> x* (- (+ x x-2) threshold))
					     (< x* (+ (+ x x-2) threshold)))
					(begin
					  (set! startpos (- (+ x x-2) x*))
					  'grab)
					#f))
				  (lambda (button x* y*)
				    (mouse-change-x2 (+ (- x* x) startpos)))
				  (lambda (button x* y*)
				    (mouse-change-x2 (+ (- x* x) startpos))))))
	  (apply add-mouse-cycle mouse-cycle)
	  (set! timeslider-mouse-cycle-right mouse-cycle)))
  

  ;; Changing slider position.
  
  (define (mouse-setpos x-1*)
    (define dx (- x-1* x-1))
    ;;(c-display "x-1*:" x-1* "x-1:" x-1 "dx:" dx)

    (let ((slider-width (get-slider-width)))
      (if (> (+ x-2 dx) slider-width)
	  (set! dx (- slider-width x-2)))
      (if (< (+ x-1 dx) 0)
	  (set! dx (- x-1)))
  
      (inc! x-1 dx)
      (inc! x-2 dx)
      (-> parent repaint x y (1- x2) (1- y2))
      (report!)))


  (def-var mouse-cycle-move #f)

  (let ((startpos 0))
    (set! mouse-cycle-move
	  (delta-mouse-cycle parent
			     (lambda (button x* y*)
			       (let ((pos* (if vertical y* x*))
				     (pos (if vertical y x)))
				 (if (and (= button 1)
					  (or (not timeslider)
					      (and (>= pos* (+ pos (- x-1 threshold)))
						   (<= pos* (+ pos (+ x-2 threshold))))))
				     (begin
				       (when (not (eq? slider-color slider-pressed-color))
					     (set! slider-color slider-pressed-color)
					     (-> parent repaint x y (1- x2) y2))
				       (set! startpos x-1)
				       'grab)
				     #f)))
			     (lambda (button dx dy)
			       (let ((pos* (if vertical dy dx)))
				 (mouse-setpos (+ startpos pos*))))
			     (lambda (button dx dy)
			       (let ((pos* (if vertical dy dx)))
				 (set! slider-color slider-nonpressed-color)
				 (mouse-setpos (+ startpos pos*))))))
    (apply add-mouse-cycle mouse-cycle-move))

  )


#|
(define slider (<slider> win 25 250 0.2 0.4 (lambda x (apply c-display x)) ':vertical #t))
(-> mixer-area add-sub-area slider 250 100)
(-> win paint)

(-> slider set-values! 20 40)




|#
