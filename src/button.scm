

(def-area-subclass (<button> parent width height text report-func :optional report-releasebutton)
  
  (def-var nonpressed-bg-color green-color)
  (def-var pressed-bg-color gray-color)

  (define background-color nonpressed-bg-color)
  (def-var text-color black-color)

  (def-var background-alpha 0.27)

  (def-var underline #f)

  (def-var text)

  (define ispressed #f)

  (define (paint g x* y* x2* y2*)
    ;;(c-display "slider:paint called with" g (<-o background-color) x y x2 y2 x-1 x-2);; width height)

    ;;(-> g setColor background-color)
    ;;(-> g fillRect (1+ x) (1+ y) (1- width) (1- height))

    (-> g do-alpha (if ispressed
		       0.27
		       background-alpha)
	(lambda ()
	  (-> g fill-rect background-color (1+ x) (1+ y) x2 y2)))

    ;(-> g draw-string text text-color x y)
    ;;(c-display "drawit" text text-color font8 x y width height)
    (-> g draw-string-in-box text text-color font8 (+ 4 x) y (- width 4) height)
    (if underline
	(-> g drawLine x (+ y height -2) (+ x width) (+ y height -2)))
    )

  (add-mouse-cycle (lambda (button x* y*)
		     (set! ispressed #t)
		     (if (not report-releasebutton)
			 (report-func))
		     (set! background-color pressed-bg-color)
		     (-> parent repaint x y x2 y2)
		     'grab)
		   (lambda (button x* y*)
		     #f)
		   (lambda (button x* y*)
		     (set! ispressed #f)
		     (if report-releasebutton
			 (report-func))
		     (set! background-color nonpressed-bg-color)
		     (-> parent repaint x y x2 y2)))

    
  )

