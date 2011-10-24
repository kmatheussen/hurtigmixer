





;; This is the miniature version of the mixer seen inside the time slider.


(def-area-subclass (<miniature-area> parent slider width height das-mixer)

  (def-method (get-miniature-pos sound-area callback)
    (-> mixer-area get-position
	(lambda (m-x m-y m-x2 m-y2 m-width m-height)
	  (-> sound-area get-position
	      (lambda (x* y* x2* y2* width* height*)
		(let* ((sound-start-time (-> sound-area start-playtime))
		       (sound-end-time   (-> sound-area end-playtime))
		       (mixer-end-time (-> mixer-area end-time))
		       (sound-length (- sound-end-time sound-start-time)))
		  (callback (i-scale sound-start-time 0 mixer-end-time x x2)
			    (i-scale y* m-y m-y2 (+ y 4) (- y2 3))
			    (max 1 (i-scale sound-length 0 mixer-end-time 0 width))
			    (i-scale height* 0 m-height 0 (- height 7)))))))))




  ;; Paint stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define last-cx 0)

  (def-method (paint-cursor cx)
    (set! cx (i-scale (-> mixer getPlayPos) 0 (-> mixer-area end-time) x x2))
    (if (not (= cx last-cx))
	(let ((g (-> parent ll-window-g))
	      (image (-> parent image)))
	  (-> g setClip x y (- x2 x) (- y2 y))
	  (-> g setColor redbrown-color)
	  (-> g drawLine cx (+ y 4) cx (- y2 4))
	  (-> g blit-sub-image image last-cx (+ y 4) (1+ last-cx) (- y2 3))
	  (set! last-cx cx))))


  (define (blit g image x* y* x2* y2*)
    ;;(c-display "blitting" x* x2*)
    (let* ((cx (i-scale (-> mixer getPlayPos) 0 (-> mixer-area end-time) x x2))
	   (cursor-changed? (not (= cx last-cx))))
      (if (and (>= cx x*)
	       (< cx x2*))
	  (begin
	    (when cursor-changed?
	      (-> g setClip x y (- x2 x) (- y2 y))
	      (-> g blit-sub-image image last-cx y (1+ last-cx) (1+ y2)))
	    (-> g setColor cursor-color)
	    (-> g blit-sub-image image x* y* cx y2*)
	    (-> g drawLine cx (+ y 4) cx (- y2 4))
	    (-> g blit-sub-image image (1+ cx) y* x2* y2*)
	    (set! last-cx cx))
	  (begin
	    (if cursor-changed?
		(paint-cursor cx))
	    (-> g blit-sub-image image x* y* x2* y2*))))
    ;;(-> g draw-rect green-color x* y* (- x2* x*) (- y2* y*))
    )


  (define (post-paint g x* y* x2* y2*)
    (for-each (lambda (sound-area)
		(get-miniature-pos sound-area
				   (lambda (x y width height)
				     (define x2 (+ x width))
				     (define y2 (+ y height))
				     (when (and (> x2* x)
						(< x* x2)
						(> y2* y)
						(< y* y2))
					;(-> g draw-rect gray-color x y width height)
				       (-> g draw-rect (-> sound-area background-color) x y width height)
				       (-> g draw-rect (-> sound-area background-color) (1+ x) (1+ y) (- width 2) (- height 2))
				       '(-> (-> sound-area audiofile-buffer)
					    paintWave
					    g wave-color wave-shade-color
					    0
					    x y (+ x width) (+ y width)
					    x (+ x width)
					    7 #t (-> sound-area backwards) (-> (-> sound-area sound-object) vol))
					;(-> g draw-rect (-> sound-area background-color) (+ 2 x) (+ 2 y) (- width 4) (- height 4))
				       ))))
	      (-> mixer-area get-sound-areas))
    )





  ;; Mouse stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;; Remove the sliders mouse handlers; Lets control all mouse stuff here instead of in the slider object.
  (-> slider mouse-cycles '())

  ;; Change size of area
  (apply add-mouse-cycle (-> slider timeslider-mouse-cycle-left))
  (apply add-mouse-cycle (-> slider timeslider-mouse-cycle-right))

 


  ;; Move sound objects
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Works by hooking into the sound-area.move-mouse-cycle variable

  (let ()

    (define curr-sound-area #f)
    (define curr-mouse-cycle #f)

    (define (scaleit func button x* y*)
      (get-miniature-pos curr-sound-area
			 (lambda (s-x s-y s-width s-height)
			   (-> curr-sound-area get-position
			       (lambda (c-x c-y c-x2 c-y2 c-width c-height)
				 (-> curr-sound-area get-px1/px2
				     (lambda (px1 px2)
				       (func button
					     (i-scale x* s-x (+ s-x s-width) px1 px2)
					     (i-scale y* s-y (+ s-y s-height) c-y c-y2)))))))))

    (def-method (mouse-drag button x* y*)
      (if curr-mouse-cycle
	  (scaleit (cadr curr-mouse-cycle) button x* y*)))

    (def-method (mouse-release button x* y*)
      (if curr-mouse-cycle
	  (scaleit (caddr curr-mouse-cycle) button x* y*))
      (set! curr-mouse-cycle #f)
      (set! curr-sound-area #f))

    (add-mouse-cycle (lambda (button x* y*)
		       (call/cc
			(lambda (return)
			  (if (not (= button *right-mouse-button*))
			      (return #f))
			  (for-each (lambda (sound-area)
				      (get-miniature-pos sound-area
							 (lambda (s-x s-y s-width s-height)
							   (when (and (>= x* s-x)
								      (< x* (+ s-x s-width))
								      (>= y* s-y)
								      (< y* (+ s-y s-height)))
								 (set! curr-sound-area sound-area)
								 (set! curr-mouse-cycle (-> sound-area move-mouse-cycle))
								 (let ((ret (scaleit (car curr-mouse-cycle) button x* y*)))
								   (if ret
								       (return ret)))))))
				    (-> mixer-area get-sound-areas))
			  #f)))
		     mouse-drag
		     mouse-release))



  ;; Move area
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (add-mouse-cycle (lambda (button x* y*)
		     ;;(c-display "button:" button)
		     (if (<= button 3)
			 ((car (-> slider mouse-cycle-move)) 1 x* y*)
			 #f))
		   (cadr (-> slider mouse-cycle-move))
		   (caddr (-> slider mouse-cycle-move)))
  


  ;; Set playposition
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (let ((playposfunc (lambda (button x* y*)
		       (if (and (= 1 button)
				(-> mixer isPlayingQuestionmark))
			   (let ((newpos (c-scale x* x x2 0 (-> mixer-area end-time))))
			     (-> mixer-area stop)
			     (-> mixer-area paint-cursor newpos)
			     (-> mixer requestTo_setPlayPos newpos))
			   (if (>= button 4)
			       (let ((newpos (c-scale x* x x2 0 (-> mixer-area end-time))))
				 (-> mixer requestTo_setPlayPos newpos)
				 (paint-cursor newpos)
				 (-> mixer-area play)
				 #f))))))
    (add-mouse-cycle playposfunc playposfunc playposfunc))

  )


