

(define (callit cont) (cont))


(def-class (<fade-area> parent sound-area fade-time* :key (in? #t))
  
  (def-var fade-time fade-time*)

  (define sound-object (-> sound-area sound-object))
  
  (def-var fade-color blue-color)
  (def-var fade-handle-color blue-color)

  (def-var fade-handle-size 10)
  (def-method (set-fade-handle-size! sound-area-height)
    (set! fade-handle-size (min 30 (c-integer (/ sound-area-height 4)))))
  
  (set-fade-handle-size! (-> sound-area height))

  (def-var alpha (get-alpha 0.2))


  (def-method (set-time! time)

    (if (< time 0)
	(set! time 0))
    (if (> time (-> sound-area get-playlength))
	(set! time (-> sound-area get-playlength)))

    (set! fade-time time)
    (if in?
	(-> sound-object requestTo_setFadeIn fade-time)
	(-> sound-object requestTo_setFadeOut fade-time)))


  (set-time! fade-time)


  
  (def-method (paint g x y x2 y2 width height)
    (-> sound-area get-px1/px2
	(lambda (px1 px2)
	  
	  (define fade-width (i-scale fade-time 0 (-> sound-area get-playlength) 0 (- px2 px1)))
	  
	  (if in?


	      ;; in
	      (let ()
		
		(define fx px1)
		(define fx2 (+ fx fade-width))
		
		(if (> fade-width 1)
		    (-> g do-alpha alpha
			(lambda ()
			  ;;(c-display "drawing fade-in" fx y "-" fx2 y "-" fx y2)
			  (-> g fill-triangle fade-color
			      fx y
			      fx2 y
			      fx y2))))
		
		;;(define fx2 (max (+ 2 px1) fx2))
		;;(define fx22 (min (+ fx2 fade-handle-size) (1- (/ (+ px1 px2) 2))))
		
		;;(-> g draw-rect fade-handle-color fx2 (+ 2 y) (- fx22 fx2) fade-handle-size)
		;;(-> g setColor fade-handle-color)
		;;(-> g drawArc fx2 (+ 2 y) (- fx22 fx2) fade-handle-size 0 360)
		)
	      

	      ;; out
	      (let ()
		(define fx2 px2)
		(define fx (- px2 fade-width))
		
		(if (> fade-width 1)
		    (-> g do-alpha alpha
			(lambda ()
			  (-> g fill-triangle fade-color
			      fx y
			      fx2 y
			      fx2 y2))))
		
		;;(define fx_org fx)
		;;(define fx (max (- fx fade-handle-size) (1+ (/ (+ px1 px2) 2))))
		;;(define fx2 (min fx_org (- px2 2)))
		;;(-> g draw-rect fade-handle-color fx (+ 2 y) (- fx2 fx) fade-handle-size)
		)))))



  ;; Fade-in and fade-out


;  (let ((start-x 0)
;	(start-y 0)
;	(dx 0)
;	(dy 0))
;    (add-mouse-cycle (lambda (bytton x* y*)
		       
  


  )


