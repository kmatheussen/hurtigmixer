

(def-area-subclass (<list-area> parent width height :key (slider-color redbrown-color) (background-color #f))

  (def-var text-color verygray-color)
  (def-var slider-color)
  (def-var background-color)

  (def-var element-height 13)
  (def-var slider-width 12)

  ;; list of text-areas.
  (def-var elements '())

  (def-method (reset!)
    (for-each remove-sub-area elements)
    (set! elements '())
    (set-slider-range!))

  (def-method (get-elements) (map (lambda (element)
				    (-> element text))
				  elements))

  (define max-shown (c-integer (/ height element-height)))
  (set! element-height (c-integer (/ height max-shown)))

  (define (paint g x* y* x2* y2*)
    (-> parent paint-background g x* y* x2* y2*)
    )

  (define (slider-changed x-1 x-2)
    (let* ((len (length elements))
	   (first (c-integer (c-scale x-1 0 1 0 len))))
      ;;(c-display (* 1.0 x-1) (* 1.0 x-2) "slider-changed first/last" first 'xxx len)
      (c-for-each (lambda (n element)
		    (let ((ypos (c-scale n first (+ first max-shown 1) y y2)))
		      (cond ((< ypos y)
			     (-> element do-paint #f))
			    ((>= ypos y2)
			     (-> element do-paint #f))
			    (else
			     (-> element do-paint #t)
			     (-> element set-position!
				 (+ x slider-width)
				 ypos)))))
		  elements))
    (-> slider repaint-me!)
    (repaint-me!))
  

  (def-var slider (<slider> win slider-width height 0 1 slider-changed ':vertical #t))
  (-> slider slider-border-color slider-color)
  (-> slider slider-pressed-color slider-color)
  (-> slider slider-nonpressed-color slider-color)
  (-> slider slider-color slider-color)
  (-> slider background-color background-color)
  (-> slider alpha-val alpha-1.00)

  (add-sub-area slider x y)

  (define (set-slider-range!)
    (-> slider set-values!
	(if (null? elements)
	    0
	    (max 0 (c-scale (- (length elements) max-shown)
			    0 (length elements)
			    0 1)))
	1
	#f))

  (def-method (add-element text :key mouse-cycle)
    (if mouse-cycle
	(set! mouse-cycle (map (lambda (func)
				 (lambda (button x* y* text-area)
				   (func button x* y* text)
				   ))
			       mouse-cycle)))
    (let* ((text-area (<text-area> parent width element-height text ':mouse-cycle mouse-cycle ':text-color text-color)))
      ((if (null? elements)
	   (lambda (callback)
	     (callback (+ x slider-width) y
		       (+ x width) (+ y element-height)
		       (- width slider-width) element-height))
	   (<- (last elements) get-position))
       (lambda (l-x l-y l-x2 l-y2 l-width l-height)
	 (push-back! text-area elements)
	 (add-sub-area text-area l-x (+ l-y element-height))
	 (set-slider-range!)
	 (-> slider report!) 
	 (repaint-me!)
	 text-area))))
    

  
  )


#|
(c-import text-area)
(c-import list-area)

(define la (<list-area> win 200 250 "Egen maskin:"))
(-> mixer-area add-sub-area la 100 50)

(define num 0)
(let ((num (number->string (inc! num 1))))
  (-> la add-element (string-append "2_channel_short.wav" num))
  (-> la add-element (string-append "Blub_mono_16.wav" num)
      ':mouse-cycle(list 
		    (lambda x (c-display "got it" num x))
		    (lambda x #f)
		    (lambda x #f)))
  (-> win paint))

(-> (-> la slider) set-values! 0.3 0.7)
(-> win paint)


|#
