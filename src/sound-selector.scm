

(define renderinghints (new-static <java.awt.RenderingHints>))


(def-area-subclass (<sound-selector> parent width height title)

  (def-var background-color red-color)
  (def-var border-color black-color)

  (def-var title-height 16)
  (def-var borderwidth-left 4)
  (def-var borderwidth-top 12)
  (def-var soundlist-areas '())
  
  (def-var moving-text-color black-color)
  (def-var moving-background-color (-> Various getColor 0.83 0.66 0.51))
  
;;  (define (paint g x* y* x2* y2*)    
    ;;(c-display "sound-selector->paint called" x* y* x2* y2*)
    ;;(-> g setColor background-color)
    ;;(-> g fillRect x y width height)
    ;;(-> parent paint-background g x* y* x2* y2*)
    
    ;;(-> g setColor border-color)
    ;;(-> g drawRoundRect x y (- width 2) (- height 3) 20 20)
    ;;(-> g drawLine x (+ y (- title-height borderwidth)) x2 (+ y (- title-height borderwidth)))
;;     )
    
  


  (def-var title-area (<area> parent width title-height));(<text-area> parent width title-height title ':text-color verygray-color))
  (add-sub-area title-area (+ x borderwidth-left) (+ y 1 borderwidth-top))

  ;; This function is added as a method to <list-area> for soundlist list-areas.
  (define* (add-sound soundlist filename :key is-URL)
    (call/cc 
     (lambda (return)

       (when (member filename (-> soundlist filenames))
	     (c-display (string-append "Fila \"" filename "\" er allerede i lista."))
	     (return #f))


       ;;(define audiofile (lambda (callback)
		;;	   (get-audiofile parent filename callback ':is-URL is-URL)))

       ;;(when (not audiofile)
	   ;;  (c-display (string-append "Kunne ikke finne \"" filename "\", eller fila har et ulovlig format."))
	 ;;    (return #f))
       
       (-> soundlist filenames filename)

       (letrec* ((filename2 (filename-without-path filename))
		 (filetext-area (-> soundlist add-element filename2
				     ':mouse-cycle
				     (let ((dx 0)
					   (dy 0)
					   ;;(last-x 0)(last-y 0)(last-x2 0)(last-y2 0)
					   (moving-text-area #f))
				       (list (lambda (button x* y* text)
					       (if (or (= button *left-mouse-button*)
						       (= button *right-mouse-button*))
						   (-> filetext-area get-position
						       (lambda (ft-x ft-y ft-x2 ft-y2 ft-width ft-height)
							 (set! moving-text-area (<text-area> parent ft-width ft-height filename2
											     ':text-color moving-text-color ':background-color moving-background-color
											     ':background-alpha 0.66))
							 (-> parent add-sub-area moving-text-area ft-x ft-y)
							 ;;(-> parent repaint l-x l-y l-x2 l-y2)
							 (set! dx (- ft-x x*))
							 (set! dy (- ft-y y*))
							 ;;(c-display 'h3)
							 'grab))
						   #f))
					     (lambda (button x* y* text)
					       (let ((x* (+ x* dx))
						     (y* (+ y* dy)))
						 (-> moving-text-area repaint-me!)
						 (-> moving-text-area set-position! x* y*)
						 (-> moving-text-area repaint-me!)))
					     (lambda (button x* y* text)
					       (-> moving-text-area get-position
						   (lambda (l-x l-y l-x2 l-y2 l-width l-height)
						     (let ((x* (+ x* dx))
							   (y* (+ y* dy))
							   (moving-text-area moving-text-area)) ;; need a copy for the get-audiofile continuation below.
						       (-> moving-text-area add-mouse-cycle
							   (lambda x
							     (-> sound-holder cancelLoading filename)
							     (-> parent remove-sub-area moving-text-area)
							     (-> parent repaint
								 (min l-x x*) (min l-y y*)
								 (max l-x2 (+ x* l-width)) (max l-y2 (+ y* l-height)))
							     #f)
							   #f
							   #f)
						       (get-audiofile filename
								      (lambda (audiofile)
									(-> parent remove-sub-area moving-text-area)
									(if audiofile
									    (-> mixer-area add-sound audiofile x* y*))
									(-> parent repaint
									    (min l-x x*) (min l-y y*)
									    (max l-x2 (+ x* l-width)) (max l-y2 (+ y* l-height)))
									(-> parent check-paint))
								      (lambda (seconds end-time)
									;;(c-display 'g7 seconds end-time filename2)
									(-> moving-text-area text (string-append
												   (number->string seconds) "/"
												   (if (> end-time 0)
												       (number->string end-time)
												       "?")
												   ":"
												   filename2))
									(-> moving-text-area repaint-me!)
									(-> parent check-paint))
								      ':is-URL is-URL))))))))))
	 #t))))

  
  (def-method (create-soundlist height*)
    (-> (if (null? soundlist-areas)
	    title-area
	    (last soundlist-areas))
	get-position
	(lambda (l-x l-y l-x2 l-y2 l-width l-height)
	  (if (> (+ l-y2 height* borderwidth-top 6) y2)
	      (set! height* (- y2 l-y2 borderwidth-top 6)))
	  (let ((soundlist (<list-area> parent (- width (* borderwidth-left 2)) height*  ':slider-color white-color ':background-color redbrown-color)))
	    (add-sub-area soundlist (+ x borderwidth-left) (+ borderwidth-top l-y2))
	    (let ((filenames '()))
	      (-> soundlist add-method 'filenames (lambda args
						    (if (not (null? args))
							(set! filenames (cons (car args) filenames))
							filenames))))
	    (-> soundlist add-method 'add-sound (lambda args
						  (apply add-sound (cons soundlist args))))
	    (push-back! soundlist soundlist-areas)
	    (-> parent repaint x y x2 y2)
	    soundlist))))
  
  
  (def-method (reset!)
    (for-each (lambda (soundlist-area)
		(let ((filenames '()))
		  (-> soundlist-area add-method 'filenames (lambda args
							     (if (not (null? args))
								 (set! filenames (cons (car args) filenames))
								 filenames))))
		(-> soundlist-area add-method 'add-sound (lambda args
							   (apply add-sound (cons soundlist-area args))))
		
		(-> soundlist-area reset!))
	      soundlist-areas))
  )


#|
(define ss (<sound-selector> win 200 150 "Egen Maskin:"))
(-> mixer-area add-sub-area ss 100 50)

(define sl (-> ss create-soundlist 100 "C:\lydkatalog\\"))
(define sl2 (-> ss create-soundlist 100 "D:\lydkatalog\\"))

(begin
  (-> sl2 add-sound "/home/kjetil/2_channel_short.wav")
  (-> win paint))


|#
