


(c-import area)
(c-import mouse)

(define debug-background-painting #f)


(define use-buffered-images (and #f is-running-linux?)) ;; Volatile images are created using Pixmaps on X11. Painting waveforms line by line on pixmaps is quite slow.

(c-define (ll-create-window width height paint-callback mousepress-callback mousedrag-callback mouserelease-callback mousewheel-callback keypress-callback keyrelease-callback)
  (c-display "ll-create1")
  (let* ((createframe (new <CreateFrame>
			   (java-wrap mousepress-callback) (java-wrap mousedrag-callback) (java-wrap mouserelease-callback) (java-wrap mousewheel-callback)
			   (java-wrap keypress-callback) (java-wrap keyrelease-callback) (java-wrap paint-callback)
			   is-running-applet
			   is-running-standalone))
	 (frame (-> createframe frame)))
    (c-display "ll-create2")
    (when (not is-running-applet)
	  (-> frame setBounds 500 100 width height)
	  (-> frame setVisible #t))
    frame))

		    

(define static-VolatileImage (new-static <java.awt.image.VolatileImage>))
(define IMAGE_INCOMPATIBLE  (-> static-VolatileImage IMAGE_INCOMPATIBLE))
(define IMAGE_RESTORED (-> static-VolatileImage IMAGE_RESTORED))
(define IMAGE_OK (-> static-VolatileImage IMAGE_OK))




;; <c-window> is the "parent", and therefore implements "repaint"

(def-area-subclass (<c-window> width height)

  ;;(define (paint-internal g)
  ;;  (-> base-area paint-internal g x-repaint y-repaint x2-repaint y2-repaint))

  (define parent this)
  
  (def-method (force-paint)
    (my-paint-internal-default)
    (-> ll-window repaint)
    (blit-me)
    )
  

  (define ll-window #f)
  (def-var ll-window-g #f)

  (def-method (get-frame)
    ll-window)

  (define insets-x 0)
  (define insets-y 0)
  (define insets-width width)
  (define insets-height height)

  (define (get-graphics)
    (when (not ll-window-g)
	  (set! ll-window-g (<my-graphics> (-> ll-window getGraphics) ll-window))
	  (let ((insets (-> ll-window insets)))
	    (set! insets-x (+ 1 (-> insets left)))
	    (set! insets-y (+ 1 (-> insets top)))
	    (set! insets-width (- width (-> insets left) (-> insets right) 3))
	    (set! insets-height (- height (-> insets top) (-> insets bottom) 1 -20))))
    (-> ll-window-g setClip insets-x insets-y insets-width (+ 20 insets-height))
    ll-window-g)

  (define (blit-me)
    ;;(c-display "I am blitting")
    (validate-image)
    (-> (get-graphics) drawImage image 0 0 ll-window)
    (if (-> image contentsLost)
	(blit-me)))


  (def-var x-repaint #f)
  (define  y-repaint #f)
  (define x2-repaint #f)
  (define y2-repaint #f)

  (def-method (repaint x* y* x2* y2*)
    ;;(c-display "repaint asked to repaint" x* y* x2* y2*)
    (if x-repaint
	(begin
	  (set! x-repaint (min x* x-repaint))
	  (set! y-repaint (min y* y-repaint))
	  (set! x2-repaint (max x2* x2-repaint))
	  (set! y2-repaint (max y2* y2-repaint)))
	(begin
	  (set! x-repaint x*)
	  (set! y-repaint y*)
	  (set! x2-repaint x2*)
	  (set! y2-repaint y2*))))



  ;; Background image (buffered)
  ;;;;;;;;;;;;;;;;;;;
  (def-var background-image #f)

  (define (get-background-image-url)
    (cond ((get-config-string 'background-image)
           (c-display "  !!!! TRYING TO LOAD BACKGROUND " (config-full-url (get-config-string 'background-image))
                      (-> (new <java.lang.String> (config-full-url (get-config-string 'background-image)))
                          startsWith "http://"))
           (if (or (-> (new <java.lang.String> (config-full-url (get-config-string 'background-image)))
                       startsWith "http://")
                   (-> (new <java.lang.String> (config-full-url (get-config-string 'background-image)))
                       startsWith "file:"))
               (new <java.net.URL> (config-full-url (get-config-string 'background-image)))
               (config-full-url (get-config-string 'background-image))))
          ((or is-running-applet is-running-standalone)
           (new <java.net.URL> (<-> urlbase "hurtigmixer_bakgrunn.png")))
          (else
           "hurtigmixer_bakgrunn.png")))

  (def-method (paint-background g x y x2 y2)
    (when (not background-image)
      (let ((insets (-> ll-window insets)))

        (define x-offset 0)
        (define y-offset 0)

        (when (get-config-area-list-with-help 'background-image-pos)
          (set! x-offset (car (get-config-area-list-with-help 'background-image-pos)))
          (set! y-offset (cadr (get-config-area-list-with-help 'background-image-pos))))
        
        (set! background-image (-> g get-image (get-background-image-url)
                                   ':ret-image (create-image width height #t)
                                   ':width (+ width (-> insets left))
                                   ':height (+ height (-> insets top))
                                   ':x-pos (- (-> insets left) x-offset)
                                   ':y-pos (- (-> insets top) y-offset)))))
    
    (if debug-background-painting
	(let ((colors (list white-color green-color red-color blue-color gray-color yellow-color)))
	  (-> g fill-rect (nth (c-integer (rand-between 0 (length colors))) colors) x y (- x2 x) (- y2 y))
	  )
	(-> g blit-sub-image background-image x y x2 y2))

    )


  (def-method (paint-background-old g x y x2 y2)
    (when (not background-image)
      (set! background-image (create-image width height #t))
      
      (c-display 'qERGQERGQERGQERGQERG1111)
      (define background-image-g (<my-graphics> (-> background-image getGraphics) ll-window))
      (c-display "   " (get-config-string 'background-image) 'qERGQERGQERGQERGQERG2222)
      (define background-temp (-> (-> (new-static <java.awt.Toolkit>) getDefaultToolkit) getImage (get-background-image-url)))
      ;;(c-display "background-temp: " background-temp)
      
      (-> Various waitForImage background-temp)
      ;;(-> background-image-g blit-sub-image background-temp x y x2 y2)
      (let ((insets (-> ll-window insets)))
        (-> background-image-g draw-image background-temp (-> insets left) (-> insets top)))
      (-> background-image-g dispose)
      (-> background-temp flush))
    
    (if debug-background-painting
	(let ((colors (list white-color green-color red-color blue-color gray-color yellow-color)))
	  (-> g fill-rect (nth (c-integer (rand-between 0 (length colors))) colors) x y (- x2 x) (- y2 y))
	  )
	(-> g blit-sub-image background-image x y x2 y2))

    )



  ;; Intermediate image (volatile)
  ;;;;;;;;;;;;;;;;;;;;;
  (def-var image #f)
  (def-var g #f)

  ;; Uncomment to ensure perfect graphics. Not very efficient since it draws up everything twice. (doesn't work anymore since blitting has been delegated to the sub-areas)
  ;;(define (paint g x* y* x2* y2*)
  ;;  (paint-background g x* y* x2* y2*))
  
  (define (create-image!)    
    (if image
	(-> image flush)) ;; don't remove!
    (set! image (create-image width height #f))
    (-> image setAccelerationPriority 1)
    (set! g (<my-graphics> (-> image getGraphics) ll-window))
    (paint-background g 0 0 width height)
    image)


  (def-method (validate-volatile-image image paint-me-func create-me-func)
    (let ((valcode (-> image validate (-> ll-window getGraphicsConfiguration))))
      (cond ((= valcode IMAGE_RESTORED)
	     (c-display "IMAGE_RESTORED")
	     (paint-me-func)
	     (validate-volatile-image image paint-me-func create-me-func))
	    ((= valcode IMAGE_INCOMPATIBLE)
	     (c-display "IMAGE_INCOMPATIBLE")
	     (set! image (create-me-func))
	     (paint-me-func)
	     (validate-volatile-image image paint-me-func create-me-func)))))

  (define (validate-image)
    (validate-volatile-image image
			     my-paint-internal-default
			     create-image!))

  (define (my-paint-internal x* y* x2* y2*)
    (paint-internal g x* y* x2* y2* 0 0 width height)
    (-> g setClip 0 0 width height)
    ;;(-> mixer-area paint-cursor g)
    ;;(-> g draw-rect black-color (1+ (-> insets left)) (1+ (-> insets top)) 200 200)
    )

  (define (my-paint-internal-default)
    (my-paint-internal 0 0 width height))

  (define post-paint-func #f)
  (def-method (add-post-paint func)
    (set! post-paint-func func))

  (define self-blitting-areas '())
  (def-method (add-self-blitting-area! area)
    (push! area self-blitting-areas))

  (def-method (area-only-inside-a-self-blitting-area? x y x2 y2)
    (let loop ((areas self-blitting-areas))
      (define area (and (pair? areas) (car areas)))
      (cond ((null? areas)
	     #f)
	    ((-> area get-position (lambda (x* y* x2* y2* width height)
				     (and (>= x   x*)
					  (<  x2 x2*)
					  (>=  y  y*)
					  (<  y2 y2*))))
	     #t)
	    (else
	     (loop (cdr areas))))))
    
  (def-method (check-paint)
    (when x-repaint
      (validate-image)
      ;;(c-display "check-paint " x-repaint y-repaint x2-repaint y2-repaint)
      (when (not (area-only-inside-a-self-blitting-area? x-repaint y-repaint x2-repaint y2-repaint))
	;;(c-display "not in a self-blitting area" self-blitting-areas)
	(paint-background g x-repaint y-repaint x2-repaint y2-repaint))
      
      (my-paint-internal x-repaint y-repaint (+ 1 x2-repaint) (+ 1 y2-repaint))
      
      ;;(-> (get-graphics) blit-sub-image image x-repaint y-repaint x2-repaint y2-repaint)
      
      (if (not (area-only-inside-a-self-blitting-area? x-repaint y-repaint x2-repaint y2-repaint))
	  (-> (get-graphics) blit-sub-image image x-repaint y-repaint x2-repaint y2-repaint))
      
      ;;(c-display 'u5 image x-repaint y-repaint x2-repaint y2-repaint)
      (blit-internal (get-graphics) image x-repaint y-repaint x2-repaint y2-repaint)
      ;;(-> (get-graphics) draw-rect blue-color x-repaint y-repaint (- x2-repaint x-repaint) (- y2-repaint y-repaint))
      
      (if (-> image contentsLost)
	  (begin
	    (c-display "CONTENTS LOST")
	    (check-paint))
	  (begin
	    (set! x-repaint #f)
	    
	    (if post-paint-func
		(let ((f post-paint-func))
		  (set! post-paint-func #f)
		  ;;(c-display "post-painting" x-repaint)
		  (f (get-graphics))
		  ;;(c-display "post-painting finished" x-repaint)
		  ))
	    ))
      )
    )
    
  
  (def-method (create-image width height :optional buffered)
    (if use-buffered-images
	(set! buffered #t))
    ;;(set! buffered #t)
    (let ((ret (if buffered
		   (if #t
		       (if #t
			   (if #t
			       (-> (-> ll-window getGraphicsConfiguration) createCompatibleImage width height (-> green-color TRANSLUCENT))
			       (-> (-> ll-window getGraphicsConfiguration) createCompatibleImage width height))
			   (-> ll-window createImage width height))
		       (new <java.awt.image.BufferedImage> width height (-> (new-static <java.awt.image.BufferedImage>) TYPE_INT_RGB)))
		   (if #f
		       (-> (-> ll-window getGraphicsConfiguration) createCompatibleImage width height (-> green-color TRANSLUCENT))
		       (if #t
			   (if #t
			       (-> (-> ll-window getGraphicsConfiguration) createCompatibleVolatileImage width height (-> green-color TRANSLUCENT))
			       (-> ll-window createVolatileImage width height))
			   (-> ll-window createImage width height))))))
      (when buffered
	(-> ret add-method 'contentsLost (lambda ()
					   #f))
	(-> ret add-method 'validate (lambda x
				       (+ IMAGE_RESTORED IMAGE_INCOMPATIBLE 2000))))
      ;;(c-display "I got the image: " (<-o ret))
      ret))


  ;;;;;;;;;;;;;; Mouse stuff ;;;;;;;;;;;;;;;;;;;
  ;; Note that drag/release may not return!.
  (define curr-mouse-cycle #f)
  (def-method (mouse-press button x* y*)
    ;;(c-display "mouse-press" curr-mouse-cycle)
    (if (not curr-mouse-cycle)
	(set! curr-mouse-cycle (get-mouse-cycle button x* y*))))
  (def-method (mouse-drag button x* y*)
    (if curr-mouse-cycle
	((cadr curr-mouse-cycle) button x* y*)))
  (def-method (mouse-release button x* y*)
    ;;(c-display "mouse-release enter" curr-mouse-cycle)
    (let ((mouse-cycle curr-mouse-cycle))
      (set! curr-mouse-cycle #f)
      (if mouse-cycle
	  ((caddr mouse-cycle) button x* y*)))
    ;;(c-display "mouse-release leave" curr-mouse-cycle)
    )
  
  
  (def-var curr-key #f)
  (def-var last-input-event #f)
  (def-method (control-pressed?)
    (and last-input-event
	 (-> last-input-event isControlDown)))
  (def-method (shift-pressed?)
    ;;(c-display "shift-pressed?" last-input-event (-> last-input-event isShiftDown))
    (and last-input-event
	 (-> last-input-event isShiftDown)))

  (def-var last-button 0)

  (define (create-mouse-handler callback)
    (define BUTTON1 #f)
    (define BUTTON2 #f)
    (lambda (mouse-event)
      (let ((jc (<java-class> mouse-event)))
	(when (not BUTTON1)
	  (set! BUTTON1 (-> jc BUTTON1))
	  (set! BUTTON2 (-> jc BUTTON2)))
	(set! last-input-event jc)
	(define button (-> jc getButton))
        (set! last-button (cond ((= button BUTTON2) *middle-mouse-button*)
                                ((and (shift-pressed?)
                                      (= button BUTTON1))
                                 *right-mouse-button*)
                                ((= button BUTTON1)
                                 *left-mouse-button*)
                                (else
                                 *right-mouse-button*)))
	(callback last-button
		  (-> jc getX)
		  (-> jc getY))
	;;(c-display "before check-paint")
	(check-paint)
	;;(c-display "after check-paint")
	)))
		    
  (set! ll-window (ll-create-window width height
				    (lambda (g*)
				      (if is-running-applet
					  (c-display "Repainted" ll-window image))
				      (if (and g ll-window image)
					  (let loop ()
					    (validate-image)
					    (paint-background (get-graphics) insets-x insets-y (+ insets-x insets-width) (+ insets-y insets-height)) ;;insets-x insets-y 

					    (blit-internal (get-graphics) image insets-x insets-y (+ insets-x insets-width) (+ insets-y insets-height)) ;;insets-x insets-y 
					    (if (-> image contentsLost)
						(loop)))))
				    
				    (create-mouse-handler mouse-press)
				    (create-mouse-handler mouse-drag)
				    (create-mouse-handler mouse-release)
				    (lambda (mouse-wheel-event)	
				      (let ((jc (<java-class> mouse-wheel-event)))
					(set! last-input-event jc)
					(mouse-press (if (> (-> jc getWheelRotation()) 0)
							 4
							 5)
						     (-> jc getX)
						     (-> jc getY))
					(check-paint)))			      

				    (lambda (key-event)
				      (let ((jc (<java-class> key-event)))
					(set! last-input-event jc)
					(set! curr-key (-> jc getKeyCode))
					(key-pressed-internal jc)
					(check-paint)))
				    (lambda (key-event)
				      (let ((jc (<java-class> key-event)))
					(set! last-input-event jc)
					(set! curr-key #f)
					(key-released-internal jc)
					(check-paint)))
				    ))
  

  (def-method (repaint-all)
    (-> ll-window repaint 0 0 width height))

  (create-image!)

  (repaint-all)

  )


