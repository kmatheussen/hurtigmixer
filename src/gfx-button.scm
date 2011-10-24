


(def-area-subclass (<gfx-button> parent gfx-filename report-func :key report-releasebutton gfx-pressed-filename gfx-release-filename is-URL? (width 50) (height 50))

  (def-var is-pressed-alpha #f)

  (define is-pressed #f)

  (define image (-> (-> (new-static <java.awt.Toolkit>) getDefaultToolkit) getImage 
		    (cond ((or is-URL?
			       (-> (new <java.lang.String> gfx-filename) startsWith "http://"))
			   (new <java.net.URL> gfx-filename))
			  ((-> (new <java.lang.String> gfx-filename) startsWith "/")
			   (new <java.net.URL> (<-> "file:" gfx-filename)))
			  ((or is-running-applet is-running-standalone)
			   (new <java.net.URL> (string-append urlbase gfx-filename)))
			  (else
			   (new <java.net.URL> (<-> src-directory gfx-filename))))))

  (define pressed-image (and gfx-pressed-filename
                             (-> (-> (new-static <java.awt.Toolkit>) getDefaultToolkit) getImage 
                                 (cond ((or is-URL?
                                            (-> (new <java.lang.String> gfx-pressed-filename) startsWith "http://"))
                                        (new <java.net.URL> gfx-pressed-filename))
                                       ((-> (new <java.lang.String> gfx-pressed-filename) startsWith "/")
                                        (new <java.net.URL> (<-> "file:" gfx-pressed-filename)))
                                       ((or is-running-applet is-running-standalone)
                                        (new <java.net.URL> (string-append urlbase gfx-pressed-filename)))
                                       (else
                                        (new <java.net.URL> (<-> src-directory gfx-pressed-filename)))))))

  (def-method (add-to-parent parent x y x2 y2)
    (-> parent add-sub-area this
        (- (average x x2) (/ width 2))
        (- (average y y2) (/ height 2))))

  (c-display "IMAGE: " image)
  (-> Various waitForImage image)
  (if pressed-image (-> Various waitForImage pressed-image))

  (set! height (-> image getHeight (-> parent get-frame)))
  (set! width (-> image getWidth (-> parent get-frame)))
  (set! y2 (+ y height))
  (set! x2 (+ x width))

  (define (paint g x* y* x2* y2*)
    ;;(c-display "hello!" gfx-filename)
    (-> g draw-image image x y)
    ;;(-> g draw-rect black-color x y width  height)
    (when (and is-pressed-alpha 
	       is-pressed)
      (-> g do-alpha alpha-0.25
	  (lambda ()
	    (-> g fill-rect blue-color x y x2 y2))))
    )


  (add-mouse-cycle (lambda (button x* y*)
		     (c-display "mouse-press!" gfx-filename)
		     (set! is-pressed #t)
		     (if (not report-releasebutton)
			 (report-func))
		     (when (or is-pressed-alpha 
			       gfx-pressed-filename)
		       (-> parent repaint x y x2 y2))
		     'grab)
		   (lambda (button x* y*)
		     #f)
		   (lambda (button x* y*)
		     (set! is-pressed #f)
		     (if report-releasebutton
			 (report-func))
		     (when (or is-pressed-alpha 
			       gfx-release-filename)
		       ;;(set! background-color nonpressed-bg-color)
		       (-> parent repaint x y x2 y2))))
  
  )



