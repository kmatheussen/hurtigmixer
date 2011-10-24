



(define default-press-color blue-color)


(def-area-subclass (<pos-button> parent width height report-func :key report-releasebutton gfx-pressed-filename (do-paint2 #t) (press-color default-press-color))
  
  (def-var press-color)

  (define is-pressed #f)

  (define is-URL? (and gfx-pressed-filename
                       (or is-running-applet
                           is-running-standalone
                           (-> (new <java.lang.String> (config-full-url gfx-pressed-filename))
                               startsWith "http://"))))

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

  (if pressed-image (-> Various waitForImage pressed-image))

  (define (paint g x* y* x2* y2*)
    (when #f
      (-> g setColor blue-color)
      (if is-pressed
          (-> g setXORMode white-color))
      (-> parent paint-background g x y (1+ x2) (1+ y2))
      (if is-pressed
          (-> g setPaintMode)))
    (if do-paint2
        (cond ((and is-pressed pressed-image)
               (-> g draw-image pressed-image x y))
              (is-pressed
               (-> parent paint-background g x y x2 y2)
               (-> g do-antialized
                   (lambda ()
                     (-> g do-alpha alpha-0.05
                         (lambda ()
                           (-> g fill-round-rect press-color x y (- width 2) (- height 2) 14 14))))))
              (else
               (-> parent paint-background g x y x2 y2))))

    ;;(let ((colors (list white-color green-color red-color blue-color gray-color yellow-color)))
    ;;   (-> g draw-rect (nth (c-integer (rand-between 0 (length colors))) colors) x y (- x2 x) (- y2 y))
    ;;   )
    )
  

  (add-mouse-cycle (lambda (button x* y*)
		     (if (or (= button *left-mouse-button*)
			     (= button *right-mouse-button*))
			 (begin
			   (set! is-pressed #t)
			   (repaint-me!)
			   (-> parent check-paint)
			   (cond ((not report-releasebutton)
                                  (report-func)
                                  'grab)
                                 (else
                                  'grab)))
			 (begin
			   (report-func)
			   #f)))
		   (lambda (button x* y*)
		     (if (and (not is-pressed)
			      (>= x* x)
			      (>= y* y)
			      (< x* x2)
			      (< y* y2))
			 (begin
			   (set! is-pressed #t)
			   (repaint-me!))
			 (if (and is-pressed
				  (or (< x* x)
				      (>= x* x2)
				      (< y* y)
				      (>= y* y2)))
			     (set! is-pressed #f)
			     (repaint-me!))))
		   (lambda (button x* y*)
		     (define was-pressed is-pressed)
		     (set! is-pressed #f)
		     (repaint-me!)
		     (-> parent check-paint)
		     (if (and was-pressed
			      report-releasebutton
			      (>= x* x)
			      (>= y* y)
			      (< x* x2)
			      (< y* y2))
			 (report-func)))))


(define* (<pos-button/add-sub-area> parent parent-area pos report-func :key report-releasebutton)
  (define pos-button (<pos-button> parent (1+ (pos-width pos)) (1+ (pos-height pos)) report-func ':report-releasebutton report-releasebutton))
  (-> parent-area add-sub-area pos-button (pos-x pos) (pos-y pos))
  pos-button)

