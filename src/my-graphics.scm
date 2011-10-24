


;; Note, it's not slower to access my-graphics instead of graphics!
;; (all functions are looked up from a hash table anyway)


(define static-AlphaComposite (new-static <java.awt.AlphaComposite>))
(define SRC-OVER (-> static-AlphaComposite SRC_OVER))  
(define get-alpha-func (<- static-AlphaComposite getInstance))
(define (get-alpha alpha-val)
  (get-alpha-func SRC-OVER alpha-val))

(define alpha-1.00 (-> static-AlphaComposite SrcOver))
(define alpha-0.75 (get-alpha 0.75))
(define alpha-0.50 (get-alpha 0.50))
(define alpha-0.40 (get-alpha 0.40))
(define alpha-0.25 (get-alpha 0.25))
(define alpha-0.15 (get-alpha 0.15))
(define alpha-0.10 (get-alpha 0.10))
(define alpha-0.075 (get-alpha 0.075))
(define alpha-0.05 (get-alpha 0.05))

(define really-do-alpha #t)

(define font8 (-> Various getFont "Default" 8))
(define font-default #f)



;; Note that (-> Various getFont) can't be used since java's Font constructor can't take URL as argument. Font.createFont can however.

(c-define (get-font filename size)
  (define f (new-static <java.awt.Font>))
  
  (define nf (-> f createFont (-> f TRUETYPE_FONT) (if (or is-running-applet
                                                           is-running-standalone)
                                                       (-> (new <java.net.URL> (<-> urlbase filename)) openStream)
                                                       (-> (new <java.net.URL> (<-> src-directory filename)) openStream))))
  (set! nf (-> nf deriveFont (-> f PLAIN) size))
  nf
  )



(def-class (<my-graphics> g image-observer)
  
  (Super g)
  
  ;;(set! font-default (-> g getFont))
  
  (let ((renderinghints (new-static <java.awt.RenderingHints>)))
    (-> g setRenderingHint 
	(-> renderinghints KEY_TEXT_ANTIALIASING)
	(-> renderinghints VALUE_TEXT_ANTIALIAS_ON)))

  (define font-ascent 0)
  
  (define default-transform (-> g getTransform))
  (define alpha-composite #f)
  (define alpha-src-over (-> static-AlphaComposite SrcOver))
  (define last-alpha -1)
  
  ;; Getting these procedures before they are used is important so that the program won't hang a little bit the first time they are used.
  (define set-color (let ((func (<- g setColor)))
                      (lambda (color)
                        (if color
                            (func color)))))
  (define set-composite (<- g setComposite))
  (define fill-rect-func (<- g fillRect))
  (define fill-round-rect-func (<- g fillRoundRect))
  (define draw-rect-func (<- g drawRect))
  (define fill-arc-func (<- g fillArc))
  (define draw-arc-func (<- g drawArc))
  (define draw-image-func (<- g drawImage))
  (define draw-polyline-func (<- g drawPolyline))
  (define fill-polygon-func (<- g fillPolygon))
  
  (define draw-string-func (<- g drawString))
  
  ;;(def-method (set-color color)
  ;;  (set-color-func set-color))

  (define last-derived-which-font #f)
  (define last-derived-size -2000)
  (define last-derived-font #f)
  (def-method (derive-font size :key (which-font font-default))
    (if (not (and (eq? last-derived-which-font which-font)
		  (= last-derived-size size)))
	(set! last-derived-font (-> which-font deriveFont (-> which-font PLAIN) size)))
    last-derived-font)

  (def-method (set-font! :key
			 (filename  "uni05_54.ttf")
			 (size 8))
    
    (define nf (get-font filename size))

    (-> g setFont nf)
    (set! font-default nf))
  
  (if (not font-default)
      (set-font!)
      (-> g setFont font-default))

  (def-method (draw-string-in-box text color which-font x y width height )
    (define border (min (/ height 10) (/ width 10)))

    (inc! x border)
    (inc! y border)
    (inc! width (* -2 border))
    (inc! height (* -2 border))
    
    (define x2 (+ x width))
    (define y2 (+ y height))

    (define font (derive-font height ':which-font which-font))
    (define rect (-> font getStringBounds text (-> g getFontRenderContext)))

    (define layout (new <java.awt.font.TextLayout> text font (-> g getFontRenderContext)))
    (define rect (-> layout getBounds))

    (define mx (/ width (-> rect getWidth)))
    (define my (/ height (-> rect getHeight)))

    (define use-x (- x 5))
    (define use-y2 (- y2 1))

    ;;(c-display "mx/my" mx my " widht/height" (-> rect getWidth) (-> rect getHeight))

    (set-color color)

    (-> g setFont font)

    '(begin
       (-> g setClip 0 0 2000 2000)
       (draw-rect blue-color x y width height)
       (draw-rect blue-color (+ x 5) (+ y 5) (- width 10) (- height 10))
       (-> g drawLine x y x2 y2)
       (-> g drawLine x2 y x y2))

    (-> g scale mx my)
    (draw-string-func text (/ use-x mx) (/ use-y2 my))

    (-> g setTransform default-transform)

    (-> g setFont font-default)
    )

  
  (def-method (draw-string text color x y)
    (set-color color)
    ;;(draw-string-func text (+ 2 x) (+ 16 y))
    ;;(draw-string-func text (+ 2 x) (+ 6 y))
    ;;(draw-string-func text (+ 2 x) (+ y font-ascent))
    ;;(set-font!)
    (if *running-hurtigmixer*
        (draw-string-func text (+ 2 x) (+ y 6))
        (draw-string-func text (+ 2 x) (+ y font-ascent)))
    )

  ;;(-> g drawString (seconds->timestring time) x* (- y2 (-> font ROMAN_BASELINE) 2))


  ;; Warning draw-line draws pixels including (x2,y2)! (a vertical line must have y=y2 and a horizontal line must have x=x2!)
  (def-method (draw-line color x y x2 y2)
    (set-color color)
    (-> g drawLine x y x2 y2))

  (def-method (do-font font func)
    (define old-ascent font-ascent)
    (define old-font font-default)
    (-> g setFont font)
    (set! font-ascent (-> (-> g getFontMetrics)
                          getAscent))

    (func)
    (-> g setFont old-font)
    (set! font-ascent old-ascent)
    )

  (def-method (do-alpha alpha func)    
    (if really-do-alpha
	(begin
	  (set-composite (if (number? alpha)
			     (begin
			       (when (not (= alpha last-alpha))
				     (set! alpha-composite (get-alpha alpha))
				     (set! last-alpha alpha))
			       alpha-composite)
			     alpha))
	  
	  (func)
	  
	  (set-composite alpha-src-over))
	(func)))

  (def-method (do-antialized func)
    (define renderinghints (new-static <java.awt.RenderingHints>))
    (define oldhint (-> g getRenderingHint 
			(-> renderinghints KEY_ANTIALIASING)))

    (-> g setRenderingHint 
	(-> renderinghints KEY_ANTIALIASING)
	(-> renderinghints VALUE_ANTIALIAS_ON))
    
    (func)

    (-> g setRenderingHint
	(-> renderinghints KEY_ANTIALIASING)
	oldhint))


  (def-method (fill-rect color x y width height)
    (set-color color)
    (fill-rect-func x y width height))

  (def-method (fill-rect-abs color x y x2 y2)
    (fill-rect color x y (- x2 x -1) (- y2 y -1)))

  (def-method (fill-round-rect color x y width height arc-width arc-height)
    (set-color color)
    (fill-round-rect-func x y width height arc-width arc-height))

  (def-method (draw-rect color x y width height)
    (set-color color)
    (draw-rect-func x y (1- width) (1- height)))

  (def-method (draw-rect-abs color x y x2 y2)
    (set-color color)
    (draw-rect-func x y (- x2 x 1) (- y2 y 1)))

  (def-method (draw-arc color x y width height start-angle arc-angle)
    (set-color color)
    (draw-arc-func x y width height start-angle arc-angle))

  (def-method (fill-arc color x y width height start-angle arc-angle)
    (set-color color)
    (fill-arc-func x y width height start-angle arc-angle))

  (define (get-image-url-or-string url-or-string)
    (cond ((not (string? url-or-string))
           url-or-string)
          ((or (-> (new <java.lang.String> (config-full-url url-or-string))
                   startsWith "http://")
               (-> (new <java.lang.String> (config-full-url url-or-string))
                   startsWith "file:"))
           (new <java.net.URL> (config-full-url url-or-string)))
          ((or is-running-applet is-running-standalone)
           (new <java.net.URL> (<-> urlbase url-or-string)))
          (else
           url-or-string)))


  (def-method (get-image url-or-string :key ret-image width height (x-pos 0) (y-pos 0))
    (set! url-or-string (get-image-url-or-string url-or-string))
    (c-display "trying to get image" url-or-string)
    (define toolkit-image (-> (-> (new-static <java.awt.Toolkit>) getDefaultToolkit) getImage url-or-string))
    (-> Various waitForImage toolkit-image)
    (if (not width)
        (set! width (-> toolkit-image getWidth image-observer)))
    (if (not height)
        (set! height (-> toolkit-image getHeight image-observer)))
    (if (not ret-image)
        (set! ret-image (new <java.awt.image.BufferedImage> width height (-> (new-static <java.awt.image.BufferedImage>) TYPE_INT_RGB))))
    (let ()
      (define ret-g (<my-graphics> (-> ret-image getGraphics) image-observer))
      (-> ret-g draw-image toolkit-image x-pos y-pos)
      (-> ret-g dispose)
      (-> toolkit-image flush)
      ret-image))
  
  (def-method (get-texturepaint url-or-string)
    (let ((image (get-image url-or-string)))
      (c-display "width/length " (-> image getWidth image-observer) (-> image getHeight image-observer))
      (new <java.awt.TexturePaint> image (new <java.awt.Rectangle> 0 0 1000 1000))));(-> image getWidth image-observer) (-> image getHeight image-observer)))))

  (def-method (do-painter painter func)
    (let ((old-painter (-> g getPaint)))
      (-> g setPaint painter)
      ;;(-> g setPaint texturepaint)
      (func)
      (-> g setPaint old-painter)))
  
  (def-method (do-stroke stroke-width func)
    (let ((old-stroke (-> g getStroke)))
      (-> g setStroke (new <java.awt.BasicStroke> stroke-width))
      (func)
      (-> g setStroke old-stroke)))
    
  (def-method (make-poly len getXY)
    (define xPoly (java-array-new <jint> len))
    (define yPoly (java-array-new <jint> len))
    (c-for 0 < len 1
           (lambda (i)
             (getXY i (lambda (x y)
                        ;;(c-display "i/len:" i len "x/y:" x y)
                        (java-array-set! xPoly i (->jint x))
                        (java-array-set! yPoly i (->jint y))))))
    (list xPoly yPoly len))

  (def-method (set-poly! poly pos x y)
    (java-array-set! (car poly) pos (->jint x))
    (java-array-set! (cadr poly) pos (->jint y)))
    
  (def-method (draw-polyline color poly)
    (set-color color)
    (apply draw-polyline-func poly))

  (def-method (fill-polygon color poly)
    (set-color color)
    (apply fill-polygon-func poly))

  (def-method (draw-polyline-old border-color fill-color len getXY)
    (define xPoly (java-array-new <jint> len))
    (define yPoly (java-array-new <jint> len))
    (c-for 0 < len 1
           (lambda (i)
             (getXY i (lambda (x y)
                        ;;(c-display "x/y" x y)
                        (java-array-set! xPoly i (->jint x))
                        (java-array-set! yPoly i (->jint y))))))
    (-> g setStroke (new <java.awt.BasicStroke> 2.5))
    (set-color fill-color)
    (-> g setPaint texturepaint)
    (fill-polygon-func xPoly yPoly len)
    (set-color border-color)
    (draw-polyline-func xPoly yPoly len))

  (define xPoly (java-array-new <jint> 3))
  (define yPoly (java-array-new <jint> 3))
  (def-method (fill-triangle color x1 y1 x2 y2 x3 y3)
    (java-array-set! xPoly 0 (->jint x1))
    (java-array-set! xPoly 1 (->jint x2))
    (java-array-set! xPoly 2 (->jint x3))
    (java-array-set! yPoly 0 (->jint y2))
    (java-array-set! yPoly 1 (->jint y2))
    (java-array-set! yPoly 2 (->jint y3))
    (set-color color)
    (fill-polygon-func xPoly yPoly 3))


  ;;(define image-observer-null (new-static <java.awt.Component>))

  (define max-x2 #f)
  (define max-y2 #f)


  (def-method (blit-sub-image image x1 y1 x2 y2)
    (when win
	  (when (not max-x2)
		(set! max-x2 (-> win x2))
		(set! max-y2 (-> win y2)))

	  (if (> x2 max-x2)
	      (set! x2 max-x2))
	  (if (> y2 max-y2)
	      (set! y2 max-y2)))

    ;;(c-display "blitting" x1 y1 x2 y2 "width:" (- x2 x1) "height:" (- y2 y1))
    ;;(-> g setClip x1 y1 x2 y2)
    ;;(-> g setClip 0 0 x2 y2)

    (if (not (draw-image-func image
;			 0 0
;			 0 0 x2 y2
;			 0 0 x2 y2
			 x1 y1 x2 y2
			 x1 y1 x2 y2
			 ;;(java-null (java-class 'java.awt.Component))
			 ;;image-observer-null
			 image-observer
			 ))
	(c-display "BLIT_SUB_IMAGE Not returned!!!")))
    
  (def-method (draw-image image x y)
    (draw-image-func image x y image-observer))

  )


