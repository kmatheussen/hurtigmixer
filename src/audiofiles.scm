

(define af-wave-color black-color)
(define af-text-color red-color)
(define renderinghints (new-static <java.awt.RenderingHints>))

(define audiofiles (make-hash-table 519))


(define java.lang.Math (new-static <java.lang.Math>))

(define (rand-between x1 x2)
  (c-scale (-> java.lang.Math random) 0 1 x1 x2))
(define (coin)
  (if (> (-> java.lang.Math random) 0.5)
      #t
      #f))

(define (generate-wave-color-vals-white-background)
  (define ext-color (c-integer (rand-between 0 2.999)))
  (append (map (lambda (i)
		 (* (if (= i 0)
			0.85
			1.0)
		    (if (or (= i ext-color)
			    (> (-> java.lang.Math random) 0.9))
			(if (coin)
			    0.6
			    0.99)
			(rand-between 0.73 0.95))))
	       (iota 3))
	  '()))
(define (generate-wave-color-vals)
  (define ext-color (c-integer (rand-between 0 2.999)))
  (append (map (lambda (i)
		 (* (if (= i 0)
			0.85
			1.0)
		    (if (or (= i ext-color)
			    (> (-> java.lang.Math random) 0.9))
			(if (coin)
			    0.1
			    0.99)
			(rand-between 0.03 0.95))))
	       (iota 3))
	  '()))
;	  (list (rand-between 0.1 0.9))))
	  ;;(list (rand-between 0.8 0.9))))


;; Force loading of the MyAudioFileBuffer class.
(get-java-classholder 'MyAudioFileBuffer)


(def-class (<audiofile> filename_or_URL)
  (def-var filename filename_or_URL)

  (def-var audio-file-info (-> sound-holder getAudioFileInfo filename_or_URL 44100))
  ;;(c-display "null? " audio-file-info (java-null? audio-file-info))
  (define audiofile-buffer (and audio-file-info (begin (c-display "audio-file-info: " audio-file-info #t) (new <MyAudioFileBuffer> audio-file-info))))

  (def-var rgb (generate-wave-color-vals))
  (def-var background-color (-> Various getColor
				(car rgb) (cadr rgb) (caddr rgb)))

  (def-method (get-scaled-color scale)
    (if (> scale 1)
	(set! scale (sqrt scale)))
    (apply (<- Various getColor) (map (lambda (c)
					(min 0.99 (max 0 (* scale  c))))
				      rgb)))
  
  (if audiofile-buffer
      (Super audiofile-buffer))

  (define visitors 0)

  (def-method (add-visitor)
    ;;(c-display "add-vistitor" visitors)
    (inc! visitors 1))

  (def-method (remove-visitor)
    (dec! vistiors 1)
    (when (= 0 visitors)
	  (-> image flush)
	  (set! image #f)))

  (if (not audiofile-buffer)
      (set! this #f))

  )



(c-define* (get-audiofile filename callback progressfunc :key is-URL)
  ;;(c-display "get-audiofile start")
  (let ((callbacks (new <MyCallbacks> 
			(java-wrap
			 (lambda (foundit)
			   (c-display 1)
			   (if foundit
			       (let ((audiofile (if (hashq-ref audiofiles (string->symbol filename))
						    (hashq-ref audiofiles (string->symbol filename))
						    (<audiofile> filename))))
				 (c-display 2)
				 (when audiofile
				   (hashq-set! audiofiles (string->symbol filename) audiofile)
				   (-> audiofile add-visitor))
				 (c-display 3)
				 (callback audiofile)
                                 (c-display 4)
                                 )
			       (callback #f))))
			(java-wrap progressfunc))))
    (c-display "adding sound")
    (-> sound-holder addSound filename 44100 callbacks callbacks)))


#|
Nope. Can't wait here.
(c-define (get-audiofile-now parent filename)
  (define ret #f)
  (get-audiofile parent filename (lambda (x) (set! ret x)) (lambda (seconds end-time)
							     (c-display seconds "/" end-time)))
  (-> sound-holder waitForSound filename)
  ret)
|#


(c-define (remove-audiofile audiofile)
  (-> audiofile remove-visitor))

