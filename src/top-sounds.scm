
;;(define top-sounds-URL "http://www.notam02.no/hurtigmikser/lydsettet/")


(c-define (sound-icon parent image-url sound-url x y :key mid-x mid-y)
  
  (define button (<gfx-button> parent image-url (lambda x x)
			       ':is-URL? (or is-running-applet
					     is-running-standalone
					     (-> (new <java.lang.String> (config-full-url image-url))
						 startsWith "http://"))))

  (-> button mouse-cycles '())
  
  (define move-button (<gfx-button> parent image-url (lambda x x)
				    ':is-URL? (or is-running-applet
						  is-running-standalone
						  (-> (new <java.lang.String> (config-full-url image-url))
						      startsWith "http://"))))

  (define start-x 0)
  (define start-y 0)
  (define last-dx 0)
  (define last-dy 0)
  (apply (<- button add-mouse-cycle)
	 (delta-mouse-cycle parent
			    (lambda (button-num x* y*)
			      (-> button get-position (lambda (x y x2 y2 width height)
							(set! start-x x)
							(set! start-y y)
							(c-display "Hai!" x* y*)
							(-> parent add-sub-area move-button x y)))
			      'grab)
			    (lambda (button-num dx dy)
			      ;;(c-display "moved!" dx dy)
			      (set! last-dx dx)
			      (set! last-dy dy)
			      (-> move-button repaint-me!)
			      (-> move-button set-position! (+ dx start-x) (+ dy start-y))
			      (-> move-button repaint-me!))
			    (lambda (button-num dx dy)
			      (c-display "top-sounds, released!")
			      (set! last-dx dx)
			      (set! last-dy dy)
			      )
			    ':release-thunk
			    (lambda ()
			      (c-display "top-sounds, release-thunk")
			      (get-audiofile sound-url
					     (lambda (audiofile)
					       (-> move-button repaint-me!)
					       (-> parent remove-sub-area move-button)
					       (if audiofile
						   (-> mixer-area add-sound audiofile (+ last-dx start-x) (+ last-dy start-y)))
					       (-> parent check-paint))
					     (lambda (seconds end-time)
					       (c-display seconds "/" end-time))))))
  
  (when mid-x
    (define das-button-width (-> button width))
    (set! x (- mid-x (/ das-button-width 2))))
  (when mid-y
    (define das-button-height (-> button height))
    (set! y (- mid-y (/ das-button-height 2))))

  (-> parent add-sub-area button x y))

#|
(sound-icon win )
|#

(c-define (init-top-sounds parent sounds
			   :key 
			   (horizontal #t)
			   (sounds-x top-sounds-x)
			   (sounds-y top-sounds-y)
			   (sounds-x2 top-sounds-x2)
			   (sounds-y2 top-sounds-y2))

  (define len (length sounds))

  (if horizontal
      (c-for-each (lambda (n top-sound)
		    (define image-url (car top-sound))
		    (define sound-url (cadr top-sound))
		    
		    (define das-x1 (c-scale n 0 len sounds-x sounds-x2))
		    (define das-x2 (c-scale (1+ n) 0 len sounds-x sounds-x2))
		    
		    (sound-icon parent image-url sound-url #f sounds-y ':mid-x (average das-x1 das-x2))
		    )
		  
		  sounds)
      (c-for-each (lambda (n top-sound)
		    (define image-url (car top-sound))
		    (define sound-url (cadr top-sound))
		    
		    (define das-y1 (c-scale n 0 len sounds-y sounds-y2))
		    (define das-y2 (c-scale (1+ n) 0 len sounds-y sounds-y2))
		    
		    (sound-icon parent image-url sound-url sounds-x #f ':mid-y (average das-y1 das-y2))
		    )
		  
		  sounds)))


#|

(init-top-sounds win (get-top-sounds)
		 ':horizontal #f
		 ':sounds-x 10
		 ':sounds-x2 100
		 ':sounds-y 100
		 ':sounds-y2 600)


(init-top-sounds win)
(-> win check-paint)
(-> win force-paint)

(get-top-sounds top-sounds-URL)
|#

  
