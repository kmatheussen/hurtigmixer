
(if (not is-running-applet)
    (eval '(import os)))









(c-define (get-two-dec-string n)
  (define whole (c-integer n))
  (define rest (- n whole))
  (define dec100 (c-integer (* rest 100)))
  (<-> (number->string whole) (if (< dec100 10) ".0" ".") (number->string dec100)))

#|
(get-two-dec-string 6.00001)
|#
  




(c-define (get-percent-string n)
  (define dec100 (c-integer (* n 100)))
  (<-> (number->string dec100) "%"))

#|
(get-percent-string 1.99992999)
|#




(c-define (init-srate-slider parent)
  (define (get-srate-val percent)
    (cond ((and (> percent 0.48)
		(< percent 0.52))
	   1.0)
	  ((<= percent 0.5)
	   (c-scale percent 0.0 0.48 0.0 1.0))
	  (else
	   (c-scale percent 0.52 1.0 1.0 5.0))))
  
  (define to-string get-percent-string)
  
  (define srate-slider (<slider> parent srate-slider-width srate-slider-height
				 0.45 0.55
				 (lambda (x-1 x-2)
				   (let ((newval (exact->inexact (get-srate-val (/ (+ x-1 x-2) 2)))))
				     ;;(c-display newval (exact->inexact x-1) (exact->inexact x-2))))))
				     (-> mixer srate_change newval)
				     (-> srate-text text (to-string newval))
				     (-> srate-text repaint-me!)
				     ))
				 ':paint-x-1 #f))
  (-> srate-slider set-solid! redbrown-color)
  
  (-> parent add-sub-area srate-slider srate-slider-x srate-slider-y)
  
  (define srate-text (<text-area> parent (/ srate-slider-width 2) 9 (to-string 1)))
  (-> parent add-sub-area srate-text (- (average srate-slider-x srate-slider-x2) 8) (- (average srate-slider-y srate-slider-y2) 4))
  )






(c-define (init-size-slider parent)

  (define to-string get-percent-string)
  (define size-slider (<slider> parent size-slider-width size-slider-height
				(c-scale (-> mixer-area sound-object-height) 20  mixer-area-height 0 1)
				(c-scale (* (-> mixer-area sound-object-height) 1.4) 20 mixer-area-height 0 1)
				(lambda (x-1 x-2)
				  (-> size-text text (to-string (c-scale (-> mixer-area sound-object-height) 0  mixer-area-height 0 1)))
				  (-> size-text repaint-me!)
				  (-> parent add-post-paint
				      (lambda (g)
					(-> mixer-area set-sound-object-height! (i-scale x-1 0 1 20 mixer-area-height))
					(-> parent check-paint))))
				':paint-x-1 #f))
  (-> size-slider set-solid! redbrown-color)
  (-> parent add-sub-area size-slider size-slider-x size-slider-y)
  
  (define size-text (<text-area> parent (/ size-slider-width 2) 9 (to-string (c-scale (-> mixer-area sound-object-height) 0  mixer-area-height 0 1))))
  (-> parent add-sub-area size-text (- (average size-slider-x size-slider-x2) 8) (- (average size-slider-y size-slider-y2) 4))
  )
	





(define *max-volume* 2.0)

(c-define (init-volume-slider parent)


   (define (to-string vol)
     (define num (min 99 (i-scale vol 0 *max-volume* 0 99)))
     (if (< num 10)
	 (string-append "0" (number->string num))
	 (number->string num)))
   
   (define scaled-volume (c-scale (-> (-> mixer volume) get) 0 *max-volume* 1 0))
   (define volume-slider (<slider> parent volume-slider-width volume-slider-height
				   (- scaled-volume 0.000000001) scaled-volume
				   (lambda (x-1 x-2)
				     (set! x-1 (c-scale x-1 0 1 1 0))
				     (-> (-> mixer volume) set (c-scale x-1 0 0.9 0 *max-volume*))
				     (c-display "x-1/x-2" x-1 x-2 (-> mixer volume))
				     (-> volume-text set-and-force-paint! (to-string (-> (-> mixer volume) get_goal)))
				     (-> volume-text repaint-me!)
				     )
				   ':paint-x-1 #f
				   ':vertical #t))
   (-> volume-slider set-solid! redbrown-color)
   
   (-> parent add-sub-area volume-slider volume-slider-x volume-slider-y)
   
   (define volume-text (<text-area> parent volume-slider-width 20 (to-string (-> (-> mixer volume) get_goal))))
   (-> parent add-sub-area volume-text (+ 0 volume-slider-x) (- (average volume-slider-y volume-slider-y2) 8))
   
   )


(define add-sound-func #f)

(define update-soundlists #f)


(c-define (init-controls app parent)
  
  (if (defined? 'top-sounds-x)
      (init-top-sounds parent (get-top-sounds)))
  
  (define filerequester (<filerequester> parent))
  
  (define button-poss (list 'play-pos     (lambda () (-> app play))
			    'stop-pos     (lambda () (-> app stop))
			    'rewind-pos   (lambda () (-> app rewind))
			    'loop-on-pos  (lambda () (-> mixer setLooping #t))
			    'loop-off-pos (lambda () (-> mixer setLooping #f))
			    'trim-pos     (lambda () (-> app trim))
			    'reset-pos    (lambda () (-> app reset!))
			    'undo-pos     undo
			    'redo-pos     redo
			    
			    'add-sound-pos (lambda () (add-sound-func))
			    
			    'save-local-pos (lambda () (save-local parent))
			    'save-server-pos (lambda () (save-on-server parent))
			    ))


  ;; Sound-icons
  (for-each (lambda (sound-icon-parameters)
	      (apply (lambda (image sound x y)
		       (sound-icon parent (config-full-url image) (config-full-url sound) x y))
		     sound-icon-parameters))
	    (get-config-sound-icons))

  ;; Vertical-sounds
  (for-each (lambda (vertical-sounds-parameters)
	      (apply (lambda (x y width height sounds)
		       (init-top-sounds win sounds
					':horizontal #f
					':sounds-x x
					':sounds-x2 (+ x width)
					':sounds-y y
					':sounds-y2 (+ y height)))
		     vertical-sounds-parameters))
	    (get-config-vertical-sounds))

  ;; Horizontal-sounds
  (for-each (lambda (sounds-parameters)
	      (apply (lambda (x y width height sounds)
		       (init-top-sounds win sounds
					':horizontal #t
					':sounds-x x
					':sounds-x2 (+ x width)
					':sounds-y y
					':sounds-y2 (+ y height)))
		     sounds-parameters))
	    (get-config-horizontal-sounds))

  ;; Buttons
  (let loop ((button-pos button-poss))
    (apply (lambda (pos thunk . rest)
             (if (defined? pos)
                 (<pos-button/add-sub-area> parent parent (eval pos) thunk ':report-releasebutton #t))
             (if (pair? rest)
		 (loop rest)))
           button-pos))

  ;; Link-texts
  (for-each (lambda (link)
              (apply (lambda (text x y width height r g b url . rest)
		       (define button (<button> parent (* (string-length text) width) height text
						(lambda ()
						  (-> Various openURL url))
						#t))
		       (-> button background-alpha 0.0)
		       (-> button underline #t)
		       (-> button text-color (-> Various getColor r g b))
		       (-> parent add-sub-area button x y)
		       (if (not (null? rest))
                           (loop rest)))
                     link))
            (get-config-link-texts))

  ;; Link-areas
  (for-each (lambda (link)
              (apply (lambda (x y width height url . rest)
		       (define button (<pos-button> parent width height
						(lambda ()
						  (-> Various openURL url))
						':report-releasebutton #t))
		       (-> parent add-sub-area button x y)
		       (if (not (null? rest))
                           (loop rest)))
                     link))
            (get-config-link-areas))

  ;; Links
  (for-each (lambda (link)
              (apply (lambda (picture x y url . rest)
		       (define button  (<gfx-button> parent (config-full-url picture)
						     (lambda ()
						       (-> Various openURL url))
						     ':is-URL? (or is-running-applet
								   is-running-standalone
								   (-> (new <java.lang.String> (config-full-url picture))
								       startsWith "http://"))
						     ':report-releasebutton #t))
		       (-> button is-pressed-alpha #t)
		       (-> parent add-sub-area button x y)
                       (if (not (null? rest))
                           (loop rest)))
                     link))
            (get-config-links))



  ;; Sound selectors
  (define musikkverksted #f)
  (define musikkverksted-soundlist #f)


  (when (defined? 'musikkverksted-x)
    (set! musikkverksted (<sound-selector> parent musikkverksted-width musikkverksted-height "MUSIKKVERKSTED"))
    (-> parent add-sub-area musikkverksted musikkverksted-x musikkverksted-y)
    (set! musikkverksted-soundlist (-> musikkverksted create-soundlist (- musikkverksted-height 20))))

	      

  ;;(-> musikkverksted-soundlist add-sound "http://www.notam02.no/~kjetism/Blub_mono16.wav" ':is-URL #t)
  ;;(-> musikkverksted-soundlist add-sound "http://www.notam02.no/~kjetism/2_channel_short.wav" ':is-URL #t)
  ;;(-> musikkverksted-soundlist add-sound "http://www.notam02.no/~kjetism/mp3/wav/klass_test_8.wav" ':is-URL #t)


  (define min-lydhylle #f)
  (define min-lydhylle-soundlist #f)

  (when (defined? 'min-lydhylle-x)
    (set! min-lydhylle (<sound-selector> parent min-lydhylle-width min-lydhylle-height "MIN LYDHYLLE"))
    (-> parent add-sub-area min-lydhylle min-lydhylle-x min-lydhylle-y)
    (set! min-lydhylle-soundlist (-> min-lydhylle create-soundlist (- min-lydhylle-height 20))))

  
  (set! update-soundlists
	(lambda ()
          (if musikkverksted
              (-> musikkverksted reset!))
          (if min-lydhylle
              (-> min-lydhylle reset!))

          (if musikkverksted
              (for-each (lambda (urlname)
                          (-> musikkverksted-soundlist add-sound urlname ':is-URL #t))
                        (get-musikkverkstedfiles)))
	  (if min-lydhylle
              (for-each (lambda (urlname)
                          (-> min-lydhylle-soundlist add-sound urlname ':is-URL #t))
                        (get-userfiles)))))

  (update-soundlists)


  ;;(-> min-lydhylle-soundlist add-sound "http://www.notam02.no/~kjetism/Blub_mono16.wav" ':is-URL #t)
  ;;(-> min-lydhylle-soundlist add-sound "http://www.notam02.no/~kjetism/2_channel_short.wav" ':is-URL #t)
  ;;(-> min-lydhylle-soundlist add-sound "http://www.notam02.no/~kjetism/rockmeamadeus.mp3" ':is-URL #t)
  ;;(-> min-lydhylle-soundlist add-sound "http://www.notam02.no/~kjetism/mp3/preludium.ogg" ':is-URL #t)
  ;;(-> min-lydhylle-soundlist add-sound "http://www.notam02.no/~kjetism/cemb2_16.flac" ':is-URL #t)
  ;;(-> min-lydhylle-soundlist add-sound "/hom/kjetism/Blub_mono16.wav")
  ;;(-> min-lydhylle-soundlist add-sound "/hom/kjetism/2_channel_short.wav")

  

  ;; Egen Maskin
  (let ()

    (define min-datamaskin #f)
    (define egen-soundlist #f)

    (when (defined? 'min-datamaskin-x)
      (set! min-datamaskin (<sound-selector> parent min-datamaskin-width min-datamaskin-height "MIN DATAMASKIN"))
      (-> parent add-sub-area min-datamaskin min-datamaskin-x min-datamaskin-y)
      (set! egen-soundlist (-> min-datamaskin create-soundlist (- min-datamaskin-height 20))))
    
    (define filerequester (<filerequester> parent))

    (set! add-sound-func (lambda ()
                           (if min-datamaskin
                               (-> filerequester load "Velg lydfil. Mp3, wav eller aiff."
                                   (lambda (directory filename fullpath)
                                     (if filename
                                         (-> egen-soundlist add-sound fullpath)))))))
    ;;	  (-> egen-soundlist add-sound "/home/kjetil/Blub_mono16.wav")
    ;;	  (-> egen-soundlist add-sound "/home/kjetil/2_channel_short.wav")
    ;;(-> egen-soundlist add-sound "/hom/kjetism/Blub_mono16.wav")
    ;;(-> egen-soundlist add-sound "/hom/kjetism/2_channel_short.wav")
    ;;	  (-> egen-soundlist add-sound "/home/kjetil/wind.wav")
    
    )


  )






