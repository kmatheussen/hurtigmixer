
(eval '(import threading))


(c-import sound-area)
(c-import slider)
(c-import button)
(c-import gfx-button)
(c-import pos-button)
(c-import rectangle)
(c-import miniature-area)



#|

Naar brukeren trykker ned museknappen og velger en lyd skjer foelgende:
1. Tegn opp alle lydene, bortsett fra den som blir trykket.
2. Kopier bilde over i et nytt et.
3. Ta vekk alle lyder bortsett fra den som blir trykket (for-each sub-area-remove (remove current-sound sounds))

Naar brukeren flytter en lyd:
1. Paste den delen av bildet som er spesifisert med x* y* x2* y2*
(2. Tegn trykket lyd. gjort automatisk)

Naar brukeren releaser mus:
1. Paste den delen av bildet som er spesifisert med x* y* x2* y2*
(2. Tegn trykket lyd. gjort automatisk)
3. (for-each sub-area-add (remove current-sound sounds))

|#


(define *default-sound-volume* 0.5)

(define sound-area-copystate #f)




(def-area-subclass (<mixer-area> parent width height das-mixer
				 :key
				 (end-time (or (get-config-number 'shortest-duration)
					       10.0))
				 (max-end-time (* 20 60)))

  (define temp 2)

  (def-var end-time)

  (def-var background-color black-color)
  (def-var sound-object-height (c-integer (/ height 5)))

  (def-var image #f)
  (def-var image-g #f)

  (def-var events (-> (new-static <CreateFrame>) events))



  (def-method (set-sound-object-height! new-height)
    (run-operation (lambda ()
		     (set-sound-object-height!-internal new-height))))

  (def-method (set-sound-object-height!-internal new-height)
    (set! sound-object-height new-height)

    (for-each (lambda (sound-area)
		(define pan (-> (-> sound-area sound-object) getPan))
		(define new-y (pan->y pan))
		(-> sound-area repaint-me!)
		(-> sound-area set-height! new-height new-y)
		(-> sound-area repaint-me!))
	      sub-areas)

    (-> parent add-post-paint
	(lambda (g)
	  (-> miniature-area repaint-me!)
	  (-> parent check-paint)))
    )
 

  (define (create-image!)
    (if image
	(-> image flush)) ;; don't remove!
    (-> parent get-position
	(lambda (x y x2 y2 width height)
	  (set! image (-> parent create-image width height #f))))
    ;;(c-display "IMAGE_INCOMPATIBE in mixer-area->create-image!")
    ;;(-> image setAccelerationPriority 0)
    (c-display "CREATE_GRAPHICS")
    (set! image-g (<my-graphics> (-> image getGraphics) (-> parent get-frame)))
    (c-display "CREATE_GRAPHICS2")
    image)

  ;;(create-image!)

  (define temp-sub-areas '())

  (def-method (get-sound-areas)
    (append temp-sub-areas sub-areas))
  
  (define (paint-image)
    (c-display "paint-image")
    
    (-> image-g setClip x y width height)
    (-> parent paint-background image-g x y x2 y2)
    
    (for-each (lambda (sub-area)
		(-> sub-area paint-internal image-g x y x2 y2 x y x2 y2))
	      temp-sub-areas))
  
  (define (validate-image)
    (-> parent validate-volatile-image image
	paint-image
	create-image!))



  (define last-cx 0)

  (define (blit g w-image x* y* x2* y2*)
    (if timebar
	(-> timebar get-shown-times
	    (lambda (shown-start-time shown-end-time)
	      (let* ((playpos (-> mixer getPlayPos))
		     (cx (i-scale playpos shown-start-time shown-end-time x x2))
		     (cursor-changed? (not (= cx last-cx))))
		(if (and (>= cx x*)
			 (< cx x2*))
		    (begin
		      (when cursor-changed?
			(-> g setClip x y (- x2 x) (- y2 y))
			(-> g blit-sub-image w-image (- last-cx 0) y  (+ 1 last-cx) (1+ y2)))
		      (-> g blit-sub-image w-image x* y* cx y2*)
		      (-> g setColor cursor-color)
		      (-> g drawLine cx y cx y2)
		      (-> g blit-sub-image w-image (+ 1 cx) y* x2* y2*)
		      (set! last-cx cx))
		    (begin
		      (when cursor-changed?
			(paint-cursor playpos)
			(-> g setClip x y (- x2 x) (- y2 y)))
		      (if is-using-image 
			  (-> g blit-sub-image w-image   x* y* x2* y2*)
			  (-> g blit-sub-image w-image   x* y* x2* y2*)))))))))


  (def-method (paint-cursor :optional (playpos (-> das-mixer getPlayPos)))
    ;;(c-display "paint-cursor" playpos)
    ;(-> vu-area paint-vu)
    (-> miniature-area paint-cursor (i-scale playpos 0 end-time x x2))
    (-> timebar get-shown-times
	(lambda (shown-start-time shown-end-time)
	  (let ((cx (i-scale playpos shown-start-time shown-end-time x x2)))
	    (if (not (= cx last-cx))
		(let ((g (-> parent ll-window-g))
		      (image (-> parent image)))
		  (-> g setClip x y (- x2 x) (- y2 y))
		  (-> g setColor cursor-color)
		  (-> g drawLine cx y cx y2)
		  (-> g blit-sub-image image (- last-cx 0) y (+ 1 last-cx) (1+ y2))
		  (set! last-cx cx))))))
    (-> vu-area paint-vu))


  (-> (-> das-mixer cursor) addCursorCallback
      (java-wrap paint-cursor))
  
  

  (define (paint g x* y* x2* y2*)
    (if is-using-image
	(begin
	  (validate-image) ;; hmm.
	  (-> g blit-sub-image image x* y* x2* y2*)
	  (when (-> image contentsLost)
		(c-display "  CONTENTS LOST")
		(paint-image)
		(paint g x y x2 y2)))
	(begin
	  (-> parent paint-background g x* y* x2* y2*))))


  (def-method (set-curr-sound-area! new-sound-area)
    (when (not (eq? curr-sound-area new-sound-area))
      (if new-sound-area
	  (let ((g (-> parent ll-window-g)))
	    (-> g setClip x y (- x2 x) (- y2 y))
	    (-> new-sound-area paint-border g)))
      (if curr-sound-area
	  (-> curr-sound-area repaint-me!))
      (set! curr-sound-area new-sound-area)))


  (define (post-paint g x* y* x2* y2*)
    (if curr-sound-area
	(-> curr-sound-area paint-border g)))


  (define (add-sound-areas-to-rect! rectangle)
    (for-each (lambda (sub-area)
		(when (eq? '<sound-area> (-> sub-area class-name))
		      (-> sub-area get-position
			  (lambda (x* y* x2* y2* width* height*)
			    (if (or (and (>= x* x)
					 (< x* x2))
				    (and (>= x2* x)
					 (< x2* x2))
				    (and (> x2* x2)
					 (<= x* x)))
				(-> rectangle add-max x* y* x2* y2*))))))
	      sub-areas))



  (def-method (reset!)
    (run-operation (lambda ()
		     (reset!-internal))))

  (def-method (reset!-internal)
    (-> das-mixer requestTo_removeAll)
    (-> mixer requestTo_setPlayPos 0)
    (set! sub-areas '())
    (set! curr-sound-area #f)
    (trim)
    (reset-undo!))


  (def-method (play)
    (-> das-mixer startPlaying))

  (def-method (stop)
    (-> das-mixer stopPlaying))

  (def-method (rewind)
    (paint-cursor 0)
    (-> das-mixer requestTo_setPlayPos 0))

  (def-var autotrim #f)

  (def-method (trim)
    (run-operation (lambda ()
		     (trim-internal))))

  (def-method (trim-internal)
    (define last-end (or (get-config-number 'shortest-duration)
			 10))
    (for-each (lambda (sound-area)
		(let ((sound-end (+ 0.1 (-> sound-area start-time) (-> sound-area end-read-pos))))
		  (if (> sound-end last-end)
		      (set! last-end sound-end))))
	      sub-areas)
    (set-end-time! last-end)
    (repaint-me!)
    (-> miniature-area repaint-me!))


  (let ((playposfunc (lambda (button x* y*)
                       ;;(c-display "button" button)
		       (-> timebar get-shown-times (lambda (start end)
						     (cond ((and (or (= *left-mouse-button* button)
                                                                     (= *scroll-down-mouse-button* button))
                                                                 (-> das-mixer isPlayingQuestionmark))
                                                            (let ((newpos (c-scale x* x x2 start end)))
                                                              (stop)
                                                              (-> mixer requestTo_setPlayPos newpos)
                                                              (paint-cursor newpos)))
                                                           ((= *scroll-up-mouse-button* button)
                                                            (let ((newpos (c-scale x* x x2 start end)))
                                                              (-> mixer requestTo_setPlayPos newpos)
                                                              (paint-cursor newpos)
                                                              (play))))
                                                     #f)))))
    (add-mouse-cycle playposfunc playposfunc playposfunc))

  '(let ((playposfunc (lambda (button x* y*)
		       (-> timebar get-shown-times (lambda (start end)
						     (if (and (= *left-mouse-button* button)
							      (-> das-mixer isPlayingQuestionmark))
							 (let ((newpos (c-scale x* x x2 start end)))
							   (stop)
							   (-> mixer requestTo_setPlayPos newpos)
							   (paint-cursor newpos))
							 (if (= button *scroll-up-mouse-button*)
							     (let ((newpos (c-scale x* x x2 start end)))
							       (-> mixer requestTo_setPlayPos newpos)
							       (paint-cursor newpos)
							       (play)
                                                               #f)
                                                             #f)))))))
    (add-mouse-cycle playposfunc playposfunc playposfunc))

  (def-method (delete-sound sound-area)
    (run-operation (lambda ()
		     (delete-sound-internal sound-area))))

  (def-method (delete-sound-internal sound-area)
    (-> sound-area get-position
	(lambda (x* y* x2* y2* width* height*)
	  (-> parent paint-background (-> parent ll-window-g) x* y* x2* y2*)
	  (-> parent repaint x* y* x2* y2*)
	  (-> miniature-area repaint-me!)
	  (-> das-mixer requestTo_remove (-> sound-area sound-object))
	  (remove-sub-area sound-area)
	  (when (eq? curr-sound-area sound-area)
	    (set-curr-sound-area! (find (lambda (sub-area)
					  (eq? '<sound-area> (-> sub-area class-name)))
					(reverse sub-areas)))
	    ;; What if temp-sub-areas are non-null, but top-area is null? (yes, that can actually happen)
	    (if curr-sound-area
		(-> curr-sound-area repaint-me!))))))


  (def-method (add-sound-area sound-area time pan :optional (add-it #t))
    (run-operation (lambda ()
		     (add-sound-area-internal sound-area time pan add-it))))

  (def-method (add-sound-area-internal sound-area time pan :optional (add-it #t))
    (define sound-object (-> sound-area sound-object))
    (-> miniature-area repaint-me!)
    (-> sound-object setPan pan)
    (if add-it
	(-> das-mixer requestTo_add sound-object (* time srate))
	(-> sound-object requestTo_setPos time))
    
    (set-curr-sound-area! sound-area)
    
    (add-sub-area sound-area (- (time->x time) *sound-area-threshold*) (pan->y pan))
    (-> sound-area set-start-time! time)

    (-> sound-area repaint-me!)
    
    (when (>= (+ 0.1 time (-> sound-object getPlayLength)) end-time)
      (set-end-time! (+ time (* 2 (-> sound-object getPlayLength))))
      (repaint-me!)))
  
  (def-method (duplicate x* y*)
    (run-operation (lambda ()
		     (duplicate-internal x* y*))))

  (def-method (duplicate-internal x* y*)
    (let ((sound-area (find (lambda (sound-area)
			      (-> sound-area get-position
				  (lambda (x y x2 y2 width height)
				    (and (> x* x)
					 (< x* x2)
					 (> y* y)
					 (< y* y2)))))
			    sub-areas)))
      (if sound-area
	  (let* ((time (-> sound-area start-time))
		 (pan (-> (-> sound-area sound-object) pan))
		 (copy (<sound-area-from-state> parent (-> sound-area make-state) (-> sound-area start-time) (-> (-> sound-area sound-object) pan))))
	    (add-sound-area copy time pan #f)
	    (-> copy set-start-playtime! time)
	    (-> copy set-position! (- (time->x time) *sound-area-threshold*) (pan->y pan))
	    ;;(-> timeslider report!)
	    (list sound-area copy))
	  #f)))


  (def-method (split x* y*)
    (run-internal (lambda ()
		    (split-internal x* y*))))

  (def-method (split-internal x* y*)
    (let ((dup (duplicate x* y*)))
      (if dup
	  (apply (lambda (left right)
		   (let ((split-time (get-time x*))
			 (start-playtime (-> left start-playtime))
			 (start-sound-time (-> left start-time)))
		     
		     (-> left end-read-pos (- split-time start-sound-time))
		     (-> (-> right sound-object) requestTo_setEndReadPos (- split-time start-sound-time))
		     (-> left adjust-position)

		     (-> right start-read-pos (- split-time start-sound-time))
		     (-> (-> right sound-object) requestTo_setStartReadPos (- split-time start-sound-time))
		     (-> right adjust-position)
		     )
		   )
		 dup))))

  (def-method (key-pressed key-event)
    (run-operation (lambda ()
		     (key-pressed-internal key-event))))

  (define resampling-quality 1)

  (def-method (key-pressed-internal key-event)
    (let ((key-code (-> key-event getKeyCode)))
      (cond ((= key-code (-> key-event VK_SPACE))
	     ;;http://weitz.de/cl-interpol/
	     (if (-> das-mixer isPlayingQuestionmark)
		 (stop)
		 (play))
	     #t)
	    ((and (or (= key-code (-> key-event VK_DELETE))
		      (= key-code (-> key-event VK_BACK_SPACE)))
		  curr-sound-area)
	     (add-undo-run (let ((sound-area curr-sound-area))
			     (lambda ()
			       (delete-sound sound-area)))
			   (let ((time (-> curr-sound-area start-time))
				 (pan (-> (-> curr-sound-area sound-object) getPan))
				 (sound-area curr-sound-area))
			     (lambda ()
			       (add-sound-area sound-area time pan))))
	     #t)
	    ((= key-code (-> key-event VK_U))
	     (undo))
	    ((= key-code (-> key-event VK_H))
	     (set! resampling-quality (nth resampling-quality '(1 2 0)))
	     (cond ((= 0 resampling-quality)
		    (-> static-ResampleProducer forceLinearInterpolation #t)
		    (-> mixer setForceSincResampler #f))
		   ((= 1 resampling-quality)
		    (-> static-ResampleProducer forceLinearInterpolation #t)
		    (-> mixer setForceSincResampler #t))
		   ((= 2 resampling-quality)
		    (-> static-ResampleProducer forceLinearInterpolation #f)
		    (-> mixer setForceSincResampler #t)))
	     (set-message-area! (<-> "Resampling: " (nth resampling-quality '("worst" "default" "best")))
				':synchronized #f))
	    ((= key-code (-> key-event VK_R))
	     (redo))
	    ((= key-code (-> key-event VK_O))
	     (update-soundlists))
	    ;;((= key-code (-> key-event VK_D))
	    ;; (duplicate (-> events lastX) (-> events lastY)))
	    ;;((= key-code (-> key-event VK_S))
	    ;; (split (-> events lastX) (-> events lastY)))
	    ((= key-code (-> key-event VK_X))
	     (when curr-sound-area
		   (set! sound-area-copy (-> curr-sound-area make-copy))
		   (key-pressed (-> key-event VK_DELETE))))
	    ((= key-code (-> key-event VK_C))
	     (when curr-sound-area
		   (set! sound-area-copystate (-> curr-sound-area make-state))))
	    ((= key-code (-> key-event VK_V))
	     (when sound-area-copystate		   
		   (let ((das-sound-area (<sound-area-from-state> parent sound-area-copystate 1 0))
			 (already-added #f))
		     (add-undo-run (let ((time (get-time (-> events lastX)))
					 (pan (max -1 (min 1 (get-pan (-> events lastY))))))
				     (if (< (+ time (-> das-sound-area get-playlength)) 0.1)
					 (set! time (- 0.1 (-> das-sound-area get-playlength))))
				     (lambda ()
				       (add-sound-area das-sound-area time pan already-added)
				       (set! already-added #t)
				       (-> das-sound-area set-start-playtime! time)
				       (-> das-sound-area set-position! (- (time->x time) *sound-area-threshold*) (pan->y pan))
				       (-> timeslider report!)))
				   (lambda ()
				     (delete-sound das-sound-area))))))
	    
	    (else
	     #f))))

	      



  (-> das-mixer requestTo_removeAll)
  (-> das-mixer setLength end-time)
  (-> mixer requestTo_setPlayPos 0)
  


  (def-method (get-pan y*)
    (c-scale y* y (- y2 sound-object-height) -1 1))
  
  (def-method (get-time x*)
    (-> timebar get-shown-times
	(lambda (shown-start-time shown-end-time)
	  (c-scale x* x x2 shown-start-time shown-end-time))))

  (def-method (pan->y pan)
    (i-scale pan -1 1 (+ 0 y) (- y2 sound-object-height)))

  (def-method (time->x time)
    (-> timebar get-shown-times
	(lambda (shown-start-time shown-end-time)
	  (i-scale time shown-start-time shown-end-time x x2))))


  (def-var curr-sound-area #f)



  ;; Operations

  ;; To (gode) måter å løse problemet på:
  ;; 1. Flytting av lyder kan avbrytes. (JEPP)
  ;; 2. Ingenting annet kan gjøres mens en lyd flyttes. (NOPE)
  ;; Men uavhengig av 1 eller 2, så er det et problem med undo/redo.
  ;; Hva med en egen wrapper-funksjon ala. "run-operation" som enten avbryter flytting, avbryter operasjon, eller kjører operasjon?
  ;; * Undo/redo kan jo ikke avbrytes, det blir feil. Så da må heller flytting avbrytes.
  ;;   * Men skal flytting kanselleres fullstendig, eller går det an å sette posisjon til der den er?
  ;;     Problemet er jo at flytting generer undo/redo...
  ;;     * Dette betyr at avbrudd av flytting må føre til at brikken føres tilbake til start-pos uten at undo/redo-info blir generert.
  ;;       Det er i hvert fall en rimelig enkel løsning.
  ;;       * Problemet er jo at hvis en lyd blir lastet ned, så vil en flytting bli kansellert. Tillegging av lyd kanskje gjoeres etterpaa...
  ;;
  ;; Hva med:
  ;; 3. Keyboard and mouse is locked while moving a sound.
  ;; ? Vel, i hvert fall mouse, keyboard er vel dekket med 'run-operation'. Men uansett så kan jo nye lyder legges til. Ja, men det er ikke noe problem med 'run-operation'.

  (define is-using-image #f)
  (define operations '()) ;; If currently moving a sound, operations are stored here instead of being performed.

  (define (run-operation op)
    (if is-using-image
	(push! op operations)
	(op)))

  (define (run-stored-operations)
    (for-each (lambda (op) (op)) (reverse! operations))
    (set! operations '()))


  ;; Sound areas.

  (define next-sound-position #f)

  (def-method (add-sound audiofile-buffer x* y*)
    (run-operation (lambda ()
		     (add-sound-internal audiofile-buffer x* y*))))
     
  (def-method (add-sound-internal audiofile-buffer x* y*)
    ;;(c-display "add-sound start")
    (-> timebar get-shown-times
	(lambda (shown-start-time shown-end-time)
	  (c-display "st/end" shown-start-time shown-end-time audiofile-buffer)
	  
	  (let* ((placed-in-miniature? (> y* (+ y2 6)))
		 (placed-in-input-area? (< x* (- x 20)))
		 (time (max 0 (cond (placed-in-input-area?
				     (if (or (not next-sound-position)
					     (-> das-mixer isPlayingQuestionmark))
					 (-> das-mixer getPlayPos)
					 next-sound-position))
				    (placed-in-miniature?
				     (-> miniature-area get-position
					 (lambda (x y x2 y2 width height)
					   (c-scale x* x x2 0 end-time))))
				    (else
				     (get-time x*)))))
		 (pan (min 1 (max -1 (cond (placed-in-input-area?
					    (c-scale (rand-between 0 100) 10 90 -1 1))
					   (placed-in-miniature?
					    (-> miniature-area get-position
						(lambda (x y x2 y2 width* height*)
						  (c-scale y* (+ y 4) (- y2 4 (c-scale sound-object-height 0 height 0 height*)) -1 1))))
					   (else
					    (get-pan y*))))))
		 (sound-object (new <SoundObject> audiofile-buffer pan *default-sound-volume* das-mixer #f))
		 (new-end-time #f))

	    (c-display 7)

	    (set! next-sound-position (+ time (-> sound-object getPlayLength)))

	    (if (>= (+ 0.1 time (-> sound-object getPlayLength)) end-time)
		(if placed-in-input-area?
		    (set! new-end-time (* 2 (+ time (-> sound-object getPlayLength))))
		    (set! new-end-time (+ time (-> sound-object getPlayLength) 0.11))))
	    
	    (set! x* (i-scale time shown-start-time shown-end-time x x2))
	    (set! y* (i-scale pan -1 1 y (- y2 sound-object-height)))

	    (c-display 8)



	    ;;; Start using image
	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    (define (start-using-image sound-area*)
	      (set! sound-area sound-area*)
	      ;;(c-display "WEFWEOIFJWEOFIJ !!!! qerpgoiqejrgqerg" 123123098417 123401 92834)
	      ;;(-> image flush)
	      (define prev-curr-sound-area curr-sound-area)
	      (set-curr-sound-area! sound-area)
	      ;(-> parent get-position
	;	  (lambda (x* y* x2* y2* width* height*)
	;	    (let ((g (-> parent ll-window-g)))
	;	      (-> sound-area paint-internal g x y x2 y2 x y x2 y2)
	;	      (-> g setClip 0 0 width* height*))))
	      

	      (if (not image)
		  (begin
		    (let loop ()
		      (create-image!)
		      (if (= (-> image validate (-> (-> parent get-frame) getGraphicsConfiguration))
			     IMAGE_INCOMPATIBLE)
			  (loop)))
		    (while (= (-> image validate (-> (-> parent get-frame) getGraphicsConfiguration))
			      IMAGE_INCOMPATIBLE)
			   (c-display "ai!")
			   (create-image!)))
		  (begin
		    ;;(-> image-g setComposite (-> static-AlphaComposite SrcOver))
		    ;;(set! image-g (<my-graphics> (-> image getGraphics) (-> parent get-frame)))
		    (-> image-g setClip x y x2 y2)
		    ))
	      
	      (while (= (-> image validate (-> (-> parent get-frame) getGraphicsConfiguration))
			IMAGE_INCOMPATIBLE)
		     (c-display "ai!")
		     (create-image!))

	      (set! temp-sub-areas '())

	      (for-each (lambda (sub-area)
			  (when (and (not (eq? sub-area sound-area))
				     (eq? '<sound-area> (-> sub-area class-name)))
				(push! sub-area temp-sub-areas)
				(remove-sub-area sub-area)))
			sub-areas)
	      (set! temp-sub-areas (reverse! temp-sub-areas))

	      (-> image-g blit-sub-image (-> parent image) x y x2 y2)

	      ;;(paint-cursor image-g)

	      (if #t
		  (-> sound-area get-position
		      (lambda (x y x2 y2 width height)
			(-> parent paint-background image-g x y x2 y2)
			(for-each (lambda (sub-area)
				    (-> sub-area paint-internal image-g x y (1+ x2) (1+ y2) x y (1+ x2) (1+ y2)))
				  temp-sub-areas)))
		  (paint-image))

	      (if (and prev-curr-sound-area
		       (not (eq? prev-curr-sound-area sound-area)))
		  (-> prev-curr-sound-area get-position
		      (lambda (x y x2 y2 width height)
			(-> image-g setClip x y width height)
			(-> parent paint-background image-g x y x2 y2)
			(for-each (lambda (sub-area)
				    (-> sub-area paint-internal image-g x y (1+ x2) (1+ y2) x y (1+ x2) (1+ y2)))
				  temp-sub-areas))))

	      (when (-> image contentsLost)
		(c-display "  CONTENTS LOST2")
		(paint-image))

	      (set! is-using-image #t))


	    ;;; Move sound
	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    (define (move-sound-area-internal sound-area* new-time new-pan)
	      (set! sound-area sound-area*)
	      (set! sound-object (-> sound-area sound-object))
	      
	      ;;(c-display "new-time" (* 1.0 new-time))
	      (set! new-pan (max -1 (min new-pan 1)))
	      ;;(set! new-time (max 0 new-time))
	      ;;(if (< (+ new-time (-> sound-area get-playlength)) 0.1)
	      ;;  (set! new-time (- 0.1 (-> sound-area get-playlength))))
	      
	      
	      (-> sound-area get-position
		  (lambda (old-x old-y old-x2 old-y2 width height)
		    (-> timebar get-shown-times
			(lambda (shown-start-time shown-end-time)
			  
			  (define new-x (- (time->x (- new-time (-> sound-area start-read-pos))) *sound-area-threshold*))
			  (define new-y (pan->y new-pan))
			  
			  (-> miniature-rectangle reset!)
			  (-> miniature-area get-miniature-pos sound-area
			      (lambda (x* y* width* height*)
				(-> miniature-rectangle add-max x* y* (+ x* width*) (+ y* height*))))
			  
			  
			  ;;(c-display "setting new time:" (/ new-time 1.0))
			  (-> sound-object requestTo_setPosAndPan new-time new-pan)
			  ;;(c-display "new pan/time"  (exact->inexact (get-time new-x)) (exact->inexact (get-pan new-y)))
			  ;;(c-display "width/height:" width height (- old-x2 (+ new-x width)) (- old-y2 (new-y height)))
			  (-> sound-area set-start-playtime! new-time)
			  (-> sound-area set-position! new-x new-y)
			  
			  (-> parent add-post-paint
			      (lambda (g)
				(-> miniature-area get-position
				    (lambda (x y x2 y2 width height)
				      (-> miniature-area get-miniature-pos sound-area
					  (lambda (x* y* width* height*)
					    (-> miniature-rectangle add-max x* y* (+ x* width*) (+ y* height*))))
				      (-> miniature-rectangle get-max (lambda (x* y* x2* y2*)
									(-> parent repaint x* y* (min x2 x2*) y2*)))
				      (-> parent check-paint)))))
			  
			  (-> parent repaint
			      (max x (min old-x new-x)) (min old-y new-y)
			      (min (1- x2) (max old-x2 (+ new-x width))) (max old-y2 (+ new-y height)))
			  
			  )))))

	    (define* (move-sound-area sound-area* new-time new-pan :optional run-as-operation)
	      (if run-as-operation
		  (run-operation (lambda ()
				   (move-sound-area-internal sound-area* new-time new-pan)))
		  (move-sound-area-internal sound-area* new-time new-pan)))


	    ;;; End using image
	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    (define* (end-using-image sound-area* :optional is-removed)
	      (c-display "END USING IMAGE")
	      (set! sound-area sound-area*)
	      (set! is-using-image #f)
	      (for-each (lambda (sound-area)
			  (add-sub-area sound-area (-> sound-area x) (-> sound-area y)))
			temp-sub-areas)
	      (set! temp-sub-areas '())
	      (if (not is-removed)
		  (-> sound-area lift-me!))

	      (run-stored-operations)

	      ;; Check bounds
	      (-> timebar get-shown-times
		  (lambda (shown-start-time shown-end-time)
		    (define end-playtime (-> sound-area end-playtime))
		    (when (>= end-playtime (- shown-end-time 0.1))
		      (c-display "after" end-playtime)
		      (set-end-time! (+ end-playtime 0.1))
		      (repaint-me!)
		      (c-display "repaint-me called a")
		      (-> miniature-area repaint-me!)
		      )))
	      

	      (if autotrim
		  (trim)))





	    (define miniature-rectangle (<rectangle>))

	    (define sound-area
	      (<sound-area> parent sound-object audiofile-buffer
			    (+ (i-scale (-> sound-object getPlayLength) 0 (- shown-end-time shown-start-time) 0 width) (* 2 *sound-area-threshold*)) sound-object-height
			    time (+ time (-> sound-object getPlayLength))
			    this

			    ;; press callback
			    start-using-image

			    ;; set-pos callback
			    move-sound-area

			    ;; release callback
			    end-using-image))

	    
	    (c-display 9)

	    ;;(c-display "new time/pan" time pan x* y*)
	    (-> das-mixer requestTo_add sound-object (* time srate))

	    (set-curr-sound-area! sound-area)

	    (add-sub-area sound-area (- x* *sound-area-threshold*) y*)

	    (c-display 10)

	    (-> sound-area get-position
		(lambda (x* y* x2* y2* width* height*)
		  (-> parent repaint (max x x*) y* (min (1- x2) x2*) y2*)))

	    (c-display 11)

	    (if (not new-end-time)
		(-> miniature-area get-miniature-pos sound-area
		    (lambda (x* y* width* height*)
		      (-> parent repaint x* y* (+ x* width*) (+ y* height*)))))

	    (c-display 12)

	    (if new-end-time
		(set-end-time! new-end-time))

	    (c-display 13)
	    (add-undo (let ((time (-> sound-area start-time))
			    (pan (-> (-> sound-area sound-object) getPan)))
			(lambda ()
			  (add-sound-area sound-area time pan)))
		      (lambda ()
			(c-display "HEPP, deleting sound from UNDO");
			(delete-sound sound-area)))))))

  


  (define rectangle (<rectangle>))


  ;; Time slider. (doesn't seem to be used)
  (def-method (timeslider-report-func x-1 x-2)
    ;;(c-display "x-1/x-2" x-1 x-2)
    (let ((shown-start-time (c-scale x-1 0 1 0 end-time))
	  (shown-end-time (c-scale x-2 0 1 0 end-time)))

      (-> rectangle reset!)

      (add-sound-areas-to-rect! rectangle)
      

      (for-each (lambda (sound-area)
		  (-> sound-area set-position!
		      (- (i-scale (-> sound-area start-time) shown-start-time shown-end-time
				  x x2)
			 *sound-area-threshold*)
		      (-> sound-area y))
		  (-> sound-area set-width! (+ (i-scale (-> (-> sound-area sound-object) getAbsLength)
							0 (- shown-end-time shown-start-time)
							0 width)
					       (* 0 *sound-area-threshold*)))) ;; Threshold is added in sound-area.set-width! (...hmm...)
		sub-areas)
      
      (add-sound-areas-to-rect! rectangle)

      (-> parent add-post-paint
	  (lambda (g)
	    (-> timebar set-times! shown-start-time shown-end-time)
	    (-> parent add-post-paint
		(lambda (g)
		  (-> rectangle get-max
		      (lambda (r-x r-y r-x2 r-y2)
			(when (or (and (>= r-x x)
				       (< r-x x2))
				  (and (>= r-x2 x)
				       (< r-x2 x2))
				  (and (> r-x2 x2)
				       (<= r-x x)))
			      (-> parent repaint (max r-x x) (max r-y y) (min (1- x2) r-x2) (min (1- y2) r-y2))
			      (-> parent check-paint))))))
	    (-> parent check-paint)))
      (paint-cursor)
      ))


  '(define last-new-x1 -1)
  '(define last-new-x2 -1)

  '(def-method (timeslider-report-func x-1 x-2)
    (thread/spawn 
     (lambda ()
       (let ((events (-> (new-static <CreateFrame>) globalLock)))
	 (c-display "gakk")
	 (java-synchronized (<-o events)
			    (lambda ()
			      (when (or (not (= x-1 last-new-x1))
					(not (= x-2 last-new-x2)))
				    (c-display "gakk3")
				    (set! last-new-x1 x-1)
				    (set! last-new-x2 x-2)
				    (timeslider-report-func-in-thread x-1 x-2)
				    (-> parent check-paint))))))))

  (def-method (set-end-time! newtime :optional (change-mixer #t))
    (set! end-time newtime)
    (-> das-mixer setLength end-time)
    (if (not change-mixer)
	(-> timeslider report! (lambda (x-1 x-2)
				 (-> timeslider set-values! x-1 x-2)))) ;;1.0)))
    (-> miniature-area repaint-me!)
    (-> timeslider report!))


  
  (def-method (save filename callback)
    (define callbacks (new <MyCallbacks>
			   (java-wrap (lambda x
					(c-display "callback called with" x)
					(apply callback x)))
			   (java-wrap (lambda x #t))))
    (c-display "calling save" filename)
    (-> das-mixer requestTo_save filename 2 44100 16 end-time callbacks callbacks))

  ;;(-> parent add-self-blitting-area! this)

  )



