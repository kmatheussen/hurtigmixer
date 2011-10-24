
(c-import slider)
(c-import breakpointline)
(c-import pianoroll)
(c-import keyboard)
(c-import xy-slider)
(c-import bargraph)
(c-import scale-selector)
(c-import message-area)
(c-import gfx-radiobuttons)
(c-import gfx-slider)
(c-import text-area-multiline)
(c-import melodigenerator-skalavelger)

(def-class (<melodi-area> parent x y width height das-mixer)
  
  ;;(def-method (paint g x* y* x2* y2*)
  ;;  (-> g draw-rect border-color x y (1- width) (1- height)))

  (def-method (play :optional (mixer-time 0))
    (c-display "play " mixer-time)
    (-> das-mixer enableFreeVerb)
    (-> das-mixer requestTo_setPlayPos mixer-time)
    (-> das-mixer startPlaying))

  (def-method (pause)
    (c-display "pause")
    (-> das-mixer stopPlaying))

  (def-method (continue)
    (c-display "continue")
    (-> das-mixer enableFreeVerb)
    (-> das-mixer startPlaying))

  (def-method (stop)
    (c-display "stop")
    (-> das-mixer stopPlaying)
    (-> das-mixer requestTo_setPlayPos 0))

  (def-method (reset)
    (-> mixer requestTo_removeAll)
    (-> mixer requestTo_setPlayPos 0)  
    (set! *notes* '())
    (-> *pianoroll* repaint-me!)
    )

  (def-method (gen)
    ;;(reset)
    (set! *notes* (remove (lambda (note)
                            (and (eq? (-> note audiofile-buffer) *audiofile-buffer*)
                                 (-> note remove-from-mixer)
                                 #t))
                          *notes*))
    (generate-*notes*)
    (-> *pianoroll* repaint-me!))

  (def-method (save filename callback)
    (c-display "melodigenerator->save")
    (define callbacks (new <MyCallbacks>
			   (java-wrap (lambda x
					(c-display "callback called with" x)
					(apply callback x)))
			   (java-wrap (lambda x #t))))
    (c-display "calling save" filename)
    (-> das-mixer requestTo_save filename 2 44100 16 (get-mixer-time mix-length) callbacks callbacks))


  ;;(-> win check-paint))

  ;;(-> parent repaint-me!)
  ;;(-> parent check-paint)
  
  ;;(add-sub-area (<breakpointline> parent (- width w/2 1) (- height w/2 1) ':breakpoint-width breakpoint-width)
  ;;	 (+ w/2 x) (+ y w/2))
  ;;(-> parent repaint-all)
  ;;(-> parent check-paint)
  )

(define keyboard-width (get-config-number 'keyboard-width))
(define keyboard-area #f)
(define melodi-area #f)
(define pitchspeed-area #f)
(define tempo-area #f)
(define tempo-area-min-timeslice 0.01)
(define tempo-area-max-timeslice 10.0)
(define scaleselect-area #f)
(define reverb-slider-area #f)
(define volume-slider-area #f)
(define notelen-slider-area #f)



;;(define (set-progress-bar! a b)
;;  (c-display "set-progress-bar!" a b))

;;(define (set-message-area! message)
;;  (c-display "set-message-area!" message))


;;(define soundfilename (config-full-url (get-config-string 'soundfile)));;"/hom/kjetism/hurtigmixer/sounds/choir.wav")

;; Called after all samples have been loaded.
(c-define (init-melodigenerator2 parent)
  (get-position-list 'melodigenerator-area
		     (lambda (x y width height)
		       (set! *pianoroll* (<pianoroll> parent (- width keyboard-width) height mixer))
		       ;;(set! keyboard-area (<keyboard> parent (- keyboard-width 2) height))
		       (-> parent add-sub-area *pianoroll* (+ x keyboard-width) y)
		       ;;(-> parent add-sub-area keyboard-area x y)
		       (set! melodi-area (<melodi-area> parent x y width height mixer))
		       (init-controls melodi-area parent)))
  
  (get-position-list 'pitchspeed-area
		     (lambda (x y width height)
		       (set! pitchspeed-area (<xy-slider> parent width height
							  (lambda (joy-x joy-y)
							    (define speed (superscale joy-x '((0 0.1)(0.5 1)(1.0 10))))
							    (define pitch (superscale joy-y '((0 0.5)(0.5 1)(1.0 5))))
							    ;;(c-scale joy-y 0 1 0.1 4.0))
							    (set-pianoroll-speedpitch! speed pitch)
							    ;;(c-display "speed:" speed "pitch:" pitch joy-x joy-y)
							    )))
		       (-> parent add-sub-area pitchspeed-area x y)))
  
  (get-position-list 'tempo-area
		     (lambda (x y width height)
		       (set! tempo-area (<bargraph> parent width height))
		       (-> tempo-area pre-callback (lambda (finish-func)
						     (recalculate-timings finish-func)))
		       (-> parent add-sub-area tempo-area x y)))
  
  (get-position-list 'scaleselect-area
		     (lambda (x y width height)
		       (set! scaleselect-area (<scale-selector> parent width height))
		       (-> parent add-sub-area scaleselect-area x y)))
  
  (get-position-list 'reverb-slider
		     (lambda (x y width height)
                       (define reverb-amount 0.1)
                       (-> (-> mixer freeVerb) setWet reverb-amount)
		       (set! reverb-slider-area (<gfx-slider> parent width height
                                                              (c-scale reverb-amount 0 1 0 1)
                                                              (c-scale reverb-amount 0 1 0 1.0)
                                                              (config-full-url "INNER_CIRCLE.png")
                                                              (lambda (x-1)
                                                                (c-display "x-1/x-2" (* 1.0 x-1))
                                                                (-> (-> mixer freeVerb) setRoomSize (c-scale x-1 0 1 0.2 0.9))
                                                                (-> (-> mixer freeVerb) setWet (c-scale x-1 0 1 0 1))
                                                                (-> (-> mixer freeVerb) setDamp (c-scale x-1 0 1 0.5 0.3))
                                                                )))
		       ;;(-> reverb-slider-area 0.5 0.6)
		       (-> parent add-sub-area reverb-slider-area x y)))
  
  (get-position-list 'volume-slider
		     (lambda (x y width height)
		       (define goal (-> (-> mixer volume) get_goal))
		       (set! volume-slider-area (<gfx-slider> parent width height
                                                              (c-scale goal 0 3.0 1.0 0)
                                                              (c-scale goal 0 3.0 1.0 0)
                                                              (config-full-url "INNER_CIRCLE.png")
                                                              (lambda (x-1)
                                                                (c-display "x-1" (* 1.0 x-1))
                                                                (-> (-> mixer volume) set (c-scale x-1 0 1 0 3.0)))))
		       ;;(-> reverb-slider-area 0.5 0.6)
		       (-> parent add-sub-area volume-slider-area x y)))
  
;;  (get-position-list 'notelen-slider
;;		     (lambda (x y width height)
;;		       (set! notelen-slider-area (<slider> parent width height
;;							   (-> (-> mixer volume) get_goal)
;;							   (-> (-> mixer volume) get_goal)
;;							   (lambda (x-1 x-2)
;;							     (set-note-lengths (c-scale x-1 0 1 3.0 0.1))
;;							     (-> *pianoroll* repaint-me!)
;;							     )
;;							   ':paint-x-1 #f
;;							   ':vertical #f))
;;		       ;;(-> reverb-slider-area 0.5 0.6)
;;		       (-> parent add-sub-area notelen-slider-area x y)))
  
  ;;(define shake-button (<pos-button> parent shake-button-width shake-button-height shake-all-notes))
  (define shake-button (<pos-button> parent shake-button-width shake-button-height (lambda () (quantize-all-notes))))
  (-> parent add-sub-area shake-button shake-button-x shake-button-y)

  (define play-button #f)

  (define (play-button-pressed)
    (if (-> *das-mixer* isPlayingQuestionmark)
        (-> melodi-area pause)
        (-> melodi-area continue)))

  (define (stop-button-pressed)
    (-> melodi-area stop)
    (-> *pianoroll* paint-cursor 0))

  (set! play-button (<pos-button> parent play-button-width play-button-height play-button-pressed
                                  ':report-releasebutton #f
                                  ':gfx-pressed-filename (config-full-url "play_pressed.png")))
  (-> parent add-sub-area play-button play-button-x play-button-y)

  (define stop-button (<pos-button> parent stop-button-width stop-button-height stop-button-pressed
                                    ':report-releasebutton #f
                                    ':gfx-pressed-filename (config-full-url "stop_pressed.png")))
  (-> parent add-sub-area stop-button stop-button-x stop-button-y)

  (define gen-button (<pos-button> parent gen-button-width gen-button-height (lambda ()
                                                                               (-> melodi-area gen)
                                                                               (weigh-all-notes))
                                   ':report-releasebutton #t
                                   ':gfx-pressed-filename (config-full-url "generer_pressed.png")))
  (-> parent add-sub-area gen-button gen-button-x gen-button-y)

  (define invert-button (<pos-button> parent invert-button-width invert-button-height invert-all-notes))
  (-> parent add-sub-area invert-button invert-button-x invert-button-y)

  (define reverse-button (<pos-button> parent reverse-button-width reverse-button-height reverse-all-notes))
  (-> parent add-sub-area reverse-button reverse-button-x reverse-button-y)

  (define glissando-button (<pos-button> parent glissando-button-width glissando-button-height glissando-all-notes))
  (-> parent add-sub-area glissando-button glissando-button-x glissando-button-y)


  (melodigenerator-skalavelger parent)

  (define reset-button (<pos-button> parent reset-button-width reset-button-height (<- melodi-area reset)
                                     ':report-releasebutton #t
                                     ':gfx-pressed-filename (config-full-url "nullstill_pressed.png")))
  (-> parent add-sub-area reset-button reset-button-x reset-button-y)
  
  (define save-button (<pos-button> parent save-button-width save-button-height (lambda () (save-local parent))
                                    ':report-releasebutton #t
                                    ':gfx-pressed-filename (config-full-url "saveasfile_pressed.png")))
  (-> parent add-sub-area save-button save-button-x save-button-y)

  (define random-button (<pos-button> parent random-button-width random-button-height random-all-notes))
  (-> parent add-sub-area random-button random-button-x random-button-y)

  (define unrandom-button (<pos-button> parent unrandom-button-width unrandom-button-height unrandom-all-notes))
  (-> parent add-sub-area unrandom-button unrandom-button-x unrandom-button-y)

  ;;(init-message-area parent)

  (set! help-area (<text-area-multiline> parent help-area-width help-area-height (nth 4 (get-config-area-list-with-help 'help-area))
                                         ':font (get-font "Archer-Medium-Pro.ttf" 13.5)))
  (-> parent add-sub-area help-area help-area-x help-area-y)
  (-> help-area text-color veryverygray-color)

  ;;(define slider-test (<gfx-slider> parent 200 20 0.1 0.1
  ;;                                  (config-full-url (get-config-string 'paint-cursor))
  ;;                                  (lambda (x1)
  ;;                                    (c-display "slidertest" (* 1.0 x1)))))
  ;;(-> parent add-sub-area slider-test 200 (- help-area-y 150))

  ;;(define sound-selector-area (<soundtype> parent sound-selector-area-width sound-selector-area-height))
  ;;(-> parent add-sub-area sound-selector-area sound-selector-area-x sound-selector-area-y)


 ;;(add-gfxr-icon parent (config-full-url "piano_sel.jpg") (config-full-url "piano_unsel.jpg") (lambda () (c-display "piano selected")) 250 605)
  ;;(add-gfxr-icon parent (config-full-url "singer_sel.jpg") (config-full-url "singer_unsel.jpg") (lambda () (c-display "singer selected")) 400 605)

  (recalculate-timings (lambda () #t))

  ;;(-> mixer enableFreeVerb)


  ;; Help text for the audio palette

  (define events (-> (new-static <CreateFrame>) events))

  (let loop ((gfxr-parameterss (reverse (get-config-sound-selector-buttons))))
    (apply (lambda (x y width height name image soundfile soundfile-middle r g b help-text)
             (inc! x (-> insets left))
             (inc! y (-> insets top))
             (-> events addMousePlacementEvent x y width height
                 (java-wrap
                  (lambda ()
                    (define text (let loop ((ret "") ;; no fold found. :-(
                                            (help-text help-text))
                                   (if (null? help-text)
                                       ret
                                       (loop (<-> ret (car help-text) " ") (cdr help-text)))))
                    ;;(c-display "hepp" (car help-text) (string? (car help-text)))
                    (when help-area
                      (-> help-area set-and-force-paint! text)))))
             (if (pair? (cdr gfxr-parameterss))
                 (loop (cdr gfxr-parameterss))))
           (car gfxr-parameterss)))

  (-> parent repaint-me!)
  (-> parent check-paint)
  (c-display "FINISHED INIT MELODIGENERATOR."))



(c-define (init-melodigenerator parent)

  (-> mixer requestTo_removeAll)
  (-> mixer requestTo_setPlayPos 0)  

  (-> mixer enableFreeVerb)
  (-> (-> mixer freeVerb) setWet 0.74)
  (-> (-> mixer freeVerb) setRoomSize 0.6)
  (-> (-> mixer volume) set 1.5)

  (set! upper (get-config-number 'upper-note))
  (set! lower (get-config-number 'lower-note))

  (-> (-> parent g) set-font! ':filename "Archer-Medium-Pro.ttf" ':size 13.5)
  ;;(-> (-> parent g) set-font! ':filename "GothamRnd-Book.ttf" ':size 12.5)

  (set! default-press-color veryverygray-color)

  (let loop ((gfxr-parameterss (reverse (get-config-sound-selector-buttons))))
    (apply (lambda (x y width height name image soundfile soundfile-middle r g b help-text)
             (define x2 (+ x width))
             (define y2 (+ y height))
             (define color (<color-range> 64 (/ r 100.0) (/ g 100.0) (/ b 100.0) 0.1 1.9))
             (inc! x (-> insets left))
             (inc! y (-> insets top))
             (inc! x2 (-> insets left))
             (inc! y2 (-> insets top))
             
             (if (and #f 
                      (or (-> (new <java.lang.String> (config-full-url soundfile))
                              startsWith "http://")
                          (-> (new <java.lang.String> (config-full-url soundfile))
                              startsWith "file:")))
                 (set! soundfile (new <java.net.URL> (config-full-url soundfile)))
                 (set! soundfile (config-full-url soundfile)))

             (define audiofile-buffer #f)

             ;; This function is safe to call, even when the rest of the melodigenerator hasn't loaded yet.
             (define (selected-func)
               (c-display "    SOMETHIGN something selected " image audiofile-buffer)
               (set! *audiofile-buffer* audiofile-buffer)
               (set! *audiofile-color* color)
               (set! *audiofile-middlenote* soundfile-middle))

             (define gfxr #f)

             (define filerequester (<filerequester> parent))
             
             (let get-soundfile ((soundfile soundfile))
               (get-audiofile soundfile
                              (lambda (audiofile-buffer*)					
                                ;;(sleep 2000)
                                (c-display "            AUDIOFILE_BUFFER audiofile-buffer" audiofile-buffer* soundfile)
                                (when audiofile-buffer*
                                  (set! audiofile-buffer audiofile-buffer*)
                                  (c-display "ai6")
                                  (cond ((not gfxr)
                                         (set! gfxr (add-gfxr parent
                                                              name
                                                              (config-full-url image)
                                                              (-> color get 0.5)
                                                              selected-func
                                                              (lambda ()
                                                                (-> filerequester load "Velg lydfil. Mp3, wav eller aiff."
                                                                    (lambda (directory filename fullpath)
                                                                      (when filename
                                                                        (-> gfxr name filename)
                                                                        (-> (-> gfxr text-area) text filename)
                                                                        (-> (-> gfxr text-area) repaint-me!)
                                                                        (c-display "ai4")
                                                                        (get-soundfile fullpath)))))
                                                              x y x2 y2))
                                         (-> parent check-paint)
                                         (if (pair? (cdr gfxr-parameterss))
                                             (loop (cdr gfxr-parameterss))
                                             (init-melodigenerator2 parent)))
                                        (else
                                         (selected-func)))
                                  (c-display "ai7")))
                              (lambda (seconds end-time)
                                (c-display "Please wait, loading " soundfile "." seconds "/" end-time)))))
           
           (car gfxr-parameterss))))


 


