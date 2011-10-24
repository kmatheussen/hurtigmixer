
(c-import gfx-button)
(c-import sort)
(c-import map-note-to-grid-file)


(define *pianoroll* #f)
(define *audiofile-buffer* #f)
(define *audiofile-color* #f)
(define *audiofile-middlenote* #f)
(define *das-mixer* #f)
(define *pianoroll-speed* 1.0)
(define *pianoroll-pitch* 1.0)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Algorithmic stuff ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define duration-weight
  '( (1/1  1)
     (0.75 1)
     (1/2  5)
     (1/3  7)
     (1/4  3)
     (1/8  2)
     (1/6  8)
     (1/12 2)
     ))

(c-define (get-duration)
  (define weights '())
  (let loop ((d duration-weight))
    (when (not (null? d))
      (for-each (lambda (i)
		  (push! (caar d) weights))
		(iota (cadr (car d))))
      (loop (cdr d))))
  (* 1.0 (list-ref weights (between 0 (length weights)))))

#|
(get-duration)
|#


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; other stuff ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (note->freq note)
  (* 8.17579891564 (exp (* 0.0577622650 note))))

(define (freq->note freq)
  (/ (log (/ freq 8.17579891564))
     0.0577622650))

(define (get-real-life-note note)
  (c-integer (round (freq->note (* (note->freq note) *pianoroll-pitch*)))))


#|
(/ (note->freq 40) (* 440.0 0.5)))
|#

(define (get-tempobar-scaled-time time)
  (-> tempo-area get-position
      (lambda (t-x t-y t-x2 t-y2 t-width t-height)
	(define default-timepiece-full t-width) ;; i.e. (* t-width 1.0)
	(define actual-timepiece (-> tempo-area get-integrated-value (i-scale time 0 songlen 0 t-width)))
	(c-scale actual-timepiece 0 default-timepiece-full 0 songlen))))

#|
(map get-tempobar-scaled-time '(0 4.5 9))
(map (<- tempo-area get-integrated-value) (iota 15))
(map (<- tempo-area get-value) (iota 25))
(get-mixer-time 9)
|#


;; 0.5/10 -> 20
;; 2.0/10 -> 5
(define (get-mixer-time time)
  (/ (get-tempobar-scaled-time time) *pianoroll-speed*))


#|
(define (get-pianoroll-time mixer-time)
  (* mixer-time *pianoroll-speed*))
|#


;; binary search
(define get-pianoroll-time
  (get-config-area-list-with-help
   'melodigenerator-area
   (lambda (x y width height help-text)
     (let ((least-diff (/ songlen (- width (get-config-number 'keyboard-width)))))
  
       (lambda (mixer-time)
	 (let loop ((min 0.0)
		    (max songlen))
	   (let* ((time (average min max))
		  (test-mixer-time (get-mixer-time time)))
	     ;;(c-display min time max least-diff)
	     (cond ((< (- max time) least-diff)
		    time)
		   ((< mixer-time test-mixer-time)
		    (loop min time))
		   (else
		    (loop time max))))))))))
    

#|
(let ((pianoroll-time 0.3))
  (define mixer-time (get-mixer-time pianoroll-time))
  (list pianoroll-time mixer-time (get-pianoroll-time mixer-time)))
|#

;; 0.5/1.0 -> 0.5
;; 0.5/0.5 -> 2.0
;; 0.5/2.0 -> 0.25
(define (get-mixer-srate srate)
  (* srate *pianoroll-pitch*))
;;  (/ srate *pianoroll-speed*))



(def-class (<note> note* start length :key parent-note)
  (def-var note (c-integer note*))
  (def-var orgnote note*)
  (def-var start)
  (def-var orglength length)
  (def-var length)

  (def-var audiofile-buffer (or (and parent-note (-> parent-note audiofile-buffer)) *audiofile-buffer*))

  (def-var sound-object (new <SoundObject> audiofile-buffer (c-scale note lower upper -1.0 1.0) note-volume *das-mixer* #f))

  (def-var color (or (and parent-note (-> parent-note color)) *audiofile-color*))

  (def-var category-name (string->symbol (-> *audiofile-buffer* filename)))

  (def-var middle-note (or (and parent-note (-> parent-note middle-note)) *audiofile-middlenote*))

  (def-var border-color (-> color get 0.3))
  (def-var fill-color   (-> color get 0.4))
  (def-var shine-color1 (-> color get 0.35))
  (def-var shine-color2 (-> color get 1.0))


  (def-method (compatible? note)
    (eq? category-name (-> note category-name)))

  (def-method (get-end)
    (+ start length))

  (def-method (set-start-but-keep-length! start*)
    (set! start start*)
    (-> sound-object requestTo_setPos (get-mixer-time start)))
    
  (def-method (set-start! start*)
    (define end (get-end))
    (set! start start*)
    ;;(c-display "start-time" start)
    (-> sound-object requestTo_setPos (get-mixer-time start))
    (set! length (- end start))
    (-> sound-object requestTo_setEndReadPos (- (get-mixer-time end)
						(get-mixer-time start)))
    )

  (def-method (set-end! end)
    (set! length (- end start))
    (-> sound-object requestTo_setEndReadPos (- (get-mixer-time end)
						(get-mixer-time start)))
    )


  (def-method (get-srate)
    (let ((srate (/ (note->freq (+ note middle-note)) (/ srate 240.0))))
      ;;(c-display "setting srate to" srate)
      (get-mixer-srate srate)))

  (define (set-srate)
    (-> sound-object requestTo_setSRate (get-srate)))

  (def-method (set-note! note*)
    (set! note note*)
    (set-srate)
    (-> sound-object setPan (c-scale note lower upper -1.0 1.0)))

  (def-method (set-length! length*)
    (set-end! (+ start length*)))

  (def-method (move! note* start*)
    (define delta (- note* note))
    (set! note note*)
    (set! orgnote (+ orgnote delta))
    (set! start start*)
    (-> sound-object requestTo_setPos (get-mixer-time start))
    (set-srate)
    )

  (def-method (remove-from-mixer)
    (-> *das-mixer* requestTo_remove sound-object))

  (def-method (mixer-speed-has-changed)
    (set-start! start)
    (set-srate))

  (-> *das-mixer* requestTo_add sound-object (* srate (get-mixer-time start)))
  ;;(c-display "start-time" start)
  (set-srate)
  (-> sound-object requestTo_setEndReadPos (get-mixer-time length))

  ;;(-> sound-object requestTo_setFadeIn 0.08)
  (-> sound-object requestTo_setFadeOut 0.08)


  )

#|
(define n (<note> 50 2 3))
(-> n start)
|#

(define note-color green-color)
(define border-color black-color)
(define grid-octave-colors (<color-range> 64 (/ 228 250.0) (/ 222 255.0) (/ 226 255.0) 1.07 0.95))
(define grid-colors        (<color-range> 64 (/ 223 250.0) (/ 222 255.0) (/ 216 255.0) 1.07 0.95))

(define note-volume 0.5)

(define upper 50)
(define lower 40)

(define songlen 9.0);;mix-length);9.0)

(define *notes* '())

(define-macro (in . code)
  `(-> *pianoroll* get-position
       (lambda (x y x2 y2 width height)
	 ,@code)))

(define (get-ys note kont)
  (in (let ((ry1 (+ 2 (c-scale (+ 1 note) lower upper y2 y))))
        (kont ry1 
              (+ 1 (c-scale note lower upper y2 y))))))

(define (map-to-grid y*)
  (in (+ 2 (c-scale (+ 1 (->note y*)) lower upper y2 y))))
  
(define (->x note)
  (in (c-scale (-> note start) 0 songlen x x2)))
(define (->y note)
  (get-ys (-> note note) (lambda (y y2)
                           y)))
(define (->y_orgpos note)
  (get-ys (-> note orgnote) (lambda (y y2)
                              y)))
(define (->width note)
  (in (c-scale (-> note length) 0 songlen 0 width)))
(define (->height)
  (get-ys lower (lambda (y y2)
                  (- y2 y))))

(define (->y2 note)
  (get-ys (-> note note) (lambda (y y2)
                           y2)))
(define (->x2 note)
  (in (+ (->x note) (->width note))))

(define (->start x*)
  (in (c-scale x* x x2 0 songlen)))
(define (->note y*)
  (in (i-scale y* y y2 upper lower)))
(define (->note-float y*)
  (in (c-scale y* y y2 upper lower)))
(define (->length width*)
  (in (c-scale width* 0 width 0 songlen)))


;; Merges notes with the same pitch which overlap in time.
(c-define (cleanup-note-list :optional notes)
  (define (overlap? a b)
    (and (>= (-> a start)
	     (-> b start))
	 (< (-> a start)
	    (+ (-> b start) (-> b length)))))

  (define (expand-note! note with)
    (-> note set-start! (min (-> with start) (-> note start)))
    (-> note set-end!   (max (-> note get-end) (-> with get-end))))
  
  (define (doit notes)
    (define has-changed #f)
    (let loop ((notes (sort-all-notes2 notes)))
      (define note (and      (pair? notes)       (car notes)))
      (define next (and note (pair? (cdr notes)) (cadr notes)))
      (cond ((not note)
	     '())
	    ((not next)
	     (list note))
	    ((and (= (-> note note) (-> next note))
		  (or (overlap? note next)
		      (overlap? next note)))
	     (expand-note! note next)
	     (-> next remove-from-mixer)
	     (loop (cons note
			 (cddr notes))))
	    (else
	     ;;(c-display (= (-> note note) (-> next note))
	     ;;	          (overlap? note next)
	     ;;	          (overlap? next note))
	     (cons note
		   (loop (cdr notes)))))))

  (if (not notes)
      (set! *notes* (apply append (map doit (get-segregated-*notes*))))
      (doit notes)))


(c-define (cleanup-note-list-with-respect-to-last-added-note :optional notes)
  (define (overlap? a b)
    (and (>= (-> a start)
	     (-> b start))
	 (< (-> a start)
	    (+ (-> b start) (-> b length)))))

  (define (remove-same note notes is-removed-func)
    (define first (and (pair? notes) (car notes)))
    (cond ((and first
		(= (-> note note) (-> first note))
		(or (overlap? note first)
		    (overlap? first note)))
	   (is-removed-func first)
	   (cdr notes))
	  (first
	   (cons first
		 (remove-same note (cdr notes) is-removed-func)))
	  (else
	   '())))
  
  (define (expand-note note with)
    (-> note set-start! (min (-> with start) (-> note start)))
    (-> note set-end!   (max (-> note get-end) (-> with get-end))))

  (define (doit notes)
    (let loop ((notes notes))
      (if (null? notes)
	  '()
	  (let ((note (car notes)))
	    ;;(c-display "note" note)
	    (define got-same #f)
	    (define rest (remove-same note (cdr notes)
				      (lambda (same)
					;;(c-display "same" same)
					(expand-note note same)
					(set! got-same #t)
					(-> same remove-from-mixer))))
	    (if got-same
		(loop (cons note rest))
		notes)))))

  (if (not notes)
      (set! *notes* (apply append (map doit (get-segregated-*notes*))))
      (doit notes)))



(c-define (add-note x* y* width*)
  (define notenum (->note-float y*))
  (when (and (>= notenum lower)
             (< notenum upper))
    (define note (<note> (->note-float y*) (->start x*) (->length width*)))
    (push! note *notes*)
    ;;(cleanup-note-list-with-respect-to-last-added-note)
    (cleanup-note-list)
    note))

#|
(add-note 50 1 2)
(add-note 50 1.5 2)
(repaint-note win (car *notes*))
(-> win check-paint)
|#


(c-define (repaint-note parent note)
  (-> parent repaint (->x note) (->y note) (->x2 note) (->y2 note)))

(c-define (remove-note parent x* y* width* :key only-current)
  (define add-notes '())
  (define remove-notes '())
  (define tone (->note y*))
  (define start (->start x*))
  (define end (->start (+ x* width*)))


  (for-each (lambda (note)

              ;; note-start and note-end contains a bit more time before and after. The reasons is to avoid leaving very short notes.
              ;; These two are only used in testing, not when setting new start and end points.
              (define note-start (+ (-> note start) 0.1)) 
              (define note-end   (- (-> note get-end) 0.1))

	      (when (and (= tone (-> note note))
			 (or (not only-current)
			     (eq? (-> note audiofile-buffer) *audiofile-buffer*)))
		(cond ((and (>  end   note-start)        ;; cut start
			    (<= start note-start)
			    (<  end   note-end))
		       (-> note set-start! end)
		       (repaint-note parent note))
		      ((and (<= start note-start)        ;; cut entire note
			    (>=  end  note-end))
		       (push! note remove-notes)
		       (-> note remove-from-mixer)
		       (repaint-note parent note))
		      ((and (> start note-start)         ;; cut end
			    (< start note-end)
			    (> end   note-end))
		       (-> note set-end! start)
		       (repaint-note parent note))
		      ((and (> start note-start)         ;; split note
			    (< end   note-end))
		       (push! (<note> tone end (- (-> note get-end) end) ':parent-note note) add-notes)
		       (-> note set-end! start)
		       (repaint-note parent note)))))
	    *notes*)

  (when remove-notes
    (set! *notes* (let loop ((notes *notes*))
		    (if (pair? notes)
			(if (memq (car notes) remove-notes)
			    (loop (cdr notes))
			    (cons (car notes) (loop (cdr notes))))
			'()))))

  (for-each (lambda (note)
              (push! note *notes*)
              (repaint-note parent note))
            add-notes)
  
  *notes*)
			 
(c-define (set-note-lengths ratio)
  (for-each (lambda (note)
	      (-> note set-length! (* ratio (-> note orglength))))
	    *notes*))

(define shake-factor 0.2)
(c-define (shake-all-notes)
  (for-each (lambda (note)
	      (define direction (between 0 (* 2 3.14159)))
	      (define length (between 0 shake-factor))
	      (define away (* (/ length 5) (cos direction)))
	      (define up (* length (sin direction)))
	      (c-display "away/up" away up)
	      (-> note orgnote (boundaries lower (+ (-> note orgnote) up) upper))
	      (-> note set-note! (c-integer (-> note orgnote)))
	      (-> note set-start-but-keep-length! (boundaries 0.0 (+ (-> note start) away) (- songlen 0.1)))
	      )
	    *notes*)
  (-> *pianoroll* repaint-me!))

(define quantize-size 0.3)
(c-define (get-quantize-val val)
  (let loop ((try-val 0))
    (if (>= try-val val)
        try-val
        (loop (+ try-val quantize-size)))))

(c-define (quantize-all-notes)
  (for-each (lambda (note)
              (define pos (-> note start))
              (define end (-> note get-end))
              (-> note set-start! (get-quantize-val pos))
              (-> note set-end! (get-quantize-val end)))
	    *notes*)
  (-> *pianoroll* repaint-me!))

(c-define (invert-all-notes)
  (for-each (lambda (note)
	      (-> note set-note! (i-scale (-> note note) lower upper upper lower))
	      (-> note orgnote (c-scale (-> note orgnote) lower upper upper lower))
	      )
	    *notes*)
  (-> *pianoroll* repaint-me!))

(c-define (reverse-all-notes)
  (for-each (lambda (note)
	      (define length (-> note length))
	      (-> note set-start-but-keep-length! (- (c-scale (-> note start) 0 songlen songlen 0)
						     length))
	      )
	    *notes*)
  (-> *pianoroll* repaint-me!))

;; Sort by start-time
(c-define (sort-all-notes :optional notes)
  (let ((ret (sort! (if notes notes *notes*) (lambda (a b)
					       (< (-> a start) (-> b start))))))
    (if (not notes)
	(set! *notes* notes))
    ret))
	
;; First sort by note, then by start-time
(c-define (sort-all-notes2 :optional notes)
  (let ((ret (sort! (if notes notes *notes*) (lambda (a b)
					       (define note-a (-> a note))
					       (define note-b (-> b note))
					       (cond ((< note-a note-b)
						      #t)
						     ((> note-a note-b)
						      #f)
						     (else
						      (< (-> a start) (-> b start))))))))
    (if (not notes)
	(set! *notes* notes))
    ret))
	
(define glissando-newnote-length 0.4)


(c-define (get-segregated-*notes*)
  (let loop ((notes *notes*)
	     (ret '()))
    (cond ((null? notes )
	   ;;ret
	   (map cdr (sort! ret (lambda (a b)
				 (string<? (symbol->string (car a)) (symbol->string (car b))))))
	   )
	  (else
	   (let* ((note (car notes))
		  (seg (assq (-> note category-name) ret)))
	     (cond (seg
		    (set-cdr! seg (cons note (cdr seg)))
		    (loop (cdr notes) ret))
		   (else		       
		    (loop (cdr notes)
			  (cons (list (-> note category-name)
				      note)
				ret)))))))))

#|
(pretty-print (get-segregated-*notes*))

(begin *notes*)
|#		       
	   
(c-define (glissando-all-notes)
  (for-each (lambda (notes)
	      (define next-stop 1)
	      (let ((note (car notes)))
		(when (and note
			   (eq? *audiofile-color* (-> note color))
			   (eq? *audiofile-buffer* (-> note audiofile-buffer)))
		  (let loop ((notes (sort-all-notes (list-copy notes))))
		    (when (and (not (null? notes))
			       (not (null? (cdr notes))))
		      (define a (car notes))
		      (define b (cadr notes))
		      
		      (define start-time (-> a start))
		      (define end-time (-> b start))
		      (define start-note (-> a note))
		      (define end-note (-> b note))
		      
		      ;; Up
		      (let loop ((curr-note (1+ start-note)))
			(when (< curr-note end-note)
			  (push! (<note> curr-note (c-scale curr-note start-note end-note start-time end-time) glissando-newnote-length)
				 *notes*)
			  (loop (1+ curr-note))))
		      
		      ;; Down
		      (let loop ((curr-note (1- start-note)))
			(when (> curr-note end-note)
			  (push! (<note> curr-note (c-scale curr-note start-note end-note start-time end-time) glissando-newnote-length)
				 *notes*)
			  (loop (1- curr-note))))
		      
		      (loop (cdr notes)))))))
	    (get-segregated-*notes*))

  (cleanup-note-list)
  (-> *pianoroll* repaint-me!))

(c-define (weigh-all-notes :optional new-weight)
  (if new-weight      
      (-> scaleselect-area set-new-weights! new-weight))
  (remap-notes-to-grid)
  (-> scaleselect-area repaint-me!)
  (-> *pianoroll* repaint-me!))


(c-define (random-all-notes)
  (let loop ((i 0))
    (when (< i 30)
      (push! (<note> (between lower upper) (between 0 songlen) (between 0.1 0.8))
	     *notes*)
      (loop (1+ i))))
  (cleanup-note-list)
  (-> *pianoroll* repaint-me!))

(c-define (unrandom-all-notes)
  (in
   (let loop ((i 0))
     (when (< i 20)
       (remove-note win (between x x2) (between y y2) (between (/ width 20) (/ width 10)) ':only-current #t)
       (loop (1+ i)))))
  (cleanup-note-list)
  (-> *pianoroll* repaint-me!))

		 
	      

(define time-setting-time (-> static-java.lang.System currentTimeMillis))
(define time-last-set-time 0)

(c-define (recalculate-timings set-vals-func)
  (define pianoroll-time #f)
  (define time-since-last-time (/ (- (-> static-java.lang.System currentTimeMillis)
				     time-setting-time)
				  1000.0))
  (if (< time-since-last-time 0.5)
      (set! pianoroll-time (get-pianoroll-time (+ time-last-set-time time-since-last-time)))
      (set! pianoroll-time (get-pianoroll-time (-> *das-mixer* getPlayPos))))
  
  (set-vals-func)

  (for-each (lambda (note)
	      (-> note mixer-speed-has-changed)
	      #t)
	    *notes*)
  (-> *das-mixer* setLength (get-mixer-time mix-length))

  (let ((new-mixer-time (get-mixer-time pianoroll-time)))
    ;;(c-display "pianoroll-time: " pianoroll-time new-mixer-time)
    (set! time-last-set-time new-mixer-time)
    (set! time-setting-time (-> static-java.lang.System currentTimeMillis))
    (-> *das-mixer* requestTo_setPlayPos new-mixer-time)))


(c-define (set-pianoroll-speedpitch! new-speed new-pitch)
  (recalculate-timings (lambda ()
			 (set! *pianoroll-speed* new-speed)
			 (set! *pianoroll-pitch* new-pitch)))
  )


(c-define (find-category-names)
  (let loop ((notes *notes*)
             (category-names '()))
    (if (null? notes)
        category-names
        (loop (cdr notes)
              (delete-duplicates (cons (-> (car notes) category-name) category-names) eq?)))))

(c-define (get-note x* y*)
  (find (lambda (note)
	  (and (>  x* (->x  note))
	       (<= x* (->x2 note))
	       (>  y* (->y  note))
	       (<= y* (->y2 note))))
	*notes*))


(c-define (mixertime->ticks time ticks-per-second)
  (* (get-mixer-time time) (/ ticks-per-second 1)))

(c-define (get-notelist category-name)
  (map (lambda (note)
         (list (-> note note)
               (-> note start)
               (-> note get-end)))
       (filter (lambda (note)
                 (eq? category-name (-> note category-name)))
               *notes*)))

(c-define (get-midi-events default-velocity ticks-per-second channel category-name)
  (define ret '())
  (for-each (lambda (event)
              (apply (lambda (note start end)
                       (push! (list channel note default-velocity (mixertime->ticks start ticks-per-second)) ret)
                       (push! (list channel note 0                (mixertime->ticks end   ticks-per-second)) ret))
                     event))
            (get-notelist category-name))
  ret)


(c-define* (save-midi parent :key tempfilename cont)
  (-> (get-save-filerequester parent) save "Velg plassering. Filending boer ende paa .mid"
      (lambda (directory filename fullpath)
        (when filename
          (set! fullpath (add-filetype fullpath "mid"))
          (define midisystem (new-static <javax.sound.midi.MidiSystem>))
          (define midi-divisiontype (-> (new-static <javax.sound.midi.Sequence>) PPQ))
          (define midi-resolution 10000)
          (define ticks-per-second (* midi-resolution 2));(* midi-divisiontype midi-resolution))
          (define category-names (find-category-names))                      
          (define seq (new <javax.sound.midi.Sequence> midi-divisiontype midi-resolution))
          (define track (-> seq createTrack))
          (for-each (lambda (channel category-name)
                      (for-each (lambda (event)
                                  (c-display "event" event)
                                  (apply (lambda (channel note velocity tick)
                                           (set! note (get-real-life-note (+ 36 note))) ;; take the *pianoroll-pitch* into account
                                           (when (and (> note 0)
                                                      (< note 128))
                                             (define midimessage (new <javax.sound.midi.ShortMessage>))
                                             (-> midimessage setMessage (+ 144 channel) note velocity)
                                             (define midievent (new <javax.sound.midi.MidiEvent> midimessage tick))
                                             (-> track add midievent)))
                                         event))
                                (get-midi-events 64 ticks-per-second channel category-name)))
                    (iota (length category-names))
                    category-names)
          (-> midisystem write seq (if (> (length category-names) 1) 1 0) (new <java.io.File> fullpath))))))


#|
(write-midi-file "/tmp/test.mid")

(get-notelist)
(begin ticks-per-second)
|#

;;(new <javax.sound.midi.MidiMessage> 1 1 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Algorithmic stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random)
  (-> java.lang.Math random))
(define (between a b)
  (c-scale (random) 0 1 a b))
(define (int-between a b)
  (i-scale (random) 0 1 a b))

(c-define (get-melodi-old)
  (define min-length 0.1)
  (define max-length 1.5)
  (define pause-chance 0.1)
  (define max-jump 2)
  (let loop ((start 0)
	     (last-note (average lower upper))
	     (notes '()))
    (define note (if (<= (random) pause-chance)
		     0
		     (boundaries lower (between (- last-note max-jump) (+ last-note max-jump)) upper)))
    (define length (between min-length max-length))
    (if (> (+ start length) songlen)
	(set! length (- songlen start)))
    (cond ((< length min-length)
	   (reverse! notes))
	  (else
	   (loop (boundaries 0.01 
			     (between (- start (/ length 1.5)) (+ start length)) 
			     songlen)
		 (if (= 0 note)
		     last-note
		     note)
		 (if (= 0 note)
		     notes
		     (cons (list note
				 start
				 length)
			   notes)))))))

(define (get-middle-C)
  (let loop ((low (c-integer (average lower upper)))
             (high (c-integer (average lower upper))))
    (cond ((= root-note (get-chroma low))
           low)
          ((= root-note (get-chroma high))
           high)
          (else
           (loop (1- low)
                 (1+ high))))))


(c-define (get-melodi)
  (define min-length 0.1)
  (define max-length 1.5)
  (define pause-chance 0.02)
  (define max-jump (int-between 2 12))
  (let loop ((start 0)
	     (last-note (average lower upper))
	     (notes '()))
    (define note (cond ((= 0 start)
                        (get-middle-C))
                       ((<= (random) pause-chance)
                        0)
                       (else
                        (boundaries (+ lower 4)
                                    (between (if (< last-note (+ lower 8))
                                                 last-note
                                                 (- last-note max-jump))
                                             (if (> last-note (- upper 8))
                                                 last-note
                                                 (+ last-note max-jump)))
                                    (- upper 4)))))
    (define length (* 1.0 (get-duration)));(between min-length max-length))
    (if (> (+ start length) songlen)
	(set! length (- songlen start)))
    (cond ((>= (+ length start) songlen)
	   (reverse! notes))
	  (else
	   (loop (+ start length)
		 (if (= 0 note)
		     last-note
		     note)
		 (if (= 0 note)
		     notes
		     (cons (list note
				 start
				 (* (between 1.0 1.5) length))
			   notes)))))))
#|
(get-melodi)
|#

(c-define (generate-*notes*)
  ;;(set! *notes* '())
  (for-each (lambda (tone)
	      (push! (apply <note> tone) *notes*)
	      (cleanup-note-list))
	    (get-melodi)))


#|
(begin
  (-> mixer requestTo_removeAll)
  (-> mixer requestTo_setPlayPos 0)  
  (generate-*notes*)
  (-> *pianoroll* repaint-me!)
  (-> win check-paint))

|#


#|
(-> java.lang.Math random)
(rand 2)
(between 2 3)
|#

#|
(c-define (get-cursor gfx-filename)
  (-> (-> (new-static <java.awt.Toolkit>) getDefaultToolkit) createCustomCursor
      (-> (-> (new-static <java.awt.Toolkit>) getDefaultToolkit) getImage
	  (if (or is-running-applet is-running-standalone)
	      (new <java.net.URL> (string-append urlbase gfx-filename))
	      gfx-filename))
      (new <java.awt.Point> 10 10)
      gfx-filename))

(define paint-cursor (get-cursor "paint.png"))
(define erase-cursor (get-cursor "erase.png"))
|#

(c-define (set-cursor! cursor)
  (-> (-> win get-frame) setCursor
      cursor))

(define (get-current-cursor)
  (-> (-> win get-frame) getCursor))


(define (get-weight note)
  (define chroma (modulo note 12))
  (if (not (vector-ref (-> scaleselect-area on-offs) chroma))
      0.0
      (vector-ref (-> scaleselect-area weights) chroma)))

  
#|
(set-cursor! paint-cursor)
(set-cursor! erase-cursor)
(set-cursor! java-null)
|#


(def-area-subclass (<pianoroll> parent width height das-mixer)

  (set! *das-mixer* das-mixer)

  (define current-cursor-x #f)
  (define prev-cursor-x 0)
  (define note-alpha alpha-0.40)

  ;; Called from the cursor update thread in the sound engine.
  (def-method (paint-cursor :optional (playpos (-> das-mixer getPlayPos)))
    (set! current-cursor-x (c-scale (get-pianoroll-time playpos) 0 songlen x x2))
    ;;(c-display "paint-cursor playpos " playpos "/" (get-pianoroll-time playpos))
    (-> parent repaint current-cursor-x y (1+ current-cursor-x) y2)
    (-> parent repaint (1- prev-cursor-x) y (1+ prev-cursor-x) y2)
    (-> parent check-paint)
    (set! prev-cursor-x current-cursor-x)
    )

  (-> (-> das-mixer cursor) addCursorCallback
      (java-wrap paint-cursor))

  ;; Called from this->paint
  (define (paint-mixer-cursor g)
    (-> g draw-line black-color current-cursor-x y current-cursor-x y2)
    ;;(c-display "hepp")
    )

  (define (paint-grid g y* y2*)
    (define name-height (-> scaleselect-area name-height))
    (c-for lower < upper 1
	   (lambda (note)
             (get-ys note (lambda (y y2)
                            (when (or (and (>= y y*)
                                           (< y y2*))
                                      (and (>= y2 y*)
                                           (< y2 y2*)))
                              (define weight (get-weight note))
                              (define chroma (modulo note 12))
                              (define color (if (= root-note chroma)
                                                (-> grid-octave-colors get weight)
                                                (-> grid-colors get weight)))                              
                              (-> g fill-rect color x y width (- y2 y))))))))

  (define (paint-note g note x* y* x2* y2*)
    (define nx (->x note))
    (define ny (->y note))
    (define ny_orgpos (->y_orgpos note))
    (define nw (->width note))
    (define nh (->height))
    (define nx2 (+ nx nw))
    (define ny2 (+ ny nh))
    
    (when (and (> nx2  x*)
               (< nx  x2*)
               (> ny2  y*)
               (< ny  y2*))
      (-> g do-alpha note-alpha
          (lambda ()
            (-> g fill-rect (-> note fill-color) nx ny nw nh)))
      (-> g draw-rect (-> note border-color) nx ny nw nh)
      (-> g draw-line (-> note shine-color1) nx ny (- nx2 2) ny)
      (-> g draw-line (-> note shine-color1) nx ny nx (- ny2 2))
      (-> g draw-line (-> note shine-color2) (1+ nx) (+ 1 ny) (- nx2 2) (+ 1 ny))
      (-> g draw-line (-> note shine-color2) (1+ nx) (+ 1 ny) (1+ nx) (- ny2 2))
      )
    )

  (def-method (paint g x* y* x2* y2*)
    (paint-grid g y* y2*)
    (for-each (lambda (note)
                (paint-note g note x* y* x2* y2*))
              (apply append (get-segregated-*notes*)))
    (if current-cursor-x
	(paint-mixer-cursor g))
    )

  (define paint-cursor (<gfx-button> parent (config-full-url (get-config-string 'paint-cursor)) (lambda x x))) 
  (define erase-cursor (<gfx-button> parent (config-full-url (get-config-string 'erase-cursor)) (lambda x x))) 
  
  (define default-cursor (get-current-cursor))
  
  (define paint-cursor-width (* 3 (/ height (- upper lower)))) ; (min (-> paint-cursor width) (-> erase-cursor width)))
  (define erase-cursor-width (* 3 (/ height (- upper lower)))) ;(min (-> paint-cursor width) (-> erase-cursor width)))
  
  (define paint-cursor-skew -28)
  (define erase-cursor-skew -15)


  ;; Play
  (let ((func (lambda (button x* y*)
		(when (= button 5)
		  (-> melodi-area play (get-mixer-time (c-scale x* x x2 0 songlen)))
		  #f))))
    (add-mouse-cycle func func func))

  ;; Stop
  (let ((func (lambda (button x* y*)
		(when (= button 4)
		  (-> melodi-area stop)
		  #f))))
    (add-mouse-cycle func func func))


  (def-method (key-pressed key-event)
    (let ((key-code (-> key-event getKeyCode)))
      (c-display "key-event/key-code" key-event key-code)
      (when (= key-code (-> key-event VK_SPACE))
	(if (-> mixer isPlayingQuestionmark)
	    (-> melodi-area stop)
	    (-> melodi-area play))
	#t)))

  (define (repaintcursor)
    (-> paint-cursor get-position
	(lambda (x y x2 y2 width height)
	  (-> parent repaint x (- y (->height)) (1+ x2) (+ 1 y2 (->height))))))

  (define singleplayernote #f)
  (define (singleplay note)
    (if singleplayernote
        (-> *das-mixer* stopSingleFilePlayer))
    (-> *das-mixer* addSingleFilePlayer (-> note audiofile-buffer) 0 (-> note get-srate) note-volume)
    (set! singleplayernote note))

  ;; Add notes
  (define last-note #f)
  (add-mouse-cycle (lambda (button x* y*)
		     (when (= button *left-mouse-button*)
		       (set-cursor! (-> Various transparentCursor))
		       (-> parent add-sub-area paint-cursor x* (+ y* paint-cursor-skew));;(map-to-grid y*))
		       (repaintcursor)
                       (let ((note (add-note x* y* paint-cursor-width)))
                         (when note
                           (singleplay note)
                           (set! last-note (-> note note))
                           'grab))))
		   (lambda (button x* y*)
		     (-> paint-cursor repaint-me!)
		     (-> paint-cursor set-position! x* (+ y* paint-cursor-skew));;(map-to-grid y*))
		     (repaintcursor)
                     (let ((note (add-note x* y* paint-cursor-width)))
                       (if note
                           (when (not (= (-> note note) last-note))
                             (singleplay note)
                             (set! last-note (-> note note))))))
		   (lambda (button x* y*)
                     (-> *das-mixer* stopSingleFilePlayer)
		     (-> paint-cursor repaint-me!)
		     (-> parent remove-sub-area paint-cursor)
		     (set-cursor! default-cursor)
		     (add-note x* y* paint-cursor-width)
		     (repaint-me!)
		     ))
  
  ;; Remove notes
  (add-mouse-cycle (lambda (button x* y*)
		     (when (= button *right-mouse-button*)
		       (set-cursor! (-> Various transparentCursor))
		       (-> parent add-sub-area erase-cursor x* (+ erase-cursor-skew (map-to-grid y*)))
		       (remove-note parent x* y* paint-cursor-width)
		       'grab))
		   (lambda (button x* y*)
		     (-> erase-cursor repaint-me!)
		     (-> erase-cursor set-position! x* (+ erase-cursor-skew (map-to-grid y*)))
		     (remove-note parent x* y* paint-cursor-width)
		     (-> erase-cursor repaint-me!)
		     )
		   (lambda (button x* y*)
		     (-> erase-cursor repaint-me!)
		     (-> parent remove-sub-area erase-cursor)
		     (remove-note parent x* y* paint-cursor-width)
		     (set-cursor! default-cursor)
		     (repaint-me!)
		     ))
  
  
  (define (move-note note x* y*)
    (-> note move! (->note y*) (->start x*)))
  
  
  ;; Move note (middle button)
  (let ((note #f)
	 (start-x 0)
	 (start-y 0))
    (define (moved button dx dy)
      (-> parent repaint (->x note) (->y note) (1+ (->x2 note)) (1+ (->y2 note)))
      (define nx (+ start-x dx))
      (define ny (+ start-y dy))
      (move-note note nx ny)
      (-> parent repaint (->x note) (->y note) (1+ (->x2 note)) (1+ (->y2 note))))
    (apply add-mouse-cycle
	   (delta-mouse-cycle parent
			      (lambda (button x* y*)
				(when (= button *middle-mouse-button*)
				  (define note* (get-note x* y*))
				  (when note*
				    (set! note note*)
				    (set! start-x (->x note))
				    (set! start-y (->y note))
				    'grab)))
			      moved
			      moved)))


  )


