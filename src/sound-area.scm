

(c-import fade-area)



;;; (x+threshold)=sx1 <= px1 <= px2 <= sx2=(x2-threshold)

(define wave-color (new <java.awt.Color> 0.33 0.26 0.21))
(define wave-shade-color (new <java.awt.Color> 0.83 0.76 0.71))

(define *wave-text-color* blue-color)
(define wave-non-color light-gray-color)
(define *large-background-text-color* blue-color)

(define *sound-area-threshold* 20)

(define paint-waveforms #t)

(set! wave-color light-gray-color)
(set! wave-shade-color black-color)

;;(set! wave-color gray-color)
(set! wave-shade-color black-color)

(define *square-rect-color* gray-color)
(define *square-text-color* black-color)

(define *min-srate* 0.01)
(define *max-srate* 10)


(define *fade-in-time* 0.05)
(define *fade-out-time* 0.05)


(define *min-sound-length* 0.01)

(c-define (number->two-dec-string number)
  (define neg (< number 0))
  (if neg
      (set! number (abs number)))
  (set! number (c-integer (* 100 number)))
  (define whole (c-integer (/ number 100)))
  (define dec (- number (* whole 100)))
  (<-> (if neg "-" "") (number->string whole) (if (< dec 10) ".0" ".") (number->string dec)))

#|
(c-integer (* 100 -0.1))
(c-integer (/ (* 100 -0.1) 100))
(number->two-dec-string -0.1)
(round -0.6)
(floor 0.6)
(floor -0.1)
(truncate -0.1)

|#


(c-define (<sound-area-from-state> parent s time pan)

  (define sound-object (new <SoundObject> (s 'audiofile-buffer) pan (s 'vol) mixer #f))
  (-> mixer requestTo_add sound-object
      (* (if (s 'backwards)
	     (+ time (- (s 'end-read-pos) (s 'start-read-pos))) ;;(-> sound-object getAbsLength) 
	     (+ time (s 'start-read-pos)))
	 srate))

  (-> sound-object requestTo_setSRate (s 'srate))
  (-> sound-object reverse (s 'backwards))
  (-> sound-object requestTo_setStartReadPos (s 'start-read-pos))
  (-> sound-object requestTo_setEndReadPos (s 'end-read-pos))
  (-> timebar get-shown-times
      (lambda (shown-start-time shown-end-time)
	(define m-width (-> mixer-area width))
	(define res (<sound-area> parent sound-object (s 'audiofile-buffer) 
				  (+ (i-scale (- (s 'end-read-pos) (s 'start-read-pos)) 0 (- shown-end-time shown-start-time) 0 m-width) (* 2 *sound-area-threshold*)) 
				  (-> mixer-area sound-object-height)
				  time (+ time (- (s 'end-time) (s 'start-time)))
				  mixer-area
				  (s 'press-callback)
				  (s 'set-pos-callback)
				  (s 'release-callback)))
	(-> res start-read-pos (s 'start-read-pos))
	(-> res end-read-pos (s 'end-read-pos))
	(-> (-> res fade-in) set-time! (s 'fade-in-time))
	(-> (-> res fade-out) set-time! (s 'fade-out-time))
	res)))



(def-area-subclass (<sound-area> parent sound-object* audiofile-buffer*
				 width height 
				 start-time*
				 end-time*

				 mixer-area

				 press-callback

				 set-pos-callback

				 release-callback)

  (def-var sound-object sound-object*)
  (def-var audiofile-buffer audiofile-buffer*)
  (define channels (-> audiofile-buffer nChannels))

  ;;(define envelope (new <Envelope> (-> sound-object getStartPlay_inSamples) 0 (-> sound-object getEndPlay_inSamples) 1))
  ;;(-> sound-object requestTo_addEnvelope envelope)

  (def-var start-time start-time*)
  (def-var end-time end-time*)

  (def-method (start-playtime)
    ;;(c-display "abs-start-time: " (- start-time start-read-pos) ", start-time:" (* 1.0 start-time) ", start-read-pos:" start-read-pos)
    (+ start-time start-read-pos))

  (def-method (end-playtime)
    (+ start-time end-read-pos))
  

  (def-var background-color (-> audiofile-buffer background-color))


  (def-var readpos-color yellow-color)
  
  ;;(def-var selected-sound #t)

  (def-var backwards (-> sound-object reverse))


  ;; Vars for start and end reading pos'.
  (def-var start-read-pos 0)
  (def-var end-read-pos (- end-time start-time))

  (def-method (get-playlength)
    (- end-read-pos start-read-pos))


  (define threshold *sound-area-threshold*)
  (def-var threshold-y 5)
  
  (define pathless-filename (filename-without-path (-> sound-object getFileName)))


  (def-var paint-background #t)
  
  (def-var large-background-text #f)

  (def-method (get-abs-length)
    (- end-time start-time))




  (def-var fade-in (<fade-area> parent this *fade-in-time* ':in? #t))
  (def-var fade-out (<fade-area> parent this *fade-out-time* ':in? #f))




  (def-method (get-sx1/sx2 cont)
    (cont (+ x threshold) (- x2 threshold)))

    
  (def-method (get-px1/px2 cont)
    (get-sx1/sx2 (lambda (sx1 sx2)
		   (define length (get-abs-length))
		   (define rp-x1 (i-scale start-read-pos 0 length sx1 sx2))
		   (define rp-x2 (i-scale end-read-pos 0 length sx1 sx2))
		   (cont rp-x1 rp-x2))))
  (def-method (get-px1)
    (get-px1/px2 (lambda (px1 px2) px1)))
  (def-method (get-px2)
    (get-px1/px2 (lambda (px1 px2) px2)))
  
  (def-method (get-square-poss cont)
    (get-px1/px2 (lambda (px1 px2)
		   (define x-inc (c-integer (/ (- px2 px1) 4)))
		   (define y-inc (c-integer (/ height 3)))
		   (cont (- px1 threshold) (+ px1 x-inc) (- px2 x-inc -1) (+ px2 threshold -1)
			 y (+ y y-inc) (- y2 y-inc -1) (1- y2)))))

  (def-method (fade-in-square cont)
    (get-square-poss (lambda (x1 x2 x3 x4 y1 y2 y3 y4)
		       (cont x1 y1 x2 y2))))
  (def-method (fade-in-square cont)
    (get-square-poss (lambda (x1 x2 x3 x4 y1 y2 y3 y4)
		       (cont x1 y1 x2 y2))))
  (def-method (fade-out-square cont)
    (get-square-poss (lambda (x1 x2 x3 x4 y1 y2 y3 y4)
		       (cont x3 y1 x4 y2))))
  (def-method (startsound-square cont)
    (get-square-poss (lambda (x1 x2 x3 x4 y1 y2 y3 y4)
		       (cont x1 y2 x2 y3))))
  (def-method (endsound-square cont)
    (get-square-poss (lambda (x1 x2 x3 x4 y1 y2 y3 y4)
		       (cont x3 y2 x4 y3))))
  (def-method (resample1-square cont)
    (get-square-poss (lambda (x1 x2 x3 x4 y1 y2 y3 y4)
		       (cont x1 y3 x2 y4))))
  (def-method (resample2-square cont)
    (get-square-poss (lambda (x1 x2 x3 x4 y1 y2 y3 y4)
		       (cont x3 y3 x4 y4))))
  (def-method (volume-square cont)
    (get-square-poss (lambda (x1 x2 x3 x4 y1 y2 y3 y4)
		       (cont x2 y1 x3 y3))))
  (def-method (reverse-square cont)
    (get-square-poss (lambda (x1 x2 x3 x4 y1 y2 y3 y4)
		       (cont x2 y3 x3 y4))))
  (def-method (move-square cont)
    (get-square-poss (lambda (x1 x2 x3 x4 y1 y2 y3 y4)
		       (cont x1 y1 x4 y4))))


  (def-method (is-inside-square? square x* y*)
    (square (lambda (x1 y1 x2 y2)
	      (and (>= x* x1)
		   (< x* x2)
		   (>= y* y1)
		   (< y* y2)))))

  (def-method (paint-square g square color)
    (square (lambda (x1 y1 x2 y2)
	      (-> g fill-rect color x1 y1 (- x2 x1 1) (- y2 y1 1)))))

  (define* (square-mouse-cycle square press drag release :key (check-button *left-mouse-button*) release-thunk)
    (delta-mouse-cycle parent
		       (lambda (button x* y*)
			 (if (and (= button check-button)
				  (is-inside-square? square x* y*))
			     (get-px1/px2 (lambda (px1 px2)
					    (press px1 px2)
					    (lift-me!)
					    (-> mixer-area set-curr-sound-area! this)
					    'grab))
			     #f))
		       drag
		       release
		       ':release-thunk release-thunk
		       ))



  ;; Painted by the current sound
  (def-method (paint-border g)

    (if (not (= 0 y)) ;; paint-border might be called even if the sound-area hasn't been positioned yet.

	(let ()
	  (define px1 (get-px1))
	  (define px2 (get-px2))
	  
	  (define m-x (-> mixer-area x))
	  (define m-x2 (-> mixer-area x2))
	  (define x-clip (max m-x px1))
	  (define x2-clip (min m-x2 px2))
	  (-> g setClip x-clip y (- x2-clip x-clip) (- y2 y))

	  ;; Text
	  (if large-background-text
	      (-> g do-alpha alpha-0.10
		  (lambda ()
		    (-> g draw-string-in-box
			(if large-background-text
			    large-background-text
			    (<-> (number->two-dec-string (start-playtime)) "/" (number->two-dec-string (-> sound-object pan))))
			*large-background-text-color* font8 px1 y (- px2 px1) height))))
	  
	  (begin
	    (-> g setFont font8)
	    ;; Squares (mouse positions)
	    (for-each (lambda (square name val)
			(square (lambda (x1 y1 x2 y2)
				  (set! x1 (max x1 px1))
				  (-> g draw-string name *square-text-color* x1 y1)
				  (-> g draw-string val *square-text-color* x1 (+ y1 8))
				  (-> g draw-rect *square-rect-color* x1 y1 (- x2 x1 -1) (- y2 y1 -1)))))
		      (list fade-in-square fade-out-square startsound-square endsound-square resample1-square resample2-square volume-square      reverse-square)
		      '(    "fade inn"     "fade ut"       "start"           "slutt"         "hastighet"      "hastighet"      "volum"            "baklengs")
		      (let ((resample (number->two-dec-string (-> sound-object srate_change))))
			(list (number->two-dec-string (-> fade-in fade-time))
			      (number->two-dec-string (-> fade-out fade-time))
			      (number->two-dec-string start-read-pos)
			      (number->two-dec-string (- end-time (end-playtime)))
			      resample
			      resample
			      (number->two-dec-string (-> sound-object vol))
			      (if backwards "<-<-<-<-" "->->->->"))))
	    (-> g setFont font-default))
	  
	  ;;(-> g draw-rect black-color px1 y (- px2 px1) (- y2 y))
	  (-> g setColor black-color)
	  (-> g drawRoundRect px1 y (- px2 px1 1) (- y2 y 1) 8 8)

	  )))





  (def-method (paint-shade g)
    (if paint-waveforms
	(get-px1/px2 (lambda (px1 px2)
		       (let ((paintit (lambda (ch y* y2*)
					(-> g do-alpha alpha-0.25
					    (lambda ()
					      (if (> px1 x)
						  (-> audiofile-buffer paintWave g wave-color wave-non-color ch
                                                      x y* x2 y2*
                                                      x px1
                                                      5 #f #t (-> sound-object vol)))
					      (if (< px2 x2)
						  (-> audiofile-buffer paintWave g wave-color wave-non-color ch
                                                      x y* x2 y2*
                                                      px2 x2
                                                      5 #f #t (-> sound-object vol))))))))
			 (if (= 1 channels)
			     (paintit 0 y y2)
			     (let ((middle (c-integer (+ y (/ height 2)))))
			       (paintit 0 y middle)
			       (paintit 1 middle y2))))))))


  (define filename-text "")
  (define last-painted-srate -12341234)

  (c-display 'a0)

  (def-method (paint g x* y* x2* y2*)
    (get-sx1/sx2
     (lambda (sx1 sx2)
       (get-px1/px2 
	(lambda (px1 px2)


	  ;; Background
	  (if paint-background
	      (-> g do-alpha alpha-0.25
		  (lambda ()
		    (-> g fill-rect background-color px1 y (- px2 px1) height))))


	  '(when (eq? (-> mixer-area curr-sound-area) this)
		(-> g setFont font8)
		;; Squares (mouse positions)
		(for-each (lambda (square name)
			    (square (lambda (x1 y1 x2 y2)
				      (-> g draw-string name *square-text-color* x1 y1)
				      (-> g draw-rect *square-rect-color* x1 y1 (- x2 x1 -1) (- y2 y1 -1)))))
			  (list fade-in-square fade-out-square startsound-square endsound-square resample1-square resample2-square volume-square      reverse-square)
			  '(    "fade inn"     "fade ut"       "start"           "slutt"         "hastighet"      "hastighet"      "volum"            "baklengs"))
		(-> g setFont font-default))


	  ;; Waveform    
	  (if paint-waveforms
	      (let ((paintit (lambda (ch y* y2*)	
			       (if (and (eq? (-> mixer-area curr-sound-area) this)
					(> px1 sx1))
				   (-> g do-alpha alpha-0.50
				       (lambda ()
					 (-> audiofile-buffer paintWave g wave-color wave-shade-color ch
                                             sx1 y* sx2 y2*
                                             sx1 px1
                                             7 #f backwards (-> sound-object vol)))))
			       (-> audiofile-buffer paintWave g wave-color wave-shade-color ch
                                   sx1 y* sx2 y2*
                                   px1 px2
                                   3 #t backwards (-> sound-object vol))
			       (if (and (eq? (-> mixer-area curr-sound-area) this)
					(< px2 sx2))
				   (-> g do-alpha alpha-0.50
				       (lambda ()
					 (-> audiofile-buffer paintWave g wave-color wave-shade-color ch
                                             sx1 y* sx2 y2*
                                             px2 sx2 7
                                             #f backwards (-> sound-object vol)))))
			       )))
		(if (= 1 channels)
		    (paintit 0 y y2)
		    (let ((middle (c-integer (+ y (/ height 2)))))
		      (paintit 0 y middle)
		      (paintit 1 middle y2)))))


;	  (-> g do-alpha alpha-0.10
;	      (lambda ()
;		(for-each (lambda (square color)
;			    ;;(square (lambda (x1 y1 x2 y2)
;				;;      (-> g draw-rect color x1 y1 (- x2 x1) (- y2 y1))))
;			    (paint-square g square color)
;			    )
;			  (list fade-in-square fade-out-square startsound-square endsound-square resample1-square resample2-square volume-square      reverse-square)
;			  (list green-color green-color        blue-color blue-color             red-color red-color               background-color   (if backwards
;																			  black-color
;																			  white-color)))))

	  
	  
	  
	  ;; Fade in/fade out
	  (-> fade-in paint g x y x2 y2 width height)
	  (-> fade-out paint g x y x2 y2 width height)


	  (define (draw-handle color x1 y1 x2 y2)
	    (when #f
		  (-> g do-alpha alpha-0.15 ;;alpha-0.075
		      (lambda ()
			;;(-> g fill-rect color (1+ x1) (1+ y1) (- x2 x1 2) (- y2 y1 2)
			(-> g setColor color)
			(-> g fillRoundRect (1+ x1) (1+ y1) (- x2 x1 6) (- y2 y1 6) 8 8)))
		  
		  ;;(-> g draw-rect color x1 y1 (- x2 x1 1) (- y2 y1 1))
		  (-> g setColor color)
		  ;;(-> g drawRoundRect x1 y1 (- x2 x1 1) (- y2 y1 1) 4 4)
		  
		  ))

	  
	  ;; Start-play handle
	  (let ((x* (1+ (max x (- px1 threshold)))))
	    (draw-handle background-color
			 x* (+ y threshold-y)
			 px1 (+ (- (average y y2) threshold-y))))
	  
	  ;; End-play handle
	  (let ((x* (1+ px2)))
	    (draw-handle background-color
			 x* (+ y threshold-y)
			 (+ x* threshold) (- (average y y2) threshold-y)))
	  

	  ;; Start-resample handle
	  (let ((x* (1+ (max x (- px1 threshold)))))
	    (draw-handle background-color
			 x* (+ (average y y2) threshold-y)
			 px1 (+ (- y2 threshold-y))))
	  
	  ;; End-resample handle
	  (let ((x* (1+ px2)))
	    (draw-handle background-color
			 x* (+ (average y y2) threshold-y)
			 (+ x* threshold) (- y2 threshold-y)))
	  
	  (when (not (eq? (-> mixer-area curr-sound-area) this))
		(define m-x (-> mixer-area x))
		(define m-x2 (-> mixer-area x2))
		    
		(define x-clip (max m-x px1))
		(define x2-clip (min m-x2 px2))
		    
		(-> g setClip x-clip y (- x2-clip x-clip) (- y2 y))
		    
		    ;; volume
					;(-> g draw-string (number->string (-> sound-object vol))
					;    wave-text-color
					;    (+ px1 1) (+ y 20))

		    
		;; Reverse button
					;		(reverse-square (lambda (x1 y1 x2 y2)
					;				  (-> g draw-string "baklengs" (if backwards 
;								   white-color
					;								   red-color)
					;				      (+ x1 1) (+ y1 1))))
		   
		;; srate
		(if (not (= last-painted-srate (-> sound-object srate_change)))
		    (set! filename-text (number->two-dec-string (-> sound-object srate_change))))
		
		;; vol
		(-> g draw-string pathless-filename *wave-text-color* (+ px1 1) (+ y 1))
		(-> g draw-string (<-> (number->two-dec-string (-> sound-object vol)) "/" filename-text) *wave-text-color* (+ px1 1) (+ y 20)))
	  
	  )))))



  (def-method (set-start-playtime! playtime)
    (define dx (- playtime (start-playtime)))
    (set! start-time (- playtime start-read-pos))
    (inc! end-time dx))

  (def-method (set-start-time! start-time*)
    ;;(c-display "set-times!" (* 1.0 start-time*) (* 1.0 end-time*))
    (define length (- end-time start-time))
    (set! start-time start-time*)
    (set! end-time (+ start-time length)))
  
  (def-method (set-times! start-time* end-time*)
    ;;(c-display "set-times!" (* 1.0 start-time*) (* 1.0 end-time*))
    (set! start-time start-time*)
    (set! end-time end-time*))

  ;; Not too sure about adding threshold here...
  (def-method (set-width! width*)
    (set! width* (+ (c-integer width*) (* 2 threshold)))
    (when (not (= width width*))
	  (set! width width*)
	  (set! x2 (+ x (1- width)))))

  (def-method (set-height! height* y*)
    (set! height* (c-integer height*))
    (set! height height*)
    (-> fade-in set-fade-handle-size! height)
    (-> fade-out set-fade-handle-size! height)
    (set! y y*)
    (set! y2 (+ y (1- height))))



			  
			


  (def-method (make-state)
    (define state (make-hash-table 19))
    (for-each (lambda (varname var)
		(hashq-set! state varname var))
	      '(    backwards start-read-pos end-read-pos start-time end-time audiofile-buffer press-callback set-pos-callback release-callback)
	      (list backwards start-read-pos end-read-pos start-time end-time audiofile-buffer press-callback set-pos-callback release-callback))
    (hashq-set! state 'fade-in-time (-> fade-in fade-time))
    (hashq-set! state 'fade-out-time (-> fade-out fade-time))
    (hashq-set! state 'vol (-> sound-object vol))
    ;;(hashq-set! state 'pan (-> sound-object pan))
    (hashq-set! state 'srate (-> sound-object srate_change))

    (lambda (which)
      (hashq-ref state which)))


  ;; Reverse on/off
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (reverse-me! reverse-or-not-reverse)
    (set! backwards reverse-or-not-reverse)
    (let ((new-start-read-pos (- (get-abs-length) end-read-pos))
	  (new-end-read-pos (- (get-abs-length) start-read-pos)))

      (define start-px1 (get-px1))

      (set! start-read-pos new-start-read-pos)
      (set! end-read-pos new-end-read-pos)
      (-> sound-object requestTo_setStartReadPos new-start-read-pos)
      (-> sound-object requestTo_setEndReadPos new-end-read-pos)
      (-> sound-object reverse reverse-or-not-reverse)

      (define end-px1 (get-px1))

      (when (not (= start-px1 end-px1))
	    (move! (- start-px1 end-px1) 0)
	    (set-start-playtime! (-> mixer-area get-time (get-px1)))
	    (-> sound-object requestTo_setPos (-> mixer-area get-time (get-px1))))
      ))
  
  
  (apply add-mouse-cycle
	 (square-mouse-cycle reverse-square
			     (lambda (px1 px2)
			       (add-undo-run (let ((val (not backwards)))
					       (lambda ()
						 (lift-me!)
						 (reverse-me! val)
						 (-> mixer-area set-curr-sound-area! this)
						 ;;(-> sound-object reverse backwards)
						 (repaint-me!)
						 'grab))
					     (let ((val backwards))
					       (lambda ()
						 ;;(set! backwards val)						 
						 ;;(-> sound-object reverse backwards)
						 (reverse-me! val)
						 (repaint-me!)))))
			     (lambda x #t)
			     (lambda x #t)))



  ;; This function does the same as (-> mixer-area get-time). Better use that one instead, less mess.
  ;;(define (x->time x*)
  ;;  (get-px1/px2
  ;;   (lambda (px1 px2)
  ;;     (c-scale x* px1 px2 start-time end-time))))


  (def-method (adjust-position)
    (get-px1/px2 (lambda (px1 px2)
		   ;;(c-display "ai" (* 1.0 start-read-pos) (* 1.0 (-> mixer-area get-time px1)))
		   (-> sound-object requestTo_setPos (-> mixer-area get-time px1)))))
    

  ;; Start and end position
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;; Left-side pos mouse handler
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def-method (mouse-change-rp1 start-read-pos* :key (set-background-text #t))

    (set! start-read-pos start-read-pos*)

    (if (< start-read-pos 0)
	(set! start-read-pos 0))
    (if (>= start-read-pos (- end-read-pos *min-sound-length*))
	(set! start-read-pos (- end-read-pos *min-sound-length*)))

    (if set-background-text
	(set! large-background-text (number->two-dec-string start-read-pos)))

    (-> sound-object requestTo_setStartReadPos start-read-pos);;(* start-read-pos (-> sound-object srate_change)))
;    (-> sound-object requestTo_setStartReadPos (c-scale start-read-pos
;							0 (* (-> sound-object getAbsLength) (-> sound-object srate_change))
;							0 (-> sound-object getAbsLength)))
    
    ;;(c-display "mc-1" dx start-read-pos start-time end-time)
    ;; I don't know why I have to add the block below. Its a hack.
    (adjust-position)

    (-> miniature-area repaint-me!)
    (-> parent check-paint)

    (-> parent repaint x y x2 y2))



  (apply add-mouse-cycle
	 (let* ((initial-start-read-pos 0)
		(start-pos 0))
	   (square-mouse-cycle startsound-square
			       (lambda (px1 px2)
				 (set! large-background-text (number->two-dec-string start-read-pos))
				 (set! initial-start-read-pos start-read-pos)
				 (set! start-pos px1))
			       (lambda (button dx dy)
				 ;;(c-display "start-time" start-time)
				 (mouse-change-rp1 (- (-> mixer-area get-time (+ start-pos dx))
						      start-time)))
			       (lambda (button dx dy)
				 ;;(set! large-background-text #f)
				 (add-undo-run (let ((start (- (-> mixer-area get-time (+ start-pos dx))
							       start-time)))
						 (lambda ()
						   (mouse-change-rp1 start ':set-background-text #f)))
					       (let ((start initial-start-read-pos))
						 (lambda ()
						   (mouse-change-rp1 start ':set-background-text #f))))))))
  


  ;; Right-side pos mouse handler
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def-method (mouse-change-rp2 end-read-pos* :key (set-background-text #t))

    (let ((length (get-abs-length)))      

      (set! end-read-pos end-read-pos*)

      (if (<= end-read-pos (+ *min-sound-length* start-read-pos))
	  (set! end-read-pos (+ *min-sound-length* start-read-pos)))
      
      (if (>= end-read-pos length)
	  (set! end-read-pos length))

      (if set-background-text
	  (set! large-background-text (number->two-dec-string (- end-time (end-playtime)))))
      
      ;;(c-display "mc-2" dx end-read-pos start-time end-time)
      (-> sound-object requestTo_setEndReadPos end-read-pos);;(c-scale end-read-pos
							;;0 (-> sound-object getAbsLength)
							;;0 (* (-> sound-object getAbsLength) (-> sound-object srate_change))))

      (-> miniature-area repaint-me!)
      (-> parent check-paint)
    
      (repaint-me!)))


  (apply add-mouse-cycle
	 (let* ((initial-end-read-pos 0)
		(end-pos 0))
	   (square-mouse-cycle endsound-square
			       (lambda (px1 px2)
				 (number->two-dec-string (- end-time (end-playtime)))
				 (set! initial-end-read-pos end-read-pos)
				 (set! end-pos px2))
			       (lambda (button dx dy)
				 ;;(c-display "time" (* 1.0 (-> mixer-area get-time (+ end-pos dx))) ", newpos: " (+ 0.0 end-pos dx) "start-time" (* 1.0 start-time))
				 (mouse-change-rp2 (- (-> mixer-area get-time (+ end-pos dx))
						      start-time)))
			       ;;(mouse-change-rp2 (- (x->time (+ end-pos dx)) start-start-time)))
			       (lambda (button dx dy)
				 ;;(set! large-background-text #f)
				 (add-undo-run (let ((start (- (-> mixer-area get-time (+ end-pos dx)) 
							       start-time)))
						 (lambda ()
						   (mouse-change-rp2 start ':set-background-text #f)))
					       (let ((start initial-end-read-pos))
						 (lambda ()
						   (mouse-change-rp2 start ':set-background-text #f))))))))




  ;; Fade-in / fade-out
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (fade-mouse-cycle fade square)
	   (let* ((initial-fade 0)
		  (get-new-time (lambda (dx)
				  (if (eq? fade fade-out)
				      (set! dx (* dx -1)))
				  (-> timebar get-shown-times
				      (lambda (shown-start-time shown-end-time)
					(+ (-> mixer-area get-time (+ (-> mixer-area x) dx))
					   (- shown-start-time)
					   initial-fade))))))
	     (square-mouse-cycle square
				 (lambda (px1 px2)
				   (number->two-dec-string (-> fade fade-time))
				   (set! initial-fade (-> fade fade-time)))
				 (lambda (button dx dy)
				   (define new-time (get-new-time dx))
				   (-> fade set-time! new-time)
				   (set! large-background-text (number->two-dec-string (-> fade fade-time))) ;;new-time))
				   ;;(c-display "new-time" (* 1.0 (-> fade fade-time)))
				   (repaint-me!))
				 (lambda (button dx dy)
				   ;;(set! large-background-text #f)
				   (add-undo-run (let ((time (get-new-time dx)))
						   (lambda ()
						     (-> fade set-time! time)
						     (repaint-me!)))
						 (let ((time initial-fade))
						   (lambda ()
						     (-> fade set-time! time)
						     (repaint-me!))))))))

  (apply add-mouse-cycle (fade-mouse-cycle fade-in fade-in-square))
  (apply add-mouse-cycle (fade-mouse-cycle fade-out fade-out-square))



  ;; Sample rate change
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define miniature-rectangle (<rectangle>))

  (def-method (set-srate_change! srate :key (change-end #t) (check-end #f))
    (repaint-me!)

    (if (< srate *min-srate*)
	(set! srate *min-srate*))
    (if (> srate *max-srate*)
	(set! srate *max-srate*))

    (if (not check-end)
	(set! large-background-text (number->two-dec-string srate)))

    (let* ((srate*100 (c-integer (* 100 srate)))
	   (whole (c-integer (/ srate*100 100)))
	   (dec (- srate*100 (* whole 100))))
      (when (and (=  whole 1)
		 (= dec 0))
	    ;;(c-display "hepp!")
	    (set! srate 1.0)))

    ;; miniature
    (let ()
      (-> miniature-rectangle reset!)
      (-> miniature-area get-miniature-pos this
	  (lambda (x* y* width* height*)
	    (-> miniature-rectangle add-max x* y* (+ x* width*) (+ y* height*)))))

    (define old-px1 0)
    (define old-px2 0)
    (get-px1/px2 (lambda (px1 px2)
		   (set! old-px1 px1)
		   (set! old-px2 px2)))


    (define old-length (-> sound-object getAbsLength))
    (-> sound-object requestTo_setSRate srate)
    (define new-length (-> sound-object getAbsLength))

    (define old-start-time start-time)

    (set! start-read-pos (c-scale start-read-pos 0 old-length 0 new-length))
    (set! end-read-pos (c-scale end-read-pos 0 old-length 0 new-length))
    (-> fade-in set-time! (c-scale (-> fade-in fade-time) 0 old-length 0 new-length))
    (-> fade-out set-time! (c-scale (-> fade-out fade-time) 0 old-length 0 new-length))

    (-> timebar get-shown-times
	(lambda (shown-start-time shown-end-time)
	  (set-width! (i-scale (-> sound-object getAbsLength)
			       0 (- shown-end-time shown-start-time)
			       0 (-> mixer-area width)))))

    (let ((new-start-time (-> mixer-area get-time (+ x threshold))))
      (set-times! new-start-time (+ new-start-time (-> sound-object getAbsLength))))

    
    (get-px1/px2 (lambda (px1 px2)
		   (if change-end
		       (move! (- old-px1 px1) 0)
		       (move! (- old-px2 px2) 0))))
    
    (define new-start-time (-> mixer-area get-time (+ x threshold))) 
    (define new-end-time (+ new-start-time (-> sound-object getAbsLength)))
    (set-times! new-start-time new-end-time)

    (get-px1/px2 (lambda (px1 px2)
		   (-> sound-object requestTo_setPos (-> mixer-area get-time px1))))


    ;;(set! end-time (+ start-time new-length))

    (set! background-color (-> audiofile-buffer get-scaled-color srate))

    ;; miniature
    (when #t
	  (-> parent add-post-paint
	      (lambda (g)
		(-> miniature-area get-miniature-pos this
		    (lambda (x* y* width* height*)
		      (-> miniature-rectangle add-max x* y* (+ x* width*) (+ y* height*))))
		(-> miniature-rectangle get-max (lambda (x* y* x2* y2*)
						  (-> parent repaint x* y* x2* y2*)))
		(-> parent check-paint))))

    (if (and check-end 
	     (>= (end-playtime) (-> mixer-area end-time)))
	(-> mixer-area set-end-time! (+ new-end-time 0.1)))

    (repaint-me!))
		


  ;; Right-side sample rate mouse handler
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (apply add-mouse-cycle
	 (let* ((start-srate 0)
		(start-width 0)
		(start-length 0)
		(get-new-srate (lambda (dx)
				 (define new-size (+ start-width dx))				 
				 (if (< new-size 2)
				     (set! new-size 2))
				 (define new-length (-> mixer-area get-time (+ (-> mixer-area x) new-size)))
				 ;;(/ start-length new-length))))
				 (* start-srate (/ 1 (c-scale new-size 0 start-width 0 1))))))

	   (square-mouse-cycle resample2-square
			       (lambda (px1 px2)
				 (number->two-dec-string (-> sound-object srate_change))
				 (set! start-length (- (end-playtime) (start-playtime)))
				 (set! start-width (- px2 px1))
				 (set! start-srate (-> sound-object srate_change)))
			       (lambda (button dx dy)
				 (set-srate_change! (get-new-srate dx)))
			       (lambda (button dx dy)
				 ;;(set! large-background-text #f)
				 (add-undo-run (let ((start (get-new-srate dx)))
						 (lambda ()
						   (set-srate_change! start ':check-end #t)))
					       (let ((start start-srate))
						 (lambda ()
						   (set-srate_change! start ':check-end #t))))))))



  ;; Left-side sample rate mouse handler
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (apply add-mouse-cycle
	 (let* ((start-srate 0)
		(start-width 0)
		(start-length 0)
		(get-new-srate (lambda (dx)
				 (define new-size (- start-width dx))
				 (if (< new-size 2)
				     (set! new-size 2))
				 (define new-length (-> mixer-area get-time (+ (-> mixer-area x) new-size)))
				 ;;(/ start-length new-length))))
				 (* start-srate (/ 1 (c-scale new-size 0 start-width 0 1))))))
	   (square-mouse-cycle resample1-square
			       (lambda (px1 px2)
				 (number->two-dec-string (-> sound-object srate_change))
				 (set! start-length (- (end-playtime) (start-playtime)))
				 (set! start-width (- px2 px1))
				 (set! start-srate (-> sound-object srate_change)))
			       (lambda (button dx dy)
				 (set-srate_change! (get-new-srate dx) ':change-end #f))
			       (lambda (button dx dy)
				 ;;(set! large-background-text #f)
				 (add-undo-run (let ((start (get-new-srate dx)))
						 (lambda ()
						   (set-srate_change! start ':change-end #f)))
					       (let ((start start-srate))
						 (lambda ()
						   (set-srate_change! start ':change-end #f))))))))


  (c-display 'a2)

  ;; Setting the volume
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((start-volume 0)
	(start-height 0))
    (apply add-mouse-cycle
	   (square-mouse-cycle volume-square
			       (lambda (px1 px2)
				 (number->two-dec-string (-> sound-object vol))
				 (set! start-height height)
				 (set! start-volume (-> sound-object vol)))
			       (lambda (button dx dy)
				 (define new-vol (max 0.001 (- start-volume (/ dy 32))))
				 (-> sound-object vol new-vol)
				 (set! large-background-text (number->two-dec-string new-vol))
				 (when #f ;; I'm not shure this is a good idea. :-
				       (repaint-me!)
				       (define new-height (i-scale (-> sound-object vol) 0 1 0 start-height))
				       (-> mixer-area get-position
					   (lambda (x y x2 y2 width height)
					     (set-height! new-height
							  (i-scale (-> sound-object getPan) -1 1 y (- y2 new-height))))))
				 (repaint-me!))
			       (lambda (button dx dy)
				 ;;(set! large-background-text #f)
				 (add-undo-run (let ((newvol (max 0.001 (- start-volume (/ dy 32)))))
						 (lambda ()
						   (-> sound-object vol newvol)
						   (repaint-me!)))
					       (let ((newvol start-volume))
						 (lambda ()
						   (-> sound-object vol newvol)
						   (repaint-me!))))))))
	      



  ;; Moving the sound
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (def-var move-mouse-cycle
    (let ()

      (define start-move-time 0)
      (define start-move-pan 0)
      
      (define start-x 0)
      (define start-y 0)

      (define is-removed #f)

      (square-mouse-cycle move-square
			  (lambda (px1 px2)
			    (set! start-move-time start-time)
			    (set! start-move-pan (-> sound-object getPan))
			    (set! start-x px1) ;;(+ x threshold))
			    (set! start-y y)
			    (set! large-background-text #f) ;;(<-> (number->two-dec-string (start-playtime)) "/" (number->two-dec-string (-> sound-object pan))))
			    (press-callback this))
			  (lambda (button dx dy)
			    ;;(c-display "sound-pos drag called" button dx dy)
			    ;;(c-display 'ctrl (-> parent control-pressed?))
			    (define new-time (-> mixer-area get-time (+ start-x dx)))
			    (define new-pan (-> mixer-area get-pan (+ start-y dy)))
			    (set-pos-callback this new-time new-pan)
			    ;;(set! large-background-text (<-> (number->two-dec-string (start-playtime)) "/"  (number->two-dec-string (-> sound-object pan))))
			    )
			  (lambda (button dx dy)
			    ;;(c-display "sound-pos release called" x );;button x* y*)
			    ;;(set! large-background-text #f)
			    (define time (-> mixer-area get-time (+ start-x dx)))
			    (if (< (+ time (get-playlength)) 0)
				(add-undo-run (lambda ()
						(set! is-removed #t)
						(-> mixer-area delete-sound this))
					      (let ((time start-move-time)
						    (pan start-move-pan))
						(lambda ()
						  (-> mixer-area add-sound-area this time pan))))
				(add-undo-run (let ((time time)
						    (pan (-> mixer-area get-pan (+ start-y dy))))
						(lambda ()
						  (set-pos-callback this time pan #t)))
					      (let ((time start-move-time)
						    (pan start-move-pan))
						(lambda ()
						  (set-pos-callback this time pan #t))))))
			  ':release-thunk (lambda () (release-callback this is-removed))
			  ':check-button *right-mouse-button*)))
  
  (apply add-mouse-cycle move-mouse-cycle)




  (c-display 15)



  ;;(c-display "s9")
  )




