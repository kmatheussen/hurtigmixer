
(define help-area #f)


(define all-positions-hurtigmixer
  '( (add-sound 102 428 40 15)
     
     (save-local 879 206 68 24)

     (message-area 858 335 115 11)
     (progress-bar 858 347 115 21)

     (save-server 880 294 65 25)

     (play     176 522 39 39)
     (stop     219 522 39 39)
     (undo     262 522 18 18)
     (redo     283 522 18 18)
     (rewind   262 543 39 18)
     (loop-on  176 564 18 16)
     (loop-off 197 564 18 16)
     (reset    219 564 39 16)
     (trim     262 564 39 16)

     (srate-slider  307 533 110  16)
     (size-slider   307 564 110  16)
     (volume-slider 422 522  18  58)

     ;;(vu-area 840 403 35 190)
     ;;(vu-area 794 126 39 321)
     (vu-area 446 522  18  58)

     (musikkverksted 12  70 141 168)
     (min-lydhylle   12 245 141 168)
     (min-datamaskin 12 420 141 168)

     (top-sounds 171 0 655 49)

     (miniature-area 482 510 349 84)
     (timebar        187 68 621 20)
     ;;(timebar        165 68 667 20)

     (mixer-area     187 91 621 392)
     ;;(mixer-area     165 89 667 416)

     ))

(define all-positions-melodigenerator
  '( (tempo-area           64  46 581  50)
     (melodigenerator-area 14 113 641 422)

     (message-area 858 335 115 11)
     (progress-bar 858 347 115 21)

     (pitchspeed-area     501 555 160 79)
     (scaleselect-area    138 555 350 79)
     (reverb-slider       114 555 17 79)
     (volume-slider        94 555 17 79)
     (notelen-slider       14 614 75 17)

     (shake-button     14 521 92 10)
     (play-button      14 533 92 10)
     (stop-button      14 533 92 10)
     (gen-button       14 545 92 10)
     (invert-button    14 557 92 10)
     (reverse-button   14 569 92 10)
     (glissando-button 14 581 92 10)

     (moll-button          1 601 82 10)
     (dur-button           15 590 82 10)
     (heltone-button       30 590 82 10)
     (dim-button           45 590 82 10)
     (arabisk-button        60 590 82 10)
     (blues-button         65 590 82 10)
     (kromatisk-button      80 590 82 10)
     (fri-button            95 590 82 10)

     (reset-button          1   1  40 10)
     (save-button           1  15  40 10)
     (random-button         1  30  40 10)
     (unrandom-button       1  45  40 10)

     (help-area             1 660 800 20)
     
     (sound-selector-area 958 563 72 96)
     ))

  


(c-define (generate-positions dx dy)
  (define events (-> (new-static <CreateFrame>) events))
  (define tobeevaled '())

  ;; Might be overrided by a background-image-pos entry in a config-file.
  (define x-offset 0)
  (define y-offset 0)

  (when (get-config-area-list-with-help 'background-image-pos)
    (set! x-offset (car (get-config-area-list-with-help 'background-image-pos)))
    (set! y-offset (cadr (get-config-area-list-with-help 'background-image-pos))))

  (-> events nowhereMousePlacementCallback
      (java-wrap
       (lambda ()
         (when help-area
           ;;(c-display "NOWERE")
           (-> help-area set-and-force-paint! (nth 4 (get-config-area-list-with-help 'help-area)))))))

  (for-each (lambda (position)
	      (c-display "position: " position)
	      (if (>= (length position) 5)
		  (apply (lambda (name x y width height . help-text?)
			   (define help-text (if (pair? help-text?)
                                                 (car help-text?)
                                                 ""))
                           (c-display "help-text: -" name "-" help-text "-" help-text?)
                           (let ((x (+ dx x (- x-offset)))
                                 (y (+ dy y (- y-offset))))
                             (push! `(define ,(symbol-append name '-pos) (list ,x ,y ,width ,height)) tobeevaled)
                             (for-each (lambda (var-name val)
                                         (push! `(define ,(symbol-append name '- var-name) ,val) tobeevaled))
                                       '( x  y  width  height  x2           y2)
                                       `(,x ,y ,width ,height ,(+ x width) ,(+ y height)))
                             (if (not (string=? "" help-text))
                                 (-> events addMousePlacementEvent x y width height
                                     (java-wrap
                                      (lambda ()
                                        ;;(c-display "hepp" help-text)
                                        (when help-area
                                          (-> help-area set-and-force-paint! help-text))))))))
                         position)))
            (map (lambda (position)
		   (define conf (get-config-area-list-with-help (car position)))
		   (if conf
		       (cons (car position) conf)
		       (append position (list ""))))
		 (if (and (defined? '*running-hurtigmixer*)
			  *running-hurtigmixer*)
		     all-positions-hurtigmixer
		     all-positions-melodigenerator)))

  (eval `(begin ,@tobeevaled)))


(define pos-x car)
(define pos-y cadr)
(define pos-width third)
(define pos-height fourth)
(define (pos-x2 pos) (+ (pos-x pos) (pos-width pos)))
(define (pos-y2 pos) (+ (pos-y pos) (pos-height pos)))

(define (get-position-list name func)
  (func (eval (symbol-append name '-x))
	(eval (symbol-append name '-y))
	(eval (symbol-append name '-width))
	(eval (symbol-append name '-height))))

#|

(generate-positions 100 100)
(begin add-sound-pos)
(list add-sound-x add-sound-y add-sound-x2 add-sound-y2 add-sound-width add-sound-height)
(generate-positions 0 0)
(get-config-area-list-with-help 'tempo-area)
|#



