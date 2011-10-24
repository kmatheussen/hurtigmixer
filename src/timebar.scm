
#|

0 500



|#



(define (seconds->timestring time)
  (set! time (c-integer time))
  (let* ((seconds (modulo time 60))
	 (minutes (c-integer (/ time 60))))
    (if (< seconds 10)
	(string-append (number->string minutes) ":0" (number->string seconds))
	(string-append (number->string minutes) ":" (number->string seconds)))))


#|
;; (second = minute/60)
(c-define (find-first-second-pixel width start-time end-time)
  (c-scale (ceiling start-time) start-time end-time 0 width))

(find-first-second-pixel 100 4.6 9.5)
(number->string 5.23545)
(format #t "~A\n" 50.234234)


(seconds->timestring #f)
(= (floor 50.0) 50)
(begin \")




|#



(def-area-subclass (<timebar> parent width height start-time end-time)

  (def-var background-color gray-color)
  (def-var bar-color redbrown-color)
  (def-var text-color redbrown-color)

  (def-var font #f)

  (def-var min-pixels-between-text (/ width 4))
  
  (define (paint g x* y* x2* y2*)
    ;(-> g setColor background-color)
    ;(-> g fillRect x* y* (- x2* x*) (1+ (- y2* y*)));;x y width height)
    (-> parent paint-background g x* y* x2* y2*)
    (if (not font)
	(set! font (-> g getFont)))
    
    (define inc-time (max 1 (c-integer (ceiling (c-scale min-pixels-between-text 0 width 0 (- end-time start-time))))))

    (if (not (= 0 (modulo inc-time 2)))
	(inc! inc-time 1))
    (if (not (= 0 (modulo inc-time 5)))
	(inc! inc-time (- 5 (modulo inc-time 5))))
    (if (> (- end-time start-time) 110)
	(if (not (= 0 (modulo inc-time 30)))
	    (inc! inc-time (- 30 (modulo inc-time 30)))))
    (if (> (- end-time start-time) 110)
       (if (not (= 0 (modulo inc-time 30)))
           (inc! inc-time (- 30 (modulo inc-time 30)))))

    (define t1 4)
    (define t2 8)

    (let loop ((time (* inc-time (c-integer (/ start-time inc-time))))) ;;(1+ (floor start-time))))
      (let ((x* (c-scale time start-time end-time x x2)))
	(when (< x* x2)
	      ;;(-> g setColor bar-color)
	      ;;(-> g drawLine x* (- y2 (/ height 1.5)) x* y2)
	      ;;(-> g fill-triangle bar-color (- x* height/2) y (+ x* height/2) y x* (+ y (* 2 height/2)))
	      (-> g fill-triangle bar-color (- x* t1) y (+ x* t1) y x* (+ y t2))

	      (-> g draw-string (seconds->timestring time) text-color (- x* 12) (+ 12 y))
	      (loop (+ time inc-time))))))


  (def-method (set-times! start-time* end-time*)
    (set! start-time start-time*)
    (set! end-time end-time*)
    (-> parent repaint x y (1- x2) y2))

  (def-method (get-shown-times cont)
    (cont start-time end-time))


  (let ((setplayposfunc (lambda (button x* y*)
			  (get-shown-times (lambda (start end)
					     (-> mixer requestTo_setPlayPos (c-scale x* x x2 start end))
					     (-> mixer-area paint-cursor))))))
						 
    (add-mouse-cycle setplayposfunc setplayposfunc setplayposfunc))

  )
