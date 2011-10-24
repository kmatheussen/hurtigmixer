
(def-area-subclass (<xy-slider> parent width height callback :key (x-joy 0.5) (y-joy 0.5) (joy-width 32))
				
  (def-var joystick-color (apply get-RGB-color (map (lambda (x) (* x 0.50)) (list 227 226 150)))) ;; alpha val of 0.25
  
  (def-var x-joy)
  (def-var y-joy)
  (def-var joy-width)
  (def-var border 2)

  (set! joy-width (min (- width 2) (- height 2) joy-width))


  (define (mx) (+ x border (/ joy-width 2)))
  (define (my) (+ y border (/ joy-width 2)))
  (define (mx2) (- x2 (+ 1 border (/ joy-width 2))))
  (define (my2) (- y2 (+ 1 border (/ joy-width 2))))

  (define (get-button-pos kont)
    (define joy-x (i-scale x-joy 0 1 (mx) (mx2)))
    (define joy-y (i-scale y-joy 1 0 (my) (my2)))
    (define w/2 (/ joy-width 2))
    (kont (- joy-x w/2) (- joy-y w/2) (+ joy-x w/2) (+ joy-y w/2)))

  (define (get-tempo-pos kont)
    (get-button-pos (lambda (jx jy jx2 jy2)
                      (kont jx y jx2 y2))))

  (define (get-pitch-pos kont)
    (get-button-pos (lambda (jx jy jx2 jy2)
                      (kont x jy x2 jy2))))

  (define (make-inside?-func kont-func)
    (lambda (x* y*)
      (kont-func (lambda (x y x2 y2)
                   (and (>= x* x)
                        (>= y* y)
                        (< x* x2)
                        (< y* y2))))))

  (define inside-button? (make-inside?-func get-button-pos))
  (define inside-tempo? (make-inside?-func get-tempo-pos))
  (define inside-pitch? (make-inside?-func get-pitch-pos))
  
  (def-method (paint g x* y* x2* y2*)
    (get-button-pos (lambda (jx jy jx2 jy2)
                      ;;(-> g draw-rect black-color jx jy (- jx2 jx) (- jy2 jy))
                      
                      ;;(-> g fill-rect joystick-color (- joy-x w/2) (- joy-y w/2) joy-width joy-width)
                      ;;(-> g draw-rect black-color (- joy-x w/2) (- joy-y w/2) joy-width joy-width)
                      
                      (-> g do-antialized
                          (lambda ()
                            (-> g do-alpha alpha-0.15
                                (lambda ()
                                  (-> g fill-arc joystick-color jx jy joy-width joy-width 0 360)
                                  ;;(-> g draw-line joystick-color (- joy-x w/2) (- joy-y w/2) (+ joy-x w/2) (+ joy-y w/2))
                                  ;;(-> g draw-line joystick-color (+ joy-x w/2) (- joy-y w/2) (- joy-x w/2) (+ joy-y w/2))
                                  ))
                            (-> g do-alpha alpha-0.05
                                (lambda ()
                                  (define border-color black-color)
                                  (-> g fill-rect joystick-color jx y joy-width height)
                                  (-> g fill-rect joystick-color x jy width joy-width)
                                  (-> g draw-line border-color jx y jx y2)
                                  (-> g draw-line border-color jx2 y jx2 y2)
                                  (-> g draw-line border-color x jy x2 jy)
                                  (-> g draw-line border-color x jy2 x2 jy2)
                                  ;;(-> g draw-rect gray-color x jy width joy-width)
                                  )))))))
 

  (define (moved-joystick x* y*)
    (c-display "moved" x* y* (mx) (mx2) (c-scale x* (mx) (mx2) 0.0 1.0))
    (set! x-joy (boundaries 0.0 (c-scale x* (mx) (mx2) 0.0 1.0) 1.0))
    (set! y-joy (boundaries 0.0 (c-scale y* (my) (my2) 1.0 0.0) 1.0))
    (callback x-joy y-joy)
    (repaint-me!)
    )

  (define (moved-tempo x* y*)
    (set! x-joy (boundaries 0.0 (c-scale x* (mx) (mx2) 0.0 1.0) 1.0))
    (callback x-joy y-joy)
    (repaint-me!)
    )
  
  (define (moved-pitch x* y*)
    (set! y-joy (boundaries 0.0 (c-scale y* (my) (my2) 1.0 0.0) 1.0))
    (callback x-joy y-joy)
    (repaint-me!)
    )
  

  ;; Reset
  (add-mouse-cycle (lambda (button x* y*)
                     (when (= button *right-mouse-button*)
                       (set! x-joy 0.5)
                       (set! y-joy 0.5)
                       (callback x-joy y-joy)
                       (repaint-me!))
                     #f)
                   #f
                   #f)

  (define (make-mover inside? moved-func)
    (let* ((start-x 0)
           (start-y 0)
           (moved-delta (lambda (button dx dy)
                          (moved-func (+ dx start-x) (+ dy start-y)))))
      (apply add-mouse-cycle
             (delta-mouse-cycle parent
                                (lambda (button x* y*)
                                  (when (and (= button *left-mouse-button*)
                                             (inside? x* y*))
                                    (get-button-pos (lambda (jx jy jx2 jy2)
                                                      (set! start-x (average jx jx2))
                                                      (set! start-y (average jy jy2))))
                                    'grab))
                                moved-delta
                                moved-delta))))


  (make-mover inside-button? moved-joystick)
  (make-mover inside-tempo? moved-tempo)
  (make-mover inside-pitch? moved-pitch)
  )


				
	   
