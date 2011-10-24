

(define root-note 0) ;; i.e. C

(define moll-weight      '(8 0 2 6 0 2 0 3 4 0 5 0 ))
(define dur-weight       '(6 0 2 0 3 4 0 5 0 3 0 2))
(define heltone-weight   '(3 0 1 0 2 0 1 0 2 0 1 0))
(define dim-weight       '(1
                           0
                           0.12244897959183676
                           0.8979591836734694
                           0
                           0.1428571428571429
                           0.8979591836734694
                           0
                           0.12244897959183676
                           0.8163265306122449
                           0
                           0.8979591836734694))
;;5 0 1 3 0 1 4 0 1 3 0 1))
(define arabisk-weight   '(4 3 0 0 2 2 0 3 2 0 0 2))
(define blues-weight     '(7 0 0 3 0 3 2 6 0 0 3 2))
(define kromatisk-weight '(3 2 2 2 2 2 2 2 2 2 2 2))
(define fri-weight       '(0 0 0 0 0 0 0 0 0 0 0 0))



(def-class (<active-bar-colors>)
  (def-var border             (get-RGB-color 167 165 155))
  (def-var border-upleft      (get-RGB-color 181 180 171))
  (def-var border-upleft2     (get-RGB-color 181 179 171))
  (def-var inner-borderupleft (get-RGB-color 232 231 223))
  (def-var inner-borderleft   (get-RGB-color 229 227 218))
  (def-var inner-border       (get-RGB-color 218 216 203))
  (def-var fill               (get-RGB-color 219 217 204))
  )

(def-class (<inactive-bar-colors>)
  (def-var border             (get-RGB-color 192 192 190))
  (def-var border-upleft      (get-RGB-color 202 202 201))
  (def-var border-upleft2     (get-RGB-color 202 202 200))
  (def-var inner-borderupleft (get-RGB-color 253 253 251))
  (def-var inner-borderleft   (get-RGB-color 252 252 251))
  (def-var inner-border       (get-RGB-color 251 251 249))
  (def-var fill               (get-RGB-color 252 252 250))
  )


(def-area-subclass (<scale-selector> parent width height
				     :key
				     callback 
				     (on-offs (vector #t #t #f #f #t #t #f #t #t #f #f #t))
				     (weights (vector 0.41 0.3 0.0 0.0 0.2 0.2 0 0.3 0.2 0.0 0.0 0.2))
				     (note-names (vector "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "B" "H"))
				     (button-height 8)
				     (name-height 8)
				     )


  (def-var font (get-font "GothamRnd-Medium.ttf" 11))

  (def-var org-weights weights)
  (def-var org-on-offs on-offs)

  (def-var weights)
  (def-var on-offs)
  (def-var note-names)

  (def-var root-note-color (-> Various getColor 0.6 0.2 0.5))

  (def-var on-color red-color)
  (def-var bar-color blue-color)

  (def-var num-notes-in-scale (vector-length weights))

  (def-var button-height)
  (def-var name-height)

  (def-var bar/button-border 3)
  ;;(def-method (is-on n)
  ;;  (and (vector-ref on-offs n) (> (vector-ref weights n) 0.01)))

  ;; account for root-note
  (define (rotate-scale)
    (define org-weights (list->vector (list-copy (vector->list weights))))
    (define org-on-offs (list->vector (list-copy (vector->list on-offs))))
    (c-for 0 < 12 1
           (lambda (i)
             (vector-set! weights i (vector-ref org-weights (modulo (- i root-note) 12)))
             (vector-set! on-offs i (vector-ref org-on-offs (modulo (- i root-note) 12))))))

  (def-method (set-new-weights! new-weights)
    (set! weights (list->vector (list-copy (if (pair? new-weights) new-weights (vector->list new-weights)))))
    (define highest (apply max (vector->list weights)))
    (c-for-each (lambda (i weigh)
		  (if (> highest 0.0)
		      (vector-set! weights i (/ weigh highest)))
		  (vector-set! on-offs i (> weigh 0.0)))
		(vector->list weights))
    (set! org-weights (list->vector (list-copy (vector->list weights))))
    (set! org-on-offs (list->vector (list-copy (vector->list on-offs))))
    (rotate-scale)
    (c-display weights on-offs)
    )

  (define (get-x-poss num cont)
    (define x* (i-scale num 0 12 x x2))
    (define bar-width (/ width num-notes-in-scale))
    (cont x* (+ x* (- bar-width 3))))
  
  (define (get-bar-vars num cont)
    (get-x-poss num (lambda (x x2)
                      (cont x x2 y (+ y 49)))))
                      
  (define (get-button-vars num cont)
    (get-bar-vars num (lambda (x x2 by by2)
                        (define y (+ by2 3))
                        (define y2 (+ by2 3 8))
                        (cont x y x2 y2 (- x2 x) (- y2 y)))))


  (define active-colors (<active-bar-colors>))
  (define inactive-colors (<inactive-bar-colors>))

  (define (paint-bar g colors x y y-val width height) ;; y-val is the value of the bar in pixels. I.e. the height of the variable size box.
    (define x2 (+ x width))
    (define y2 (+ y height))
    (-> g draw-rect (-> colors border) x y width height)
    (-> g draw-line (-> colors border-upleft) x y (1+ x) y)
    (-> g draw-line (-> colors border-upleft2) x (1+ y) x (1+ y))

    (-> g fill-rect-abs (-> colors fill) (+ 2 x) (+ 2 y-val) (- x2 3) (- y2 3))

    (-> g draw-rect-abs (-> colors inner-border) (1+ x) (1+ y-val) (1- x2) (1- y2))

    (when (< y-val (- y2 2))
      (-> g draw-line (-> colors inner-borderupleft) (1+ x) (1+ y-val) (1+ x) (1+ y-val))
      (-> g draw-line (-> colors inner-borderleft) (1+ x) (+ 2 y-val) (1+ x) (- y2 2)))
    )

  (define (paint g x* y* x2* y2*)
    ;;(-> g draw-rect black-color x y (1- width) (1- height))
    (-> g do-font font
        (lambda ()
          (c-for 0 < 12 1
                 (lambda (num)
                   (define on-off (vector-ref on-offs num))
                   (define colors (if on-off active-colors inactive-colors))
                   (get-bar-vars num (lambda (x x2 y y2)
                                       (define val (vector-ref weights num))
                                       (define bar-y (i-scale val 0 1 y2 y))
                                       (paint-bar g colors x y bar-y (- x2 x) (- y2 y))))
                   ;;(when (vector-ref on-offs n)
                   ;;  (-> g fill-rect bar-color   x* (- (by) bar-height bar/button-border) bar-width bar-height))
                   ;;(-> g draw-rect black-color x* (- (by) bar-height bar/button-border) bar-width bar-height)
                   (get-button-vars num (lambda (x* y* x2* y2* width* height*)
                                          (paint-bar g colors x* y* y* width* height*)
                                          (define text (vector-ref note-names num))
                                          (-> g do-alpha alpha-0.75
                                              (lambda ()
                                                (define color (if (= num root-note) 
                                                                  root-note-color
                                                                  black-color))
                                                ;;(-> g draw-rect black-color x* (- y2 7 name-height) width* (+ 7 name-height))
                                                (if (= 1 (string-length text))
                                                    (-> g draw-string text color (+ 4 x*) (- y2 7 name-height))
                                                    (-> g draw-string text color (+ 1 x*) (- y2 7 name-height)))))))
                   )))))

  ;; set root note
  (add-mouse-cycle (lambda (button mx* my*)
                     (call/cc (lambda (return)
                                (c-for 0 < 12 1
                                       (lambda (num)
                                         (get-button-vars num (lambda (x* y* x2* y2* width* height*)
                                                                (set! y* (- y2 7 name-height))
                                                                (when (and (>= mx* x*)
                                                                           (>= my* y*)
                                                                           (<  mx* x2*)
                                                                           (<  my* (+ y* 7 name-height)))
                                                                  (c-display "heppsann" num x* mx* (* 1.0 x2*))
                                                                  (set! root-note (- root-note))
                                                                  (rotate-scale) ;; rotate back first
                                                                  (set! root-note num)
                                                                  (rotate-scale)
                                                                  (c-display weights)
                                                                  (repaint-me!)
                                                                  (-> parent check-paint)
                                                                  (remap-notes-to-grid)
                                                                  (-> *pianoroll* repaint-me!)
                                                                  (return 'grab))))))
                                #f))))

  ;; set on/off
  (add-mouse-cycle (lambda (button x* y*)
                     (call/cc (lambda (return)
                                (c-for 0 < 12 1
                                       (lambda (num)
                                         (get-button-vars num (lambda (x y x2 y2 width height)
                                                                (when (and (>= x* x)
                                                                           (< x* x2)
                                                                           (>= y* y)
                                                                           (< y* y2))
                                                                  (vector-set! on-offs num (not (vector-ref on-offs num)))
                                                                  (repaint-me!)
                                                                  (-> parent check-paint)
                                                                  (remap-notes-to-grid)
                                                                  (-> *pianoroll* repaint-me!)
                                                                  (return 'grab))))))
                                #f))))

  ;; set value
  (let ((start-x 0)
        (start-y 0))
    (define (get-num x*)
      (call/cc (lambda (return)
                 (c-for 0 < 12 1
                        (lambda (num)
                          (get-bar-vars num (lambda (x x2 y y2)
                                              (when (and (>= x* x)
                                                         (< x* x2))
                                                (return num))))))
                 #f)))
    (define (moved button dx dy)
      (define x* (+ dx start-x))
      (define y* (+ dy start-y))
      (define num (get-num x*))
      (when num
        (get-bar-vars num (lambda (x x2 y y2)
                            (vector-set! weights num (boundaries 0.0 (c-scale y* y y2 1.0 0.0) 1.0))
                            (repaint-me!)
                            (-> parent check-paint)
                            (remap-notes-to-grid)
                            (-> *pianoroll* repaint-me!))))
      #t)
    (apply add-mouse-cycle (delta-mouse-cycle parent
                                              (lambda (button x* y*)
                                                (define num (get-num x*))
                                                (if num
                                                    (get-bar-vars num (lambda (x x2 y y2)
                                                                        (c-display "hepp" x* y* x x2 y y2 num)
                                                                        (when (and (>= x* x)
                                                                                   (< x* x2)
                                                                                   (>= y* y)
                                                                                   (< y* y2))
                                                                          (set! start-x x*)
                                                                          (set! start-y y*)
                                                                          (moved button 0 0)
                                                                          'grab)))))
                                              moved
                                              moved)))
  )





