


(def-area-subclass (<soundtype> parent width height)

  (define sounds (make-list 4))
  (define logos (make-list 4))
  (define names (make-list 4))

  (define vertical-air 5)

  (define (get-pos num kont)
    (kont x (i-scale num 0 4 y y2) x2 (- (i-scale (1+ num) 0 4 y y2)
                                         vertical-air)))

  (define (paint g x* y* x2* y2*)
    (for-each (lambda (num sound logo name)
                (get-pos num (lambda (x y x2 y2)
                               (-> g fill-rect green-color x y (- x2 x) (- y2 y))
                               (c-display sound logo name)))
                )
              (iota 4)
              sounds
              logos
              names)
    (-> g draw-line blue-color x (average y y2) x (average y y2))
    (-> g draw-rect red-color x y (1- width) (1- height))
    )
  
  (def-method (add-sound num sound logo name)
    (list-set! sounds num sound)
    (list-set! logos num logo)
    (list-set! names num name)
    (repaint-me!))

  )



