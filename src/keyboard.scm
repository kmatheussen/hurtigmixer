


(def-area-subclass (<keyboard> parent width height)
  
  (def-method (paint g x* y* x2* y2*)
    (c-display "KEYBOARD PAINT START")
    (c-for lower < upper 1
	   (lambda (note)
	     (define chroma (modulo note 12))
	     (define weight (get-weight note))
	     (define y* (i-scale (1+ note) upper lower y y2))
	     (define y2* (i-scale note upper lower y y2))
	     (define is-black (memv chroma '(1 3 6 8 10)))
	     ;;(c-display "y/y2" y* y2*)
	     (if is-black
		 (-> g fill-rect black-color (+ 10 x) (+ y* 2) (- width 13) (- y2* y* 4))
		 (-> g fill-rect white-color x y* (- width 2) (- y2* y*)))
	     (if is-black
		 (-> g draw-rect black-color (+ 10 x) (+ y* 2) (- width 13) (- y2* y* 4))
		 (-> g draw-rect black-color x y* (- width 2) (- y2* y*)))))
    (c-display "KEYBOARD PAINT END")
    )
)

