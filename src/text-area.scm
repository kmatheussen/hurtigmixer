


(def-area-subclass (<text-area> parent width height text :key mouse-cycle (text-color black-color) background-color background-alpha (font #f))

  (def-var text-color)

  (def-var text)

  (def-var font)

  (def-method (paint g x* y* x2* y2*)

    (if background-color
	(let ((func (lambda ()
		      (-> g fill-rect background-color x y width height))))
	  (if background-alpha
	      (-> g do-alpha background-alpha func)
	      (func))))

    (let ((doit (lambda ()            
                  (-> g draw-string text text-color x (+ 0 y)))))
      (if font
          (-> g do-font font doit)
          (doit))))
            


  (def-method (set-and-force-paint! newtext)
    (set! text newtext)
    (repaint-me!)
    (-> parent check-paint))


  (if mouse-cycle
      (apply add-mouse-cycle (map (lambda (func)
				    (lambda (button x* y*)
				      (func button x* y* this)
				      ))
				  mouse-cycle)))
  
  )



