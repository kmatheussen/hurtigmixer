
(c-import gfx-button)


(define (<gfx-slider> parent width height x-1 x-2 gfx-filename report-func)

  (def-var slider (<slider> parent width height x-1 x-2 report-func))

  

  

  

(c-define 

  (define button (<gfx-button> parent gfx-filename (lambda x x)))

  (-> button mouse-cycles '())

  (define start-y 0)
  (define last-dy 0)

  (apply (<- button add-mouse-cycle)
	 (delta-mouse-cycle parent
			    (lambda (button-num x* y*)
			      (-> button get-position (lambda (x y x2 y2 width height)
							(set! start-y y)
							(c-display "Hai!" x* y*)))
			      'grab)
			    (lambda (button-num dx dy)
			      ;;(c-display "moved!" dx dy)
                              (report-func (c-scale (+ dy start-y) y (+ y height) x-1 x-2))
			      (set! last-dy dy)
			      (-> button repaint-me!)
			      (-> button set-position! x (+ dy start-y))
			      (-> button repaint-me!))
			    (lambda (button-num dx dy)
			      (c-display "top-sounds, released!")
			      (set! last-dy dy)
			      )))


  (-> parent add-sub-area button x y))

