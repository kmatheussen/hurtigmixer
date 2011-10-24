
(module-name 'll-frame)


;;(java-import <java.awt.Frame>)


(def-class (<ll-frame> paint-func) (Super (new <java.awt.Frame>))
  (def-method (paint g)
    (paint-func g)))



#|

(define f (<ll-frame> (lambda (g)
			(c-display "updated" g))))

(-> f setBounds 700 100 300 300)
(-> f setVisible #t)
(-> f setVisible #f)
(-> f repaint)
(let ((last-x 0)
      (last-y 0)
      (comp :: <java.awt.Component> f))
  (-> comp addMouseListener
      (object (<java.awt.event.MouseAdapter>)
	      ((mousePressed (e :: <java.awt.event.MouseEvent>)) <void>
	       (c-display "mousepressed" (-> e getX) (-> e getY))
	       (set! last-x (-> e getX))
	       (set! last-y (-> e getY)))
	      ((mouseReleased (e :: <java.awt.event.MouseEvent>)) <void>
	       (c-display "mousereleased" (-> e getX) (-> e getY)))))
	      
  (-> comp addMouseMotionListener
      (object (<java.awt.event.MouseMotionAdapter>)
	      ((mouseDragged (e :: <java.awt.event.MouseEvent>)) <void>
	       (let ((g :: <java.awt.Graphics>
			(-> comp getGraphics))
		     (x :: <int> (-> e getX))
		     (y :: <int> (-> e getY)))
		 (-> g drawLine last-x last-y x y)
		 (set! last-x x)
		 (set! last-y y))))))


|#
