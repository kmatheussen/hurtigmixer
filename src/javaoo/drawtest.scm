

;; Load necessarry files.
(load "various.scm")
(c-import java)


;; Set up window
(define frame (new <java.awt.Frame> "Drawing Window"))
(begin
  (-> frame setVisible #t)
  (-> frame setBounds 500 400 400 400))


;; A small drawing class.
(def-class (<painter> g)
  (Super g (new-static <java.lang.Math>)) ;; <painter> is both a subclass of g's class and java.lang.Math
  (define last-x 200)
  (define last-y 200)
  (def-method (draw mouse-x mouse-y)
    (=> drawLine last-x last-y mouse-x mouse-y)
    (=> drawLine 200 200 (=> abs (- mouse-x 200)) (=> abs (- mouse-y 200)))
    (+ 2 3 4)
    (set! last-x mouse-x)
    (set! last-y mouse-y)))


;; Connect everything.
(-> frame 
    addMouseMotionListener
    (let* ((get-x (get-java-function <java.awt.event.MouseEvent> getX))
	   (get-y (get-java-function <java.awt.event.MouseEvent> getY))
	   (painter (<painter> (-> frame getGraphics)))
	   (callback (lambda (e)
		       (-> painter draw (get-x e) (get-y e)))))
      (java-implement <java.awt.event.MouseMotionListener>
		      (mouse-dragged callback)
		      (mouse-moved callback))))



#|

;; Various


;; List names of all methods in frame
(-> frame dir)


;; create new painter.
(define painter (<painter> (-> frame getGraphics)))


;;; List all methods and superclasses (including the superclasses methods) in painter.
(-> painter dir)


;; Draw someting
(-> painter draw 50 300)


;;; Check its contents again. drawLine and abs has been added to the list of methods.
(-> painter dir)


;; Get the two java super objects in painter.
(-> (-> painter get-super 0) get-object)
(-> (-> painter get-super 1) get-object)


;; Add yet another superobject
(-> painter add-super (new <java.awt.Rectangle> 800 200 300 250))
(list (-> painter x)
      (-> painter y)
      (-> painter width)
      (-> painter height))

;; Try evaluating the two previous blocks once more. This time it doesn't take that long.
(-> painter add-super (new <java.awt.Rectangle> 800 200 300 250))
(list (-> painter x)
      (-> painter y)
      (-> painter width)
      (-> painter height))


;; To get java objects, instead of writing (-> frame getobject), we just write (<-o frame)
(<-o frame)
(<-o (-> painter get-super 0))
(<-o (-> painter get-super 1))
(<-o (-> painter get-super 2))


;; Another way to set bounds.
(-> frame setBounds (-> painter get-super 2))


;; Get backtrace
(bt)

|#
