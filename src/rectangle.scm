



(def-class (<rectangle>)
  (define x #f)
  (define y #f)
  (define x2 #f)
  (define y2 #f)


  (def-method (add-max x* y* x2* y2*)
    (if x
	(begin
	  (if (< x* x)
	      (set! x x*))
	  (if (< y* y)
	      (set! y y*))
	  (if (> x2* x2)
	      (set! x2 x2*))
	  (if (> y2* y2)
	      (set! y2 y2*)))
	(begin
	  (set! x x*)
	  (set! y y*)
	  (set! x2 x2*)
	  (set! y2 y2*))))

  (def-method (reset!)
    (set! x #f))

  (def-method (get-max callback)
    (if x
	(callback x y x2 y2))))


