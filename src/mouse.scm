



(define *left-mouse-button* 1)
(define *middle-mouse-button* 2)
(define *right-mouse-button* 3)

(define *scroll-up-mouse-button* 5)
(define *scroll-down-mouse-button* 4)


(define *ctrl-mouse-multiplicator* 0.1)



(c-define* (delta-mouse-cycle parent press drag release :key release-thunk)
  (define start-x 0)
  (define start-y 0)

  (define was-control? #f)

  (define last-x-add 0)
  (define last-y-add 0)

  (define x-add 0)
  (define y-add 0)

  (define has-moved #f)

  (define (movement-func callback)
    (lambda (button x* y*)

      (define dx (+ x-add (- x* start-x)))
      (define dy (+ y-add (- y* start-y)))

      (define changed? (or has-moved
			   (not (= 0 dx))
			   (not (= 0 dy))))
      (set! has-moved (or has-moved changed?))

      (define res #f)

      (if changed?
	  (set! res (if (-> parent control-pressed?)
			(begin
			  (when (not was-control?)
				(set! x-add dx)
				(set! y-add dx))
			  (set! was-control? #t)
			  (callback button
				    (+ x-add (* *ctrl-mouse-multiplicator* dx))
				    (+ y-add (* *ctrl-mouse-multiplicator* dy)))
			  )
			(begin
			  '(when was-control?
				 (inc! x-add last-x-add)
				 (inc! y-add last-y-add))
			  (set! was-control? #f)
			  (callback button dx dy)))))

      (set! last-x-add dx)
      (set! last-y-add dy)
      
      (if (and release-thunk
	       (eq? callback release))
	  (release-thunk))
      
      res))


  (list

   (lambda (button x* y*)
     (and-let* ((res (press button x* y*)))
	(set! was-control? (-> parent control-pressed?))
	(set! has-moved #f)
        (set! start-x x*)
	(set! start-y y*)
	(set! x-add 0)
	(set! y-add 0)
	res))

   (movement-func drag)
   (movement-func release)))



