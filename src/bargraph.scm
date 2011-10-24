

(def-area-subclass (<bargraph> parent width height)

  (def-var pre-callback #f) ;; Used in case something needs to be run before modifying the values. (see melodigenerator.scm)
  (def-var callback #f)

  (def-var tempo-color (get-RGB-color 122 175 64))

  (define pixels-per 4)

  (define (pixel-> x)
    (c-integer (/ x pixels-per)))

  (define values (new <TempoBarGraphHelper> width 0.5 tempo-area-min-timeslice tempo-area-max-timeslice))
  
  ;;(def-var values (make-vector width 0.5))
  ;;(def-var values-integrated (make-vector width 0.0)) ;; To speed up get-average-value

  (def-var bar-color blue-color)
  
  (def-method (get-integrated-average-value i)
    (-> values getIntegratedAverageValue (boundaries 0 i (1- width))))

  (def-method (get-integrated-value i)
    (-> values getIntegratedValue (boundaries 0 i (1- width))))

  (def-method (get-value i)
    (-> values getValue (boundaries 0 i (1- width))))
  ;;(vector-ref values-integrated i))

#|
  ;; Why is this function so slow?
  (define (calculate-values-integrated! start)
    (let loop ((i start)
	       (sum (if (= 0 start) 0.0 (vector-ref values-integrated (1- start)))))
      (when (< i width)
	(let ((sum (+ sum (vector-ref values i))))
	  (vector-set! values-integrated i (/ sum i))
	  (loop (1+ i)
		sum)))))
|#

  (define texturepaint #f)
  
  (define (paint g x* y* x2* y2*)
    (define pixels-per 4)
    (when (not texturepaint)
      (c-display "trying to load" (config-full-url "Tempo100.png"))
      (set! texturepaint (-> g get-texturepaint (config-full-url "Tempo100.png")))
      (c-display "finished loading"))
    ;;(-> g draw-rect black-color x y (1- width) (- height 2))
    (set! x* (i-boundaries x x* x2))
    (set! x2* (i-boundaries x x2* x2))
    (  ;;-> g do-alpha 0.5
        (lambda ()
          (-> g do-antialized
              (lambda ()
                (define len (c-integer (/ (- x2* x*) pixels-per)))
                (define prev-y 0)
                (let ((poly (-> g make-poly
                                (+ 2 len)
                                (lambda (i kont)
                                  (cond ((= i 0)
                                         (kont x y2))
                                        ((= i len)
                                         (kont x2 prev-y))
                                        ((= i (+ len 1))
                                         (kont x2 y2))
                                        (else
                                         (set! i (* (1- i) pixels-per))
                                         (let ()
                                           (define oldi (+ i x*))
                                           (let ()
                                             (define avg (let loop ((i 0)
                                                                    (sum 0.0))
                                                           (if (or (= i pixels-per)
                                                                   (>= (+ i (- oldi x)) width))
                                                               (/ sum i) ;pixels-per)
                                                               (loop (1+ i)
                                                                     (+ sum (-> values getUnmappedValue (+ i (- oldi x))))))))
                                             (set! prev-y (boundaries y
                                                                      (i-scale avg 0.0 1.0 y2 y)
                                                                      (- y2 2)))
                                             (kont (+ x* i) prev-y)))))))))
                  (-> g do-painter texturepaint
                      (lambda ()
                        (-> g fill-polygon #f poly)))
                  (-> g set-poly! poly (1+ len) x2 prev-y)
                  (-> g do-stroke 1.2
                      (lambda ()
                        (-> g draw-polyline tempo-color poly)))))))))
  

  (define last-pointed-x #f)
  (define last-pointed-y #f)

  (define (pointed-pointed x* y*)
    (set! x* (boundaries x x* x2))
    (define bar (- x* x))
    (-> values setValue bar (boundaries 0.0 (c-scale y* y y2 1.0 0.0) 1.0))
    ;;(calculate-values-integrated! bar)
    ;;(-> parent repaint x* y (1+ x*) y2)
    (-> parent repaint x y x2 y2)
    )
  
  (define (pointed-do button x* y* is-finished)
    (when last-pointed-x
      (if (> last-pointed-x x*)
	  (c-for (1+ x*) < last-pointed-x 1
		 (lambda (x**)
		   (pointed-pointed x** (i-scale x** x* last-pointed-x y* last-pointed-y))))
	  (c-for (1+ last-pointed-x) < x* 1
		 (lambda (x**)
		   (pointed-pointed x** (i-scale x** x* last-pointed-x y* last-pointed-y))))))
    
    (pointed-pointed x* y*)
    
    (if is-finished
        (-> values calculateIntegratedValues 0))

;;  (if last-pointed-x
;;      (-> values calculateIntegratedValues (boundaries 0 (- (min last-pointed-x x*) x) (1- width)))
;;      (-> values calculateIntegratedValues (boundaries 0 x* (1- width)))))

    (if callback
	(callback this))

    (set! last-pointed-x x*)
    (set! last-pointed-y y*)
    )

  (define (pointed button x* y* is-finished)
    (if (and is-finished pre-callback)
	(pre-callback (lambda () (pointed-do button x* y* is-finished)))
	(pointed-do button x* y* is-finished)))

  ;;(define old-volume 0.0)

  (add-mouse-cycle (lambda (button x* y*)
		     (when (or (= button *left-mouse-button*)
			       (= button *right-mouse-button*))
		       (set! last-pointed-x #f)
		       (set! last-pointed-y #f)
		       (pointed button x* y* #f)
                       ;;(set! old-volume (-> (-> *das-mixer* volume) get_goal))
                       ;;(-> (-> *das-mixer* volume) set 0.0)
		       'grab))
		   (lambda (button x* y*)
                     (pointed button x* y* #f))
                   (lambda (button x* y*)
                       (pointed button x* y* #t)
                     ;;(-> (-> *das-mixer* volume) set old-volume)
		     ;;(-> values calculateIntegratedValues 0)
		     ))
  

  ;;(calculate-values-integrated! 0)
  )



					
  

  



