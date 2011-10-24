



(def-area-subclass (<vu-area> parent width height)
  
;;  (define (paint g x* y* x2* y2*)
  ;;    (-> parent paint-background g x* y* x2* y2*))
  

  (define last-vu 0)
  
  (def-method (paint-vu)
    (let ((g (-> parent ll-window-g))
	  (image (-> parent image))
	  (vu (i-scale (-> mixer maxVal) 0 1 y2 y)))
      (-> g setClip x y (- x2 x) (- y2 y))
      (if (< vu last-vu)
	  (-> g fill-rect blue-color x vu (- x2 x) (- last-vu vu))
	  (if (> vu last-vu)
	      (-> g blit-sub-image image x last-vu x2 vu)))
      (set! last-vu vu)))
  
  (def-method (blit g image x* y* x2* y2*)
    (let ((vu (i-scale (-> mixer maxVal) 0 1 y2 y)))
      (-> g setClip x y (- x2 x) (- y2 y))
      (-> g blit-sub-image image x y x2 vu)
      (-> g fill-rect blue-color x vu (- x2 x) (- y2 vu))
      (set! last-vu vu)))
  
  )








