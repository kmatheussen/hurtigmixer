


(define java.awt.Color (new-static <java.awt.Color>))
(define green-color (-> java.awt.Color green))
(define black-color (-> java.awt.Color black))
(define white-color (-> java.awt.Color white))
(define red-color (-> java.awt.Color red))
(define gray-color (-> java.awt.Color gray))
(define light-gray-color (-> java.awt.Color lightGray))
(define yellow-color (-> java.awt.Color yellow))
(define blue-color (-> java.awt.Color blue))
(define redbrown-color (-> Various getColor 0.83 0.25 0.25))
(define verygray-color (-> Various getColor 0.25 0.25 0.25))
(define veryverygray-color (-> Various getColor (/ 43.0 255) (/ 43.0 255) (/ 41.0 255)))
(define cursor-color redbrown-color)

(define (get-RGB-color R G B)
  (-> Various getColor (/ R 255.0) (/ G 255.0) (/ B 255.0)))

(def-class (<color-range> rangelen r g b min-factor max-factor)
  
  (define colors (make-vector rangelen))

  (c-for 0 < rangelen 1
         (lambda (i)
           (define scaleval (/ i (1+ rangelen)))
           (vector-set! colors i (-> Various getColor 
                                     (max 0.0 (min 1.0 (* r (c-scale i 0 (1- rangelen) min-factor max-factor))))
                                     (max 0.0 (min 1.0 (* g (c-scale i 0 (1- rangelen) min-factor max-factor))))
                                     (max 0.0 (min 1.0 (* b (c-scale i 0 (1- rangelen) min-factor max-factor))))))))

  (def-method (get val)
    (vector-ref colors (c-integer (c-scale val 0 1 0 (1- rangelen)))))

  )


