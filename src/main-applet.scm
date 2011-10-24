
(define old-load #f)


(if (not old-load)
    (begin
      (set! old-load load)
      (set! load (lambda (filename . rest)
		   (display "...loading \"")(display filename)(display "\".")(display urlbase)(newline)
		   (let ((ret (old-load (string-append urlbase filename))))
		     (display "...finished loading \"")(display filename)(display "\".")(newline)
		     ret)))))

(define is-running-applet #t)
(define is-running-standalone #f)
(define *running-melodigenerator* #f)
(define *running-hurtigmixer* #t)
(define please-dont-trace  #t)

;; melodigenerator under
;;(define *running-melodigenerator* #t)
;;(define *running-hurtigmixer* #f)
;;(define mix-length 9)

(load "main.scm")

