(define old-load #f)


(if (not old-load)
    (begin
      (set! old-load load)
      (set! load (lambda (filename . rest)
		   (display "...loading \"")(display urlbase)(display filename)(newline)
		   (let ((ret (old-load (string-append urlbase filename))))
		     (display "...finished loading \"")(display urlbase)(display filename)(newline)
		     ret)))))

(define is-running-applet #f)
(define is-running-standalone #t)
(define please-dont-trace  #t)
(load "main.scm")
