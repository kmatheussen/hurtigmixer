
;;(native-module r5rs "sisc.modules.R5RS")
;;(include "sisc/libs/srfi/srfi-23.scm")
;(include "sisc/modules/srfi/srfi-8.scm")

(display "11111")(newline)
(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error "Bad argument" val pred caller)))))
(display "22211111")(newline)

(if (and (not is-running-applet)
	 (not srfi-1-is-compiled))
    (eval '(load (string-append (get-parent-url "srfi-1.scm") "/sisc/modules/srfi/optional-args.scm"))))

(if (not is-running-applet)
    (eval '(define-syntax receive
	     (syntax-rules ()
	       ((receive formals expression body ...)
		(call-with-values (lambda () expression)
		  (lambda formals body ...)))))))

;;(include "sisc/modules/srfi/optional-args.scm")
(display "22222")(newline)
(include "sisc/modules/srfi/srfi-1.scm")
(display "33333")(newline)


