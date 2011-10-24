


(define-macro (d-define-macro def . body)
  `(begin
     (set! macro-generation (1+ macro-generation))
     ,(c-display "pair?" (pair? def))
     ,(if (pair? def)
	  `(hashtable/put! all-macros ',(car def) (lambda ,(cdr def)
						    ,@body))
	  `(hashtable/put! all-macros ',def ,@body))

     (define-macro ,def ,@body)))

(define-macro (c-defmacro name def . body)
  `(d-define-macro (,name ,@def) ,@body))

(define (d-macroexpand-1 expr)
  (if (or (not (pair? expr))
	  (null? expr)
	  (not (symbol? (car expr))))
      expr
      (let ((qua (hashq-ref all-macros (car expr))))
	(if (not qua)
	    (begin
	      ;;(c-display "Error in expand-a-macro. Macro for " expr " Not found.")
	      expr)
	    (apply qua (cdr expr))))))

  
(d-define-macro (add a b . c)
		(let ((d (+ b 9)))
		  `(+ ,a ,b ,d ,@c)))

(d-define-macro add (lambda (a b)
		      (let ((c (+ b 10)))
			`(+ ,a ,b ,c))))

(c-defmacro add (a b . d)
	    (let ((c (+ b 15)))
	      `(+ ,a ,b ,c ,@d)))

(d-macroexpand-1 '(add 2 3 4 5))


(let ((a 5))
  (c-define-macro (b)
    a)
  (b))

(let ((a 5))
  (define b (lambda ()
	      a))
  a)


(define pi 3.14159265)
(define (pan1 val)
  (set! val (c-scale val -1 1 (/ pi 4) (/ (- pi) 4)))
  (list (* (sqrt 2) 0.5 (+ (cos val) (sin val)))
	(* (sqrt 2) 0.5 (- (cos val) (sin val)))))

(define (pan2 pan)
  (let ((scale (- 2 (* 2 (sqrt 2))))
	(x (c-scale pan -1 1 0 1)))
    (list (* (- 1 x)
	     (+ (* scale (- 1 x))
		(- 1 scale)))
	  (* x (+ (* scale x)
		  (- 1 scale))))))

(define (get-stereo-pan pan)
  (if (<= pan 0)
      (list (pan2 -1)
	    (pan2 (c-scale pan -1 0 -1 1)))
      (list (pan2 (c-scale pan 0 1 -1 1))
	    (pan2 1))))
      
(get-stereo-pan -1)
(get-stereo-pan -0.5)
(get-stereo-pan 0)
(get-stereo-pan 0.5)
(get-stereo-pan 1)

(let ((val 0.7))
  (list (pan1 val)
	(pan2 val)))




(expand-file "frametest.scm" "frametest.sce")

(expand-file "gencode/<mixer-area>.scm" "testing.sce")

(load "gencode/<mixer-area>.scm")

(load "testing.sce")



(begin
  (define is-running-applet #f)
  (load "various.scm"))


(begin
  (load "dummy.scm")
  (define is-running-applet #f)
  (compile-file "srfi-1.scm" "scc/srfi-1.scc"))

(if (and (not is-running-applet)
	 (not (file-is-file? "scc/srfi-1.scc")))
    (compile-file "srfi-1.scm" "scc/srfi-1.scc"))

(begin
  (load "dummy.scm")
  (compile-file "various.scm" "various.scc"))







(c-import macros)
(c-import trace)
(c-import lambda-lifter)
(c-import java)
(c-import oo)
(c-import c-window)
(c-import colors)

