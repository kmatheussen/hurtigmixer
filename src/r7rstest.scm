

(define (testing)
  (display "test1")(newline)
  (include "r7rstest_include.scm")
  (define a 5)
  (define-macro (macrotest c)
    `(+ ,c 2))
  (macrotest a b))

(let ((a 1)
      (b 2))
  (+ a (+ 6 b)))

#|
(display "hello world")
(newline)
|#

(let ((a 1))
  `(,(begin
       (set! a 2)
       3)
    ,a))

;(let ((a* 1)
;     (funcname 'a))
; (define-toplevel (,funcname)
;   a*))
 

(let ((a* '(1 2)))
  (define-toplevel (a)
    (car a*)))


(define-macro (add . rest)
  `(+ ,@rest))

;;(apply add '(1 2))

(begin :aiai)

(define (keyword->symbol key)
  (string->symbol (list->string (cdr (string->list (symbol->string key))))))

(begin (keyword->symbol :aiai))


(display (let ((a* '(1 2)))
	   (define-toplevel (a)
	     (car a*))
	   (a)))

(define-macro (dosomething b)
  `(+ 50 ,b 60))
(define-macro (dosomething2 a . b)
  `(+ 50 ,a ,@b 60))
(define-macro (dosomething3 . c)
  (let ((d c))
    `(dosomething2 (dosomething 5) ,@d)))

(define-macro (lettest)
  `(let ((a 5))
     (dosomething 50)))

(display (macroexpand '(lettest)))
(newline)



