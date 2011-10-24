

(define (get-bug-func)
  (lambda ()
    (let ((a 90)
	  (b #f))
      (if a
	  (+ a b)))))

(define (dobug)
  (let ((dobug-innerfunc (get-bug-func)))
    (dobug-innerfunc)))
		 
(define (callbug)
  (dobug))



(define-macro (callbug-macro)
  `(callbug))

(define-macro (callbug-macro2)
  (let ((a 90)
	(b #f))
    (if a
	(+ a b))))

(define (create-bug-func-helper name)
  `(define (,name)
     (let ((a 90)
	   (b #f))
       (if a
	   (+ a b)))))

(define-macro (create-bug-func name)
  (create-bug-func-helper name))

(create-bug-func callbug2)

(eval '(define (callbug3)
	 (callbug))
      (interaction-environment))
(eval '(define (callbug3)
	 (callbug)))
(define (callbug3)
  (callbug))



#|
(callbug3)
(eval '(eval '(apply callbug '())))
|#


(define amacros (make-hash-table 251))
(begin amacros)

(define-macro (define-a-macro def body)
  (hashq-set! amacros (car def) (list (cdr def) body))
  `(define ,def (eval ,body)))

(define-a-macro (dosomething b)
  `(+ 50 ,b #f))

(define-macro (expand-a-macro expr)
  (let ((qua (hashq-ref amacros (car expr) amacros)))
    `(let ,(map list (car qua) (cdr expr))
       ,(cadr qua))))


