

(define *do-trace?* #t)
(define *trace-length* 50)
(define *trace-history* (make-vector *trace-length* #f))
(define *trace-write-num* -1)


(define* (bt :optional (len (min 5 *trace-length*)))
  (pretty-print (let loop ((num *trace-write-num*)
			   (count len))
		  (if (= count 0)
		      '()
		      (let ((num (if (= num -1)
				     (1- *trace-length*)
				     num)))	  
			(cons (vector-ref *trace-history* num)
			      (loop (1- num)
				    (1- count)))))))
  (newline))


(define (add-trace-do . trace)
  ;;(c-display trace)
  (inc! *trace-write-num* 1)
  (if (>= *trace-write-num* *trace-length*)
      (set! *trace-write-num* 0))
  (vector-set! *trace-history* *trace-write-num* trace))


(define-macro (add-trace traceid infostring command)
  (let* ((atoms '()))
    (for-each (lambda (atom?)
		(if (symbol? atom?)
		    (push-back! atom? atoms)))
	      (cdr command))
    `(begin
       ;;(c-display "atoms" ',atoms)
       (add-trace-do ',traceid ,infostring ',command ;;',atoms
		     ,@(map (lambda (atom)
			      `(list (quote ,atom) '|=| ,atom))
			    atoms))
       ,command)))
	   


(define (add-traces-do infostring expr)
  (let loop ((expr expr))
    (schemecodeparser expr
		      ':use-customsymbolhandler? (lambda (expr)
						   ;;(c-display "custom?" expr)
						   (if #t
						       #f
						       (or (eq? 'lambda (car expr))
							   (and (eq? 'define (car expr))
								(pair? (cadr expr))))))
		      ':customsymbolhandler (lambda (expr)
					      `(,(car expr) ,(cadr expr)
						(add-trace-do ',(get-trace-gennum) (list ',(car expr) ',(cadr expr)))
						,@(map loop (cddr expr))))
		      ':elsefunc (lambda (expr)
				   (let ((ret (map loop expr)))
				     (cond ((pair? (car expr))
					    ret)
					   ((or (eq? 'or (car expr))
						(eq? 'and (car expr)))
					    ret)
					   (else
					    `(add-trace ,(c-gensym "t") ,infostring ,ret))))))))

					  
				       
				     



(c-define-macro (add-traces infostring . expr)
  (if *do-trace?*
      (apply append (map-in-order (lambda (expr)
				    (add-traces-do infostring expr))
				  (map a-macroexpand expr)))
      expr))


(define-macro (c-macroexpand2 expr)
  `(a-macroexpand (eval (caddr (caddr (a-macroexpand ,expr))))))


(define (c-define-do definename def body)
  (let ((funcname (if (pair? def) (car def) def)))
    `(begin
       (reset-gensym)
       (cache-to-disk-do (symbol-append ',funcname
					'|-c-|
					  ',definename)
			 `(,',definename ,(quote ,def)
			    ,@(map (lambda (expr)
				     (add-traces-do (symbol->string ',funcname) expr))
				   (fix-defines-do
				    (a-macroexpand ',body))))))))

(c-define-macro (c-define def . body)
  (c-define-do 'define def body))

(c-define-macro (c-define* def . body)
  (c-define-do 'define* def body))




;;(define-macro (c-define-macro def . body)
;;  (let ((newbody (add-traces body)))
;;    (hashtable/put! all-macros (car def) (list (cdr def) newbody))
;;    `(define-macro ,def ,@newbody)))


#|
(c-define (aiai2)
	 (+ 2 3 ((lambda () 4))))

(a-macroexpand '(c-define (aiai2)
		   (+ 2 3 ((lambda () 4)))))

(a-macroexpand '(add-traces (define (aiai)
			      (+ 2 3 ((lambda () 4))))))


(aiai)
(begin *trace-history*)

(define (aiai)
  (add-trace
   (get-trace-gennum)
   '(define (aiai)))
  (+ 2
     3
     ((lambda ()
        (add-trace (get-trace-gennum) '(lambda ()))
        4))))



|#
