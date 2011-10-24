
#|
  Kjetil Matheussen, 2007
    
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#



(define *do-trace?* #t)
(define *trace-length* 50)
(define *trace-history* (make-vector *trace-length* #f))
(define *trace-write-num* -1)

(c-display "GAKKGAKKGAKK")
(c-import lambda-lifter)
(c-display "GAKKGAKKGAKK2")

(define (get-bt-copy)
  (define ret (make-vector *trace-length* #f))
  (c-for 0 < *trace-length* 1
	 (lambda (n)
	   (vector-set! ret n (vector-ref *trace-history* n))))
  ret)


;; bt=back-trace
(define* (bt :optional (len 5) (trace *trace-history*))
  (set! len (min len *trace-length*))
  (pretty-print (let loop ((num *trace-write-num*)
			   (count len))
		  (if (= count 0)
		      '()
		      (let ((num (if (= num -1)
				     (1- *trace-length*)
				     num)))	  
			(cons (vector-ref trace num)
			      (loop (1- num)
				    (1- count)))))))
  (newline))

(define getThreadFunc #f)

(eval '(import threading))

(define trace-lock (mutex/new))

(define (add-trace-do . trace)
  ;;(c-display trace)
  (mutex/lock! trace-lock)

  (inc! *trace-write-num* 1)
  (when (>= *trace-write-num* *trace-length*)
    ;;(c-display "HMM" (defined? 'Various))
    (when #f
      (display "BHEPP write-num: ")(display *trace-write-num*)
      (display ", write-length: ")(display *trace-length*)(display " id: ")
      (display (when (defined? 'Various) 
		 ((java-method-procedure (get-jmethod (java-class 'Various) 'getThreadId)) jnull)))
      (newline))
    (set! *trace-write-num* 0))
  (vector-set! *trace-history* *trace-write-num* trace)

  (mutex/unlock! trace-lock))


#|
(define (add-trace-do . trace)
  #t)
|#

(define (append-var-to-trace! var)
  ;;(c-display "appending" var)
  (vector-set! *trace-history* *trace-write-num*
	       (append! (vector-ref *trace-history* *trace-write-num*)
			(list var))))

(define-macro (dont-trace expr)
  expr)
(define-macro (dont-trace2 expr)
  expr)

(define-macro (at-old infostring command)
  (let* ((atoms '()))
    (for-each (lambda (atom?)
		(if (symbol? atom?)
		    (push-back! atom? atoms)))
	      (cdr command))
    `(let ((at-das-atoms (list ,@(map (lambda (atom)
					`(list (quote ,atom) '|=| 'unknown-value))
				      atoms))))
       ;;(c-display "atoms" ',atoms)
       (add-trace-do ',infostring ',command at-das-atoms)
       ,@(map (lambda (atom num)
		`(set-car! (cddr (list-ref at-das-atoms ,num))
			   ,atom))
	      atoms
	      (iota (length atoms)))
       ,command)))


;; at=add trace
(define-macro (at infostring command)
  (let* ((args (cdr command))
	 (num-args (length args))
	 (argnums (iota num-args))
	 (at-argl-names (map gensym argnums))
	 (at-das-atoms (gensym))
	 (ret (gensym))
	 (at-arg-names (map gensym argnums)))
    `(let ,(map (lambda (argl-name num arg)
		  `(,argl-name (list ',(if (pair? arg)
					   (string->symbol (string-append "arg" (number->string num)))
					   arg)
				     '|=| 'unknown-value)))
		at-argl-names
		argnums
		args)
       (let ((,at-das-atoms (list ,@at-argl-names)))
	 (add-trace-do ',infostring ',command ,at-das-atoms)
	 (let* ,(map (lambda (arg-name argl-name arg)
		       `(,arg-name (let ((,ret ,arg))
				     (set-car! (cddr ,argl-name) ,ret)
				     ,ret)))
		     at-arg-names
		     at-argl-names
		     args)
	   (,(car command) ,@(map (lambda (arg arg-name)
				    (if (pair? arg)
					arg-name
					arg))
				  args
				  at-arg-names)))))))


(if please-dont-trace
    (eval '(define-macro (at infostring command)
	     command)))


#|  

(at "funcname 1" (command 1 a (+ b c)))

->

(let ((at-argl0 (list '1 '|=| 'unknown-value))
      (at-argl1 (list 'a '|=| 'unknown-value))
      (at-argl2 (list 'arg2 '|=| 'unknown-value)))
  (let ((at-das-atoms (list argl0 argl1 argl2)))
    
    (add-trace-do "funcname 1" '(command 1 a (+ b c)) at-das-atoms)
    
    (let* ((at-arg0 (let ((ret 1))
		      (set-car! (cddr at-argl0) ret)
		      ret))
	   (at-arg1 (let ((ret a))
		      (set-car! (cddr at-argl1) ret)
		      ret))
	   (at-arg2 (let ((ret (+ b c)))
		      (set-car! (cddr at-argl2) ret)
		      ret)))
      (command at-arg1 at-arg2 at-arg3))))


  
|#




(define (add-traces-do infostring expr)
  (if please-dont-trace
      expr
      (let loop ((expr expr))
	(schemecodeparser expr
			  ':symbolhandler		      
			  (list 'dont-trace
				(lambda (expr)
				  (cadr expr)))
			  ':elsefunc (lambda (expr)
				       (cond ((or (eq? 'or (car expr))
						  (eq? 'and (car expr))
						  (eq? 'if (car expr))
					      (eq? 'begin (car expr)))
					      (map loop expr))
					     ((eq? 'dont-trace2 (car expr))
					      (map loop (cadr expr)))
					     (else
					      (let ((info (string-append infostring " " (symbol->string (c-gensym "")))))
						`(at ,info ,(map loop expr))))))))))


				       
				     



(c-define-macro (add-traces infostring . expr)
  (if *do-trace?*
      (apply append (map (lambda (expr)
			   (add-traces-do infostring expr))
			 (map c-macroexpand expr)))
      expr))


(define-macro (c-macroexpand2 expr)
  `(c-macroexpand (eval (caddr (caddr (c-macroexpand ,expr))))))


;; body must be macroexpanded before calling.
(define (c-define-do definename def body)
  (let ((funcname (if (pair? def) (car def) def)))
    `(begin
       (reset-gensym)
       (cache-to-disk-do ',funcname
			 `(,',definename ,(quote ,def)
			    ,@(map (lambda (expr)
				     (add-traces-do (symbol->string ',funcname) expr))
				   (fix-defines-do
				    ',body)))))))

(c-define-macro (c-define def . body)
  (let ((funcname (if (pair? def) (car def) def)))
    `(begin
       (reset-gensym)
       ,@(map (lambda (func)	
		(c-define-do (car func) (cadr func) (cddr func)))
	      (lambda-lifter funcname (c-macroexpand `(define* ,def ,@body)))))))


(c-define-macro (c-define* def . body)
  (let ((funcname (if (pair? def) (car def) def)))
    `(begin
       (reset-gensym)
       ,@(map (lambda (func)	
		(c-define-do (car func) (cadr func) (cddr func)))
	      (lambda-lifter funcname (c-macroexpand `(define* ,def ,@body)))))))




;;(define-macro (c-define-macro def . body)
;;  (let ((newbody (add-traces body)))
;;    (hashtable/put! all-macros (car def) (list (cdr def) newbody))
;;    `(define-macro ,def ,@newbody)))


#|
(c-define (aiai2)
   (+ 2 3 ((lambda () 4))))

(c-define (aiai2)
	  (+ 2 3 (+ 4 5)))


(c-macroexpand '(c-define (aiai2)
		   (+ 2 3 ((lambda () 4)))))

(c-macroexpand '(add-traces (define (aiai)
			      (+ 2 3 ((lambda () 4))))))


(aiai2)
(bt)

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
