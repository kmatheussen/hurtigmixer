
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



(require-library 'sisc/libs/srfi/srfi-22)
(import srfi-22)

(define (main arguments)
  (c-display "arguments" arguments))

(load "various.scm")


(define (fix-macros-as-functions-do expr)
  (schemecodeparser expr
		    ':elsefunc
		    (lambda (expr)
		      `(,(car expr) ,@(map (lambda (expr)
					     (if (assq expr (hash-table->alist all-macros))
						 (let ((args (c-gensym)))
						   `(lambda ,args
						      (if (procedure? ,expr)
							  (apply expr args)
							  (case (length ,args)
							    ,@(map (lambda (num-args)
								     `((,num-args) (,expr ,@(map (lambda (argnum)
												   `(list-ref ,args ,argnum))
												 (iota num-args)))))
								   (iota 30))))))
						 expr))
					   (cdr expr))))))

#|
(c-define-macro (add . rest)
  `(+ ,@rest))

(fix-macros-as-functions-do '(apply add '(1 2)))

  (let ((list '(1 2)))
    (apply add list)
  => 3

(eval (fix-macros-as-functions-do '(map add '(1 2) '(3 4))))
  => '(4 6)


|#


(define (d-macroexpand expr)
  (schemecodeparser expr
		    ':elsefunc (lambda (expr)
				 (if (or (not (pair? expr))
					 (null? expr)
					 (not (symbol? (car expr)))
					 (eq? 'define-toplevel (car expr)))
				     expr
				     (let ((topexpand (c-macroexpand-1 expr)))
				       ;;(c-display "expr/topexpand" expr topexpand)
				       (if (eq? expr topexpand)
					   (fix-macros-as-functions-do expr)
					   (d-macroexpand topexpand)))))))


(define (handle-macros expr)
  (let loop ((expr expr))
    (schemecodeparser expr
		      ':elsefunc (lambda (expr)
				   (let ((topexpand (c-macroexpand-1 expr)))
				     ;;(c-display "expr/topexpand" expr topexpand)
				     (if (eq? expr topexpand)
					 (fix-macros-as-functions-do expr)
					 (loop topexpand)))))))
  

(define (add-macros-do expr)
  (schemecodeparser expr
		    ':symbolhandler (list 'define-macro
					  (lambda (expr)
					    ;;(c-display "got macro" (cadr expr))
					    (let ((def (cadr expr))
						  (body (cddr expr)))
					      (hashtable/put! all-macros (car def) (list (cdr def) body))
					      `(hashq-set! all-macros ',(car def) ',(list (cdr def) body)))))))

(define (handle-includes expr)
  (schemecodeparser expr
		    ':blockhandler		      
		     (lambda (exprs)
		       (let ((ret '()))
			 (for-each (lambda (expr)
				     (if (and (pair? expr)
					      (eq? 'include (car expr)))
					 (for-each (lambda (expr)
						     (push! (handle-includes expr) ret))
						   (sourcefile->list (cadr expr)))
					 (push! (handle-includes expr) ret)))
				   exprs)
			 (reverse! ret)))))
		     

#|
(let ((a 1)
      (b 2))
  (+ a b))
->
(let* ((,a 1)
       (,b 2))
  (let ((a ,a)
	(b ,b))
    (+ a b)))
|#

(define (fix-let-order-do expr)
  (schemecodeparser expr
		    ':use-customsymbolhandler? (lambda (expr)
						 (and (eq? 'let (car expr))
						      (pair? (cadr expr))
						      (>= (length (cadr expr)) 2)
						      (any pair? (map cadr (cadr expr)))))
		    ':customsymbolhandler
		    (lambda (expr)
		      (let ((varnames (map c-gensym (cadr expr))))
			`(let* ,(map list
				     varnames
				     (map fix-let-order-do
					  (map cadr (cadr expr))))
			   (let ,(map list (map car (cadr expr)) varnames)
			     ,@(fix-let-order-do (cddr expr))))))))

#|
(fix-let-order-do '(let ((a (+ 1 3))
			 (b 2))
		     (+ a b)))
(fix-let-order-do '(let ((a 1))
		     (let ((b 2))
		       (+ a b))))

|#


;; Make letrec a macro to ensure correct evaluation order
#|
(letrec ((a (lambda ()
	      b))
	 (b 2))
  (+ (a) b))
->
(let ((a #f)
      (b #f))
  (set! a (lambda ()
	    b))
  (set! b 2)
  (+ (a) b))
|#

(hashtable/put! all-macros 'letrec2
		(list '(vars . body)
		      (list '`(let ,(map (lambda (var)
					   `(list ,(car var) #f)) 
					 vars)
				,@(map (lambda (var)
					 `(set! ,(car var) ,(cadr var))) 
				       vars)
				,@body))))

#|
(d-macroexpand '(letrec2 ((a (lambda ()
			      b))
			 (b 2))
		  (+ (a) b)))
|#


(define (fix-evaluation-order-do expr)
  (schemecodeparser expr
		    ':elsefunc (lambda (expr)
				 (cond ((or (eq? 'or (car expr))
					    (eq? 'and (car expr))
					    (eq? 'if (car expr))
					    (eq? 'begin (car expr))
					    (eq? 'set! (car expr))
					    (>= (length expr) 2)
					    (null? (cdr expr))
					    (not (any pair? (cdr expr))))
					(map fix-evaluation-order-do expr))
				       (else
					(let ((varnames (map c-gensym (cdr expr))))
					`(let* ,(map list
						     varnames
						     (map fix-evaluation-order-do (cdr expr)))
					   (,(car expr) ,@varnames))))))))
		      



#|
(let ((a 1))
  ,'a)
->
(let ((a 1))
  (begin
    (define-macro (tempmacro)
      (quote a))
    (tempmacro)))


(let ((a 1)
      (b 2))
  ,@(map (lambda (val)
	   `(begin
	      (display (string-append (symbol->string ',val) "=" (number->string ,val)))
	      (newline)))
	 '(a b)))
->
(let ((a 1)
      (b 2))
  (begin
    (define-macro (tempmacro)
      `(begin ,@(map (lambda (val)
		       `(begin
			  (display (string-append (symbol->string ',val) "=" (number->string ,val)))
			  (newline)))
		     '(a b))))
    (tempmacro)))
|#

(define tempmacro (gensym))

(define (fix-unquote-do expr)
  (schemecodeparser expr
		    ':symbolhandler
		    (list 'unquote
			  (lambda (expr)
			    ;;(c-display "cadr" (cadr expr))
			    (hashtable/put! all-macros tempmacro
					    (list '()
						  (list (cadr expr))))
			    (fix-unquote-do (d-macroexpand `(,tempmacro)))))))

#|
(fix-unquote-do '(let ((a 1))
		   ,'a))

(fix-unquote-do '(let ()
		   ,(+ 8 5)))
|#

(define (fix-unquote-splicing-do expr)
  (schemecodeparser expr
		    ':symbolhandler
		    (list 'unquote-splicing
			  (lambda (expr)
			    ;;(c-display "cadr" (cadr expr))
			    (hashtable/put! all-macros tempmacro
					    (list '()
						  (list (cadr expr))))
			    (fix-unquote-splicing-do `(begin 
							,@(d-macroexpand `(,tempmacro))))))))

#|
(fix-unquote-splicing-do '(let ((a 1)
			       (b 2))
			   ,@(map (lambda (val)
				    `(begin
				       (display (string-append (symbol->string ',val) "=" (number->string ,val)))
				       (newline)))
				  '(a b))))
|#


(define (fix-quasiquote-order-do expr)
  (schemecodeparser expr
		    ':use-customsymbolhandler? (lambda (expr)
						 (eq? 'quasiquote (car expr)))
		    ':customsymbolhandler
		    (lambda (expr)
		      (letrec* ((unquotes '())
				(parser (lambda (expr)
					  ;;(c-display "expr" expr)
					  (cond ((and (pair? expr)
						      (eq? 'unquote (car expr)))
						 (let ((name (c-gensym)))
						   ;;(c-display "got something:" expr (cadr expr))
						   (push! (list name (fix-quasiquote-order-do (cadr expr))) unquotes)
						   (list 'unquote name)))
						((and (pair? expr)
						      (eq? 'unquote-splicing (car expr)))
						 (let ((name (c-gensym)))
						   ;;(c-display "got something2:" expr (cadr expr))
						   (push! (list name (fix-quasiquote-order-do (cadr expr))) unquotes)
						   (list 'unquote-splicing name)))
						((pair? expr)
						 ;;(c-display "yes, pair:" expr)
						 ;;(c-display "car/cadr" (car expr) (cadr expr))
						 (map parser expr))
						
						(else
						 expr))))
				(newexpr (map parser expr)))
			;;(c-display "unquotes:" unquotes)
			(if (null? unquotes)
			    expr
			    `(let ,(reverse! unquotes)
			       ,newexpr))))))
  


#|
(fix-quasiquote-order-do '(let ((a 1))
			    `(,(begin
				 (set! a 2)
				 3)
			      ,a)))

(fix-quasiquote-order-do '`(a b ,(+ 2 3) ,@(list 'c 'd) ((((e)))) f g))

->
(let ((gen-1 (+ 2 3))
      (gen-2 (list 'c 'd)))
  `(a b ,gen1 ,@gen2 e f g))

`(a b ((((,(+ d 5))))) e)
->
(let ((gen-1 (+ d 5)))
  `(a b ((((,gen-1)))) e))

|#




(define (fix-define-toplevel-do expr)
  ;;(c-display "fix-dtd" expr)
  (let* ((toplevels '())
	 (body (let loop ((expr expr))
		 (schemecodeparser expr
				   ':symbolhandler
				   (list 'define-toplevel
					 (lambda (expr)
					   ;;(c-display "im here" expr)
					   (if (pair? (cadr expr))
					       (set! expr `(define-toplevel ,(car (cadr expr))
							     (lambda ,(cdr (cadr expr))
							       ,@(cddr expr)))))
					   (let ((name (cadr expr))
						 (func (caddr expr)))
					     (push! name toplevels)
					     (map loop `(set! ,name ,func)))))))))
    ;;(c-display "ai")
    (if (null? toplevels)
	body
	`(begin ,@(map (lambda (name)
			 `(define ,name #f))
		       toplevels)
		,body))))

#|
(fix-define-toplevel-do '(let ((a* 1))
			   (define-toplevel (a)
			     a*)))

(fix-defines-do (fix-define-toplevel-do '(let ((a* 1))
					   (define-toplevel (a)
					     a*))))


  (let ((a* 1))
    (define-toplevel (a)
      a*))
  (a)
  => 1

  (let ((a* 1)
	(funcname 'a))
    (define-toplevel (,funcname)
      a*))
  (a)
  => 1
|#



(define (fix-carandcdr-do expr)
  (schemecodeparser expr
		    ':elsefunc
		    (lambda (expr)
		      ;;(c-display "fix-cd expr" expr)
		      (if (or (eq? 'car (car expr))
			      (eq? 'cdr (car expr)))
			  (if (pair? (cadr expr))
			      (let ((val (c-gensym)))
				`(let ((,val ,(cadr expr)))
				   (if (null? ,val)
				       '()
				       (,(car expr) ,val))))
			      `(if (null? ,(cadr expr))
				   '()
				   (,(car expr) ,(cadr expr))))
			  (map fix-carandcdr-do expr)))))



(define (fix-keywords-do expr)
  (schemecodeparser expr
		    ':elsefunc
		    (lambda (expr)
		      `(,(car expr) ,@(map (lambda (expr)
					     ;;(c-display "fix-kd expr" expr (symbol? expr)
						;;;	(and (symbol? expr) (char=? #\: (car (string->list (symbol->string expr))))))
					     (if (and (symbol? expr)
						      (char=? #\: (car (string->list (symbol->string expr)))))
						 `(quote ,expr)
						 (fix-keywords-do expr)))
					   (cdr expr))))))

(define (generate-r7rs-code sourcefile)
  (map (lambda (expr)
	 (fix-carandcdr-do
	  (fix-evaluation-order-do
	   (fix-let-order-do
	    (fix-defines-do
	     (fix-unquote-splicing-do
	      (fix-unquote-do
	       (fix-keywords-do
		(handle-includes
		 (handle-macros
		  (fix-quasiquote-order-do
		   (handle-includes
		    (fix-define-toplevel-do
		     (add-macros-do
		      expr))))))))))))))
       (sourcefile->list sourcefile)))

#|
(generate-r7rs-code "r7rstest.scm")
(assq 'c '((a 1)( b 2)))
|#


(define (gen-r7rs-code infile outfile)
  (let ((code (append (generate-r7rs-code "r7rs_library.scm")
		      (generate-r7rs-code infile))))
    (with-output-to-file outfile
      (lambda ()
	(newline)
	(for-each (lambda (expr)
		    (pretty-print expr)
		    (newline)
		    (newline))
		  code)))))

#|
(gen-r7rs-code "r7rstest.scm" "r7rsgentest.scm")
|#
