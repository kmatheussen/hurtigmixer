
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



#|
(load "various.scm")
(c-import oo)
|#


;; (find-setted-variables '(a d i e f) '((set! a b) (set! a 9) (let ((e 2)) (set! e 9)) ((((set! d ((((set! f 50) (set! g 90)))))))))
;; -> '(a d f) 
;; Not fully optimized, but completely safe. Doing the optimizing will make "find-setted-variables" use more time, so maybe its best to let it be like it is.
(define (find-setted-variables varlist expr)
  (let ((ret '()))
    (let loop ((expr expr))
      ;;(c-display "expr" expr)
      (if (list? expr)
	  (begin
	    (if (and (= (length expr) 3)
		     (eq? 'set! (car expr))
		     (member (cadr expr) varlist))
		(push! (cadr expr) ret))
	    (for-each loop expr))))
    (sort (delete-duplicates! ret)
	  (lambda (a b)
	    (string>? (symbol->string a) (symbol->string b))))))

#|
(find-setted-variables '(a d i e f) '((set! a b) (set! a 9) (let ((e 2)) (set! e 9)) ((((set! d ((((set! f 50) (set! g 90))))))))))
;; -> (a e d f)
|#

(define (find-setted-variables varlist expr)
  (let ((ret '()))
    (let loop ((expr expr))
      ;;(c-display "expr" expr)
      (if (list? expr)
	  (begin
	    (if (and (= (length expr) 3)
		     (eq? 'set! (car expr))
		     (member (cadr expr) varlist))
		(push! (cadr expr) ret))
	    (for-each loop expr))))
    (if (null? ret)
	ret
	(sort varlist (lambda (a b)
			(string>? (symbol->string a) (symbol->string b)))))))



;; Same situation here as in "find-setted-variables" regarding optimization.
(define (find-referenced-variables varlist expr)  
  (let ((ret '()))
    (let loop ((expr expr))
      ;;(c-display "expr" expr)
      (if (list? expr)
	  (for-each loop expr)
	  (if (and (symbol? expr)
		   (member expr varlist))
	      (push! expr ret))))
    (sort (delete-duplicates! ret)
	  (lambda (a b)
	    (string>? (symbol->string a) (symbol->string b))))))
#|
(find-referenced-variables '(a d i e f t) '((set! a b) (+ t y) (let ((e 2)) (set! e 9)) ((((set! d ((((set! f 50) (set! g 90))))))))))
;; -> (a t e d f)
|#

(define (find-referenced-variables varlist expr)  
  (sort (delete-duplicates! varlist)
	(lambda (a b)
	  (string>? (symbol->string a) (symbol->string b)))))


(define *do-lambda-lift* #f)

(define-macro (lambda-lifter basename expr)
  expr)


;; Returns a list of functions based on the contents of expr.
;; See examples after the function definition.
;;
;; Warning, the function does not check if it is actually legal to lift the lambda!
;; (For example, if there is another lambda inside the lambda thats referencing to a variable outside the outer lambda which changes its value, this will not work)
(define (lambda-lifter basename expr)

  (if (not *do-lambda-lift*)
      (list (schemecodeparser expr
			      ':symbolhandler
			      (list 'lambda-lift
				    (lambda (expr)
				      (caddr expr)))))
      (begin
	(let* ((newfuncs '())
	       (newexpr (let loop ((expr expr))
			  (schemecodeparser expr
					    ':symbolhandler
					    (list 'lambda-lift
						  (lambda (expr)
						    (let* ((varlist (schemecodeparser-get-varlist))
							   (funcname (if (char=? #\> (last (string->list (symbol->string basename))))
									 (symbol-append (string->symbol (list->string (c-butlast (string->list (symbol->string basename)))))
											'|.| (cadr expr) '|>|)
									 (symbol-append basename '|.| (cadr expr))))
							   (lambda-func (if (eq? 'lambda (car (caddr expr)))
								     (caddr expr)
								     (car (cdaadr (caddr expr))))) ;; The lambda* macro sometimes put the lambda inside a let.
							   (lambda-args (cadr lambda-func))
							   (lambda-body (loop (cddr lambda-func)))
							   (referenced-variables (find-referenced-variables varlist lambda-func))
							   (setted-variables (find-setted-variables varlist lambda-func))
							   (setted-variables-newnames (map (lambda (varname)
											     (symbol-append 'll-new- varname))
											   setted-variables))
							   (define-args (delete-duplicates
									 (let ((list (if (proper-list? lambda-args)
											 (append referenced-variables lambda-args)
											 (if (pair? lambda-args)
											     (append referenced-variables
												     (drop-right lambda-args 0)
												     (list (cdr (last-pair lambda-args))))
											     (append referenced-variables (list lambda-args))))))
									   ;;(c-display "list" list lambda-args)
									   list)))
							   (funccaller `(,funcname ,@define-args)))
						      ;;(c-display "varlist" varlist)
						      (if (null? setted-variables)
							  (begin
							    (push! `(define (,funcname ,@define-args)
								      ,@lambda-body)
								   newfuncs)
							    `(lambda ,lambda-args
							       (dont-trace
								,funccaller)))
							  (begin
							    (push! `(define (,funcname ,@define-args)
								      (let ((ll-ret (begin
										      ,@lambda-body)))
									(values ,@setted-variables
										ll-ret)))
								   newfuncs)
							    `(lambda ,lambda-args
							       (dont-trace
								(call-with-values (lambda () ,funccaller)
								  (lambda (,@setted-variables-newnames ll-ret)
								    ,@(map (lambda (varname newname)
									     `(set! ,varname ,newname))
									   setted-variables
									   setted-variables-newnames)
								    ll-ret)))))))))))))
	  (append (reverse! newfuncs) (list newexpr))))))




#|


(c-define* (ai :optional (b 5))
  (let ((c (lambda-lift c-lifted
			(lambda* (d :key (rest 5))
				 (+ 2 b d rest)))))
    (c 3)))



(begin (ai 3))


(lambda-lifter 'ai (c-macroexpand '(c-define* (ai :optional (b 5))
				     (let ((c (lambda-lift c-lifted
							   (lambda* (d :key (rest 5))
								    (+ 2 b d rest)))))
				       (c 3)))))

(lambda-lifter '<ai> (c-macroexpand '(define (<ai> . b)
				     (lambda-lift c-lifted
						  (lambda (d . rest)
						    (+ 2 b d (car rest)))))))


(car (cdaadr '(let ((lambda*:L3 (lambda (d . lambda*:G4)
			  (let ()
			    (let* ((ra->kbl rest-arg->keyword-binding-list)
				   (kb:G5 (ra->kbl lambda*:G4 (quote (:rest)) #f)))
			(let* ((rest (cond ((assq (quote rest) kb:G5) => cdr)
					   (else 5))))
			  (let ()
			    (+ 2 b d rest))))))))
	lambda*:L3))
)


(c-macroexpand '(lambda* (:key (rest 5))
		 rest))


(begin
  `(begin
     ,@(map (lambda (func)	
	      (c-define-do (car func) (cadr func) (cddr func)))
	    (lambda-lifter (c-macroexpand '(define (a b)
					     (let ((c (lambda-lift c-lifted
								   (lambda (d)
								     (+ 2 b d)))))
					       (c 3))))))))


(xcons '(a b d e) 'c)

(drop-right '(a b c . d) 0)
(last-pair '(a b c . d))

(let ((args '(a b c . d)))
  `(apply func (append (list ,@(drop-right args 0)) ,(cdr (last-pair args)))))

(pair? '(a . b))

(let ((args 'a))
  `(apply func (append (list ,@(drop-right args 0)) ,(cdr (last-pair args)))))

->
(append (list a b c) d)
(dotted-list? '(a . b))



Note, rest argument is not handled!

(lambda-lifter '(define (<a> b)
		  (let ((c (lambda-lift c-lifted
					(lambda (d)
					  (+ 2 b d)))))
		    (c 3))))
->
((define (c-lifted b d)
   (+ 2 b d))
 (define (a b)
   (let ((c (lambda (d)
	      (c-lifted b d))))
     (c 3))))

(c-define (a b)
  ( (lambda-lift c-lifted (lambda (d)
			    (set! b d)))
    3))
->
((c-define (c-lifted b d)
   (let ((ll-ret (begin
		   (set! b d))))
     (values b
	     ll-ret)))
 (c-define (a b)
   ( (lambda (d)
       (call-with-values (lambda () (c-lifted b d))
	 (lambda (ll-new-b ll-ret)
	   (set! b ll-new-b)
	   ll-ret)))
     3)))
|#





#|
(lambda-lifter-do '(define (ai d)
		     (lambda (a)
		       (let* ((a 5))
			 (+ d a 2)))))

(lambda-lifter-do (c-macroexpand '(def-class (<bank> a)
				    (def-method (aiai b)
				      (+ a b c)))))

(c-macroexpand2 '(def-class (<bank> a)
		   (def-method (aiai b)
		     (+ a b c))))


(c-macroexp				   

(append (cons 1 2) '(3 4 5))

(define (append-varlists varlist1 varlist2)
  (append (let append ((varlist varlist1))
	    (cond ((null? varlist) varlist)
		  ((not (pair? varlist)) (list varlist))
		  (else (cons (car varlist)
			      (append (cdr varlist))))))
	  varlist2))
		       
(append-varlists 'c '(d e f))

(not (pair? '()))
(null? 'a)
|#



