
#|
  Kjetil Matheussen, 2006.
    
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
For .emacs:

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(def-method\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 (cond ((match-beginning 1) font-lock-function-name-face)
	     ((match-beginning 2) font-lock-variable-name-face)
	     (t font-lock-type-face))
       nil t))))
(font-lock-add-keywords
 'scheme-mode
 '(("(\\(def-class\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face
       nil t))))
(font-lock-add-keywords
 'scheme-mode
 '(("(\\(c-import\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face
       nil t))))

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(def-var\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face
       nil t))))

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(def-constructor\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face
       nil t))))
(put 'letrec* 'scheme-indent-function 1)
(font-lock-add-keywords
 'scheme-mode
 '(("(\\(letrec[*]\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face
       nil t))))

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(new\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face
       nil t))))

|#


;; Todo:
;; * this->varname and this->methodname is inconvenient. varname and methodname is enough.
;; * Save expanded macro to file, and load it. Will (hopefully) solve debug problem. (done)
;; * define/def-var/def-method must be evaluated in order, and must be available from everywhere. (done)
;; * java-implement doesn't quite work. (done)



;;(c-define-macro (define* def . body)
;;  `(define ,def
;;     ,@body))
;;(c-define-macro (lambda* def . body)
;;  `(lambda ,def
;;     ,@body))

(c-define-macro (add-method2 nameandvars . body)
  `(dont-trace2 (add-method ',(car nameandvars) (lambda ,(cdr nameandvars) ,@body))))

(c-define-macro (add-method2* nameandvars . body)
  `(dont-trace2 (add-method ',(car nameandvars) (lambda* ,(cdr nameandvars) ,@body))))

(c-define-macro (def-method nameandvars . body)
  (if (and (list? nameandvars)
	   (or (member ':optional nameandvars)
	       (member ':rest nameandvars)
	       (member ':key nameandvars)))
      `(define ,(car nameandvars)
	 (add-method2* ,nameandvars ,@body))
      `(define ,(car nameandvars)
	 (add-method2 ,nameandvars ,@body))))

(c-define-macro (def-var name . initial)
  (if (null? initial)
      `(add-method ',name (lambda rest
			    (if (null? rest)
				,name
				(set! ,name (car rest)))))
      (let ((thisname name))
	(when (eq? name (car initial))
	      (c-display "WARNING! (def-var)" name "and" (car initial) "are equal.")
	      (error "gakk"))
	`(define ,thisname
	   (begin
	     (add-method2 (,name . rest) (if (null? rest) ,thisname (set! ,thisname (car rest))))
	     ,(car initial))))))

(c-define-macro (def-constructor nameandvars . body)
  (let* ((name (car nameandvars))
	(args (cdr nameandvars))
	(name2 (symbol-append 'constructor- name)))
    `(add-method2* ,(cons name2 args) ,@body)))

#|
(c-define (object? o)
  (and (procedure? o)
       (catch #t
	      (lambda ()
		(-> o instance? (-> o class-name)))
	      (lambda (key . args)
		#f))))
|#

(c-define-macro (instance? object class)
  `(-> ,object instance? ',class))

(c-define-macro (Super . rest)
  `(for-each add-super!
	     (list ,@rest)))


(c-define-macro (-> object method . args)
  (if (number? object)
      `(list-set! ,method ,object ,(car args))
      `(,object ',method ,@args)))

(c-define-macro (<- object method)
  (if (number? object)
      `(list-ref ,method ,object)
      `(-> ,object get-method ',method)))

(c-define-macro (=> . rest)
  `(-> this ,@rest))

(c-define (def-class-do def . body)

  (define newvars '())
  (define newbody '())
  
  (for-each (lambda (a)
	      (if (and (pair? a)
		       (eq? (car a) 'def-constructor))
		  (let* ((name (caadr a))
			 (constructor-name (symbol-append 'constructor- name))
			 (classname (symbol->string (car def)))
			 (reversedclassnameaslist (reverse (string->list classname)))
			 (funcname (if (member (car reversedclassnameaslist) '(#\> #\) #\] #\}))
				       (symbol-append (apply symbol (reverse (cdr reversedclassnameaslist)))
						      '/
						      name
						      (symbol (car reversedclassnameaslist)))
				       (symbol-append (car def) '/ name))))
		    (define-toplevel funcname
		      (lambda args
			(let ((classfunc (eval-string classname)))
			  (define-toplevel funcname
			    (lambda args
			      (apply (-> (classfunc) get-method constructor-name) args)))
			  (apply (-> (classfunc) get-method constructor-name) args)))))))
	    body)
  
  (set! newbody (map (lambda (t)
		       (cond ((not (pair? t)) t)
			     ((or (eq? (car t) 'define)
				  (eq? (car t) 'define*)
				  (eq? (car t) 'define-dontlift))
			      (if (pair? (cadr t))
				  (set! newvars (cons (car (cadr t)) newvars))
				  (set! newvars (cons (cadr t) newvars)))
			      (if (pair? (cadr t))
				  (cond ((eq? (car t) 'define*)
					 `(set! ,(car (cadr t)) (lambda-lift ,(car (cadr t))
									     (lambda* ,(cdr (cadr t))
										      ,@(cddr t)))))
					((eq? (car t) 'define)
					 `(set! ,(car (cadr t)) (lambda-lift ,(car (cadr t))
									     (lambda ,(cdr (cadr t))
									       ,@(cddr t)))))
					(else
					 `(set! ,(car (cadr t)) (lambda* ,(cdr (cadr t))
									 ,@(cddr t)))))
			      
				  `(set! ,(cadr t) ,(caddr t))))
			     ((eq? (car t) 'define-method)
			      (c-display "Error! define-method can not be used in def-class. Perhaps you ment def-method?"))
			     ((eq? (car t) 'def-method)
			      (let* ((nameandvars (cadr t))
				     (body (cddr t))
				     (defname (car nameandvars)))
				(set! newvars (cons defname newvars))
				(if (and (list? nameandvars) ;; Yes: "list?", not "pair?".
					 (or (member ':optional nameandvars)
					     (member ':rest nameandvars)
					     (member ':key nameandvars)))
				    `(dont-trace2 (set! ,defname (dont-trace2 (add-method ',(car nameandvars)
											  (lambda-lift ,(car nameandvars)
												       (lambda* ,(cdr nameandvars)
														,@body))))))
				    `(dont-trace2 (set! ,defname (dont-trace2 (add-method ',(car nameandvars)
											  (lambda-lift ,(car nameandvars)
												       (lambda ,(cdr nameandvars)
													 ,@body)))))))))
			     ((eq? (car t) 'def-var)
			      (if (= 2 (length t))
				  `(add-method ',(cadr t) (lambda rest
							    (if (null? rest)
								,(cadr t)
								(set! ,(cadr t) (car rest)))))
				  (let* ((name (cadr t))
					 (initial (caddr t))
					 (thisname name))
				    (when (eq? name initial)
					  (c-display "WARNING! (def-var) " name "and" initial "are equal.")
					  (error "gakk"))
				    (set! newvars (cons thisname newvars))
				    `(begin
				       (add-method2 (,name . rest) (if (null? rest) ,thisname (set! ,thisname (car rest))))
				       (set! ,thisname ,initial)))))
			     ((eq? (car t) 'def-constructor)
			      (let* ((nameandvars (cadr t))
				     (body (cddr t))
				     (name (car nameandvars))
				     (args (cdr nameandvars))
				     (name2 (symbol-append 'constructor- name)))
				`(add-method2* ,(cons name2 args) ,@body)))
			     (else
			      t)))
		     body))

  
  ;;(c-display "newvars" newvars)
  ;;(c-display "newbody" newbody)
  ;;(c-display "def" def)
  ;;(c-display "cardef" (car def))
  ;;(c-display "newvars" newvars)
  ;;(c-display (map (lambda (var)
  ;;		    (list var #f))
  ;;		  (reverse! newvars)))
  ;; (c-display newbody)

  `(c-define* ,def
     (let* ((methods (dont-trace (make-hash-table 251)))
	    (supers '())
	    (super (lambda args (c-display "\n\nError! \"super\" is not a method. Perhaps you ment \"Super\"?\n\n"))) ;; super is later redefined.
	    (this (lambda (name . rest)
		    (dont-trace
		     (apply (or (hashq-ref methods name)
				(any (lambda (super) (-> super get-method name))
				     supers)
				(error (format #t "No such method: \"~A\" in class \"~A\".~%" name ',(car def)))
				(lambda x (error (format #t "No such method: \"~A\" in class \"~A\".~%" name ',(car def)))))
			    rest))))
	     (add-super! (lambda (asuper)
			   (dont-trace
			    (begin
			      (if (null? supers)
				  (set! super asuper))
			      (set! supers (append supers (list asuper)))
			      (-> asuper add-subobject this)))))
	     (subobjects '())
	     (method-properties (dont-trace (make-hash-table 251)))
	    (add-method-from-superclass (lambda-lift add-method-from-superclass
						     (lambda (name func level super)
						       (let* ((addit (lambda ()
								       (hashq-set! methods name func)
								       (hashq-set! method-properties name (list super level))
								       (for-each (lambda (subobject)
										   (-> subobject add-method-from-superclass name func (1+ level) this))
										 subobjects)))
							      (find-super-num (lambda (super)
										(length (memq super (reverse supers)))))
							      (oldmethod (hashq-ref methods name))
							      (oldproperties (hashq-ref method-properties name))
							      (oldsuper (and oldproperties (car oldproperties)))
							      (oldlevel (and oldproperties (cadr oldproperties))))
							 (if (and oldmethod
								  (not oldproperties))
							     (c-display "Error in oo.scm/def-class/add-method-from-superclass. oldproperties is #f in class:" ',def))
							 (if (or (not oldmethod)
								 (= 0 level)
								 (< level oldlevel)
								 (eq? super oldsuper)
								 (and (= level oldlevel)
								      (< (find-super-num super) (find-super-num oldsuper))))
							     (addit))
							 func))))
	    (add-method (lambda (name func)
			  (add-method-from-superclass name func 0 this)))

	    (dispatch-preds #f)
	    (dispatch-funcs #f)
	    (add-dispatcher (lambda (pred func)
			      (dont-trace
			       (cond ((not dispatch-preds)
				      (set! dispatch-preds pred)
				      (set! dispatch-funcs func))
				     ((procedure? dispatch-preds)
				      (set! dispatch-preds (list dispatch-preds pred))
				      (set! dispatch-funcs (list dispatch-funcs func)))
				     (else
				      (set! dispatch-preds (append dispatch-preds (list pred)))
				      (set! dispatch-funcs (append dispatch-funcs (list func))))))))
	    
	    ,@(map (lambda (var)
		     (list var #f))
		   (reverse! (delete-duplicates newvars))))

       (def-var class-name ',(car def))
       
       (def-method (get-super num)
	 (list-ref supers num))
       
       (def-method (add-super asuper)
	 (add-super! asuper))
       
       (def-method (add-subobject object)
	 (push! object subobjects))
       
       (def-method (dir)
	 (dont-trace
	  (append (cons ',(car def)
			(map car (hashtable->alist methods)))
		  (map (lambda (super) (-> super dir))
		       supers))))
       
       (def-method (get-method name)
;;;	 (hashq-ref methods name))
	 (dont-trace
	  (or (hashq-ref methods name)
	      (any (lambda (super) (-> super get-method name))
		   supers))))
       
       (def-method (get-methods)
	 (hashtable->alist methods))
       
       (def-method (instance? class-name)
	 (or (eq? class-name ',(car def))
	     (any (lambda (super) (-> super instance? class-name))
		  supers)))
       
       (define (this-with-custom-dispatchers m . rest)
	 (dont-trace
	  (call-with-current-continuation
	   (lambda (return)
	     (for-each (lambda (pred func)
			 (if (pred m rest)
			     (return (func m rest))))
		       dispatch-preds
		       dispatch-funcs)
	     (apply (or (hashq-ref methods m)
			(any (lambda (super) (-> super get-method m))
			     supers)
			(lambda x (error (format #t "No such method: \"~A\" in class \"~A\".~%" m class-name))))
		    rest)))))
       
       (define (this-with-custom-dispatcher m . rest)
	 (dont-trace
	  (if (dispatch-preds m rest)
	      (dispatch-funcs m rest)
	      (apply (or (hashq-ref methods m)
			 (any (lambda (super) (-> super get-method m))
			      supers)
			 (lambda x (error (format #t "No such method: \"~A\" in class \"~A\".~%" m class-name))))
		     rest))))

       (add-method 'add-method add-method)
       (add-method 'add-method-from-superclass add-method-from-superclass)

       ,@newbody

       (for-each (lambda (super)
		   (for-each (lambda (method)
			       (if (not (hashq-ref methods (car method)))
				   (add-method (car method) (cdr method))))
			     (-> super get-methods)))
		 supers)

       (if (and this dispatch-preds)
	   (if (procedure? dispatch-preds)
	       (set! this this-with-custom-dispatcher)
	       (set! this this-with-custom-dispatchers)))
       
       this)))


(c-define-macro (def-class def . body)
  (apply def-class-do (cons def body)))


#|
(c-define (def-class3 def . body)
  (add-traces
   (fix-defines
    (apply def-class2 (cons def body)))))

(c-define-macro (def-class def . body)
  (cache-to-disk-do (symbol-append (car def) '|-class|)
		    (apply def-class3 (cons def body))))

|#




#|
(c-macroexpand '(def-class (<gakk>)
		  (def-method (iaia) 20)
		  (def-method (aiai) (this->iaia))))

(def-class (<gakk>)
  (def-method (iaia) 20)
  (def-method (aiai) (this->iaia)))
(define a (<gakk>))
(-> a aiai)
(-> a dir)

(def-class (gakk)
  (def-var g 2)
  (def-method (gg)
    this->g))

(define g (gakk))
(-> g gg)
(-> g dir)
|#


#|
(def-class (gakk)
  (define g 2)
  (define h (+ g 2))
  (c-display "ai")
  (def-var a 5)
  (c-display "h:" h))


(source-annotations '())

(begin gakk)
(define g (gakk))
(trace (gakk))

(-> g a)
(-> g add-method 'tja (lambda (c)
			 90))
(-> g dir)
(-> g tja 2)

|#


#|
(def-class (<gakk>)
  (define a #f)
  (define b #f)
  (set! a 5)
  (set! b a))
  (define a 5)
  (define b a)
  )
(define (<gakk>)
  (define a 5)
  (define b a)
  b)

(def-class (<super0>)
  (def-method (superadd)
    "super0-superadd"))

(def-class (<super1> sum) (Super (<super0>)))
  (def-var avar 2)
  (def-method (super1)
    (display "super1 sum: ")(display sum)
    (newline)))

(def-class (<super2> sum)
  (def-method (super2)
    (display "super2 sum: ")(display sum)
    (newline)))

(def-class (<bank> sum) (Super (<super1> (+ 1000 sum)) (<super2> (+ 2000 sum)))
  (def-method (print-sum)
    (display sum)(newline))
  (def-method (deposit x)
    (set! sum (+ sum x))
    (this->print-sum))
  (def-method (withdraw x)
    (set! sum (- sum x))
    (this->print-sum)))

  

(define b (<bank> 5))

(-> b superadd) -> "super0-superadd"

(-> (-> b get-super 1) add-method 'superadd (lambda ()
					      3.13))
(-> b superadd) -> 3.13



(-> (-> b get-super 1) add-method 'superadd (lambda ()
					      3.14))
(-> b superadd) -> 3.14




(-> (-> b get-super 0) add-method 'superadd (lambda ()
					      3.15))
(-> b superadd) -> 3.15



(-> (-> b get-super 0) add-method 'superadd (lambda ()
					      3.16))
(-> b superadd) -> 3.16



(-> (-> b get-super 1) add-method 'superadd (lambda ()
					      3.17)))
(-> b superadd) -> 3.16



(-> b deposit 3)
(-> b withdraw 6)
(define b->withdraw (<- b withdraw))
(begin b->withdraw)
(b->withdraw 7)
(-> b class-name)
(-> b super1)
(-> b super2)
(-> b avar)
(-> b avar 5)
(-> b avar)
(instance? b <bank>)
(instance? b <super1>)
(instance? b <super2>)
(instance? b <someother-class>)
(-> b dir)
(-> b not-a-method)
|#


#|

;;##############################################################
;; A hook class.
;;##############################################################
(def-class (<hook>)
  (define funcs '())
  (define system-funcs '())
  (define steelfunc #f)
  (def-method (add! func)
    (set! funcs (cons func funcs)))
  (def-method (add-system! func)
    (set! system-funcs (cons func system-funcs)))
  (def-method (only! func)
    (set! steelfunc func))
  (def-method (not-only!)
    (set! steelfunc #f))
  (def-method (remove! func)
    (set! funcs (remove! (lambda (f) (eq? f func))
			 funcs)))
  (def-method (run . args)
    (if steelfunc
	(apply steelfunc args)
	(call-with-current-continuation
	 (lambda (return)
	   (for-each (lambda (func)
		       (if (eq? 'stop! (apply func args))
			   (return 'stop!)))
		     (append system-funcs funcs)))))))




|#


