
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



(import s2j)

;;(c-import sort)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Implement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macro (java-implement class . callbacks)
  (let ((funcname (gensym))
	(callbacknames (map (lambda (n) (gensym)) callbacks))
	(funcnames (map (lambda (n) (gensym)) callbacks)))
    (eval `(define-java-classes ,class))
    (eval `(define-java-proxy (,funcname ,@callbacknames)
	     (,class)
	     ,@(map (lambda (callback callbackname)
		      `(define ,(symbol-append '|.| (car callback)) ,callbackname))
		    callbacks
		    callbacknames)))
    `(let (,@(map list funcnames (map cadr callbacks)))
       (,funcname ,@(map (lambda (funcname)
			   (let ((argsname (gensym)))
			     `(lambda ,argsname
				(apply ,funcname (cdr ,argsname)))))
			 funcnames)))))


#|
(java-implement <java.awt.event.MouseMotionListener>
		(mouse-dragged (lambda (e)
				 (c-display e)))
		(mouse-moved (lambda (e)
			       (c-display e))))
|#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Various handy java access functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-jmethod jclass name)
  (let* ((methods (java-class-declared-methods jclass))
	 (names (map java-method-name methods)))
    (cadr (assq name (map list names methods)))))


(define java.lang.Object->getClass #f)

(define (set-java.lang.Object->getClass!)
  (let ((jmethod (get-jmethod (java-class 'java.lang.Object) 'getClass)))
    (set! java.lang.Object->getClass (java-method-procedure jmethod))))

(set-java.lang.Object->getClass!)


(define java.lang.reflect.Method.getReturnType #f)

(define (set-java.lang.reflect.Method.getReturnType!)
  (let ((jmethod (get-jmethod (java-class 'java.lang.reflect.Method) 'getReturnType)))
    (set! java.lang.reflect.Method.getReturnType (java-method-procedure jmethod))))

(set-java.lang.reflect.Method.getReturnType!)



#|
(java.lang.reflect.Method.getReturnType (get-jmethod (java-class 'java.lang.reflect.Method ) 'getReturnType))

(java.lang.Object->getClass jo)
|#




(define (java-instance? object jclass)
  (let loop ((object-class (java.lang.Object->getClass object)))
    (cond ((null? object-class)
	   #f)
	  ((equal? object-class jclass)
	   #t)
	  (else
	   (any loop (java-class-declared-superclasses object-class))))))

#|
(define jo (java-new (java-class 'java.awt.Frame)))
(java-instance? jo (java-class 'org.omg.CORBA.portable.OutputStream)) ->#f
(java-instance? jo (java-class 'java.awt.Frame)) ->#t
(java-instance? jo (java-class 'java.lang.Object)) ->#t
(java-instance? jo (java-class 'java.awt.MenuContainer)) ->#t
|#



(define (get-converter-to-class jclass)
  (cond ((equal? jclass <jboolean>) ->jboolean)
	((equal? jclass <jdouble>) ->jdouble)
	((equal? jclass <jfloat>) ->jfloat)
	((equal? jclass <jlong>) ->jlong)
	((equal? jclass <jint>) ->jint)
	((equal? jclass <jshort>) ->jshort)
	((equal? jclass <jbyte>) ->jbyte)
	((equal? jclass <jchar>) ->jbyte)
	((equal? jclass (java-class 'java.lang.String)) ->jstring)
	(else (lambda (x)
		(if (java-instance? x jclass)
		    x
		    'error)))))
  

(define (get-jmethod-converter-funclist jmethod)
  (map get-converter-to-class
       (java-constructor-parameter-types jmethod)))

(define (get-jconstructor-converter-funclist jmethod)
  (map get-converter-to-class
       (java-constructor-parameter-types jmethod)))

(define (get-converter-for-class jclass)
  (cond ((equal? jclass <jboolean>) ->boolean)
	((equal? jclass <jdouble>) ->number)
	((equal? jclass <jfloat>) ->number)
	((equal? jclass <jlong>) ->number)
	((equal? jclass <jint>) ->number)
	((equal? jclass <jshort>) ->number)
	((equal? jclass <jbyte>) ->number)
	((equal? jclass <jchar>) ->character)
	((equal? jclass (java-class 'java.lang.String)) ->string)
	(else (lambda (x)
		x))))

(define (get-jmethod-retconverter jmethod)
  (get-converter-for-class (java.lang.reflect.Method.getReturnType jmethod)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Create scheme functions from java constructor/methods/fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dontuseeval #f)
(define (jconstructor->procedure jconstructor)
  (let* ((converter-func-list (get-jconstructor-converter-funclist jconstructor))
	 (argnum 0)
	 (argnames (map-in-order (lambda (notinuse)
				   (set! argnum (1+ argnum))
				   (symbol-append 'arg (string->symbol (number->string argnum))))
				 converter-func-list))
	 (func (java-constructor-procedure jconstructor)))
    (if dontuseeval	
	(lambda args
	    (apply func (map (lambda (func arg)
			       (func arg))
			     convertef-func-list
			     args)))
	(eval `(lambda (,@argnames)
		 (,func ,@(map (lambda (converter-func arg)
				 `(,converter-func ,arg))
			       converter-func-list
			       argnames)))))))

(define (jmethod->procedure jmethod)
  (let* ((converter-func-list (get-jmethod-converter-funclist jmethod))
	 (retconverter-func (get-jmethod-retconverter jmethod))
	 (argnum 0)
	 (argnames (map-in-order (lambda (notinuse)
				   (set! argnum (1+ argnum))
				   (symbol-append 'arg (string->symbol (number->string argnum))))
				 converter-func-list))
	 (func (java-method-procedure jmethod)))
    (if dontuseeval
	(lambda (object . args)
	  (retconverter-func (apply func (cons object (map (lambda (converter-func arg)
							     (converter-func arg))
							   converter-func-list
							   argnames)))))
	(eval `(lambda (object ,@argnames)
		 (,retconverter-func (,func object ,@(map (lambda (converter-func arg)
							    `(,converter-func ,arg))
							  converter-func-list
							  argnames))))))))



;; Returns a getter and a setter in a list.
(define (jfield->procedures jfield)
  (let* ((retconverter-func (get-converter-for-class (java-field-type jfield)))
	 (toconverter-func (get-converter-to-class  (java-field-type jfield)))
	 (getter (java-field-accessor-procedure jfield))
	 (setter (java-field-modifier-procedure jfield)))
    (list (lambda (objectname)
	    (retconverter-func (getter objectname)))
	  (lambda (objectname newvalname)
	    (setter objectname (toconverter-func newvalname))))))



(define jmethod->procedure2-debug #f)


(define (jconstructor->procedure2 jconstructor old-func)
  (let* ((converter-funclist (get-jconstructor-converter-funclist jconstructor))
	 (arglist-length (length converter-funclist))
	 (func (java-constructor-procedure jconstructor)))
    (lambda args
      ;;(c-display "hmm" (length converter-funclist) converter-funclist "args" args)
      (if (not (= (length args) arglist-length))
	  (apply old-func args)
	  (let ((convertedargs #f))
	    (with-failure-continuation
	     (lambda fk
	       #f)
	     (lambda ()	  
	       (set! convertedargs (map (lambda (converter-func arg)
					  (converter-func arg))
					converter-funclist
					args))))
	    (if convertedargs
		(apply func convertedargs)
		(apply old-func args)))))))


(define (jmethod->procedure2 jmethod old-func)
  (let* ((retconverter-func (get-jmethod-retconverter jmethod))
	 (converter-funclist (get-jmethod-converter-funclist jmethod))
	 (arglist-length (1+ (length converter-funclist)))  ;; Add 1 because the object is the first argument.
	 (func (java-method-procedure jmethod)))
    (lambda args
      ;;(c-display "hmm" (length converter-funclist) converter-funclist "args" args)
      (if (not (= (length args) arglist-length))
	  (apply old-func args)
	  (let ((convertedargs #f))
	    (with-failure-continuation
	     (lambda fk
	       #f)
	     (lambda ()	    
	       (set! convertedargs (cons (car args) ;; (car args) is the object.
					 (map (lambda (converter-func arg)
						(converter-func arg))
					      converter-funclist
					      (cdr args))))))
	    (if convertedargs
		(retconverter-func (apply func convertedargs))
		(apply old-func args)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Pollute the global namespace with java constructors/methods/fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-scheme-func-name classname methodname)
  (symbol-append classname '|->| methodname))

(define (add-java-constructor-procedure! jconstructor override?)
  (let* ((constructor? (java-constructor? jconstructor))
	 (declaring-class-func (if constructor? java-constructor-declaring-class java-method-declaring-class))
	 (method-name-func (if constructor? java-constructor-name java-method-name))
	 
	 (classname (java-class-name (java-constructor-declaring-class jconstructor)))
	 (methodname (java-constructor-name jconstructor))
	 (old-func (if (and override?
			    (defined? classname))
		       (eval classname)
		       #f)))
    ;;(c-display "procname" override? classname old-func "args:" (java-constructor-parameter-types jconstructor))
    (define-toplevel classname (if old-func
				   (jconstructor->procedure2 jconstructor old-func)
				   (jconstructor->procedure jconstructor)))))

(define (add-java-method-procedure! jmethod override?)
  (let* ((classname (java-class-name (java-method-declaring-class jmethod)))
	 (methodname (java-method-name jmethod))
	 (procname (get-scheme-func-name classname methodname))
	 (old-func (if (and override?
			    (defined? procname))
		       (eval procname)
		       #f)))
    (define-toplevel procname (if old-func
				  (jmethod->procedure2 jmethod old-func)
				  (jmethod->procedure jmethod)))))


(define (add-java-field! jfield)
  (let* ((classname (java-class-name (java-field-declaring-class jfield)))
	 (fieldname (java-field-name jfield))
	 (gettername (get-scheme-func-name classname fieldname))
	 (settername (symbol-append gettername '|-set!| ))
	 (procedures (jfield->procedures jfield))
	 (getter (car procedures))
	 (setter (cadr procedures)))
    (define-toplevel gettername getter)
    (define-toplevel settername setter)))



(define (add-java-procedure! jsomething . das-override?)
  (let ((override? (or (null? das-override?) (car das-override?))))
    (cond ((java-constructor? jsomething)
	   (add-java-constructor-procedure! jsomething override?))
	  ((java-method? jsomething)
	   (add-java-method-procedure! jsomething override?))
	  ((java-field? jsomething)
	   (add-java-field! jsomething))
	  (else 'error-unknown-something))))






#|
(set! java.awt.Window->setBounds #f)
(define setbounds1 (list-ref (java-class-declared-methods (car (java-class-declared-superclasses (java-class jc)))) 98))
(define setbounds2 (list-ref (java-class-declared-methods (car (java-class-declared-superclasses (java-class jc)))) 99))
(list setbounds1 setbounds2)
(add-java-procedure! setbounds1)
(add-java-procedure! setbounds2)

(define rectangle (java-new (java-class 'java.awt.Rectangle) (->jint 100) (->jint 200) (->jint 50) (->jint 50)))

(java.awt.Frame->setVisible jo #t)
(java.awt.Window->setBounds jo 500 600 100 200)
(java.awt.Window->setBounds jo rectangle)

|#



#|
;; Returns ((list of jmethods with same name) (...) (list of jmethods) (...)) (Not needed (yet), not properly implemented)
(define (get-jmethod-lists jclass)
  (let* ((jmethods (java-class-declared-methods jclass))
	 (jmethod-names (map java-method-name jmethods))
	 (zipped (map list jmethod-names jmethods))
	 (sorted (sort! zipped (lambda (a b)
				 (string<=? (symbol->string (car a)) (symbol->string (car b)))))))
    sorted))
(get-jmethod-lists (java-class 'java.awt.Window))
|#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Pollute + get list of toplevel polluted constructors/methods/fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define all-added-classes '())

(define (add-all-java-procedures! top-class-name . gimme) ;; if gimme is true, return list of defined procedures.
  (let ((meth-defs '())) ;; List of the names of inherited methods
    (let doit ((class-name top-class-name))
      (let* ((jclass (java-class class-name))
	     (superclasses (java-class-declared-superclasses jclass)))
	
	(if (not (null? superclasses))
	    (doit (java-class-name (car superclasses)))) ;; Superclasses' methods and constructors must be added first.

	(c-display "add-meth" (time (if (eq? class-name top-class-name)
					(for-each (lambda (meth-def)               ;; Linking to all methods in superclasses. (java.lang.Frame->setBounds is now pointing to java.lang.Window->setBounds)
						    (let ((class-name (car meth-def))
							  (method-name (cadr meth-def)))
						      (define-toplevel (get-scheme-func-name top-class-name method-name) (get-scheme-func-name class-name method-name))))
						  meth-defs)
					(set! meth-defs (append meth-defs          ;; Storing name of all methods.
								(map (lambda (jmethod)
								       (list class-name (java-method-name jmethod)))
								     (java-class-declared-methods jclass))
								(map (lambda (jfield)
								       (list class-name (java-field-name jfield)))
								     (java-class-declared-fields jclass))
								(map (lambda (jfield)
								       (list class-name (symbol-append (java-field-name jfield) '|-set!|)))
								     (java-class-declared-fields jclass)))))
				    #t))
	
	(if (not (memq class-name all-added-classes)) ;; Don't want to add yet another time.
	    (begin
	      (c-display "meth" (time (for-each add-java-procedure!
						(java-class-declared-methods jclass))))
	      (c-display "constr" (time (for-each add-java-procedure!
						  (java-class-declared-constructors jclass))))
	      (c-display "fields" (time (for-each add-java-procedure!			
						  (java-class-declared-fields jclass))))
	      (push! class-name all-added-classes)))))
    
    (if (and (not (null? gimme)) (car gimme))
	(append (map cadr meth-defs)
		(map java-method-name (java-class-declared-methods (java-class top-class-name)))
		(map java-field-name (java-class-declared-fields (java-class top-class-name)))
		(map (lambda (name)
		       (symbol-append (java-field-name name) '|-set!|))
		     (java-class-declared-fields (java-class top-class-name)))))))



	    

	

		




#|



(java-class-declared-constructors (java-class 'java.awt.Frame))
(java-constructor-parameter-types (car (java-class-declared-constructors (java-class 'java.awt.Frame))))

(add-all-java-procedures! 'java.awt.Frame)
(add-all-java-procedures! 'java.awt.Rectangle)

(define jc 'java.awt.Frame)
(define jo (java-new (java-class jc)))

(java.awt.Frame "aiai")

(equal? (java-class 'java.lang.String) (java-class 'java.awt.Frame))


(set! java.awt.Rectangle #f)
(set! all-added-classes '())


(define rect (java.awt.Rectangle 800 200 300 50))
(java.awt.Frame->setBounds jo rect)

(java-field-type (get-jmethod (java-class 'java.awt.Frame) 'getState))
(java.awt.Frame->getState jo)


(add-all-java-procedures! 'java.lang.Class)
(define aclass (java.lang.Object->getClass() (java.awt.Frame "hmm")))

(define ob (java.lang.Object))

(java.lang.Object->getClass jo)

(java.awt.Frame "ai")
(java.awt.Frame)
(java.awt.Frame (java.awt.Frame))
(java.awt.Frame (java.awt.Rectangle 20 50 29 30))

(add-all-java-procedures! 'java.lang.Object)
(java.lang.Object->toString rect)





(java.awt.Window->setBounds jo 400 200 50 90)

(java-method-parameter-types setbounds1)

(java-method-parameter-types (list-ref (java-class-declared-methods (car (java-class-declared-superclasses (java-class jc)))) 98))
(java-method-parameter-types (list-ref (java-class-declared-methods (car (java-class-declared-superclasses (java-class jc)))) 99))
(define rectangle (java.awt.Rectangle 20 30 40 5))
(java-new (java-class 'java.awt.Rectangle)))

(get-jmethod-converter-funclist (list-ref (java-class-declared-constructors (java-class 'java.awt.Rectangle)) 2))



(define constr (caddr (java-class-declared-constructors (java-class 'java.awt.Rectangle))))
(define rect (apply (java-constructor-procedure constr) (map ->jint (list 500 500 200 100))))

(java.awt.Frame->setBounds jo rect)
(java.awt.Frame->setVisible jo #t)



(list-ref (java-class-declared-methods (car (java-class-declared-superclasses (java-class jc)))) 98)

(java-interface? (java-class '|java.util.Map|))

(add-all-java-procedures! 'java.lang.Class)
(java.lang.Object->getClass jo)

(begin
  (set! java.awt.Frame->setVisible #f)
  (add-all-java-procedures! 'java.awt.Frame)
  (java.awt.Frame->setVisible jo #t))

(define frame (java.awt.Frame))

(define (getfunc)
  (let ((func (lambda ()
		(c-display "hepp"))))
    (eval `(lambda ()
	     (,func)))))

(getfunc)



(begin jsetBounds)
(java-method-name jsetBounds)


(import generic-procedures)

(define a (make-generic-procedure (lambda (a b)
				    (+ 2 3))))
(define b (make-generic-procedure))
;=> <procedure>
(define c (make-generic-procedure a b))
(c)

(with-failure-continuation
 (lambda (fk)
   (call-with-current-continuation (lambda (k) (fk error k))))
 (lambda ()
   (let ((func (lambda (a b c)
		 (+ a b c))))
     (c-display (func 2)))))


|#

