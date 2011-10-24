
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


(eval '(import s2j))


(c-import oo)


(define-macro (j-define def . body)
  `(define ,def ,@body))


;; To debug this file, uncomment the macro below.
;;(define-macro (j-define def . body)
;;  `(c-define ,def ,@body))


(c-define-macro (<-o o)
  `(-> ,o get-object))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Implement (using define-java-proxy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(c-define-macro (java-implement class . callbacks)
  (let ((funcname (c-gensym "funcname"))
	(callbacknames (map (lambda (n) (c-gensym "callback")) callbacks))
	(funcnames (map (lambda (n) (c-gensym "funcname")) callbacks)))
    (eval `(define-java-classes ,class))
    (eval `(define-java-proxy (,funcname ,@callbacknames)
	     (,class)
	     ,@(map (lambda (callback callbackname)
		      `(define ,(symbol-append '|.| (car callback)) ,callbackname))
		    callbacks
		    callbacknames)))
    `(let (,@(map list funcnames (map cadr callbacks)))
       (,funcname ,@(map (lambda (funcname)
			   (let ((argsname (c-gensym "argsname")))
			     `(lambda ,argsname
				(apply ,funcname (cdr ,argsname)))))
			 funcnames)))))

#|
(define-macro (java-implement-macro funcname funcnames callbacks callbacknames)
  `(let (,@(map list funcnames (map cadr callbacks)))
     (,funcname ,@(map (lambda (funcname)
			 (let ((argsname (c-gensym "argsname")))
			   `(lambda ,argsname
			      (apply ,funcname (cdr ,argsname)))))
		       funcnames))))

(j-define (java-implement class . callbacks)
  (let ((funcname (c-gensym "funcname"))
	(callbacknames (map (lambda (n) (c-gensym "callbackname")) callbacks))
	(funcnames (map (lambda (n) (c-gensym "funcname")) callbacks)))
    (eval `(define-java-classes ,class))
    (eval `(define-java-proxy (,funcname ,@callbacknames)
	     (,class)
	     ,@(map (lambda (callback callbackname)
		      `(j-define ,(symbol-append '|.| (car callback)) ,callbackname))
		    callbacks
		    callbacknames)))
    (java-implement-macro funcname funcnames callbacks callbacknames)))

|#


(c-display "a4")

#|
(define-macro (java-implement class . callbacks)
  (let ((funcname (c-gensym))
	(callbacknames (map (lambda (n) (c-gensym)) callbacks))
	(funcnames (map (lambda (n) (c-gensym)) callbacks)))
    `(begin
       (define-java-classes ,class)
       (define-java-proxy (,funcname ,@callbacknames)
	 (,class)
	 ,@(map (lambda (callback callbackname)
		  `(define ,(symbol-append '|.| (car callback)) ,callbackname))
		callbacks
		callbacknames))
       (let (,@(map list funcnames (map cadr callbacks)))
	 (,funcname ,@(map (lambda (funcname)
			     (let ((argsname (c-gensym)))
			       `(lambda ,argsname
				  (apply ,funcname (cdr ,argsname)))))
			   funcnames))))))
  
|#


#|
(define-macro (java-implement class . callbacks)
  (let ((funcname (c-gensym))
	(callbacknames (map (lambda (n) (c-gensym)) callbacks))
	(funcnames (map (lambda (n) (c-gensym)) callbacks)))
    `(begin
       (define-java-classes ,class)
       (define-java-proxy (,funcname ,@callbacknames)
	 (,class)
	 ,@(map (lambda (callback callbackname)
		  `(define ,(symbol-append '|.| (car callback)) ,callbackname))
		callbacks
		callbacknames))
       (let (,@(map list funcnames (map cadr callbacks)))
	 (,funcname ,@(map (lambda (funcname)
			     (let ((argsname (c-gensym)))
			       `(lambda ,argsname
				  (apply ,funcname (cdr ,argsname)))))
			   funcnames))))))
|#



#|
(java-implement <java.awt.event.MouseMotionListener>
		(mouse-dragged (lambda (e)
				 (c-display e)))
		(mouse-moved (lambda (e)
			       (c-display e))))
|#





(c-display "a4.5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Various handy and needed functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-jmethod jclass name)
  (let* ((methods (java-class-declared-methods jclass))
	 (names (map java-method-name methods)))
    (cadr (assq name (map list names methods)))))


(define java.lang.Object->getClass #f)

(j-define (set-java.lang.Object->getClass!)	  
  (let ((jmethod (get-jmethod (java-class 'java.lang.Object) 'getClass)))
    (set! java.lang.Object->getClass (java-method-procedure jmethod))))

(set-java.lang.Object->getClass!)


(define java.lang.reflect.Method.getReturnType #f)

(j-define (set-java.lang.reflect.Method.getReturnType!)
  (let ((jmethod (get-jmethod (java-class 'java.lang.reflect.Method) 'getReturnType)))
    (c-display "jmethod" jmethod)
    (set! java.lang.reflect.Method.getReturnType (java-method-procedure jmethod))))

(set-java.lang.reflect.Method.getReturnType!)



#|
(java.lang.reflect.Method.getReturnType (get-jmethod (java-class 'java.lang.reflect.Method ) 'getReturnType))

(java.lang.Object->getClass jo)
|#


(define java.lang.Object->equals (java-method-procedure (get-jmethod (java-class 'java.lang.Object) 'equals)))

(c-define-macro (java-equal? a b)
  `(->boolean (java.lang.Object->equals ,a ,b)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  The "instance?" function. Should be as fast as possible.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Best, probably
(j-define java-instance?
  (let ((instance-func? (java-method-procedure (get-jmethod (java-class 'java.lang.Class) 'isInstance))))
    (lambda (jobject jclass)
      (->boolean (instance-func? jclass jobject)))))


#|
;; Worse (I think, depends how java-class-precedence-list is implemented)
(j-define (java-instance? jobject jclass)
  (let ((object-class (java.lang.Object->getClass jobject)))
    (any (lambda (jclass2)
	   (java-equal? jclass jclass2))
	 (java-class-precedence-list object-class))))

;; Worst (Probably)
(j-define (java-instance? object jclass)
  (let loop ((object-class (java.lang.Object->getClass object)))
    ;;-display object-cl
    (cond ((null? object-class)
	   #f)
	  ((java-equal? object-class jclass)
	   #t)
	  (else
	   (any loop (java-class-declared-superclasses object-class))))))

(define jo (java-new (java-class 'java.awt.Frame)))
(java-instance? jo (java-class 'org.omg.CORBA.portable.OutputStream)) ->#f
(java-instance? jo (java-class 'java.awt.Frame)) ->#t
(java-instance? jo (java-class 'java.lang.Object)) ->#t
(java-instance? jo (java-class 'java.awt.MenuContainer)) ->#t

|#


(c-display "a6")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Functions to create functions to convert between java and scheme types.
;;;;    Well, as stated in sisc's manual, automatic conversion creates
;;;;    various situations of ambiguities and stuff. So, well, be careful,
;;;;    although you probably knew this already, since you are reading
;;;;    in the middle of this source file. :-)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define <jstring> (java-class 'java.lang.String))


(j-define (get-converter-to-class jclass)
  (cond ((java-equal? jclass <jboolean>) ->jboolean)
	((java-equal? jclass <jdouble>) ->jdouble)
	((java-equal? jclass <jfloat>) ->jfloat)
	((java-equal? jclass <jlong>) ->jlong)
	((java-equal? jclass <jint>) ->jint)
	((java-equal? jclass <jshort>) ->jshort)
	((java-equal? jclass <jbyte>) ->jbyte)
	((java-equal? jclass <jchar>) ->jchar)
	((java-equal? jclass <jstring>) ->jstring)
	(else (lambda (x)
		;;(c-display "I am called with x=" x ", jclass:" jclass)
		(if (procedure? x)
		    (let ((x (<-o x)))
		      (if (java-instance? x jclass)
			  x
			  (begin
			    (c-display "Error. I am called with x=" x ", Expected jclass:" jclass)  
			    #f)))
		    (if (java-instance? x jclass)
			x
			(begin
			  (c-display "Error. I am called with x=" x ", Expected jclass:" jclass)  
			  #f)))))))

(j-define (get-jmethod-converter-funclist jmethod)
  (map get-converter-to-class
       (java-method-parameter-types jmethod)))

(j-define (get-jconstructor-converter-funclist jmethod)
  (map get-converter-to-class
       (java-constructor-parameter-types jmethod)))

(c-display "gakk1")

(c-define (get-converter-for-class jclass)
  (cond ((java-equal? jclass <jboolean>) ->boolean)
	((java-equal? jclass <jdouble>) ->number)
	((java-equal? jclass <jfloat>) ->number)
	((java-equal? jclass <jlong>) ->number)
	((java-equal? jclass <jint>) ->number)
	((java-equal? jclass <jshort>) ->number)
	((java-equal? jclass <jbyte>) ->number)
	((java-equal? jclass <jchar>) ->character)
	((java-equal? jclass <jstring>) (lambda (s)
					  (if (java-null? s)
					      #f
					      (->string s))))
	((java-equal? jclass <jvoid>) (lambda (x) x))
	(else <java-class>)))

(c-display "gakk2")

(j-define (get-jmethod-retconverter jmethod)
  (get-converter-for-class (java.lang.reflect.Method.getReturnType jmethod)))



(c-display "a7")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Functions to create wrappers for java constructor/methods/fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dontuseeval #t) ;; eval screws up debugging. :((((
(j-define (jconstructor->procedure jconstructor)
  (let* ((converter-func-list (get-jconstructor-converter-funclist jconstructor))
	 (argnum 0)
	 (argnames (map (lambda (notinuse)
				   (set! argnum (1+ argnum))
				   (symbol-append 'arg (string->symbol (number->string argnum))))
				 converter-func-list))
	 (func (java-constructor-procedure jconstructor)))
    (if dontuseeval	
	(lambda args
	  (apply func (map (lambda (func arg)
			     (func arg))
			   converter-func-list
			   args)))
	(eval `(lambda (,@argnames)
		 (,func ,@(map (lambda (converter-func arg)
				 `(,converter-func ,arg))
			       converter-func-list
			       argnames)))))))

(j-define (jmethod->procedure jmethod)
  (let* ((converter-func-list (get-jmethod-converter-funclist jmethod))
	 (retconverter-func (get-jmethod-retconverter jmethod))
	 (argnum 0)
	 (argnames (map (lambda (notinuse)
				   (set! argnum (1+ argnum))
				   (symbol-append 'arg (string->symbol (number->string argnum))))
				 converter-func-list))
	 (func (java-method-procedure jmethod)))
    (if dontuseeval
	(lambda (object . args)
	  (retconverter-func (apply func (cons object (map (lambda (converter-func arg)
							     (converter-func arg))
							   converter-func-list
							   args)))))
	(eval `(lambda (object ,@argnames)
		 ;;(c-display "retconverter-func" ,retconverter-func)
		 (,retconverter-func (,func object ,@(map (lambda (converter-func arg)
							    `(,converter-func ,arg))
							  converter-func-list
							  argnames))))))))



;; Returns a getter and a setter in a list.
(j-define (jfield->procedures jfield)
  (let* ((retconverter-func (get-converter-for-class (java-field-type jfield)))
	 (toconverter-func (get-converter-to-class  (java-field-type jfield)))
	 (getter (java-field-accessor-procedure jfield))
	 (setter (java-field-modifier-procedure jfield)))
    (list (lambda (objectname)
	    (retconverter-func (getter objectname)))
	  (lambda (objectname newvalname)
	    (setter objectname (toconverter-func newvalname))))))


(c-display "a8")

(define jmethod->procedure2-debug #f)



;; This function is only called if there are more than one method
;; with the same name.
;; 1. Checks if arguments are of the correct type. If not,
;;    returns #f. (The caller applies the arguments to the next alternative
;;    method instead).
;; 2. Convert arguments to java objects.
(j-define (get-java-arguments return checkconverters args)
  (map (lambda (checkconverter arg)
	 (checkconverter return arg))
       checkconverters args))

(j-define (get-checkconverters argclasses)
  (map (lambda (argclass)
	 (cond ((or (java-equal? <jint> argclass)
		    (java-equal? <jlong> argclass)
		    (java-equal? <jshort> argclass)
		    (java-equal? <jbyte> argclass))
		(let ((converter (get-converter-to-class argclass)))
		  (lambda (return arg)
		    ;;(c-display "int conv")
		    ;;(if (or (not (number? arg))  
		    ;;    (not (integer? arg)))      ;; Other methods might take floating numbers. (Not too sure about this)
		    (if (not (number? arg))
			(return #f)
			(converter arg)))))
	       
	       ((or (java-equal? <jdouble> argclass)
		    (java-equal? <jfloat> argclass))
		(let ((converter (get-converter-to-class argclass)))
		  (lambda (return arg)
		    ;;(c-display "float conv")
		    (if (number? arg)
			(converter arg)
			(return #f)))))
	       
	       ((java-equal? <jstring> argclass)
		(lambda (return arg)
		  ;;(c-display "string conv")
		  (if (or (string? arg)
			  (symbol? arg))
		      (->jstring arg)
		      (return #f))))

	       ((java-equal? <jchar> argclass)
		(lambda (return arg)
		  ;;(c-display "char conv")
		  (if (char? arg)
		      (->jchar arg)
		      (return #f))))

	       ((java-equal? <jboolean> argclass)
		(lambda (return arg)
		  ;;(c-display "bool conv")
		  (->jboolean arg)))
    
	       (else
		(lambda (return arg)
		  ;;(c-display "else is called." arg argclass)
		  (cond ((procedure? arg)		      
			 (let ((jobject (<-o arg)))
			   ;;(c-display "I got a procedure." jobject argclass)
			   (if (java-instance? jobject argclass)
			       jobject
			       (return #f))))
			((not (java-object? arg))
			 (return #f))
			((java-instance? arg argclass)
			 arg)
			(else
			 (return #f)))))))
       argclasses))

  

(j-define (jconstructor->procedure2 jconstructor old-func)
  (let* ((argclasses (java-constructor-parameter-types jconstructor))
	 (checkconverters (get-checkconverters argclasses))
	 (arglength (length argclasses))
	 (func (java-constructor-procedure jconstructor)))
    (lambda args
      ;;(c-display "Constructor" (length converter-funclist) converter-funclist "args" args)
      (or (and (= arglength (length args))
	       (call/cc (lambda (return)
			  (apply func (get-java-arguments return checkconverters args)))))
	  (apply old-func args)))))


(j-define (jmethod->procedure2 jmethod old-func)
  (let* ((retconverter-func (get-jmethod-retconverter jmethod))
	 (argclasses (java-method-parameter-types jmethod))
	 (checkconverters (get-checkconverters argclasses))
	 (arglength (1+ (length argclasses))) ;; One extra for the object.
	 (func (java-method-procedure jmethod)))
    (if (eq? retconverter-func ->boolean)
	(lambda args
	  ;;(c-display "arglength:" arglength "(length args)" (length args) "argclasses" argclasses "args:" args)
	  (call/cc (lambda (mainreturn)
		     (or (and (= arglength (length args))
			      (call/cc (lambda (return)
					 (mainreturn
					  (retconverter-func
					   (apply func (cons (car args)
							     (get-java-arguments return checkconverters (cdr args)))))))))
			 (apply old-func args)))))
	(lambda args
	  (or (and (= arglength (length args))
		   (call/cc (lambda (return)
			      (retconverter-func
			       (apply func (cons (car args)
						 (get-java-arguments return checkconverters (cdr args))))))))
	      (apply old-func args))))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  The java "class holders".
;;;;;;;  Static-like classes that remembers method and field converter
;;;;;;;  functions and stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(c-define (get-creator-procedure jsomething old-func)
  (cond ((java-constructor? jsomething)
	 (if old-func
	     (jconstructor->procedure2 jsomething (old-func))
	     (jconstructor->procedure jsomething)))
	
	((java-method? jsomething)
	 (if old-func
	     (jmethod->procedure2 jsomething (old-func))
	     (jmethod->procedure jsomething)))

	((java-field? jsomething)
	 (let* ((retconverter-func (get-converter-for-class (java-field-type jsomething)))
		(toconverter-func (get-converter-to-class  (java-field-type jsomething)))
		(getter (java-field-accessor-procedure jsomething))
		(setter (java-field-modifier-procedure jsomething)))
	   (lambda (object . setting)
	     (if (null? setting)
		 (retconverter-func (getter object))
		 (setter object (toconverter-func (car setting)))))))
	(else
	 (lambda x
	   (c-display "Error, bug in javaoo/java.scm, wrong argument for <java-procedure>" jsomething old-func x)))))

#|
(get-creator-procedure (car (java-class-declared-constructors (java-class 'java.awt.Frame))) #f)
|#

(def-class (<java-nullclassholder>)
  (def-method (get-classmethod-thunk name3)
    #f)
  (def-var classmethods (make-hash-table 256)))

;; Note, <java-classholder> instances are kept in java-classholders for all eternity (or something like that),
;; so its important not to keep anything from its sub-classes, which will cause sub-objects never to be garbage-collected.
(def-class (<java-classholder> classname)
  
  (define jclass (java-class classname))
  (define super-classholder (let ((superjclass (cl-car (java-class-declared-superclasses jclass))))
			      (if (null? superjclass)
				  (<java-nullclassholder>)
				  (get-java-classholder (java-class-name superjclass)))))
  
  (c-display "Creating new classholder for" classname)

  ;; Returns a thunk that returns the classmethod
  (def-method (get-classmethod-thunk name3)
    (hashq-ref classmethods name3))

  ;; Returns the classmethod
  (def-method (get-classmethod name)
    (force (get-classmethod-thunk name)))
  
  ;; List of ((methodname procedure) (methodname procedure) ...)
  (def-var classmethods (copy-hash-table (-> super-classholder classmethods)))

  (for-each (lambda (method)
	      (let* ((name2 (car method))
		     (jsomething (cadr method))
		     (old-func (hashq-ref classmethods name2)))
		(hashq-set! classmethods
			    name2
			    (let ((proc #f))
			      (delay
				(if proc
				    proc
				    (begin
				      (c-display "Creating method" name2 "in class" classname)
				      (set! proc (get-creator-procedure jsomething old-func))
				      proc)))))))
	    (append (map (lambda (jfield)
			   (list (java-field-name jfield) jfield))
			 (java-class-declared-fields jclass))
		    (map (lambda (jmethod)
			   (list (java-method-name jmethod) jmethod))
			 (java-class-declared-methods jclass)))))
  



(define java-classholders '())
(j-define (get-java-classholder classname)
  (let ((classholder (assq classname java-classholders)))
    (if classholder
	(cdr classholder)	
	(let ((ret (<java-classholder> classname)))
	  (push! (cons classname ret) java-classholders)
	  ret))))


(c-define-macro (get-java-function classname methodname)
  (let* ((stripped-classname (string->symbol (list->string (reverse! (cdr (reverse! (cdr (string->list (symbol->string classname))))))))))
    `(-> (get-java-classholder ',stripped-classname) get-classmethod ',methodname)))


  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  The <java-class> function, called by "new"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
(j-define make-static-object cons)
(j-define get-static-class car)
(j-define get-static-object cdr)
(j-define is-it-static? pair?)

(def-class (<java-base-class> jobject)


  (define java-classholder (if (is-it-static? jobject)
			       (get-java-classholder (java-class-name (get-static-class jobject)))
			       (get-java-classholder (java-class-name (java.lang.Object->getClass jobject)))))
  
  (add-method 'get-object
    (lambda ()
      jobject))
  
  (add-method 'get-method
	      (lambda (name)
		;;(c-display "ai" name (hashq-ref methods name))
		(dont-trace
		 (or (hashq-ref methods name)
		     (let ((classmethod (-> java-classholder get-classmethod-thunk name)))
		       (if classmethod
			   (let* ((proc (classmethod)))
			     (add-method name (lambda x
						(apply proc (cons jobject x))))
			     (get-method name))
			   #f))))))
  

  (set! get-method (<- this get-method))
  
;;;  (set! this (lambda (name . rest)
;;;	       (apply (or (this->get-method name)
;;;			  (lambda x (format #t "<java-base-class>: No such method: \"~A\" in class \"~A\".~%" name (-> this class-name))))
;;;		      rest)))

  (if (is-it-static? jobject)
      (set! jobject (get-static-object jobject)))
  
  )



;; Creating a new object with def-class is quite heavy, and not always necessarry. Just make a
;; minimal object for now.

(j-define (get-dispatcher-to-<java-class> name jobject)
  (let* ((object (<java-base-class> jobject)))
    (<- object get-method)))

(j-define (<java-class> jobject)
  ;;(c-display "<java-class> called" jobject)
  (let ((dispatcher #f))
    (set! dispatcher (lambda (name)
		       (if (eq? 'get-object name)
			   (lambda ()
			     jobject)
			   (begin
			     (set! dispatcher (get-dispatcher-to-<java-class> name jobject))
			     (dispatcher name)))))
    (lambda (name . rest)
      ;;(c-display "name.rest" jobject name rest (dispatcher name))
      (apply (or (dispatcher name)
		 (lambda x (format #t "<java-class>: No such method: \"~A\" in class surrounding object \"~A\".~%" name jobject)));;this->class-name)))
	     rest))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Constructors (The "new" and "new-static" macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
(define java-constructors '())
(c-define (get-java-constructor classname)
  (let ((constructor (assq classname java-constructors)))
    (if constructor
	(cadr constructor)
	(let* ((jclass (java-class classname))
	       (jconstructors (java-class-declared-constructors jclass))
	       (constructor (jconstructor->procedure (car jconstructors))))
	  (for-each (lambda (jconstructor)
		      (set! constructor (jconstructor->procedure2 jconstructor constructor)))
		    (cdr jconstructors))
	  (push! (list classname constructor) java-constructors)
	  constructor))))
    
  
(c-define-macro (new classname . args)
  (let* ((stripped-classname (string->symbol (list->string (reverse! (cdr (reverse! (cdr (string->list (symbol->string classname))))))))))
    `(<java-class> ((get-java-constructor ',stripped-classname) ,@args))))

(c-define-macro (new-static classname)
  (let* ((stripped-classname (string->symbol (list->string (reverse! (cdr (reverse! (cdr (string->list (symbol->string classname))))))))))
    `(<java-class> (make-static-object (java-class ',stripped-classname) (java-null (java-class ',stripped-classname))))))





#|
(define frame (new <java.awt.Frame>))

(-> frame getClass)
(-> frame dir)

(-> frame setVisible #t)
(define rect (<java.awt.Rectangle> 600  500 200 300))
(-> frame setBounds (<-o rect))

(define g (<java-class> (-> frame getGraphics)))

(-> g drawLine 10 20 30 40)

(define-java-classes <java.awt.event.MouseListener>)
(define-java-proxy (mouselistener clicked entered exited pressed released)
  (<java.awt.event.MouseListener>)
  (define (.mouse-clicked p e)
    (if clicked (clicked e)))
  (define (.mouse-entered p e)
    (if entered (entered e)))
  (define (.mouse-exited p e)
    (if exited (exited e)))
  (define (.mouse-pressed p e)
    (if pressed (pressed e)))
  (define (.mouse-released p e)
    (if released (released e))))



(java-import <java.awt.Frame>)
(java-import <java.awt.event.MouseEvent>)

(define-java-classes <java.awt.event.MouseMotionListener>)
(define-java-proxy (mousemotionlistener dragged)
  (<java.awt.event.MouseMotionListener>)
  (define .mouse-dragged dragged)
  (define (.mouse-moved p e) #f))

(define last-x 0)
(define last-y 0)
(define frame (<java.awt.Frame>))
(-> frame setVisible #t)
(-> frame setBounds 300 400 150 160)

(define g (<java-class> (-> frame getGraphics)))

(-> frame addMouseMotionListener (mousemotionlistener (lambda (p e)
							(let* ((x (java.awt.event.MouseEvent->getX e))
							       (y (java.awt.event.MouseEvent->getY e)))
							  (-> g drawLine last-x last-y x y)
							  (set! last-x x)
							  (set! last-y y))))))

|#









#|



(java-class-declared-constructors (java-class 'java.awt.Frame))
(java-constructor-parameter-types (car (java-class-declared-constructors (java-class 'java.awt.Frame))))

(add-all-java-procedures! 'java.awt.Frame)
(add-all-java-procedures! 'java.awt.Rectangle)

(define jc 'java.awt.Frame)
(define jo (java-new (java-class jc)))

(java.awt.Frame "aiai")

(java-equal? (java-class 'java.lang.String) (java-class 'java.awt.Frame))


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


(eval '(import generic-procedures))

(define a (make-generic-procedure (lambda (a b)
				    (+ 2 3))))
(define b (make-generic-procedure))
;=> <procedure>
(define c (make-generic-procedure a b))
(c)

(lambda (fk)
       (if (null? args)
           (call-with-current-continuation (lambda (k) (fk error k)))
           (fk error (car args))))))
          
(with/fc
 (lambda (fk b)
   (c-display fk b)
   (b 2)) 
 (lambda ()
   (let ((func (lambda (a b c)
		 (+ a b c))))
     (c-display (func 2)))))



|#

