

questions:
-instance?
-optional arguments?


questions:
-instance?
-equal? (classes)
-debugging (backtrace/linenumbers proxy)
-macroexpand

(java-class-name <jint>)
(java-class-name (java.lang.Object->getClass (->jint 50)))
(eqv? <jint> (java.lang.Object->getClass (->jint 50)))
(java-instance? (->jint 50) <jint>)

((java-method-procedure (get-jmethod (java-class 'java.lang.Class) 'isInstance))
 (java.lang.Object->getClass (->jint 50)) <jint>)

((java-method-procedure (get-jmethod (java-class 'java.lang.Object) 'equals))
 (java.lang.Object->getClass (->jboolean #f)) <jboolean>)


(equal? (java.lang.Object->getClass (->jboolean #f)) <jint>)

 (java-class 'java.lang.Object) <jint>)


(define-macro java-equal?
  (let ((proc (java-method-procedure (get-jmethod (java-class 'java.lang.Object) 'equals))))
    (lambda (a b)
      `(->boolean (,proc ,a ,b)))))

(let* ((argclasses (java-method-parameter-types (get-jmethod (java-class 'java.awt.Window) 'setBounds))))
  (sc-expand '(java-equal? <jint> (car argclasses))))

  ((java-method-procedure (get-jmethod (java-class 'java.lang.Object) 'equals))
   <jdouble> (car argclasses)))

(begin <jfalse>)


       

(symbol? (java-class-name (java-class 'java.awt.Frame)))
(java-class-flags (java-class 'java.awt.Frame))
(java-class-flags (java-class 'java.lang.Math))
(java-class-declaring-class (java-class 'java.lang.Math))
(java-class-declared-superclasses (java-class 'java.lang.Math))
(java-class-precedence-list (java-class 'java.lang.Math))

(java-class-precedence-list (java-class 'java.awt.Frame))

(let loop ((object-class (java-class 'java.awt.Frame)))
  (c-display object-class)
  (cond ((null? object-class)
	 #f)
	(else
	 (any loop (java-class-declared-superclasses object-class))))))


(import s2j)


(define jc (java-class 'java.awt.Frame))
(define jo (java-new jc))
(java-object? jo)
((generic-java-method 'setBounds) jo 100 100 200 200)

(begin jc)

(define-java-class <java.awt.Frame>)
(define-generic-java-method setBounds)
(define frame (java-new <java.awt.Frame>))
(begin frame)


Object res = fooObj.barBaz(a, b, c)
(define res (bar-baz foo-obj a b c))


(setBounds frame 100 100 200 200)




(car (java-class-declared-methods jc))

(list-ref (java-class-declared-methods (car (java-class-declared-superclasses jc))) 98)))

(define setboundsproc (java-method-procedure (list-ref (java-class-declared-methods (car (java-class-declared-superclasses jc))) 98)))
(define setboundsmethod (java-method-method (list-ref (java-class-declared-methods (car (java-class-declared-superclasses jc))) 98)))

(begin setboundsproc)

( setboundsproc
  jo
  (->jint 100)
  (->jint 100)
  (->jint 200)
  (->jint 200))

( (java-method-procedure
   (list-ref (java-class-declared-methods (car (java-class-declared-superclasses jc))) 110))
  jo
  (->jboolean #f))


(define


(define-macro (javaclass->class javaclass)
  #t)




(+ 2 3 4)
(set! (+ 2 3 4) 90)

(define (make-setter a b)
  (cons a b))

(define-macro (set!! var val)
  (if (pair? var)
      `((cdr var) val)
      `(set! var val)))

(define-macro (create-java-field! 
))


;; Should both be true	       
(list (case-sensitive #t)
      (emit-annotations))


(import debugging)
(suppressed-stack-trace-source-kinds '())
(suppressed-stack-trace-source-kinds #f)
(max-stack-trace-depth 1)

(define-macro (create-bug-func name)
  `(define (,name)
     (let ((a 90)
	   (b #f))
       (if a
	   (+ a b)))))

(create-bug-func callbug)


(define-macro (callbug-macro)
  `(callbug))

(define-macro (callbug-macro2)
  (let ((a 90)
	(b #f))
    (if a
	(+ a b))))

(compile-file "testing2.scm" "test.scc")
(load "test.scc")

(load "testing2.scm")
(callbug)

(callbug-macro)

(callbug2)
(callbug3)




(let ()
  (define a (c-display 'a))
  (define b (c-display 'b))
  (c-display 'finish))

(let ()
  (letrec* ((a (c-display 'a))
	   (b (c-display 'b)))
    (c-display 'finish)))



(sc-expand (define-a-macro (dosomething b)
	      `(+ 50 ,b)))

(define-a-macro (dosomething b)
  `(+ 50 ,b))


(a
 (domething 60))

-> (+ 50 60)

(define (dosomething b)
  (eval `(+ 50 ,b)))

(let ((b 77))
  `(+ 50 ,b))

(callbug-macro2)



WORKING MACROSYSTEM:

(load "various.scm")
(c-import hashtable)
(load "testing2.scm")

(define amacros (make-hash-table 251))
(begin amacros)

(define-macro (define-a-macro def body)
  (hashq-set! amacros (car def) (list (cdr def) body))
  `(define ,def (neweval ,body)))

(define-a-macro (dosomething b)
  `(+ 50 ,b #f))

(define-macro (expand-a-macro expr)
  (let ((qua (hashq-ref amacros (car expr) amacros)))
    `(let ,(map list (car qua) (cdr expr))
       ,(cadr qua))))

(expand-a-macro (dosomething 77))

;; This doesn't give very nice debug output:
(dosomething 77)

;; Idea: Make a new (eval) that takes its contents, saves it into a file, and then loads it again.

(define (neweval code)
  (c-display code))
(get-macro-dosomething)


(list (quote bt) bt)


(c-define* (aiai)
  (define gakk 3)
  (define kka 4)
  (+ 2 (+ gakk kka 4)))
(aiai)
(bt)

(c-macroexpand2 '(c-define (test)
		   (+ 2 (- 3 4))))

(c-macroexpand2 '(c-define* (aiai)
		   (let ((ai 4))		     
		     (+ ai 5)
		     (define var1 5)
		     (define (var2) 6)
		     (def-var class-name aiai)
		     (+ 2 3))))

(c-macroexpand2 '(c-define* (aiai)
		   (c-display 'a)
		   (define b 9)
		   (+ 8 b)))

(a-macroexpand '(c-define (aiai) 2))
(a-macroexpand (eval (caddr (caddr (a-macroexpand '(c-define (aiai) 2))))))






(c-define* (aiai?!)
  (c-display 'a)
  (define b 9)
  (+ 8 b))






(begin
  `(define* (aiai)
     ,(fix-defines-do
       (add-traces-do
	'((define gakk '())
	  (+ 2 3))))))


(define* (aiai)
  (begin
    (add-trace
     'trace-322
     'calling
     '(begin (define gakk '()) (+ 2 3)))
    ((let* ((gakk #f))
       begin
       (set! gakk '())
       (begin
         (add-trace 'trace-321 'calling '(+ 2 3))
         (+ 2 3))))))



			   
		    
(c-macroexpand2 '(def-class (<tes>)
		  (define a 5)
		  a))
(def-class (<tes>)
  (define a 5)
  a)

(c-define tes (<tes>))
(-> tes dir)
(bt 4)


(c-macroexpand2 '(c-define (aiai)
		   (def-method (run . args)
		     args)
		   (define gakk '())
		   (+ 2 3)))
;; Made by Dan Muresan
(define (read-file fn)
  (define code
    (with-input-from-file fn
      (lambda ()
        (unfold eof-object? values (lambda _ (read-code))
                (read-code)))))
  ;; TODO: parse code
  (syntax-object->datum
    (datum->syntax-object (syntax code) code)))

(define code (read-file "/hom/kjetism/hurtigmixer/src/gencode/win-c-define.scm"))
(define code (sourcefile->list "/hom/kjetism/hurtigmixer/src/gencode/win-c-define.scm"))
(directory-list "gencode")


(list? code)

(annotation-source code)

(for-each (lam



(load "various.scm")
(c-define (cachetest)
  (+ 2 3 a))


(c-time (load "gencode/<slider>-c-define*.scm"))

(c-time (define code (sourcefile->list "gencode/<slider>-c-define*.scm")))
(c-time (begin code))

(c-time (eval (car code)))

(c-time (eval (car (sourcefile->list "gencode/<slider>-c-define*.scm"))))

(import record)
(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(define p (kons 1 2))
(pare? p)

(define-record-type :<slider>
  (<slider> parent x y)
  <slider?>
  (parent <slider->parent>)
  (x <slider->x>)
  (y <slider->y>))

(define slider (<slider> 1 2 3))
(-> (<-r slider) x)
   ->
(sc-expand '(<slider->x> slider))

(def-class (<bank> sum)
   (define adding 2)
   (def-method (get-add anothersum)
     
   (def-method (add newsum)
      (set! sum (+ sum newsum adding))))

->
(define-record-type :<bank>
  (<bank-record> sum)
  <bank-record?>
  (sum <bank-record->sum> <bank-record->set-sum!>)
  (adding <bank-record->adding> <bank-record->set-adding!>))

(define (<bank-record->add> bank-record newsum)
  (<bank-record->set-sum!> bank-record (+ (<bank-record->sum> bank-record)
					  newsum
					  (<bank-record->adding> bank-record))))



To grunner:
 1. Om-evaluering av klasse redefinerer oppfoersel av allerede opprettete instanser. (kjempeviktig!)
 2. Evaluering av klasse tar ikke saa lang tid. (kjempeviktig for sisc)


(define (<bank-record->add> bank-v newsum)
  (vector-set! bank-v <bank->sum>
	       (+ (vector-ref bank-v <bank->sum>)
		  newsum
		  (vector-ref bank-v <bank->adding>))))


(define (<bank> sum)
  ...
  (this->bank-record (<bank-record> sum))
  (add-method 'add (lambda (newsum)
		     (<bank-record->add> this->bank-record newsum))))



(sc-expand '(define-record-type :<gakk>
              (<gakk>)
	      <gakk?>))


(sc-expand '(<bank-record->sum> bank-record))

(sc-expand '(vector-ref vec slot))


;; Lambda-lift approach:

(def-class (<bank> sum)
   (define adding 2)
   (def-method (get-add anothersum)
     (+ sum anothersum adding))
   (def-method (add newsum)
      (set! sum (+ sum newsum adding))))

->

(define (<bank->get-add> sum adding anothersum)
  (+ sum anothersum adding))

(def-class (<bank> sum)
  (define adding 2)
  (def-method (get-add anothersum)
    (<bank->get-add> sum adding anothersum))
  (def-method (add newsum)
    (set! sum (+ sum newsum adding))))


(string? (generic-write '(def-class (<bank> sum)
			   (define adding 2)
			   (def-method (get-add anothersum)
			     (<bank->get-add> sum adding anothersum))
			   (def-method (add newsum)
			     (set! sum (+ sum newsum adding))))
			display 80 display))


(define a '(a b c))
(set-car! (cdr a) 'd)

(c-define (a)
   (+ 2 3 #f))
(a)
(bt)

(c-macroexpand '(def-class (<bank> sum)))
(def-class-do '(<bank> sum) '(+ 2 3))
(reverse! '())




(begin p)
(wait-for-process p)

(begin
  (define p (spawn-process "/usr/bin/oggenc /hom/kjetism/testing.wav"))
  (define port (get-process-stderr p)))

(string (integer->char (read-byte port)))



(define jframe (new <javax.swing.JFrame>))

(-> jframe show)
(-> jframe setBounds 500 400 400 400)
(-> jfrma
(define radio (new <javax.swing.ButtonGroup>))
(define ogg (new <javax.swing.JRadioButton> "ogg"))
(define mp3 (new <javax.swing.JRadioButton> "mp3"))
(-> radio add ogg)
(-> radio add mp3)

(-> jframe setLayout (new <java.awt.BorderLayout>))

(-> jframe add ogg (-> (new-static <java.awt.BorderLayout>) WEST))
(-> ogg show)

(-> jframe add mp3)
(-> mp3 show)

(-> jframe pack)
(-> jframe class-name)

;;(define jcomp (new <javax.swing.JComponent>))
(define tooltip (new <javax.swing.JToolTip>))
(-> tooltip setComponent (-> jframe get-object))
(-> tooltip setTipText "hello hello2")
(-> tooltip show)

(show-message-dialog "hello hello")

(define jframe (new <javax.swing.JFrame>))

(-> jframe show)
(-> jframe hide)
(-> jframe setBounds 500 400 400 400)
(-> jframe setUndecorated #t)

