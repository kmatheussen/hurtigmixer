



(define* (schemecodeparser expr :key elsefunc atomfunc nullfunc pairfunc use-customsymbolhandler? customsymbolhandler blockhandler)
  (let parse ((expr expr))
    (if (not blockhandler)
	(set! blockhandler (lambda (expr) (map parse expr))))


    (cond ((not (pair? expr)) 
	   (if atomfunc
	       (atomfunc expr)
	       expr))
	  ((null? expr) 
	   (if nullfunc
	       (nullfunc expr)
	       expr))
	  ((pair? (car expr))
	   (if pairfunc
	       (pairfunc expr)
	       (blockhandler expr)))
	  ((and use-customsymbolhandler?
		(use-customsymbolhandler? expr))
	   (customsymbolhandler expr))
	  ((or (eq? 'lambda (car expr))
	       ;;(eq? 'lambda* (car expr))
	       (eq? 'define (car expr))
	       ;;(eq? 'define* (car expr)))
	       )
	   `(,(car expr) ,(cadr expr)
	     ,@(blockhandler (cddr expr))))
	  ((eq? 'do (car expr))
	   `(do ,(map (lambda (a)
			`(,(car a) ,@(blockhandler (cdr a))))
		      (cadr expr))
		,@(blockhandler (cddr expr))))
	  ((and (eq? 'let (car expr))
		(symbol? (cadr expr)))
	   `(let ,(cadr expr) ,(map (lambda (a)
				      `(,(car a) ,@(blockhandler (cdr a))))
				    (caddr expr))
		 ,@(blockhandler (cdddr expr))))
	  ((or (eq? 'let (car expr))
	       (eq? 'let* (car expr))
	       (eq? 'letrec (car expr)))
	   `(,(car expr) ,(map (lambda (a)
				 `(,(car a) ,@(blockhandler (cdr a))))
			       (cadr expr))
	     ,@(blockhandler (cddr expr))))
	  ((or (eq? 'quote (car expr))
	       (eq? 'QUOTE (car expr)))
	   expr)
	  ((or (eq? 'quasiquote (car expr))
	       (eq? 'QUASIQUOTE (car expr)))
	   ;;(c-display "Warning in macros.scm/schemecodeparser: quasiquote not handled")
	   expr)
	  ((eq? 'case (car expr))
	   `(case ,(parse (cadr expr))
	      ,@(map (lambda (expr)
		       `(,(car expr) ,@(blockhandler (cdr expr))))
		     (cddr expr))))
	  ((eq? 'else (car expr))
	   `(else ,@(blockhandler (cdr expr))))
	  (else
	   (if elsefunc
	       (elsefunc expr)
	       (blockhandler expr))))))


  ;; Returns expr (eq?-able expr), unless its transformed
(define (a-macroexpand-1 expr)
  ;;(c-display "a-me-1" expr)
  (if (or (not (pair? expr))
	  (null? expr)
	  (not (symbol? (car expr))))
      expr
      (let ((qua (hashq-ref all-macros (car expr))))
	;;(if qua (c-display (car qua) (cdr expr)))
	(if (not qua)
	    (begin
	      ;;(c-display "Error in expand-a-macro. Macro for " expr " Not found.")
	      expr)
	    (if (list? (car qua))
		(begin
		  (when #t
			;;(c-display "evaling:")
			;;(pretty-print `(let ,(map (lambda (varname val)
			;;			`(,varname (quote ,val)))
			;;			  (car qua) (cdr expr))
			;;		 ,@(cadr qua)))
			(newline))
		  (eval `(let ,(map (lambda (varname val)
				      `(,varname (quote ,val)))
				    (car qua) (cdr expr))
			   ,@(cadr qua))))
		(let ((letlist '())
		      (rest '()))
		  (let loop ((argnames (car qua))
			     (vars (cdr expr)))
		    (if (pair? argnames)
			(begin
			  (push-back! (list (car argnames)
					    `(quote ,(car vars)))
				      letlist)
			  (loop (cdr argnames) (cdr vars)))
			(push-back! (list argnames `(quote ,vars))
				    letlist)))
		  ;;(c-display "letlist" letlist)(c-display "evaling:" )
		  ;;(pretty-print `(let ,letlist ,(cadr qua)))(newline)
		  (eval `(let ,letlist
			   ,@(cadr qua)))))))))

#|
(a-macroexpand-1 '(dosomething 50))
|#

(define (a-macroexpand expr)
  ;;(c-display "a-mac" expr)
  (schemecodeparser expr
		    ':elsefunc (lambda (expr)
				 (let ((topexpand (a-macroexpand-1 expr)))
				   ;;(c-display "expr/topexpand" expr topexpand)
				   (if (eq? expr topexpand)
				       `(,(car expr) ,@(map a-macroexpand (cdr expr)))
				       (a-macroexpand topexpand))))))



#|
(c-define-macro (dosomething b)
  `(+ 50 ,b 60))
(c-define-macro (dosomething2 a . b)
  `(+ 50 ,a ,@b 60))
(c-define-macro (dosomething3 . c)
  (let ((d c))
    `(dosomething2 (dosomething 5) ,@d)))

(c-define-macro (lettest)
  `(let ((a 5))
     (dosomething 50)))
(a-macroexpand '(lettest))

(dosomething2 3 4 5)
(a-macroexpand '(dosomething2 3 4 5))

(a-macroexpand '(dosomething 77))

(dosomething3 6 7 8)
(a-macroexpand '(dosomething3 6 7 8))


(define-macro (qq-expand letlist term)
  (let loop ((term term))
    (c-display "term" term letlist)
    (cond ((not (list? term)) term)
	  ((null? term) term)
	  ((eq? 'unquote (car term))
	   (c-display "hmm" (cadr term) letlist)
	   (if (assq (cadr term) letlist)
	       (cadr (assq (cadr term) letlist))
	       term))
	  (else
	   (map loop term)))))

(let ((a 60))
  (qq-expand ((a 50))
	     (let ((b (+ 6 c)))
	       `(+ ,a ,b 6)))
)


(let ((c '(6 7 8)))
  (let ((d c))
    `(dosomething2 (dosomething 5) (unquote-splicing d))))

=> '(dosomething2 (dosomething 5) 6 7 8)


(let ((a '(dosomething 5))
      (b '(6 7 8)))
  `(+ 50 (unquote a) (unquote-splicing b) 60))

=> '(+ 50 (dosomething 5) 6 7 8 60)


|#



#|
(let ()
  (set! a 5)
  (define b 6)
  (define (c) 7))
->
(let ()
  (let* ((b #f)
	 (c #f))
    (set! a 5)
    (set! b 6)
    (set! c (lambda () 7))))

|#

(define (fix-defines-do terms)
  ;;(c-display terms)
  (schemecodeparser terms
		    ':blockhandler
		    (lambda (terms)
		      ;(if (not (eq? 'define (car terms)))
		;	  (let ((temp (a-macroexpand-1 terms)))
		;	    (if (eq? 'define (car temp))
		;		(set! terms temp))))
		;      (c-display "terms" terms)
		      (let* ((defines '())
			     (newterms (map-in-order (lambda (terms)
						       (if (and (pair? terms)
								(eq? 'define (car terms)))
							   (if (pair? (cadr terms))
							       (begin
								 (push! (car (cadr terms)) defines)
								 `(set! ,(car (cadr terms)) (lambda ,(cdr (cadr terms)) ,@(cddr terms))))
							       (begin
								 (push! (cadr terms) defines)
								 `(set! ,(cadr terms) ,@(cddr terms))))
							   terms))
						     terms)))
			(if (null? defines)
			    (map fix-defines-do terms)
			    `((let* ,(map (lambda (name)
					    `(,name #f))
					  (reverse! defines))
				,@(map fix-defines-do newterms))))))))

(c-define-macro (fix-defines . terms)
  (apply append (map-in-order fix-defines-do (map a-macroexpand terms))))

#|
(begin
  (newline)
  (pretty-print (a-macroexpand '(fix-defines (let ((a 2))
					       ((lambda () 
						  (define d 9)
						  (set! a d)))
					       (define b 6)
					       (define (c) 7)
					       (+ a b (c))))))
  (newline))


(fix-defines (let ((a 2))
	       ((lambda () 
		  (define d 9)
		  (set! a d)))
	       (define b 6)
	       (define (c) 7)
	       (+ a b (c))))

(a-macroexpand '(def-var ai 50))

(begin
  (newline)
  ;(pretty-print (a-macroexpand '(def-class (<aiai>)
;				  (c-display 2)
;				  (def-method (hello) 50))))
  (pretty-print (a-macroexpand '(def-class (<aiai>)
				  (c-display 2)
				  (def-method (hello) 50))))
  (newline))

|#


(define cached-exprs (make-hash-table 251))

(define (cache-to-disk-do filename expr)
  (set! filename (symbol-append '|gencode/| filename))
  (set! filename (string->symbol (apply string-append (map (lambda (c)
							     (cond ((char=? c #\?) "_q_")
								   ((char=? c #\!) "_e_")
								   ((char=? c #\:) "_c_")
								   (else
								    (string c))))
							   (string->list (symbol->string filename))))))
  (c-display "cache-to-disk" filename (car expr))
  (let* ((scm (string-append (symbol->string filename) ".scm"))
	 (scc (string-append (symbol->string filename) ".scc"))
	 (cache-it! (lambda (expanded)
		      (c-display "Generating new file " scm)
		      (with-output-to-file scm
			(lambda ()
			  (newline)
			  (pretty-print expanded)
			  (newline)))
		      (hashq-set! cached-exprs filename expanded))))
    (if (or (not (file-is-file? scm))
	    (not (file-is-file? scm)))
	(cache-it! (a-macroexpand expr))	 
	(if #f
	    (let* ((expanded (a-macroexpand expr))
		   (cached (hashq-ref cached-exprs filename)))
	      (if (or (not cached)
		      (not (equal? cached expanded)))
		  (cache-it! expanded)))
	    (let ((cached (hashq-ref cached-exprs filename)))
	      (if (or (not cached)
		      (not (equal? cached expr)))
		  (cache-it! expr)))))
    (c-import-do filename)))



(c-define-macro (cache-to-disk filename expr)
  (cache-to-disk-do filename expr))

#|
(cache-to-disk cache-test (+ 5 6))
(cache-to-disk cache-test (def-class (<aiai>)
			    (c-display 2)
			    (def-method (hello) 50)))

(a-macroexpand '(def-class (<gakk>)
		  (def-method (iaia) 20)
		  (def-method (aiai) (this->iaia))))


(def-class (<aiai>)
  (c-display 3)
  (def-method (hello)
    (+ s 50))
  (def-method (hello2)
    (c-display "hello!")
    (+ 2 (this->hello3))))


(def-class (<aiai>)
  (add-traces
   (c-display 3)
   (def-method (hello)
     (+ s 50))
   (def-method (hello2)
     (c-display "hello!")
     (+ 2 (this->hello3)))))

(a-macroexpand '(add-traces
		 (c-display 3)
		 (def-method (hello)
		   (+ s 50))
		 (def-method (hello2)
		   (c-display "hello!")
		   (+ 2 (this->hello3)))))

(a-macroexpand '(add-traces
		 (c-display 3)
		 (c-display 4)))

(a-macroexpand '(fix-defines
		 (c-display 3)
		 (c-display 4)))


(let ()
  (<aiai>))

(define a (<aiai>))

(define (acall)
  (-> a hello2))
(acall)
(bt)

(load "<aiai>-class.scm")


(a-macroexpand '(define* (ai b)
		  b))

(load "various.scm")
(c-import oo)

|#


