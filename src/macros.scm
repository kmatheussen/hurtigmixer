
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



(define schemecodeparser-varlist '())

(define (schemecodeparser-get-varlist)
  schemecodeparser-varlist)


(define* (schemecodeparser expr :key elsefunc atomfunc nullfunc pairfunc use-customsymbolhandler? customsymbolhandler blockhandler symbolhandler (varlist '()))

  (let parse ((varlist varlist)
	      (expr expr))

    (define (blockhandlerfunc varlist expr)
      (if (not blockhandler)
	  (map (lambda (expr)
		 (parse varlist expr))
	       expr)
	  (begin
	    (set! schemecodeparser-varlist varlist)
	    (blockhandler expr))))

    ;; Like append, but varlist1 might not be a valid list (in case of optional arguments)
    (define (append-varlists varlist1 varlist2)
      (append (let append ((varlist varlist1))
		(cond ((null? varlist) varlist)
		      ((not (pair? varlist)) (list varlist))
		      (else (cons (car varlist)
				  (append (cdr varlist))))))
	      varlist2))
		       
    (set! schemecodeparser-varlist varlist)

    ;;(c-display "scp/expr:" expr)

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
	       (blockhandlerfunc varlist expr)))
	  ((and use-customsymbolhandler?
		(use-customsymbolhandler? expr))
	   (customsymbolhandler expr))
	  ((eq? 'lambda (car expr))
	   `(lambda ,(cadr expr)
	      ,@(blockhandlerfunc (append-varlists (cadr expr) varlist) (cddr expr))))
	  ((eq? 'define (car expr))
	   `(define ,(cadr expr)
	      ,@(blockhandlerfunc (append-varlists (cadr expr) varlist) (cddr expr))))
	  ((eq? 'delay (car expr))
	   `(delay ,@(blockhandlerfunc varlist (cdr expr))))
	  ((eq? 'force (car expr))
	   `(force ,@(blockhandlerfunc varlist (cdr expr))))
	  ((eq? 'do (car expr))
	   (let* ((newvars (append (map car (cadr expr)) varlist))
		  (first (map (lambda (a)
				(let ((second (parse varlist (cadr a))))
				  `(,(car a) ,second ,@(blockhandlerfunc newvars (cddr a)))))
			      (cadr expr))))
	     `(do ,first
		  ,@(blockhandlerfunc newvars (cddr expr)))))
	  ;; named let
	  ((and (eq? 'let (car expr))
		(symbol? (cadr expr)))
	   (let* ((newvars (append (cons (car expr) (map car (caddr expr)))
				   varlist))
		  (vars (map (lambda (a)
			       `(,(car a) ,@(blockhandlerfunc varlist (cdr a))))
			     (caddr expr))))
	     `(let ,(cadr expr) ,vars
		   ,@(blockhandlerfunc newvars (cdddr expr)))))
	  ((eq? 'let (car expr))
	   (let ((vars (map (lambda (a)
			 `(,(car a) ,@(blockhandlerfunc varlist (cdr a))))
		       (cadr expr))))
	     `(let ,vars
		,@(blockhandlerfunc (append (map car (cadr expr))
					    varlist)
				    (cddr expr)))))
	  ((eq? 'let* (car expr))
	   (let* ((newvars varlist)
		  ;; This needs to be generated outside quasiquote, because quasiqote elements does not need to be generated in order. (@#$@#$@#$@!!!#$@)
		  ;;   (a new scheme standard is needed, rxrs is very inconvenient and obviously not made by real programmers)
		  (let*vars (map (lambda (a)
				   (let ((ret `(,(car a) ,@(blockhandlerfunc newvars (cdr a)))))
				     (push! (car a) newvars)
				     ret))
				 (cadr expr))))
	     `(let* ,let*vars
		,@(blockhandlerfunc newvars (cddr expr)))))
	  ((eq? 'letrec (car expr))
	   (let* ((newvars (append (map car (cadr expr))
				   varlist))
		  (vars (map (lambda (a)
			      `(,(car a) ,@(blockhandlerfunc newvars (cdr a)))) ;; Not entily correct...
			     (cadr expr)))) 
	     `(letrec ,vars
		,@(blockhandlerfunc newvars (cddr expr)))))
	  ((or (eq? 'quote (car expr))
	       (eq? 'QUOTE (car expr)))
	   expr)
	  ((or (eq? 'quasiquote (car expr))
	       (eq? 'QUASIQUOTE (car expr)))
	   ;;(c-display "Warning in macros.scm/schemecodeparser: quasiquote not handled")
	   expr)
	  ((eq? 'cond (car expr))
	   `(cond ,@(map (lambda (exprs)
			   (let ((test (parse varlist (car exprs))))
			     `(,test ,@(blockhandlerfunc varlist (cdr exprs)))))
			 (cdr expr))))
	  ((eq? 'case (car expr))
	   (let ((first (parse varlist (cadr expr))))
	     `(case ,first
		,@(map (lambda (expr)
			 `(,(car expr) ,@(blockhandlerfunc varlist (cdr expr))))
		       (cddr expr)))))
	  ((and symbolhandler
		(eq? (car expr) (car symbolhandler)))
	   ((cadr symbolhandler) expr))
	  (else
	   (if elsefunc
	       (elsefunc expr)
	       `(,(car expr) ,@(map (lambda (expr)
				      (parse varlist expr))
				    (cdr expr))))))))


  ;; Returns expr (the exact same one as the input), unless its transformed
(define (c-macroexpand-1 expr)
  (if (or (not (pair? expr))
	  (null? expr)
	  (not (symbol? (car expr))))
      expr
      (let ((qua (hashq-ref all-macros (car expr))))
	(if (not qua)
	    (begin
	      ;;(c-display "Error in expand-a-macro. Macro for " expr " Not found.")
	      expr)
	    (apply qua (cdr expr))))))

#|
(c-macroexpand-1 '(dosomething 50))
|#

(define (c-macroexpand expr)
  ;;(c-display "a-mac" expr)
  (schemecodeparser expr
		    ':elsefunc (lambda (expr)
				 (let ((topexpand (c-macroexpand-1 expr)))
				   ;;(c-display "expr/topexpand" expr topexpand)
				   (if (eq? expr topexpand)
				       `(,(car expr) ,@(map c-macroexpand (cdr expr)))
				       (c-macroexpand topexpand))))))



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
(c-macroexpand '(lettest))

(dosomething2 3 4 5)
(c-macroexpand '(dosomething2 3 4 5))

(c-macroexpand '(dosomething 77))

(dosomething3 6 7 8)
(c-macroexpand '(dosomething3 6 7 8))


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
		;	  (let ((temp (c-macroexpand-1 terms)))
		;	    (if (eq? 'define (car temp))
		;		(set! terms temp))))
		      ;;(c-display "terms fdd" terms)
		      (let* ((defines '())
			     (newterms (map (lambda (terms)
						       (if (and (pair? terms)
								(eq? 'define (car terms)))
							   (if (pair? (cadr terms))
							       (begin
								 (push! (car (cadr terms)) defines)
								 `(dont-trace2 (set! ,(car (cadr terms)) (lambda ,(cdr (cadr terms)) ,@(cddr terms)))))
							       (begin
								 (push! (cadr terms) defines)
								 `(dont-trace2 (set! ,(cadr terms) ,@(cddr terms)))))
							   terms))
						     terms)))
			(if (null? defines)
			    (map fix-defines-do terms)
			    `((let* ,(delete-duplicates (map (lambda (name)
							       `(,name #f))
							     (reverse! defines)))
				,@(map fix-defines-do newterms))))))))

(c-define-macro (fix-defines . terms)
  (apply append (map fix-defines-do (map c-macroexpand terms))))

#|
(fix-defines-do '(define (r7rs-dummy) #t))


(begin
  (newline)
  (pretty-print (c-macroexpand '(fix-defines (let ((a 2))
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

(c-macroexpand '(def-var ai 50))

(begin
  (newline)
  ;(pretty-print (c-macroexpand '(def-class (<aiai>)
;				  (c-display 2)
;				  (def-method (hello) 50))))
  (pretty-print (c-macroexpand '(def-class (<aiai>)
				  (c-display 2)
				  (def-method (hello) 50))))
  (newline))

|#


(define cached-exprs (make-hash-table 251))
(define cached-unexpanded-exprs (make-hash-table 251))

;;#|
;; Read the contents of sources generated in previous sessions.

(when (and (not is-running-applet)
	   (not is-running-standalone))
      (c-display "gencode-directory:" gencode-directory)
      (for-each (lambda (filename)
		  (let ((scm (string-append gencode-directory filename))
			(str (reverse! (string->list filename))))
		    ;;(c-display filename)
		    (if (and (> (length str) 4)
			     (char=? #\m (car str))
			     (char=? #\c (cadr str))
			     (char=? #\s (caddr str))
			     (char=? #\. (cadddr str)))
			(let ((filename (let ((str (string->list (string-append "gencode/" filename))))
					  (string->symbol (list->string (sublist str 0 (- (length str) 4)))))))
			  ;;(c-display "2" filename scm)
			  (hashq-set! cached-exprs filename (car (sourcefile->list scm)))))))
		(directory-list gencode-directory))
      (c-display "Finished reading cached data."))

;;|#

(define use-cache-unexpanded #f)

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
  (let* ((scm (string-append gencode-directory (filename-without-path (symbol->string filename)) ".scm"))
	 (scc (string-append scc-directory (filename-without-path (symbol->string filename)) ".scc"))
	 (cache-it! (lambda (expanded)
		      (c-display "Generating new file " scm)
		      (with-output-to-file scm
			(lambda ()
			  (newline)
			  (pretty-print expanded)
			  (newline)))
		      (hashq-set! cached-exprs filename expanded)))
	 (cached-unexpanded? (lambda ()
			       (let ((cached-unexpanded (hashq-ref cached-unexpanded-exprs filename)))
				 (if (and cached-unexpanded
					  (= (car cached-unexpanded) macro-generation)
					  (equal? expr (cadr cached-unexpanded)))
				     (begin
				       (c-display "cached-unexpanded hit (beware, not safe)")
				       #t)
				     #f)))))
    ;;(c-display "cache-to-disk-do" (file-is-file? scc) (file-is-file? scm))
    (if (or (not (file-is-file? scc))
	    (not (file-is-file? scm)))
	(cache-it! (c-macroexpand expr))	 
	(if (or (not use-cache-unexpanded)
		(not (cached-unexpanded?)))
	    (let* ((expanded (c-macroexpand expr))
		   (cached (hashq-ref cached-exprs filename)))
	      (if (or (not cached)
		      (not (equal? cached expanded)))
		  (begin
		    ;;(c-display "about to cache, cached:" (if cached #t #f) ", equal? " (equal? cached expanded) cached expanded)
		    (cache-it! expanded))))))
    (hashq-set! cached-unexpanded-exprs filename (list macro-generation expr))
    (c-import-do filename)))

(if (or is-running-applet
	is-running-standalone)
    (set! cache-to-disk-do
	  (lambda (filename expr)
	    (set! filename (string->symbol (apply string-append (map (lambda (c)
								       (cond ((char=? c #\?) "_q_")
									     ((char=? c #\!) "_e_")
									     ((char=? c #\:) "_c_")
									     (else
									      (string c))))
								     (string->list (symbol->string filename))))))
	    (c-display "loading" "scc/" (symbol->string filename) ".scc")
	    (load (string-append src-directory "scc/" (symbol->string filename) ".scc")))))


(c-define-macro (cache-to-disk filename expr)

  (cache-to-disk-do filename expr))

#|
(cache-to-disk cache-test (+ 5 6))
(cache-to-disk cache-test (def-class (<aiai>)
			    (c-display 2)
			    (def-method (hello) 50)))

(c-macroexpand '(def-class (<gakk>)
		  (def-method (iaia) 20)
		  (def-method (aiai) (iaia))))


(def-class (<aiai>)
  (c-display 3)
  (def-method (hello)
    (+ s 50))
  (def-method (hello2)
    (c-display "hello!")
    (+ 2 (hello3))))


(def-class (<aiai>)
  (add-traces
   (c-display 3)
   (def-method (hello)
     (+ s 50))
   (def-method (hello2)
     (c-display "hello!")
     (+ 2 (this->hello3)))))

(c-macroexpand '(add-traces
		 (c-display 3)
		 (def-method (hello)
		   (+ s 50))
		 (def-method (hello2)
		   (c-display "hello!")
		   (+ 2 (this->hello3)))))

(c-macroexpand '(add-traces
		 (c-display 3)
		 (c-display 4)))

(c-macroexpand '(fix-defines
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


(c-macroexpand '(define* (ai b)
		  b))

(load "various.scm")
(c-import oo)

|#


