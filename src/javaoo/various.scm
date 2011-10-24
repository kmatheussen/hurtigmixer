
#|
  Kjetil Matheussen, 2006
    
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

(define is-running-applet #f)
(define please-dont-trace #t)

(display "Ggakk1\n")
(eval '(begin
	 (import file-manipulation)
	 (case-sensitive #t)
	 (emit-annotations #t)
	 (max-stack-trace-depth 20)))
(display "Ggakk2\n")

(include "debug.scm")

(require-library 'sisc/libs/srfi/srfi-1)
(import srfi-1)

;(define srfi-1-is-compiled #f)
;(if (and (not is-running-applet)
;	 (not (file-is-file? "scc/srfi-1.scc")))
;    (compile-file "srfi-1.scm" "scc/srfi-1.scc"))
;(set! srfi-1-is-compiled #t)
;(load "scc/srfi-1.scc")

(display "Ggakk3\n")


;;; Stupid scheme committees and their definition of map. #@$@#$@#%$@%!!!
;;; (someone should make a real scheme standard, one made for programmers, and not pedants.)
(set! map map-in-order)

(define (defined? symbol)
  (with-failure-continuation
   (lambda (error-record error-k)
     #f)
   (lambda ()
     (eval symbol))))

(display 1)(newline)
(eval '(import hashtable))
(display 2)(newline)

;; Use c-define-macro instead of define-macro, to be able to use expand-a-macro to macroexpand, and get proper debug output. (arrgh)

(if (not (defined? 'all-macros))
    (eval '(begin 
	     (define all-macros (make-hashtable eq?))
	     (define macro-generation 0))))
    
(define-macro (c-define-macro def . body)
  `(begin
     (set! macro-generation (1+ macro-generation))
     ,(if (pair? def)
	  `(hashtable/put! all-macros ',(car def) (lambda ,(cdr def)
						    ,@body))
	  `(hashtable/put! all-macros ',def ,@body))
     (define-macro ,def ,@body)))

(defmacro c-defmacro (name def . body)
  `(c-define-macro (,name ,@def) ,@body))

(display 3)(newline)

(define *gensym-num* -1)
(define (c-gensym . pre)
  (if (or (null? pre)
	  (not (string? pre)))
      (set! pre (list "gen-")))
  (set! *gensym-num* (+ 1 *gensym-num*))
  (symbol-append (string->symbol (car pre)) (string->symbol (number->string *gensym-num*))))
(define (reset-gensym)
  (set! *gensym-num* -1))


#|
(defined? 'gakkgakk)
(defined? 'defined?)
|#

(display 4)(newline)

(when (and #f
	   (defined? 'dodebug)
	   dodebug)
      (eval '(begin
	       (import debugging)
	       (suppressed-stack-trace-source-kinds '()))))

;(import debugging)
;(suppressed-stack-trace-source-kinds '())

#|
(import debugging)
(suppressed-stack-trace-source-kinds '())
(suppressed-stack-trace-source-kinds #f)
(max-stack-trace-depth 3)
|#

(define (symbol-append . args)
  (if #f
      (if (null? args)
	  ""
	  (string->symbol (string-append (symbol->string (car args) (symbol-append (cdr args))))))
      (string->symbol (apply string-append (map symbol->string args)))))

(define (c-for init pred least add proc)
  (do ((n init (+ n add)))
      ((not (pred n least)))
    (proc n)))


(display 5)(newline)

#|
(c-for 2 < 7 1
       (lambda (n) (display n)(newline)))
|#

(define (1+ a)(+ 1 a))
(define (1- a)(- a 1))

(define (c-for-each func . lists)
  (let ((n 0))
    (apply for-each (cons (lambda els
			    (apply func (cons n els))
			    (set! n (1+ n)))
			  lists))))


;; !!!!!!!
(define (c-display . args)
  (let ((printfunc display))
    (c-for-each (lambda (n arg)
		  (if (> n 0)
		      (printfunc " "))
		  (printfunc arg))
		args)
    (newline)))

(display 6)(newline)

;; Redefine pretty-print.
(define (pretty-print obj . opt)
  (let ((port (if (pair? opt) (car opt) (current-output-port))))
    (generic-write obj #f 180 (lambda (s) (display s port))) (void)))

(display 6.5)(newline)

;; Reads the content of a file containing s-expressions (for example a scheme source file), into a list.
(if (not is-running-applet)
    (eval '(define (sourcefile->list fn)
	     (define code
	       (with-input-from-file fn
		 (lambda ()
		   (unfold eof-object? values (lambda _ (read-code))
			   (read-code)))))
	     (syntax-object->datum
	      (datum->syntax-object (syntax code) code)))))


(display 7)(newline)

(define-macro (while test . body)
  `(do ()
       ((not ,test))
       ,@body))

(define (filename-without-path path)
  (let ((chars (reverse! (string->list path)))
	(result '()))
    (while (and (not (null? chars))
		(not (char=? (car chars) #\/)))
	   (set! result (cons (car chars) result))
	   (set! chars (cdr chars)))
    (list->string result)))

#|
(filename-without-path "gakk2")
|#

(display 8)(newline)


;; bug bug bug in sisc @#$@#$!@#$

(define src-directory (if is-running-applet ""
			  (get-parent-url "various.scm")))
(define gencode-directory (string-append src-directory "gencode/"))
(define scc-directory (string-append src-directory "scc/"))

#|
(define gencode-directory "gencode/")
(define scc-directory "scc/")
|#

(define (c-import-do which)
  (let ((scm (string-append src-directory (symbol->string which) ".scm"))
	(scc (string-append scc-directory (filename-without-path (symbol->string which)) ".scc")))
    ;;(c-display "which" which "scc" scc)
	  ;;(c-display "scm" scm)
    ;;(c-display "scc?/scm?" (file-is-file? scm) (file-is-file? scc))
    (begin
      (if (and (file-is-file? scm)
	       (or (not (file-is-file? scc))
		   (>= (file-last-modified scm)
		       (file-last-modified scc))))
	  (begin
	    (c-display "compiling" scm scc)
	    (with-failure-continuation
	     (lambda (error-record error-k)
	       ;;(system (string-append "rm " scc))
	       (file-delete! scc)
	       (c-display "error:" error-record)
	       (error (string-append "could not compile " scm)))
	     (lambda ()
	       (compile-file scm scc))))
	  (begin
	    ;;(c-display "scc" scc)
	    (load scc)))
      (c-display "....." scc "loaded")
      (newline))))


(define c-imported-files '())

(c-define-macro (c-import which)
  `(if is-running-applet
       (when (not (member ',which c-imported-files))
	     (push! ',which c-imported-files)
	     (load ,(string-append "scc/" (symbol->string which) ".scc")))
       (c-import-do ',which)))


#|
(c-define-macro (c-import which)
   `(load ,(string-append "scc/" (symbol->string which) ".scc")))
|#
	
(define (module-name name)
  (c-display "loading module" name))


(c-define-macro (push! val where)
  (let ((ret (c-gensym)))
    `(let ((,ret ,val))
       (set! ,where (cons ,ret ,where))
       ,ret)))

(c-define-macro (push-back! val where)
  (let ((ret (c-gensym)))
    `(let ((,ret ,val))
       (set! ,where (append! ,where (list ,ret)))
       ,ret)))

(c-define-macro (inc! var how-much)
  `(begin
     (set! ,var (+ ,how-much ,var))
     ,var))

(c-import lists)

(c-import hashtable)
(c-import optargs)
(c-import macros)
(c-import trace)
;;(c-import optargs)


(c-define-macro (c-time expr)
  `(cadr (time (eval ',expr))))

(define (cl-car lst)
  (if (null? lst)
      '()
      (car lst)))
(define (cl-cdr lst)
  (if (null? lst)
      '()
      (car lst)))


(c-define-macro (primitive-eval expr)
  `(eval ,expr))

(c-define-macro (define-toplevel symbol val)  
  `(primitive-eval `(define ,,symbol ,,val)))


#|
(let ((b 90))
  (define-toplevel 'ai2 b)
  ai2)
|#

(define <-> string-append)

(define (c-integer somekindofnumberorsomething)
;;    somekindofnumberorsomething)
  (inexact->exact (floor somekindofnumberorsomething)))

(define (c-scale x x1 x2 y1 y2)
  (+ y1
     (/ (* (- x x1)
	   (- y2 y1))
	(- x2 x1))))





(c-define-macro (letrec* vardecls . body)
  (let* ((sets '())
	 (newvardecls (map (lambda (vardecl)
			     (if (not (number? (cadr vardecl)))
				 (begin
				   ;;(c-display "vardecl" vardecl)
				   (set! sets (cons `(set! ,(car vardecl) ,(cadr vardecl)) sets))
				   `(,(car vardecl) #f))
				 vardecl))
			   vardecls)))
    ;;(c-display "sets" sets)
    `(let* ,newvardecls
       ,@(reverse! sets)
       (let ()
	 ,@body))))

#|
(c-macroexpand '(letrec* ((a (+ d 2))
			   (b (lambda () (+ (c) a d)))
			   (c (lambda () 7))
			   (d 6))
		   (+ a (b)))
)

->
(let* ((a #f)
       (b #f)
       (c #f)
       (d 6))
  (set! a (+ d 2))
  (set! b (lambda () (+ (c) a d)))
  (set! c (lambda () 7))
  (+ a (b)))
|#




(c-define-macro (when cond . rest)
  `(if ,cond
       (begin
	 ,@rest)))




