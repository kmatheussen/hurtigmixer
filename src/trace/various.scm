
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


(import file-manipulation)

(case-sensitive #t)
(emit-annotations #t)

(max-stack-trace-depth 20)

(load "debug.scm")

(define (defined? symbol)
  (with-failure-continuation
   (lambda (error-record error-k)
     #f)
   (lambda ()
     (eval symbol))))



(import hashtable)



;; Use c-define-macro instead of define-macro, to be able to use expand-a-macro to macroexpand, and get proper debug output. (arrgh)

(if (not (defined? 'all-macros))
    (eval '(define all-macros (make-hashtable eq?))))

(define-macro (c-define-macro def . body)
  `(begin
     (hashtable/put! all-macros ',(car def) (list (quote ,(cdr def)) (quote ,body)))
     (define-macro ,def ,@body)))
(defmacro c-defmacro (name def . body)
  `(begin
     (hashtable/put! all-macros ',name (list (quote ,def) (quote ,body)))
     (defmacro ,name ,def ,@body)))


(define *gensym-num* -1)
(define (c-gensym . pre)
  (if (null? pre)
      (set! pre (list "gen-")))
  (set! *gensym-num* (+ 1 *gensym-num*))
  (symbol-append (string->symbol (car pre)) (string->symbol (number->string *gensym-num*))))
(define (reset-gensym)
  (set! *gensym-num* -1))


#|
(defined? 'gakkgakk)
(defined? 'defined?)
|#

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


(define (c-import-do which)
  (let ((scm (symbol->string (symbol-append which '|.scm|)))
	(scc (symbol->string (symbol-append which '|.scc|))))    
    (c-display "scc" scc)
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
	  (load scc))
      (c-display "....." scc "loaded"))))

(c-define-macro (c-import which)
  `(c-import-do ',which))

(define (module-name name)
  (c-display "loading module" name))


(c-define-macro (push! val where)
  `(set! ,where (cons ,val ,where)))

(c-define-macro (push-back! val where)
  `(set! ,where (append ,where (list ,val))))

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


(define (map-in-order2 a b)
  (map-in-order a b))

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
				   (c-display "vardecl" vardecl)
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
(expand-a-macro '(letrec* ((a (+ d 2))
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




