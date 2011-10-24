
(define-macro (push! val where)
  (let ((ret (gensym)))
    `(let ((,ret ,val))
       (set! ,where (cons ,ret ,where))
       ,ret)))


(define undos '())
(define redos '())

(define (add-undo redo undo)
  (push! (list redo undo) undos)
  (set! redos '()))

(define (add-undo-run redo undo)
  (add-undo redo undo)
  (redo))

(define (undo)
  (if (not (null? undos))
      (let ((func (car undos)))
	(set! undos (cdr undos))
	(push! func redos)
	((cadr func)))))


(define (redo)
  (if (not (null? redos))
      (let ((func (car redos)))
	(set! redos (cdr redos))
	(push! func undos)
	((car func)))))


(define (reset-undo!)
  (set! undos '())
  (set! redos '()))

#|
(define a 9)
(display a)
(add-undo-run (lambda ()
		(set! a -5))
	      (let ((org-a a))
		(lambda ()
		  (set! a org-a))))

(display a)
(undo)
(display a)
(redo)
(display a)
|#


