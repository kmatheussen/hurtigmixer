
;; From http://www.engin.umd.umich.edu/CIS/course.des/cis400/scheme/sorts.html

(define (sorted? seq less?)
    (cond
         ((null? seq)
             #t)
         ((vector? seq)
             (let ((n (vector-length seq)))
                 (if (<= n 1)
                     #t
                     (do ((i 1 (+ i 1)))
                         ((or (= i n)
                              (less? (vector-ref seq (- i 1))
                                     (vector-ref seq i)))
                             (= i n)) )) ))
         (else
             (let loop ((last (car seq)) (next (cdr seq)))
                 (or (null? next)
                     (and (not (less? (car next) last))
                          (loop (car next) (cdr next)) )) )) ))


;;; (merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (merge a b less?) less?).
;;; Note:  this does _not_ accept vectors.  See below.

(define (merge a b less?)
    (cond
         ((null? a) b)
         ((null? b) a)
         (else (let loop ((x (car a)) (a (cdr a)) (y (car b)) (b (cdr b)))
             ;; The loop handles the merging of non-empty lists.  It has
             ;; been written this way to save testing and car/cdring.
             (if (less? y x)
                 (if (null? b)
                     (cons y (cons x a))
                     (cons y (loop x a (car b) (cdr b)) ))
                 ;; x <= y
                 (if (null? a)
                     (cons x (cons y b))
                     (cons x (loop (car a) (cdr a) y b)) )) )) ))


;;; (merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(define (merge! a b less?)
    (define (loop r a b)
         (if (less? (car b) (car a))
             (begin
                 (set-cdr! r b)
                 (if (null? (cdr b))
                     (set-cdr! b a)
                     (loop b a (cdr b)) ))
             ;; (car a) <= (car b)
             (begin
                 (set-cdr! r a)
                 (if (null? (cdr a))
                     (set-cdr! a b)
                     (loop a (cdr a) b)) )) )
    (cond
         ((null? a) b)
         ((null? b) a)
         ((less? (car b) (car a))
             (if (null? (cdr b))
                 (set-cdr! b a)
                 (loop b a (cdr b)))
             b)
         (else ; (car a) <= (car b)
             (if (null? (cdr a))
                 (set-cdr! a b)
                 (loop a (cdr a) b))
             a)))

#|
(merge! (list 1 2 3 4 5 3 5) (list 2 4) <)
|#

;;; (sort! sequence less?)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (sort! seq less?)
    (define (step n)
         (cond
             ((> n 2)
                 (let* ((j (quotient n 2))
                        (a (step j))
                        (k (- n j))
                        (b (step k)))
                     (merge! a b less?)))
             ((= n 2)
                 (let ((x (car seq))
                       (y (cadr seq))
                       (p seq))
                     (set! seq (cddr seq))
                     (if (less? y x) (begin
                         (set-car! p y)
                         (set-car! (cdr p) x)))
                     (set-cdr! (cdr p) '())
                     p))
             ((= n 1)
                 (let ((p seq))
                     (set! seq (cdr seq))
                     (set-cdr! p '())
                     p))
             (else
                 '()) ))
    (if (vector? seq)
         (let ((n (vector-length seq))
               (vector seq))                     ; save original vector
             (set! seq (vector->list seq))       ; convert to list
             (do ((p (step n) (cdr p))           ; sort list destructively
                  (i 0 (+ i 1)))                         ; and store elements back
                 ((null? p) vector)              ; in original vector
                 (vector-set! vector i (car p)) ))
         ;; otherwise, assume it is a list
         (step (length seq)) ))


;;; (sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence
(define (sort seq less?)
  (if (vector? seq)
      (list->vector (sort! (vector->list seq) less?))
      (sort! (append seq '()) less?)))

#|
(sort! (list 0 12 6 2) <)
(sort! (map symbol->string (list 'trwb 'aer 'nwr)) string<=?)

|#

