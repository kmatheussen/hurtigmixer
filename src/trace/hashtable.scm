

(define (make-hash-table initsize)
  (make-hashtable eq?))

(define hashq-set! hashtable/put!)

(define hashq-ref hashtable/get)

(define hash-table->alist hashtable->alist)

#|
;; For guile.
(define (hash-table->alist table)
  (hash-fold (lambda (key value s) (cons (cons key value) s)) '() 
	     methods)))
|#

(define (copy-hash-table table)
  (alist->hashtable (hashtable->alist table)))


#|
(define ht (make-hash-table 200))
(hashq-set! ht 'aiai 50)
(hashq-set! ht 'aiai2 60)
(map car (hashtable->alist ht))
|#
