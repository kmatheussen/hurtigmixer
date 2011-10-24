

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



(define (make-hash-table initsize)
  (make-hashtable eq?))

(define hashq-set! hashtable/put!)

(define hashq-ref hashtable/get)

(define hash-table->alist hashtable->alist)

#|
;; For guile.
(define (hash-table->alist table)
  (hash-fold (lambda (key value s) (cons (cons key value) s)) '() 
	     table))
(define hash->list hash-table->alist)
|#

(define (copy-hash-table table)
  (alist->hashtable (hashtable->alist table)))


#|
(hash-table->alist all-macros)

(define ht (make-hash-table 200))
(hashq-set! ht 'aiai 50)
(hashq-set! ht 'aiai2 60)
(map car (hashtable->alist ht))
|#
