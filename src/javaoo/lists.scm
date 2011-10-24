
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


;; srfi-2 copied from guile.
(c-defmacro and-let* (vars . body)

  (define (expand vars body)
    (cond
     ((null? vars)
      (if (null? body)
	  #t
	  `(begin ,@body)))
     ((pair? vars)
      (let ((exp (car vars)))
        (cond
         ((pair? exp)
          (cond
           ((null? (cdr exp))
            `(and ,(car exp) ,(expand (cdr vars) body)))
           (else
            (let ((var (car exp)))
              `(let (,exp)
                 (and ,var ,(expand (cdr vars) body)))))))
         (else
          `(and ,exp ,(expand (cdr vars) body))))))
     (else
      (error "not a proper list" vars))))

  (expand vars body))


;;(define filter-org filter)

(define (sublist l start end)
  (take (drop l start) (- end start)))

(define (c-butlast l)
  (if (null? l)
      l
      (reverse! (cdr (reverse l)))))

