
(require-library 'sisc/libs/srfi/srfi-1)

(import srfi-1)

(define filter-org filter)

(define (sublist l start end)
  (take (drop l start) (- end start)))
