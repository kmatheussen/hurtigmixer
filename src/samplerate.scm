



(define srate 0.5)
(define last-val 0)
(define next-read-pos 0)
(define next-val 0)
(define pos 0)

(define (init-ratechange input-func)
  (set! last-val (input-func))
  (set! next-val (input-func))
  (set! next-read-pos 1))


(define (change-rate input-func)
  (while (< next-read-pos 0)
	 (set! last-val next-val)
	 (set! next-val (input-func))
	 (set! next-read-pos (1+ next-read-pos)))
  (let ((ret (scale 0 next-read-pos (1+ next-read-pos) last-val next-val)))
    (set! next-read-pos (- next-read-pos srate))
    ret))


(define (change-rate-block num-frames block-input-func)
  (define block-pos 0)
  (define block-len (-> audiobuffers bufferSize))
  (define block (block-input-func block-len))

  (for i 0 < num-frames
       (change-rate (lambda ()
		      (let ((ret block[block-pos]))
			(set! block-pos (1+ block-pos))
			(when (> block-pos block-len)
			      (set! block (block-input-func block-len))
			      (set! block-pos 0))
			ret)))))

			      
