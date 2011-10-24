

(c-define (get-chroma note)
  (define closest (c-integer note))
  (define above (- note closest))
  (+ (modulo closest 12)
     above))

;; note is a float
(define (find-above-note-in-grid-old note)
  (define on-offs (-> scaleselect-area on-offs))
  (define start (c-integer note))
  (call/cc (lambda (return)
	     (c-for start < (+ start 13) 1
		    (lambda (note*)
		      (define chroma* (get-chroma note*))
		      (if (and (>= note* note)
			       (vector-ref on-offs chroma*))
			  (return note*)))))))

(define (find-above-note-in-grid note)
  (define on-offs (-> scaleselect-area on-offs))
  (let loop ((note* (c-integer note)))
    (define chroma* (get-chroma note*))
    (if (and (>= note* note)
	     (vector-ref on-offs chroma*))
	note*
	(loop (1+ note*)))))

;; note is a float
(define (find-below-note-in-grid-old note)
  (define on-offs (-> scaleselect-area on-offs))
  (define start (c-integer note))
  (call/cc (lambda (return)
	     (c-for start > (- start 13) -1
		    (lambda (note*)
		      (define chroma* (get-chroma note*))
		      (if (and (<= note* note)
			       (vector-ref on-offs chroma*))
			  (return note*)))))))

(define (find-below-note-in-grid note)
  (define on-offs (-> scaleselect-area on-offs))
  (let loop ((note* (c-integer note)))
    (define chroma* (get-chroma note*))
    (if (and (<= note* note)
	     (vector-ref on-offs chroma*))
	note*
	(loop (1- note*)))))


(c-define (get-note-probability-list note gravity)
  (define weights (-> scaleselect-area weights))
  (define on-offs (-> scaleselect-area on-offs))
  (map (lambda (notenum)
         (define chroma (get-chroma notenum))
         (define distance (abs (- notenum note)))
         (define weight (if (vector-ref on-offs chroma)
                            (vector-ref weights chroma)
                            0.0))
         (define probability (* weight (/ 1 (expt (1+ distance) gravity))))
         (list notenum probability))
       (map (lambda (a) (+ a lower)) (iota (- upper lower)))))

#|
(get-note-probability-list 28)
(fold + 0 (map last (get-note-probability-list 28)))
|#

(c-define (get-probable-note note-probability-list)
  (define sum (fold + 0 (map cadr note-probability-list)))
  (define pick-point (between 0.0 sum))
  (let loop ((plist note-probability-list)
             (sum 0.0))
    (define new-sum (+ sum (cadr (car plist))))
    (if (>= new-sum pick-point)
        (car (car plist))
        (loop (cdr plist)
              new-sum))))
#|
(get-probable-note (get-note-probability-list 28))
|#

(c-define (map-note-to-grid note)
  (get-probable-note (get-note-probability-list note 2)))

(c-define (map-note-to-grid-old note)
  (define weights (-> scaleselect-area weights))

  (define above (find-above-note-in-grid note))
  (define below (find-below-note-in-grid note))

  (define above-distance (- above note))
  (define below-distance (- note below))

  (cond ((= 0 above-distance)
	 above)
	((= 0 below-distance)
	 below)
	(else

	 (define above-chroma (get-chroma above))
	 (define below-chroma (get-chroma below))

	 (define above-weight (vector-ref weights above-chroma))
	 (define below-weight (vector-ref weights below-chroma))

	 (define above-val (* (/ 1.0 above-distance)
			      above-weight))
	 (define below-val (* (/ 1.0 below-distance)
			      below-weight))

	 (if (> above-val below-val)
	     above
	     below))))
			      


(define remap-notes-to-grid-generation 0)
(define remap-lock (<-o (new <java.lang.Object>)))  ;; avoid more than one thread running remap-notes-to-grid. (works fine, but uses resources to do so)

(define (remap-notes-to-grid)
  (define notes (list-copy *notes*)) ;; Make a private copy in case notes are deleted
  (define is-not-free (any (lambda (a) a) (vector->list (-> scaleselect-area on-offs))))
  (define my-generation (inc! remap-notes-to-grid-generation 1))
  
  (spawn-thread
   (lambda ()
     (java-synchronized remap-lock
        (lambda ()
          (let loop ((notes notes))
            (if (and (pair? notes)
                     (= remap-notes-to-grid-generation my-generation))
                (let* ((note (car notes))
                       (notenum (-> note orgnote))
                       (newnote (if is-not-free
                                    (map-note-to-grid notenum)
                                    notenum)))
                  (-> note set-note! newnote)
                  (loop (cdr notes)))))))
     (run-synchronized
      (lambda ()
        (-> *pianoroll* repaint-me!)
        (-> win check-paint))))))



(define (remap-notes-to-grid-org)
  (if (any (lambda (a) a) (vector->list (-> scaleselect-area on-offs)))
      (for-each (lambda (note)
                  (let ((newnote (map-note-to-grid (-> note orgnote))))
                    (-> note set-note! newnote)))
                *notes*)
      (for-each (lambda (note)
                  (-> note set-note! (-> note orgnote)))  ;; free
                *notes*)))


