

;; "a b casdf" -> ("a" "b" "casdf")
(define (text-to-words-0 curr-word chars)
  (cond ((null? chars)
         (list (list->string curr-word)))
        ((char=? #\space (car chars))
         (cons (list->string curr-word)
               (text-to-words-0 '() (cdr chars))))
        ((and (char=? (car (string->list "\\")) (car chars))
              (char=? #\n (cadr chars)))
         (cons (list->string curr-word)
               (cons "\n"
                     (text-to-words-0 '() (cddr chars)))))
        ((and (char=? (car (string->list "\\")) (car chars))
              (char=? #\F (cadr chars)))
         (cons (list->string curr-word)
               (cons "FARGE"
                     (text-to-words-0 '() (cddr chars)))))
        (else
         (text-to-words-0 `(,@curr-word ,(car chars))
                          (cdr chars)))))
(define (text-to-words text)
  (remove (lambda (string)
            (string=? "" string))
          (text-to-words-0 '() (string->list text))))

#|
(text-to-words "one\\n\\n b\\nc two \\Ffour")

(text-to-words "one b cee asdf")
|#


;; ("a" "b" "casdf") -> ("a b" "casdf")
(define (words-to-lines-0 curr-line words too-wide?)
  (define new-line (and (pair? words)
                        (if (string=? curr-line "")
                            (car words)
                            (<-> curr-line " " (car words)))))
  (cond ((null? words)
         (list curr-line))
        ((string=? "\n" (car words))
         (cons curr-line
               (words-to-lines-0 ""
                                 (cdr words)
                                 too-wide?)))
        ((string=? "FARGE" (car words))
         (cons curr-line
               (cons "FARGE"
                     (words-to-lines-0 ""
                                       (cdr words)
                                       too-wide?))))
        ((too-wide? new-line)
         (cons curr-line
               (words-to-lines-0 (car words)
                                 (cdr words)
                                 too-wide?)))
        (else
         (words-to-lines-0 new-line
                           (cdr words)
                           too-wide?))))
(define (words-to-lines words too-wide?)
  (words-to-lines-0 (car words) (cdr words) too-wide?))

#|
(words-to-lines (text-to-words gakk)
                (lambda (s)
                  (> (string-length s) 10)))
(words-to-lines (text-to-words "one\\nb\\n\\nc two three \\Ffour")
                (lambda (s)
                  (> (string-length s) 10)))

(map (lambda (i)
       (list i (words-to-lines (text-to-words "one b c two three") (lambda (s) (> (string-length s) i)))))
     (iota 20))
|#


(def-area-subclass (<text-area-multiline> parent width height text :key mouse-cycle (antialize #t) (text-color black-color) background-color background-alpha font)

  (def-var text-color)

  (def-var text)

  (def-var highlight-color (-> Various getColor 0.73 0.15 0.35))

  (def-method (paint g x* y* x2* y2*)
    (if background-color
	(let ((func (lambda ()
		      (-> g fill-rect background-color x y width height))))
	  (if background-alpha
	      (-> g do-alpha background-alpha func)
	      (func))))

    (define lines (words-to-lines (text-to-words text)
                                  (lambda (word)
                                    (define rect (-> font-default getStringBounds word (-> g getFontRenderContext)))
                                    (> (-> rect getWidth)
                                       (- width 15)))))

    ;;(c-display "lines" lines)

    (let loop ((linenum 0)
               (lines lines)
               (color text-color))
      (when (pair? lines)
        (if (string=? "FARGE" (car lines))
            (loop linenum
                  (cdr lines)
                  highlight-color)
            (let ((doit (lambda ()            
                          (-> g draw-string (car lines) color x (+ y (* linenum 19))))))
              (if font
                  (-> g do-font font doit)
                  (doit))
              (loop (1+ linenum)
                    (cdr lines)
                    color))))))
    
  (def-method (set-and-paint! newtext)
    (set! text newtext)
    (repaint-me!))

  (def-method (set-and-force-paint! newtext)
    (set! text newtext)
    (repaint-me!)
    (-> parent check-paint))


  (if mouse-cycle
      (apply add-mouse-cycle (map (lambda (func)
				    (lambda (button x* y*)
				      (func button x* y* this)
				      ))
				  mouse-cycle)))
  
  )
