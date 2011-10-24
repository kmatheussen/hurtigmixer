

;(define configpath "http://www.notam02.no/~kjetism/hurtigconfigtest/")
;;(define configpath "/home/kjetil/hurtigmixer/configtest/config.txt")
(when (not (defined? '|*config-URL*|))
  (when (or (not (defined? '*running-melodigenerator*))
	    (not *running-melodigenerator*)
	    is-running-applet
	    is-running-standalone)
    (c-display "i svarte")
    (eval '(define *config-URL* (-> (new-static <Events>) configURL)))))



;;(set! *config-URL* "http://www.notam02.no/hurtigmikser/configtest/config.txt")
;;(set! *config-URL* "http://ultimaskinen.no/5/config09_5.txt")

;;(eval '(define *config-URL* "/hom/kjetism/hurtigmixer/melodigenerator/config.txt"))

(c-display "***************************************")
(c-display "CONFIG_URL: " *config-URL*)
(c-display "***************************************")


(define das-config (make-hash-table 519))

(define (string-split string tosplit)
  (let* ((sver (new <java.lang.String> string))
         (splitted (<-o (-> sver split tosplit)))
         (ret '()))
    (c-for 0 < (java-array-length splitted) 1
           (lambda (n)
             (push! (->string (java-array-ref splitted n)) ret)))
    (reverse! ret)))
#|
(string-split "a b d d" " ")
(string-split "a b d   d" " ")
|#

(define (string-trim string)
  (-> (new <java.lang.String> string) trim))
#|
(string-trim "hello ")
|#

(define config-links '())
(define config-link-texts '())
(define config-link-areas '())
(define config-sound-icons '())
(define config-vertical-sounds '())
(define config-horizontal-sounds '())
(define config-sound-selector-buttons '())


(define (read-config-file)
  (when (not (string=? "" *config-URL*))
    (set! das-config (make-hash-table 519))
    (set! config-links '())
    (set! config-link-texts '())
    (set! config-link-areas '())
    (set! config-sound-icons '())
    (set! config-vertical-sounds '())
    (set! config-horizontal-sounds '())
    (set! config-sound-selector-buttons '())
    (for-each (lambda (line)
                (define hepp (remove (lambda (string)
                                       (string=? "" string))
                                     (map string-trim (string-split line " "))))
                (when (> (length hepp) 1)
                  (cond ((string=? "link-text" (car hepp))
			 (push! (list (nth 1 hepp)
				      (string->number (nth 2 hepp))
				      (string->number (nth 3 hepp))
				      (string->number (nth 4 hepp))
				      (string->number (nth 5 hepp))
				      (string->number (nth 6 hepp))
				      (string->number (nth 7 hepp))
				      (string->number (nth 8 hepp))
				      (nth 9 hepp))
				config-link-texts))
			((string=? "link-area" (car hepp))
			 (push! (list (string->number (nth 1 hepp))
				      (string->number (nth 2 hepp))
				      (string->number (nth 3 hepp))
				      (string->number (nth 4 hepp))
				      (nth 5 hepp))
				config-link-areas))
			((string=? "link" (car hepp))
			 (push! (list (nth 1 hepp)
				      (string->number (nth 2 hepp))
				      (string->number (nth 3 hepp))
				      (nth 4 hepp))
				config-links))
			((string=? "sound-icon" (car hepp))
			 (push! (list (nth 1 hepp)
				      (nth 2 hepp)
				      (string->number (nth 3 hepp))
				      (string->number (nth 4 hepp)))
				config-sound-icons))
			((string=? "vertical-sounds" (car hepp))
			 (push! (list (string->number (nth 1 hepp))
				      (string->number (nth 2 hepp))
				      (string->number (nth 3 hepp))
				      (string->number (nth 4 hepp))
				      (zipn 2 (map config-full-url (nth-cdr 5 hepp))))
				config-vertical-sounds))
			((string=? "horizontal-sounds" (car hepp))
			 (push! (list (string->number (nth 1 hepp))
				      (string->number (nth 2 hepp))
				      (string->number (nth 3 hepp))
				      (string->number (nth 4 hepp))
				      (zipn 2 (map config-full-url (nth-cdr 5 hepp))))
				config-horizontal-sounds))
			((string=? "sound-selector-button" (car hepp))
			 (push! (list (string->number (nth 1 hepp))
				      (string->number (nth 2 hepp))
				      (string->number (nth 3 hepp))
				      (string->number (nth 4 hepp))
				      (nth 5 hepp)
				      (nth 6 hepp)
				      (nth 7 hepp)
				      (string->number (nth 8 hepp))
				      (string->number (nth 9 hepp))
				      (string->number (nth 10 hepp))
				      (string->number (nth 11 hepp))
                                      (nth-cdr 12 hepp)
                                      )
				config-sound-selector-buttons))
			(else
			 (hashq-set! das-config (string->symbol (car hepp)) (cdr hepp)))))
                (c-display "-" (length hepp) hepp))
	      (remove (lambda (line)
			(or (string=? "" line)
			    (char=? #\# (car (string->list line)))))
		      (map string-trim (get-all-lines-in-file *config-URL*))))))
#|
(read-config-file)
|#

(define (get-config-links)
  config-links)

(define (get-config-link-texts)
  config-link-texts)

(define (get-config-link-areas)
  config-link-areas)

(define (get-config-sound-icons)
  config-sound-icons)

(define (get-config-vertical-sounds)
  config-vertical-sounds)

(define (get-config-horizontal-sounds)
  config-horizontal-sounds)

(define (get-config-sound-selector-buttons)
  (define x-offset 0)
  (define y-offset 0)
  (when (get-config-area-list-with-help 'background-image-pos)
    (set! x-offset (car (get-config-area-list-with-help 'background-image-pos)))
    (set! y-offset (cadr (get-config-area-list-with-help 'background-image-pos))))
  (map (lambda (hepp)
         (apply (lambda (x y . rest)
                  `(,(- x x-offset) ,(- y y-offset) ,@rest))
                hepp))
       config-sound-selector-buttons))

(define (get-config-string key)
  (and (hashq-ref das-config key)
       (car (hashq-ref das-config key))))

(c-define (get-config-number-list key :optional callback)
  (define res (and (hashq-ref das-config key)
		   (map string->number (hashq-ref das-config key))))
  (if (and res callback)
      (apply callback res)
      res))

(c-define (get-config-area-list-with-help key :optional callback)
  (define res (and (hashq-ref das-config key)
		   (let ((l (hashq-ref das-config key)))
		     (if (< (length l) 5)
			 (map string->number l)
			 (list (string->number (car l))
			       (string->number (cadr l))
			       (string->number (caddr l))
			       (string->number (cadddr l))
			       (let loop ((ret "")
					  (l (cddddr l)))
				 (if (null? l)
				     ret
				     (loop (string-append ret (if (string=? "" ret) "" " ") (car l))
					   (cdr l)))))))))
  (if (and res callback)
      (apply callback res)
      res))

(define (get-config-number key)
  (and (hashq-ref das-config key)
       (string->number (car (hashq-ref das-config key)))))

(define (config-full-url filename)
  (c-display "filename:" filename "config-url:" *config-URL*)
  (cond ((-> (new <java.lang.String> filename) startsWith "http://")
	 filename)
	((-> (new <java.lang.String> filename) startsWith "/")
	 filename)
	((-> (new <java.lang.String> filename) startsWith "file:")
	 (-> (new <java.lang.String> filename) substring 5))
	;;(is-running-standalone
	;; (<-> (-> (new <java.lang.String> *config-URL*) substring
	;;	  5 (1+ (-> (new <java.lang.String> *config-URL*) lastIndexOf "/")))
	;;     filename))
	(else
	 (<-> (-> (new <java.lang.String> *config-URL*) substring
		  0 (1+ (-> (new <java.lang.String> *config-URL*) lastIndexOf "/")))
	      filename))))

#|
(config-full-url "file:/div/notam02/u2/kjetism/localdomain/hurtigmixer/src/config/background_help.png")

(let ((filename "/abc/de/f"))
  (-> (new <java.lang.String> filename) substring
      0 (1+ (-> (new <java.lang.String> filename) lastIndexOf "/"))))

(config-full-url (get-config-string 'background-image))

(config-full-url "hallo")
(config-full-url "http://hallo")
(config-full-url "background.png")
|#
