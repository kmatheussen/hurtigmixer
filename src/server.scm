


(define user-name (-> (new-static <Events>) userName))
(define *getfiles-URL* (-> (new-static <Events>) getFilesURL))
(define *upload-URL* (-> (new-static <Events>) uploadURL))
(define *lydsetURL* (-> (new-static <Events>) lydsettURL))
(define *ask-name* (string=? (-> (new-static <Events>) askName) "true"))


#|
(set! *lydsetURL* "http://drivhuset.musikkverksted.no/grieg-hmx-lydsett/")
|#

(define static-javax.swing.JOptionPane (new-static <javax.swing.JOptionPane>))
(define Upload (new-static <Upload>))

(define (show-message-dialog message)
  (-> static-javax.swing.JOptionPane showMessageDialog (new <java.awt.Frame>) (new <java.lang.String> message)))

(define (get-string-dialog message)
  (-> Various getStringDialog message))


(define *temp-directory*
  (let ()
    (define temp-file (-> static-java.io.File createTempFile "hurtigmixer" ".tmp"))
    (<-> (-> (-> temp-file getParentFile) toString) (-> static-java.io.File separator))))

#|
(begin *temp-directory*)
(-> static-java.io.File separator)
|#			  



(c-define (get-all-lines-in-file file)
  (with-failure-continuation
   (lambda (error-record error-k)
     (c-display "file" file "not found in get-all-lines-in-file" error-record error-k)
     (list "error:"
	   (<-> "\"" file "\"not found.")))
   (lambda ()
     (with-input-from-file file
       (lambda ()
	 (define string "")
	 (let loop ()
	   (define temp (make-string 1000 #\newline))
	   (define read (read-string temp 0 990))
	   (when (and (number? read)
		      (> read 0))
		 (set! string (<-> string (substring temp 0 read)))
		 (loop)))
	 (c-display "aiaix" string)
	 (let* ((sver (new <java.lang.String> string))
		(splitted (<-o (-> sver split "\n")))
		(ret '()))
	   (c-for 0 < (java-array-length splitted) 1
		  (lambda (n)
		    (push! (->string (java-array-ref splitted n)) ret)))
	   (reverse! ret)))))))
					 


(define (get-userfiles)
  (get-all-lines-in-file (<-> *getfiles-URL* "?username=" user-name)))



(define (get-musikkverkstedfiles)
  (get-all-lines-in-file *getfiles-URL*))


(define (get-top-sounds)
  (define (loop image sound . rest)
    (cons (list (<-> *lydsetURL* image)
		(<-> *lydsetURL* sound))
	  (if (null? rest)
	      '()
	      (apply loop rest))))
  (apply loop (get-all-lines-in-file (<-> *lydsetURL* "list.txt"))))
			      


(c-define (save-on-server parent)
  (if (not (are-encoders-available?))
      (show-message-dialog "Kan ikke lagre på server. Kanskje du er bak en brannmur?")
      ;;(define filename (get-string-dialog (<-> "Filnavn: (boer ende paa ." extension ")")))
      (new <SoundTypeRequester> "Velg format og navn" #f #f #t *ask-name*
	   (java-wrap (lambda (filename extension)
			(define encode-func (if (string=? "mp3" extension) save-mp3 save-ogg))
			
			(when (and filename
				   (not (string=? "" filename)))
			  (define tempfilename (get-tempfilename extension))
			  (encode-func parent
				       ':tempfilename tempfilename
				       ':cont (lambda ()
						(c-display 'a1)
						(define file (new <java.io.File> tempfilename))
						(c-display 'a2)
						(define len (-> file length))
						(c-display 'a5)
						(set-message-area! (<-> "Laster opp " extension))
						(-> Upload upload filename file *upload-URL* user-name 
						    (java-wrap
						     (lambda (bytesread)
						       (c-display bytesread len)
						       (set-progress-bar! bytesread len ':synchronized #f)
						       ;;(set-message-area-progress! "" bytesread len " opplastet." ':synchronized #f)
						       (c-display 'ai))))
						(set-message-area! "ferdig!")
						(set-progress-bar! 1 1)
						(run-synchronized (lambda ()
								    (update-soundlists)
								    (-> parent check-paint)))))))))))
  
  


