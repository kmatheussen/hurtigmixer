

(def-class (<filerequester> parent :optional directory)

  
  (define frame (if is-running-applet
		    (new <java.awt.Frame>)
		    (-> parent get-frame)))


  (define jfile-chooser (new <FileRequester>))
  ;;(-> jfile-chooser setMinimumSize (new <java.awt.Dimension> 500 700))

  (-> jfile-chooser setSize 500 800)
      
  (define size (-> jfile-chooser getSize))


  (define jfile-filter (new <JFileFilter>))
  (for-each  (<- jfile-filter addType)
	     '("mp3" "mp2" "ogg" "flac" "au" "snd" "wav" "aif" "aifc" "aiff" "wave"))
  (-> jfile-chooser addChoosableFileFilter jfile-filter)
  

  (define (request is-loading text callback newdirectory)
    ;;(c-display "filerequest" is-loading text callback newdirectory)
    (-> jfile-chooser setDialogTitle text)
    (-> jfile-chooser setSize size)
    
    (-> jfile-chooser myShowDialog
	is-loading
	(java-wrap
	 (lambda (result)
	   
	   (c-display "gakk gakk!")
	   (set! size (-> jfile-chooser getSize))
	   
	   (define directory (-> (-> jfile-chooser getCurrentDirectory) getAbsolutePath))
	   (define file (and (= result (-> jfile-chooser APPROVE_OPTION))
			     (-> (-> jfile-chooser getSelectedFile) getName)))
	   
	   
	   (c-display "directory:" directory)
	   
	   (c-display "file:" file)
	   
	   (c-display "callback:" callback)
	   
	   (callback directory
		     file
		     (if file
			 (-> (-> jfile-chooser getSelectedFile) getAbsolutePath)
			 directory))
	   (-> parent check-paint)))))

  (def-method (load text callback :optional (newdirectory directory))
    (request #t text callback newdirectory))

  (def-method (save text callback :optional (newdirectory directory))
    (request #f text callback newdirectory)))


