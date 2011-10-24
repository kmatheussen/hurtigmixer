

(eval '(import threading))
(eval '(import os))


;;(define *encoder-URL* "http://archive.notam02.no/hurtigmikser/")
(define *encoder-URL* "http://folk.uio.no/ksvalast/hurtigmikser/encoders/")



(define (get-tempfilename extension)
  (define temp-file (-> static-java.io.File createTempFile "hurtigmixer" (<-> "." extension)))
  (-> temp-file toString))

(define (add-filetype filename filetype)
  (if (-> (new <java.lang.String> filename) endsWith (<-> "." filetype))
      filename
      (<-> filename "." filetype)))


(define (delete-on-exit filename)
  (-> (new <java.io.File> filename) deleteOnExit))

(define get-save-filerequester
  (let ()
    (define save-filerequester #f)
    (lambda (parent)
      (if (not save-filerequester)
	  (set! save-filerequester (<filerequester> parent)))
      save-filerequester)))

(define are-encoders-available?
  (let ((ret 'unknown))
    (lambda ()
      (if (eq? ret 'unknown)
	  (set! ret (-> Various checkDownload (<-> *encoder-URL* "download_check.txt"))))
      ret)))




(define (get-lame-name)
  (cond (is-running-windows?
	 "lame.exe")
	(is-running-linux?
	 "lame_linux")
	(is-running-macosx?
	 "lame_macosx")
	(else
	 #f)))

(define (get-oggenc-name)
  (cond (is-running-windows?
	 "oggenc.exe")
	(is-running-linux?
	 "oggenc_linux")
	(is-running-macosx?
	 "oggenc_macosx")
	(else
	 #f)))


(define (is-lame-supported?)
  (or is-running-linux?
      is-running-windows?
      is-running-macosx?))

(define (get-supported-lame-plattforms)
  (list "Linux, " "Windows, " "MacosX"))



(define (is-oggenc-supported?)
  (or is-running-linux?
      is-running-windows?
      is-running-macosx?))

(define (get-supported-oggenc-plattforms)
  (list "Linux, " "Windows, " "MacosX"))








(c-define (spawn-process-and-wait command)
  (c-display "command "command)
  (define p (spawn-process command))
  (c-display "got it" p ", now waiting...")
  (wait-for-process p)
  (c-display "finished waiting for process")
  )



(define (make-file-executable filename)
  (when (or is-running-linux?
	    is-running-macosx?)
	(-> (-> (new-static <java.lang.Runtime>) getRuntime) exec (<-> "chmod a+rx " filename))
	(sleep 2000)))

;;      (new <java.io.FilePermission> filename "execute")))
;;      (spawn-process-and-wait (<-> "chmod a+rx " filename))))





(define lame-downloaded #f)
(define (download-lame!)
  (c-display "download-lame!")
  (when (not lame-downloaded)
	(c-display "download-lame!, starting")
	(-> Various downloadFileFromURL (<-> *encoder-URL* (get-lame-name)) (new <java.io.File> (<-> *temp-directory* (get-lame-name)))
	    (java-wrap (lambda (downloaded filelength)
			 ;;(c-display "hepp before")
			 (set-progress-bar! downloaded filelength ':synchronized #f)
			 ;;(c-display "hepp after")
			 )))
	;;(set-message-area-progress! "" downloaded filelength " downloaded." ':synchronized #f))))
	
	(c-display "download-lame!, finished")
	(make-file-executable (<-> *temp-directory* (get-lame-name)))
	(c-display "download-lame!, finished making executable")
	;;(if is-running-windows?
	;;    (-> Various downloadFileFromURL (<-> *encoder-URL* "lame_enc.dll") (new <java.io.File> (<-> *temp-directory* "lame_enc.dll"))))
	(set! lame-downloaded #t))
  )

(define oggenc-downloaded #f)
(define (download-oggenc!)
  (when (not oggenc-downloaded)
	(-> Various downloadFileFromURL (<-> *encoder-URL* (get-oggenc-name)) (new <java.io.File> (<-> *temp-directory* (get-oggenc-name)))
	    (java-wrap (lambda (downloaded filelength)
			 (set-progress-bar! downloaded filelength ':synchronized #f))))
	(make-file-executable (<-> *temp-directory* (get-oggenc-name)))
	(set! oggenc-downloaded #t))
  )


(define (get-save-function)
  (if *running-hurtigmixer*
      (<- mixer-area save)
      (<- melodi-area save)))


(define (save-wav parent . rest)
  (-> (get-save-filerequester parent) save "Velg plassering. Filending boer ende paa .wav"
      (lambda (directory filename fullpath)
	(c-display "hepp" filename)
        (set! fullpath (add-filetype fullpath "wav"))
	(when filename
	  (c-display "hepp1.5" get-save-function (defined? '*running-melodigenerator*))
	  (c-display "hepp2" (get-save-function))
	  ((get-save-function) fullpath (lambda (success)
					  (c-display "hepp3")
					  (spawn-thread
					   (lambda ()
					     (c-display "succ?" success)
					     (if success
						 (begin
						   (set-progress-bar! 1 1)
						   (set-message-area!  "Ferdig"))
						 (begin
						   (set-progress-bar! 0 1)
						   (set-message-area!  "Avbrutt")))))))))))
;;					  success)))))))



#|
(spawn-process "echo" (list "50" ">/tmp/temp.temp"))
(spawn-process "/tmp/make.sh" (list "60" "70" "80" "90"))
|#

(c-define (progress-encode init commands handle-byte-func cleanup)
  (c-display "ai7")
  (spawn-thread
   (lambda ()
     (c-display "ai8" init)
     (init)
     (c-display "ai9")
     (c-display "starting process \"" commands "\"")
     (c-display spawn-process)
     (define process (spawn-process (car commands) (cdr commands)))
     (c-display "process started" process)

     (define stderr (get-process-stderr process))
     (c-display "stderr started" stderr)

     (let loop ((b (read-byte stderr)))
       (c-display b)
       (when (and #t
		  ;;(not (process-terminated? process))
		  (not (equal? b #!eof)))
	     (handle-byte-func b)
	     (loop (read-byte stderr))))
     (c-display "gakk4")
     (wait-for-process process)
     (c-display "gakk5")
     (cleanup))))

 

(c-define (save-encoded parent supported? supported-plattforms filetype download-func create-encode-commands :key filename cont)
	  
  (define (saveit fullpath)
    (c-display "ai2")
    (let ((wavfile (get-tempfilename "wav")))
      (c-display "ai3")
      (define last-byte -1)
      (c-display "ai4")
      (   (get-save-function)
	  wavfile
	  (lambda (success)
	    (c-display "ai5")
	    (when success
		  (c-display "ai6")
		  (progress-encode (lambda ()
				     (c-display "gakkgakk")
				     (set-message-area! "Laster ned konverter")
				     (c-display "gakkgakk3")
				     (download-func)
				     (c-display "gakkgakk4")
				     (set-message-area! "Starter konverter")
				     (set-message-area! (<-> "Encoding " filetype))
				     (c-display "hepp"))
				   (create-encode-commands wavfile fullpath)
				   (lambda (byte)
				     (set! last-byte byte)
				     (set-progress-bar! byte 100))
				   ;;(set-message-area-progress! "Encoding " byte 100 ""))
				   (lambda ()
				     (set-progress-bar! 1 1)
				     (set-message-area!  "Ferdig");;(<-> "Ferdig! " fullpath " er lagret."))
				     (if (not (= last-byte 100))
					 (show-message-dialog (<-> "Advarsel, troebbel med " filetype "-kodinga."))
                                         (if *running-melodigenerator*
                                             (show-message-dialog "Ferdig med å lagre fil!")))
				     (sleep 200)
				     (delete-on-exit wavfile)
				     (if cont
					 (cont)))))))))

  (if (not supported?)
      (show-message-dialog (apply <-> `("Lagring av " ,filetype "-filer virker bare paa " ,@supported-plattforms ".")))
      (if filename
	  (saveit filename)
	  (-> (get-save-filerequester parent) save (<-> "Velg plassering. Filending boer ende paa ." filetype)
	      (lambda (directory filename fullpath)
		(when filename
                  (saveit (add-filetype fullpath filetype))))))))


(define* (save-mp3 parent  :key tempfilename cont)
  (save-encoded parent
		(is-lame-supported?)
		(get-supported-lame-plattforms)
		"mp3"
		download-lame!
		(lambda (wav-name mp3-name)
		  (list (<-> *temp-directory* (get-lame-name)) "--nohist" wav-name mp3-name))
		':filename tempfilename
		':cont cont))

(define* (save-ogg parent :key tempfilename cont)
  (save-encoded parent
		(is-oggenc-supported?)
		(get-supported-oggenc-plattforms)
		"ogg" 
		download-oggenc!
		(lambda (wav-name ogg-name)
		  (list (<-> *temp-directory* (get-oggenc-name)) "--quiet" "-o" ogg-name wav-name))
		':filename tempfilename
		':cont cont))


(define* (save-local parent :key tempfilename cont)
  (if (are-encoders-available?)
      (new <SoundTypeRequester> "Velg format" #t *running-melodigenerator* #f *ask-name*
	   (java-wrap (lambda (extension)
			((if (string=? "mp3" extension)
			     save-mp3
			     (if (string=? "ogg" extension)
				 save-ogg
                                 (if (string=? "wav" extension)
                                     save-wav
                                     save-midi)))
                         parent ':tempfilename tempfilename ':cont cont))))
      (save-wav parent ':tempfilename tempfilename ':cont cont)))

		    


		    





#|
(begin
  (define p (spawn-process "echo hallo"))
  (define port (get-process-stdout p)))

(read-byte port)
(read-block 
(integer->char (read-byte port))
(integer->char 53)



|#
