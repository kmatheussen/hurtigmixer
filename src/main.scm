


#|
(define please-dont-trace #t)
(begin
  (load "dummy.scm")
  (compile-file "various.scm" "various.scc"))

(compile-file "various.scm" "various.scc")

(begin
  (define *running-melodigenerator* #f)
  (define *running-hurtigmixer* #t)
  (define please-dont-trace #t)
  (load "main.scm"))

(start-application)

(define static-ResampleProducer (new-static <ResampleProducer>))
(-> static-ResampleProducer forceLinearInterpolation)
(-> static-ResampleProducer forceLinearInterpolation #t)
(-> static-ResampleProducer forceLinearInterpolation #f)

(defined? '*config-URL*)

(define l (list 1 2 3 4 5 6))
(define same (cddr l))
(set-car! same (cadr same))
(set-cdr! same (cddr same))
(list l same)
(delete! #f (list 1 3 #f 9) eq?)
(c-import melodigenerator)
(c-import config)

(map (lambda (position)
       (c-display "position: " position)
       (let ((conf (get-config-area-list-with-help (car position))))
	 (if conf
	     (cons (car position) conf)
	     (append position (list "")))))
     (if (and (defined? '*running-hurtigmixer*)
	      *running-hurtigmixer*)
	 all-positions-hurtigmixer
	 all-positions-melodigenerator))


(begin
  (eval '(define DAS_HOME "/hom/kjetism/"))
  (eval `(define *config-URL* ,(string-append DAS_HOME "hurtigmixer/melodigenerator/config.txt")))
  (define *config-URL* (string-append DAS_HOME "hurtigmixer/melodigenerator/config.txt"))
  (eval '(define *running-melodigenerator* #t))
  (eval '(define please-dont-trace #t))
  (define mix-length 9)
  (define songlen 9)
  (eval `(load ,(string-append DAS_HOME "hurtigmixer/src/main.scm")))
  )

;; save/restore for skala-vekting.
;; midt-knapp for xy-slider
;; Skala-konfigurering
;; ** unrandom-knapp
;; ** help popup.
;; bytte lyd.
;; oktav-valg for lyder
;; ** space-bar
;; progress-bar: popup.
;; midi-save

(-> font-default getFamily)

(define gfont (get-font "GothamRnd-Book.ttf" 12.5))
(-> gfont getFamily)
(define gfont2 (get-font "Archer-Medium-Pro.ttf" 13.5))
(-> gfont2 getFamily)
(-> gfont2 ROMAN_BASELINE)

(-> (-> (-> win g) getFontMetrics gfont)
    getDescent)
(-> (-> (-> win g) getFontMetrics gfont)
    getAscent)
(-> (-> (-> win g) getFontMetrics gfont)
    getLeading)

(-> (-> win g) get-texturepaint (config-full-url "Tempo100.png"))
(get-config-sound-selector-buttons)
(+ 24 12 12 2 2 1 2 2)

(-> (-> mixer freeVerb) getWet)

(find-category-names)

(member 'a '())
(get-notelist (nth 3 (find-category-names)))
(begin *pianoroll-pitch*)
(mixertime->ticks 6 20000)
(get-real-life-note 24)
(->height)
(get-ys lower (lambda x x))

(begin (vector->list (-> scaleselect-area weights)))
(expt 2 2)

(begin #\backspace)
(define gakk hepp)

(-> help-area highlight-color (-> Various getColor 0.73 0.15 0.35))


(modulo (+ 0 1) 12)

(begin
  ;;(set! *config-URL* "/hom/kjetism/hurtigmixer/configtest/config.txt")
  (eval '(define *running-melodigenerator* #t))
  (set! *pianoroll-speed* 1.0)
  (set! *pianoroll-pitch* 1.0)
  (set! *notes* '())
  (set! gfxrs '())
  (read-config-file)
  (set! mix-length 9)
  (-> mixer setLength 9.0)
  (if (defined? 'songlen)
      (set! songlen 9))
  (start-application))

(-> font-default getName)
(time (c-for 0 < 1000 1
       (lambda (i)
         (get-probable-note (get-note-probability-list (between lower upper)))))
)

5.9 -> 6.0 (quant 0.5)

(define texturepaint #f)
(let ((width 40)
      (height 40))
  (define dasimage (-> (-> (new-static <java.awt.Toolkit>) getDefaultToolkit) getImage 
                       (config-full-url (get-config-string 'paint-cursor))))
  (-> Various waitForImage dasimage)
  (let ()
    (define ret (new <java.awt.image.BufferedImage> width height (-> (new-static <java.awt.image.BufferedImage>) TYPE_INT_RGB)))
    (let ()
      (define ret-g (<my-graphics> (-> ret getGraphics) (-> win get-frame)))
      (-> ret-g draw-image dasimage 0 0)
      (set! texturepaint (new <java.awt.TexturePaint> ret (new <java.awt.Rectangle> 0 0 width height))))))

(map (lambda (note)
       (* 1.0 (-> note start)))
     (sort-all-notes (car (get-segregated-*notes*))))

(cleanup-note-list)
(list *notes*
      (map (lambda (note)
	     (-> note note))
	   *notes*))

(map (lambda (note)
       (-> note category-name))
     (apply append (get-segregated-*notes*)))


(assq 'a '((b 1) (c 2) (c 3) (a 4 6) (cd 5)))
(-> *audiofile-buffer* filename)

(c-scale 1.5 (+ 0.0 1.0) 2.0 10.0 20.0)
(c-macroexpand-1 '(c-scale 1.5 (+ 0.0 1.0) 2.0 10.0 20.0))
(clean-up-note-list)

(get-duration)

(-> insets left)
(-> insets top)
(+ reset-button-y reset-button-height)
(begin save-button-y)

(let ((mixtime (-> *das-mixer* getPlayPos)))
  (let ((diff (abs (- (get-mixer-time (get-pianoroll-time mixtime))
		      mixtime))))
    (if (> diff 0.1)
	(c-display diff))
    #t))

(any (lambda (a) a) '(#f 2 3 4))


(-> (-> mixer freeVerb) setDamp 0.4)
(-> (-> mixer freeVerb) setRoomSize 0.3)

(-> scaleselect-area weights)
(shake-all-notes)

(begin *config-URL*)

(get-config-string 'soundfile)
);;"/hom/kjetism/hurtigmixer/sounds/choir.wav")

(begin pianoroll-x)
(get-config-number 'melodigenerator-area-x)
(exit)

(bt)
(define a (new <java.net.URL> "http://wasdf.asdf/ab1/ab2/"))
(-> a toString)


(-> (-> win g) set-font!)

(-> mixer volume 1.0)
(run-synchronized
 (lambda ()
   (-> mixer requestTo_removeAll)
   (-> mixer requestTo_setPlayPos 0)  
   (generate-*notes*)
   (-> *pianoroll* repaint-me!)
   (-> win check-paint)))

(begin
  (add-trace-do 'ai)
  (c-display *trace-write-num*))

(begin *notes*)
(set! *notes* '())
(clean-up-note-list)

(-> (-> mixer volume) set 1.9)


(-> (-> mixer freeVerb) getWet)

(get-config-number-list 'melodigenerator-area)

(-> pitchspeed-area joystick-color blue-color)

(begin
  (eval '(define *config-URL* "/hom/kjetism/hurtigmixer/melodigenerator/config.txt"))
  (define *config-URL* "/hom/kjetism/hurtigmixer/melodigenerator/config.txt")
  (set! *config-URL* "/hom/kjetism/hurtigmixer/melodigenerator/config.txt")
  )

(define *config-URL* "/hom/kjetism/hurtigmixer/melodigenerator/config.txt")
(define *running-hurtigmixer* #f)


(c-import config)
(set! *ask-name* #f)

(get-config-number 'shortest-duration)

(c-time (load "mixer-area.scm"))

(get-config-number-list 'musikkverksted)
(get-config-vertical-sounds)

(get-userfiles)

(substring "ab" 0 1)

(for-each-line-in-file "/hom/kjetism/hurtigmixer/src/main.scm" c-display)

(with-input-from-file "http://www.notam02.no/arkiv/doc/temp/getfiles.php"
  (lambda ()
    (define string (make-string 1000 #\newline))
    (read-string string 0 999)
    ;;(c-display "aiai" string)
    (let* ((sver (new <java.lang.String> string))
	   (splitted (<-o (-> sver split "\n"))))
      (c-for 0 < (java-array-length splitted) 1
	     (lambda (n)
	       (c-display (java-array-ref splitted n)))))))
					 


(let ((p (open-input-file "http://www.notam02.no/arkiv/doc/temp/getfiles.php")))
  (do ((line (read-string p) (read-line p)))
      ((eof-object? line))
    (format #t "~A\n" (string-length line)))
  (close p))


(begin
  (define t1 4)
  (define t2 8))

(-> timebar text-color light-gray-color)
(-> timebar text-color redbrown-color)
(-> timebar bar-color redbrown-color)


(-> win ctrl-key)

(define ver (-> static-java.lang.System getProperty "java.version"))
(begin ver)
(begin sver)
(define sver (new <java.lang.String> ver))
(-> sver split ".")
(java-array-length (<-o (-> sver split "")))

(string-split "a.b.c")

(-> win curr-key)

(c-display (/ (-> mixer playPos) 44100.0))
(c-display (/ (-> (-> mixer scheduler) playPos) 44100.0))

(define s sound-area-copystate)
(* 1.0 (s 'start-time))
(* 1.0 (s 'end-time))
(* 1.0 (- (s 'end-time) (s 'start-time)))


(define (fsa) (car (-> mixer-area sub-areas)))

(-> (fsa) get-playtime)
(-> (fsa) get-abs-length)
(-> (fsa) start-read-pos)
(* 1.0 (-> (fsa) end-read-pos))
(* 1.0 (-> (fsa) start-playtime))
(list (* 1.0 (-> (fsa) start-time))
      (* 1.0 (-> (fsa) end-time)))

(-> (-> (fsa) sound-object) srate_change)
(-> (-> (fsa) sound-object) startFrame)
(-> (-> (fsa) sound-object) reverse)


(set! wave-color gray-color)
(set! wave-shade-color black-color)

(-> (fsa) get-position c-display)

(define curr (-> mixer-area curr-sound-area))
(-> curr class-name)

(c-display (* 1.0 (-> (fsa) start-read-pos))
	   (* 1.0 (-> (fsa) end-read-pos))
	   (* 1.0 (- (-> (fsa) end-time) (-> (fsa) start-time))))

(define ab 2)


(get-string-dialog "aiai")


(define *large-background-text-color* black-color)


(case 2
  ((1) 1)
  (ab 2)
  (else 5))


(-> (fsa) abs-start-time)

(-> (-> (fsa) sound-object) vol)


(list (-> (fsa) end-read-pos)
      (-> (fsa) start-read-pos))


(/ (-> (-> (fsa) sound-object)
       x3)
   44100.0)

(begin
  (-> (fsa) set-srate_change!
      1.02 #f)
  (-> win check-paint))

(list (-> (fsa) end-time)
      (-> (fsa) end-read-pos))

(-> (fsa) get-position c-display)
(-> (fsa) get-px1/px2 c-display)
(-> (-> (fsa) sound-object) srate_change)




(eval '(begin
         (import file-manipulation)
         (case-sensitive #t)
         (emit-annotations #t)
         (max-stack-trace-depth 20)))
(eval '(import s2j))

(class-path-extension-append! (list src-directory))
(class-path-extension-append! (list "file:/home/kjetil/hurtigmixer/src/"))
(java-class 'atest)

(current-class-path (list "file:/tmp/test/lib"))



(-> mixer volume 0.1)
(-> win force-paint)
(-> win check-paint)
(+ 9 2)

(-> (last (-> win sub-areas)) text)

(-> mixer srate_change 1.0)
(-> mixer srate_change)

(-> mixer volume)

(<-o (-> win image))
(<-o (-> win background-image))
(<-o (-> win background-volatile-image))

(set! wave-non-color (new <java.awt.Color> 0.23 0.96 0.51))
(define alpha-0.15 (get-alpha 0.45))

(set! wave-color light-gray-color)
(set! wave-text-color gray-color)

(set! paint-waveforms #f)

(* das-width das-height 4)

(new-static <java.awt.Component>)

(define static-ResampleProducer (new-static <ResampleProducer>))
(-> static-ResampleProducer dir)

(modulo 6 2)

(-> (-> win image) getAccelerationPriority)
(-> (-> win background-volatile-image) getAccelerationPriority)
(-> (-> win background-image) setAccelerationPriority 0)


(-> (-> win image) setAccelerationPriority 1.0)
(-> (-> win background-volatile-image) setAccelerationPriority 1.0)

(-> (-> mixer-area image) getAccelerationPriority)


(-> (-> (-> win image) getCapabilities) isAccelerated)
(-> (-> (-> win image) getCapabilities) isTrueVolatile)

(-> (-> (-> win background-volatile-image) getCapabilities) isAccelerated)
(-> (-> (-> win background-volatile-image) getCapabilities) isTrueVolatile)


(c-integer (* 2 (* (/ (* 60 44100 2) 512) 2)))



(java-class 'Various)

( (let ((proc (java-method-procedure (any (lambda (jmethod)
					    (and (eq? (java-method-name jmethod) 'downCount)
						 jmethod))
					  (java-class-declared-methods (java-class 'DasApplet)))))
	(uberthis ((java-field-accessor-procedure (any (lambda (jfield)
							 (and (eq? (java-field-name jfield) 'uberthis)
							      jfield))
						       (java-class-declared-fields (java-class 'DasApplet))))
		   (java-null (java-class 'DasApplet)))))
    (lambda (dc file-name)
      (proc uberthis (->jint dc) (->jstring file-name))))
  60 'wsefaef)



((java-field-accessor-procedure (any (lambda (jfield)
				       (and (eq? (java-field-name jfield) 'uberthis)
					    jfield))
				     (java-class-declared-fields (java-class 'DasApplet))))
 (java-null (java-class 'DasApplet)))


(define uberthisproc (java-field-accessor-procedure (any (lambda (jfield)
						   (and (eq? (java-field-name jfield) 'uberthis)
							jfield))
						 (java-class-declared-fields (java-class 'DasApplet)))))

(uberthisproc (java-null (java-class 'DasApplet) 50))

(java-null (java-class 'DasApplet))





(->jstring 'asdfsdf)


(-> Various openURL "http://www.notam02.no")


(-> mixer maxVal)

(-> timebar background-color gray-color)
(-> timebar bar-color black-color)

(define url (new <java.net.URL> "http://www.gakk.no/dir/file.ext"))
(-> url getFile)

(-> mixer-area cursor-color blue-color)
(-> mixer-area cursor-color light-gray-color)
(-> mixer-area cursor-color black-color)
(-> mixer-area cursor-color white-color)
(-> mixer-area cursor-color (-> Various getColor
				0.4 0.4 1.0))
(define is-running-linux #t)

(define debug-background-painting #f)

(exact->inexact (/ 0.005 0.02))
(c-integer (/ 0.005 0.02))

(* inc-time (c-integer (/ start-time inc-time)))

(modulo 7 5)

(remainder 7 5)

(-> (-> win background-image) getWidth)

(-> (new-static <java.lang.System>) getProperty "os.name")
(string=? "bc" "bc")


(catch #t
       (-> Various downloadFileFromURL (<-> *encoder-URL* "gakkgakk") (new <java.io.File> "/tmp/tmp.tmp")
	   (java-wrap (lambda (downloaded filelength)
			(c-display "downloaded/length" downloaded filelength))))
       (lambda (key . args)
	 (c-display "key/args" key args)))


(-> Various checkDownload (<-> *encoder-URL* "download_check.txt"))

|#


(load "various.scc")

(c-import java)  

(when (and is-running-applet
	   (-> (new-static <Events>) running_melodigenerator))
  (eval '(define *running-melodigenerator* #t))
  (eval '(define *running-hurtigmixer* #f))
  (eval '(define mix-length 9))
  (eval '(define songlen 9)))


;;(define System (new-static <java.lang.System>))
;;(-> System load "/hom/kjetism/hurtigmixer/src/libtritonusvorbis.so")

(let ((uim (new-static <javax.swing.UIManager>)))
  (-> uim setLookAndFeel (-> uim getSystemLookAndFeelClassName)))

(define Various (new-static <Various>))



(begin
  (c-import platforms)
  (c-display "platforms loaded")
  (c-import server)
  (c-import config)
  (read-config-file)

  (define use-rtmixer #t)
  (define srate 44100)
  (define num-frames 4096)
  (define mix-length (or (and (defined? 'get-config-number)
			      (get-config-number 'shortest-duration))
			 10))
  (define max-end-time (* 20 60))
  (define static-ResampleProducer (new-static <ResampleProducer>))
  (define mixer (new <DasMixer> num-frames srate use-rtmixer 7 ':end-time mix-length)) ;; Note that the end-time is also set in mixer-area.
  (-> static-ResampleProducer forceLinearInterpolation #t)
  (-> mixer setForceSincResampler #t)
  (-> (-> mixer volume) set 0.7)
  (-> mixer setCursor (new <MyCursor> mixer))
  )


(begin
  (c-import area)
  (c-import my-graphics)
  (c-import synchronization)

  (c-import positions)
  
  (c-import save-sound)

  (c-import top-sounds)
  (c-import text-area)
  (c-import list-area)
  (c-import sound-selector)
  (c-import filerequester)
  (c-import slider)

  (c-import controls)
  (c-import message-area)

  (c-import undo)
  (c-import mouse)
  (c-import c-window)
  (c-import timebar)

  (c-import audiofiles)
  (c-import mixer-area)
  (if (and (defined? '*running-melodigenerator*)
	   *running-melodigenerator*)
      (c-import melodigenerator))
  ;;(c-import input-area)
  (c-import vu-area)
  ;;(c-import output-area)
  ;;(c-import show-memory-area)
  (if is-running-applet
      (-> (-> (<java-class> (make-static-object (java-class 'DasApplet) (java-null (java-class 'DasApplet))))  uberthis) downCount 0 '_))
  (define win #f)
  (define mixer-area #f)
  (define sound-holder #f)
  (define timebar #f)
  )

(define start-application #f)
;;(define |@srfi-48::format| (lambda x "sr48format-not-supported"))
(define |@srfi-48::format| format)

(begin
  (c-display "HEPP1")
  (set! start-application
	(lambda ()
	  (c-display "HEPP2")
	  ;;(set! format (lambda x "format-not-supported"))
	  (-> (-> mixer volume) set 0.7)
	  (set! audiofiles (make-hash-table 519))
	  (set! sound-area-copystate #f)
	  (set! |@srfi-48::format| format) ;;(lambda x "sr48format-not-supported"))
	  (eval
	   '(begin
	      (when win
		(when (and (defined? 'mixer-area)
			   mixer-area
			   (-> mixer-area image))
		  (-> (-> mixer-area image) flush)
		  (-> (-> mixer-area image-g) dispose))
		
		(-> (-> win background-image) flush)
		(-> (-> win g) dispose)
		(-> (-> win image) flush)
		(-> (-> win ll-window-g) dispose)
		(-> (-> win get-frame) dispose))
	      
	      
	      
	      (if sound-holder
		  (-> sound-holder reset)
		  (set! sound-holder (new <SoundHolder>)))
	      
	      (reset-undo!)
	      
	      (define das-width  (or (get-config-number 'width) 1000))
	      (define das-height (or (get-config-number 'height) 625))
	      
	      (if is-running-applet
		  (let* ((applet (-> (new-static <DasApplet>) uberthis))
			 (dimensions (-> applet getSize)))
		    (set! das-width (-> dimensions width))
		    (set! das-height (-> dimensions height))))
	      
	      
	      
	      ;;(define vu-width (or (get-config-number 'vu-width) 10))
	      
	      (c-display "HEPP3")
	      (set! win (<c-window> das-width das-height))
	      (c-display "HEPP4")
	      (define container (-> win get-frame))
	      (define insets (-> container insets))

	      (generate-positions (-> insets left) (-> insets top))
	      (init-synchronization)
	      
	      (define das-das-height (- das-height (-> insets top) (-> insets bottom)))))
		     
	  (run-synchronized
	   (lambda ()
	     (eval
	      (cond ((and (defined? '*running-hurtigmixer*)
			  *running-hurtigmixer*)
		     '(begin
			
			;;(define input-area (<input-area> win input-area-width das-das-height))
			;;(-> win add-sub-area input-area (+ input-area-x (-> insets left)) (+ input-area-y (-> insets top)))
			
			(set! mixer-area (<mixer-area> win mixer-area-width mixer-area-height mixer ':end-time mix-length ':max-end-time max-end-time))
			(-> win add-sub-area mixer-area mixer-area-x mixer-area-y)
			(-> win add-self-blitting-area! mixer-area)
			
			(init-controls mixer-area win)
			
			;; Sliders
			(init-srate-slider  win)
			(init-size-slider   win)
			(init-volume-slider win)
			
			(init-message-area win)
			
			(define timeslider (<slider> win miniature-area-width miniature-area-height  0 1 (<- mixer-area timeslider-report-func) ':timeslider #t ':paint-borders #f))
			(define miniature-area (<miniature-area> win timeslider miniature-area-width miniature-area-height mixer-area))
			(-> win add-sub-area timeslider miniature-area-x miniature-area-y)
			(-> win add-self-blitting-area! miniature-area)
			(-> timeslider add-sub-area miniature-area miniature-area-x miniature-area-y)
			
			(set! timebar (<timebar> win mixer-area-width (+ 2 timebar-height) 0 mix-length))
			(-> win add-sub-area timebar timebar-x timebar-y)
			
			
			(define vu-area (<vu-area> win vu-area-width vu-area-height))
			(-> win add-sub-area vu-area vu-area-x vu-area-y)
			
			(-> win check-paint)
			))
		    ((and (defined? '*running-melodigenerator*)
			  *running-melodigenerator*)
		     '(begin
			(init-melodigenerator win)
			))
		    (else
		     '(c-display "NOTHING TO EVAL"))))))))

  (start-application)
  (if is-running-applet 
      (-> (-> (new-static <DasApplet>) uberthis) loaded #t))
  )











#|

(-> win repaint 0 0 543 426)
(-> win paint)

(<- mixer-area get-position)

(-> mixer-area play)
(-> mixer-area stop)

(begin
  (-> mixer-area add-sound 2_channels 450 160)
  (-> win force-paint))

(begin
  (-> mixer-area set-end-time! 30)
  (-> win paint))

(-> win force-paint)


(define t (-> win start-thread))
(thread/start t)


(-> mixer setLength 26)

(-> mixer-area move! 20 20)

(-> slider dir)

(-> slider set-position! 0 20)

(string>? "d" "b")

(-> mixer-area get-position (lambda x
			      (c-display x)))



(define al '())
(push-back! 'c al)
(begin al)


(-> mixer soundObjects)

(delete 6 '(0 2 6 1 5 2) =)

(letrec ((a (lambda ()
	   (c-display "a:" a))))
  a)


(define filedialog (new <java.awt.FileDialog> (-> win get-frame) "Velg lydfil. Mp3, wav eller aiff."))
(-> filedialog show)
(string-append (-> filedialog getDirectory) (-> filedialog getFile))

(-> filedialog getFile)
(-> filedialog getDirectory)
(begin jnull)
(java-null? jnull)


(-> filedialog dir)

(define af (new <MyAudioFileBuffer> "/home/kjetil/jack_capture_04.wav"))
(-> af dir)
(-> af nFrames)
(get-audiofile "/home/kjetil/jack_capture_04.wav")




(define desc-series
  (lambda (size)
    (let countdown ((n size) (s ()))
      (if (= n 0) s
	  (countdown (- n 1) (append s (list n)))))))

(define do-from-to
  (lambda (m n)
    (let loop ((i m))
      (begin
	(define i2 (* i i))
	(define ds (desc-series i2))
	(display ds)
	(newline)
	(if (= i n) 'done
	    (loop (+ i 1)))))))
(letrec ((i2 (* i i))
	 (ds (desc-series i2)))
  (display ds))

(do-from-to 2 4)

(let loop ((i m))
  (begin
    (define i2 (* i i))
    (define ds (desc-series i2))
    ...))

(let loop ((i m))
  (letrec ((i2 (* i i))
	   (ds (desc-series i2)))
    ...))



(define afs (new <AudioFileSaver> "/tmp/test.wav" 2 44100 16 (* 44100 2 32) mixer))


(define rt (-> (new-static <java.lang.Runtime>) getRuntime))
(define r (-> rt exec "/usr/bin/lame gakk.wav"))
(-> r exitValue)
(-> r dir)




(define mp3 (new <MyAudioFileBuffer> "/home/kjetil/cemb2_b.wav.mp3"))
(-> (-> mp3 buf) len)

(define mp3-stream (-> (new-static <javax.sound.sampled.AudioSystem>) getAudioInputStream (new <java.io.File> "/home/kjetil/cemb2_b.wav.mp3")))
(define format (-> mp3-stream getFormat))
(-> format getSampleSizeInBits)


(begin
  (define is-running-applet #f)
  (define please-dont-trace #t)
  (load "various.scm"))

(c-import java)

(define rt (-> (new-static <java.lang.Runtime>) getRuntime))
(-> rt freeMemory)
(-> rt totalMemory)


(begin
  (define bytes #f)
  (define bytes (new <SoundHolder> (* 90 (* 1024 1024)) (* 5 (* 1024 1024))
		     (* 10 (* 1024 1024)) (* 20 (* 1024 1024)))))



(java-array-length (<-o (-> bytes allSounds)))



(define sound (-> sound-holder addSound "/hom/kjetism/Blub_mono16.wav"))
(define sound (-> sound-holder createAudioInputStream "/hom/kjetism/Blub_mono16.wav"))
(-> sound available)

(define sound (new <java.io.DataInputStream> sound))

(-> sound available)

(define bytes (java-array-new <jbyte> 2))
(-> sound read bytes 0 2)
(java-array-ref bytes 1)




(define stream (-> sound-holder createAudioInputStream "http://www.notam02.no/~kjetism/Blub_mono16.wav"))
(-> stream dir)



(java-array-length (<-o (-> sound-holder allSounds)))


(-> (-> input-area egen-soundlist) add-sound 
(define (<audiofile> 
2_channel_short.wav


    public AudioFileInfo getAudioFileInfo(String fileName,float srate){
(define a (-> sound-holder getAudioFileInfo "/home/kjetil/blub4.wav" 40))
(-> a peaks)


(import os)
(define p (spawn-process "C:\lame.exe /home/kjetil/testing.wav"))
(begin p)
(wait-for-process p)

(define System (new-static <java.lang.System>))
(-> System load "/hom/kjetism/hurtigmixer/src/libtritonusvorbis.so")

(define ogg (new <org.tritonus.share.sampled.AudioFileTypes> "ogg" "ogg"))

(define o (-> (new-static <javax.sound.sampled.AudioSystem>) getAudioFileTypes))
(java-array-length (<-o o))
(java-array-ref (<-o o) 3)


(-> o toString)



(define o (-> (new-static <org.tritonus.share.sampled.AudioFileTypes>) getType "FLAC" "flac"))
(define o (-> (new-static <org.tritonus.share.sampled.AudioFileTypes>) getType "OGG_VORBIS" "ogg"))
(-> o toString)


(define vo (new <org.tritonus.sampled.file.vorbis.VorbisAudioFileWriter>))

(new <javax.sound.sampled.AudioFileFormat.Type.OGG_VORBIS>)
(-> (new-static <javax.sound.sampled.AudioFileFormat>) Type)

(-> (new-static <javax.sound.sampled.AudioFileFormat.Type>) "MP3" "mp3")





(define conf (-> (-> win get-frame) getGraphicsConfiguration))
(-> conf createCompatibleVolatileImage 200 200 (-> green-color TRANSLUCENT))


(exact? 2)
(exact? 2/3)
(integer? (floor 4.0))
(floor 4.0)
(integer? 4.0)

(exact? 4.0)

(define frame (-> win get-frame))
(-> frame TOP_ALIGNMENT)
(define insets (-> frame getInsets))
(-> insets bottom)

(c-define-macro (define2 def . rest)
  (if (pair? def)
      `(define2 ,(car def)
	 (lambda ,(cdr def)
	   ,@rest))
      `(define ,def ,@rest)))

(pretty-print (c-macroexpand '(define2 ((f a) b) ...)))




|#

