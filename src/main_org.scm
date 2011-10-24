



#|
(begin
  (define is-running-applet #f)
  (define please-dont-trace #t)
  (load "dummy.scm")
  (compile-file "various.scm" "various.scc"))

(begin
  (define is-running-applet #f)
  (define please-dont-trace #t)
  (load "main.scm"))
(create-hurtigmixer)
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

(set! paint-waveforms #f)

(* das-width das-height 4)

(new-static <java.awt.Component>)

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




|#


(load "various.scc")


(c-import java)  

;;(define System (new-static <java.lang.System>))
;;(-> System load "/hom/kjetism/hurtigmixer/src/libtritonusvorbis.so")

(let ((uim (new-static <javax.swing.UIManager>)))
  (-> uim setLookAndFeel (-> uim getSystemLookAndFeelClassName)))

(define Various (new-static <Various>))



(begin
  (define use-rtmixer #t)
  (define srate 44100)
  (define num-frames 4096)
  (define mix-length 10)
  (define max-end-time (* 20 60))
  (define mixer (new <DasMixer> num-frames srate use-rtmixer 7 ':end-time mix-length)) ;; Note that the end-time is also set in mixer-area.
  (-> mixer volume 0.5)
  )


(begin
  (c-import undo)
  (c-import c-window)
  (c-import audiofiles)
  (c-import mixer-area)
  (c-import input-area)
  (c-import vu-area)
  (c-import output-area)
  ;;(c-import show-memory-area)
  (define win #f)
  (define mixer-area #f)
  (define sound-holder #f))

(define create-hurtigmixer #f)
;;(define |@srfi-48::format| (lambda x "sr48format-not-supported"))
(define |@srfi-48::format| format)

(begin
  (set! create-hurtigmixer
	(lambda ()
	  ;;(set! format (lambda x "format-not-supported"))
	  (set! audiofiles (make-hash-table 519))
	  (set! |@srfi-48::format| format) ;;(lambda x "sr48format-not-supported"))
	  (eval
	   '(begin
	      (when win
		    (when (and mixer-area
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

	      (define das-width 1000)
	      (define das-height 700)
	      
	      (if is-running-applet
		  (let* ((applet (-> (new-static <DasApplet>) uberthis))
			 (dimensions (-> applet getSize)))
		    (set! das-width (-> dimensions width))
		    (set! das-height (-> dimensions height))))
		    
	      (define input-area-width (min (/ das-width 4) 200))
	      (define vu-width 10)
	      (define output-area-width input-area-width)

	      (set! win (<c-window> das-width das-height))
	      (define container (-> win get-frame))
	      (define insets (-> container insets))

	      (define das-das-height (- das-height (-> insets top) (-> insets bottom)))

	      (define miniature-area-height (/ das-das-height 5)) ;;100)
	      (define timebar-height 20)

	      (define mixer-area-width (- das-width (-> insets left) input-area-width vu-width output-area-width (-> insets right)))
	      (define mixer-area-height (- das-das-height timebar-height miniature-area-height))

	      (define input-area (<input-area> win input-area-width das-das-height))
	      (-> win add-sub-area input-area (1+ (-> insets left)) (1+ (-> insets top)))

	      (set! mixer-area (<mixer-area> win mixer-area-width mixer-area-height mixer ':end-time mix-length ':max-end-time max-end-time))
	      (define timeslider (<slider> win mixer-area-width miniature-area-height  0 1 (<- mixer-area timeslider-report-func) ':timeslider #t))
	      (define miniature-area (<miniature-area> win timeslider mixer-area-width miniature-area-height mixer-area))
	      (-> timeslider add-sub-area miniature-area 0 0)

	      (define timebar (<timebar> win mixer-area-width (+ 2 timebar-height) 0 mix-length))

	      (-> win add-sub-area-after input-area timeslider)
	      (-> win add-sub-area-below timeslider mixer-area)
	      (-> win add-sub-area-below mixer-area timebar)

	      (define vu-area (<vu-area> win vu-width das-das-height))
	      (-> win add-sub-area-after timeslider vu-area)

	      (define output-area (<output-area> win output-area-width das-das-height))
	      (-> win add-sub-area-after vu-area output-area)

	      (-> win check-paint)
	      ))))
  (create-hurtigmixer))











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



|#

