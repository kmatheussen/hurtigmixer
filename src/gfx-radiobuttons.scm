
(c-import gfx-button)

(define gfxrs '())

(define gfxr-font (get-font "GothamRnd-Medium.ttf" 11.0))

(def-class (<gfxr> image name text-area color callback x y x2 y2 is-on)
  (def-var image)
  (def-var name)
  (def-var text-area)
  (def-var color)
  (def-var callback)
  (def-var x)
  (def-var y)
  (def-var x2)
  (def-var y2)
  (def-var is-on))

(def-area-subclass (<gfxr-backgroundarea> parent width height color)
  (def-method (paint g x* y* x2* y2*)
    (-> g do-alpha alpha-0.25
        (lambda ()
          (-> g fill-rect color x y width height)))))

(c-define (add-gfxr parent name image-filename color callback right-callback x y x2 y2)
  
  (define split-pos (i-scale 0.40 0 1 x x2))
  (define gfxr #f)
  
  (define background (<gfxr-backgroundarea> parent (- x2 x 1) (- y2 y 1) color))
  (-> parent add-sub-area background (1+ x) (1+ y))

  (define image (<gfx-button> parent image-filename #f
                              ':is-URL? (or is-running-applet
                                            is-running-standalone
                                            (-> (new <java.lang.String> (config-full-url image-filename))
                                                startsWith "http://"))))
  (define text-color verygray-color)
  (define text (<text-area> parent (- x2 split-pos) (- y2 y) (<-> (if (null? gfxrs) "* " "") name) ':text-color text-color ':font gfxr-font))
  (define pos (<pos-button> parent (- x2 x) (- y2 y) (lambda ()
                                                       (c-display "selected" name (-> parent last-button))
                                                       (if (and right-callback
                                                                (= *right-mouse-button* (-> parent last-button)))
                                                           (right-callback))
                                                       (for-each (lambda (gfxr)
                                                                   (-> (-> gfxr text-area) text (-> gfxr name))
                                                                   (-> (-> gfxr text-area) repaint-me!))
                                                                 gfxrs)
                                                       (-> text text (<-> "* " (-> gfxr name)))
                                                       (-> text repaint-me!)
                                                       (callback))
                            ':do-paint2 #f))
  (-> parent add-sub-area pos x y)

  (c-display "          ADDING button to " x y x2 y2)
  (-> image mouse-cycles '())
  (-> image add-to-parent parent x y split-pos y2)
  ;;(-> parent add-sub-area image (+ x 4) (+ y 4))
  (-> parent add-sub-area text (- split-pos 2) (+ 5 y))

  (set! gfxr (<gfxr> image name text color callback x y x2 y2 (null? gfxrs)))

  (if (null? gfxrs)
      (callback))

  (push! gfxr gfxrs)

  gfxr
  )


  


