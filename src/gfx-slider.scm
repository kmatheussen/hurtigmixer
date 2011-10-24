
(c-import slider)
(c-import gfx-button)


(def-class (<gfx-slider> parent width height x-1 x-2 gfx-filename report-func)

  (define (set-button-pos! x-1)
    (-> button set-position! 
        (i-scale x-1 0 1 (=> x) (- (=> x2) button-width))
        (- (average (=> y) (=> y2)) (/ button-height 2)))
    (-> button repaint-me!))

  (define (my-report-func x-1 x-2)
    (report-func x-1)
    (set-button-pos! x-1))

  (Super (<slider> parent width height x-1 x-2 my-report-func ':paint-borders #f))

  (define button (<gfx-button> parent gfx-filename (lambda x x)))

  (define button-width (-> button width))
  (define button-height (-> button height))

  (-> button mouse-cycles '())

  (=> add-sub-area button 0 0)
  
  (set-button-pos! x-1)
  )

