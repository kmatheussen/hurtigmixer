


(c-define (melodigenerator-skalavelger parent)

  (define selfont (get-font "GothamRnd-Medium.ttf" 12.49))
  (define font (get-font "GothamRnd-Book.ttf" 12.49))

  (define text-color     (get-RGB-color 64 63 60))
  (define selected-color (get-RGB-color 8 118 13))

  (define names (map symbol->string '(Moll Dur Heltone Dim Arabisk Blues Kromatisk Fri)))

  (define weights (map (lambda (w)
                         (map (lambda (v)
                                (* 1.0 v))
                              w))
                       (list moll-weight dur-weight heltone-weight dim-weight arabisk-weight blues-weight kromatisk-weight fri-weight)))

  (define scales (map (lambda (scale)
                        (get-position-list (symbol-append scale '-button) (lambda x x)))
                      '(moll dur heltone dim arabisk blues kromatisk fri)))

  (define prev-selected-text #f)

  (for-each (lambda (name scale weight)
              (apply (lambda (x y width height)
                       (define button (<pos-button> parent width height (lambda ()
                                                                          (when prev-selected-text
                                                                            (-> prev-selected-text font font)
                                                                            (-> prev-selected-text text-color text-color)
                                                                            (-> prev-selected-text repaint-me!))
                                                                          (-> text font selfont)
                                                                          (-> text text-color selected-color)
                                                                          (-> text repaint-me!)
                                                                          (set! prev-selected-text text)
                                                                          (weigh-all-notes weight))
                                                    ':do-paint2 #f))
                       (define text #f)
                       (cond ((string=? name "Vekting")
                              (set! text (<text-area> parent width height name ':font selfont ':text-color selected-color))
                              (set! prev-selected-text text))
                             (else
                              (set! text (<text-area> parent width height name ':font font ':text-color text-color))))
                       (-> parent add-sub-area button x y)
                       (-> parent add-sub-area text (+ 2 x) (+ 4 y))
                       )
                     scale))
            names
            scales
            weights)


  )



