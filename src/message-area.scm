


(define message-area #f)
(define progress-bar #f)
(define progress-bar-text "")
(define message-area-parent #f)

(define* (set-message-area! text :key (synchronized #t))
  (cond (*running-hurtigmixer*
         (if synchronized
             (run-synchronized (lambda ()
                                 (c-display "gakkgakk2")
                                 (-> message-area set-and-force-paint! text)))
             (-> message-area set-and-force-paint! text)))
        (else
         (c-display "gak1")
         (set! progress-bar-text text)
         (c-display "gak2 " progress-bar-text)
         (-> help-area set-and-paint! (<-> progress-bar-text " (100%)"))
         (c-display "gak3")
         )))

(define* (set-progress-bar! val maxval :key (synchronized #t))
  (let ((run (lambda ()
               (cond (*running-hurtigmixer*
                      (-> progress-bar set-values! 0 (c-scale val 0 maxval 0 1))
                      (-> progress-bar-text text (<-> (number->string (max 0 (min 100 (i-scale val 0 maxval 0 100)))) "%"))
                      (-> message-area-parent check-paint))
                     (else
                      (-> help-area set-and-force-paint! (<-> progress-bar-text " (" (number->string (c-integer (/ (* 100 val) maxval))) "%)")))))))
    (if synchronized
	(run-synchronized run)
	(run))))


(define* (set-message-area-progress! mes1 val maxval mes2 :key (synchronized #t))
  (if *running-hurtigmixer*
      (-> progress-bar set-values! 0 (c-scale val 0 maxval 0 1)))
  (set-message-area! (<-> mes1 (number->string (max 0 (min 100 (i-scale val 0 maxval 0 100)))) "%" mes2) ':synchronized synchronized))

(define (init-message-area parent)
  (set! message-area-parent parent)
  (set! message-area (<text-area> parent message-area-width message-area-height ""))
  (-> parent add-sub-area message-area message-area-x message-area-y)

  (set! progress-bar (<slider> parent progress-bar-width progress-bar-height 0 0 (lambda x #f)))
  (-> progress-bar mouse-cycles '())
  (-> parent add-sub-area progress-bar progress-bar-x progress-bar-y)

  (set! progress-bar-text (<text-area> parent (/ progress-bar-width 2) 9 "0%"))
  (-> parent add-sub-area progress-bar-text (- (average progress-bar-x progress-bar-x2) 8) (- (average progress-bar-y progress-bar-y2) 4))
  )





