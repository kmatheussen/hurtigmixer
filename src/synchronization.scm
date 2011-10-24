
(define global-lock #f)

(define (run-synchronized thunk)
  (java-synchronized global-lock thunk))

(define (init-synchronization)
    (set! global-lock (<-o (-> (new-static <CreateFrame>) globalLock)))
    )

;; Had some problem with SISC's spawn-thread, I'm not sure if this fixes it, but at least I have full control now.
(define (spawn-thread thunk)
  (-> Various spawnThread (java-wrap thunk)))
