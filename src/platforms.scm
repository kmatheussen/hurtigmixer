

(define static-java.io.File (new-static <java.io.File>))
(define static-java.lang.System (new-static <java.lang.System>))



(define os.name (-> static-java.lang.System getProperty "os.name"))

(define is-running-linux? (string=? "Linux" os.name))
(define is-running-windows? (string=? "\\" (-> static-java.io.File separator))) ;; os.name can be windows, win95, windowsxp, etc.
(define is-running-solaris? (string=? "Solaris" os.name))


;; Will probably be true for aix, etc. Oh well. (why doesn't sun make it a bit easier to find out which platform?)
(define is-running-macosx?
  (and (string=? "/" (-> static-java.io.File separator))
       (string=? ":" (-> static-java.lang.System getProperty "path.separator"))
       (not is-running-windows?)
       (not is-running-solaris?)
       (not is-running-linux?)))



