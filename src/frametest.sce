(#%program
  ((|@libraries::require-library| . 1))
  ()
  (|@libraries::require-library|)
  (#%annotate
    (|@libraries::require-library|
      (#%quote sisc/libs/srfi/srfi-1))
    ((source-file
       .
       "file:/home/kjetil/hurtigmixer/src/frametest.scm")
     (line-number . 2)
     (column-number . 1)
     (source-kind . user))))
(#%program
  ((newline . 1)
   (|%%_Lv60YIEh__message| . 1)
   (display . 2)
   ($sc-put-cte . 1))
  ()
  ($sc-put-cte newline display)
  (#%begin
    ($sc-put-cte
      'display-frametest
      (#%quote (global . display-frametest)))
    (#%annotate
      (#%define display-frametest
        (#%lambda #t
          (|%%_Lv60YIEh__message|)
          ()
          (#%begin
            (#%annotate
              (display "frametest.scm")
              ((source-file
                 .
                 "file:/home/kjetil/hurtigmixer/src/frametest.scm")
               (line-number . 5)
               (column-number . 3)
               (source-kind . user)))
            (#%annotate
              (display |%%_Lv60YIEh__message|)
              ((source-file
                 .
                 "file:/home/kjetil/hurtigmixer/src/frametest.scm")
               (line-number . 6)
               (column-number . 3)
               (source-kind . user)))
            (#%annotate
              (newline)
              ((source-file
                 .
                 "file:/home/kjetil/hurtigmixer/src/frametest.scm")
               (line-number . 7)
               (column-number . 3)
               (source-kind . user))))))
      ((source-file
         .
         "file:/home/kjetil/hurtigmixer/src/frametest.scm")
       (line-number . 4)
       (column-number . 1)
       (source-kind . user)))))
(#%program
  ((display-frametest . 1))
  ()
  (display-frametest)
  (#%annotate
    (display-frametest "initing")
    ((source-file
       .
       "file:/home/kjetil/hurtigmixer/src/frametest.scm")
     (line-number . 9)
     (column-number . 1)
     (source-kind . user))))
