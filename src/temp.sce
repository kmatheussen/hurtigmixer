(#%program
  ((|%%_v5EfwjGlK_y| . 1)
   ($syntax-dispatch . 1)
   (|%%_v5-buMGlK_tmp| . 2)
   (syntax-error . 1)
   (|%%_v501o7IlK_args| . 1)
   (syntax-object->datum . 1)
   (|%%_v5mZlAIlK_body| . 3)
   (cdr . 1)
   (cons . 7)
   (car . 1)
   (|%%_v5IVj1JlK_def| . 6)
   (pair? . 2)
   (c-display . 1)
   (list . 4)
   (|%%_v5G4qGHlK_define-macro| . 1)
   (datum->syntax-object . 1)
   (apply . 2)
   (|%%_v5k8sdHlK_tmp| . 2)
   ($sc-put-cte . 1))
  ()
  ($sc-put-cte
    syntax-error
    apply
    datum->syntax-object
    syntax-object->datum
    cons
    cdr
    car
    c-display
    pair?
    list
    $syntax-dispatch)
  ($sc-put-cte
    'd-define-macro
    (#%lambda #t
      (|%%_v5EfwjGlK_y|)
      ()
      ((#%lambda #t
         (|%%_v5-buMGlK_tmp|)
         ()
         ((#%lambda #t
            (|%%_v5k8sdHlK_tmp|)
            (|%%_v5-buMGlK_tmp|)
            (#%if |%%_v5k8sdHlK_tmp|
              (apply (#%lambda #t
                       (|%%_v5G4qGHlK_define-macro| |%%_v501o7IlK_args|)
                       ()
                       (datum->syntax-object
                         |%%_v5G4qGHlK_define-macro|
                         (apply (#%lambda #t
                                  (|%%_v5IVj1JlK_def| . |%%_v5mZlAIlK_body|)
                                  ()
                                  (list (#%quote begin)
                                        (#%quote
                                          (set! macro-generation
                                            (|1+| macro-generation)))
                                        (#%annotate
                                          (c-display
                                            "pair?"
                                            (#%annotate
                                              (pair? |%%_v5IVj1JlK_def|)
                                              ((source-file
                                                 .
                                                 "file:/home/kjetil/hurtigmixer/src/temp.scm")
                                               (line-number . 7)
                                               (column-number . 26)
                                               (source-kind . user))))
                                          ((source-file
                                             .
                                             "file:/home/kjetil/hurtigmixer/src/temp.scm")
                                           (line-number . 7)
                                           (column-number . 7)
                                           (source-kind . user)))
                                        (#%annotate
                                          (#%if (#%annotate
                                                  (pair? |%%_v5IVj1JlK_def|)
                                                  ((source-file
                                                     .
                                                     "file:/home/kjetil/hurtigmixer/src/temp.scm")
                                                   (line-number . 8)
                                                   (column-number . 11)
                                                   (source-kind . user)))
                                            (list (#%quote hashtable/put!)
                                                  (#%quote all-macros)
                                                  (list (#%quote quote)
                                                        (car |%%_v5IVj1JlK_def|))
                                                  (cons (#%quote lambda)
                                                        (cons (#%annotate
                                                                (cdr |%%_v5IVj1JlK_def|)
                                                                ((source-file
                                                                   .
                                                                   "file:/home/kjetil/hurtigmixer/src/temp.scm")
                                                                 (line-number
                                                                   .
                                                                   9)
                                                                 (column-number
                                                                   .
                                                                   60)
                                                                 (source-kind
                                                                   .
                                                                   user)))
                                                              |%%_v5mZlAIlK_body|)))
                                            (cons (#%quote hashtable/put!)
                                                  (cons (#%quote all-macros)
                                                        (cons (list (#%quote
                                                                      quote)
                                                                    |%%_v5IVj1JlK_def|)
                                                              |%%_v5mZlAIlK_body|))))
                                          ((source-file
                                             .
                                             "file:/home/kjetil/hurtigmixer/src/temp.scm")
                                           (line-number . 8)
                                           (column-number . 7)
                                           (source-kind . user)))
                                        (cons (#%quote define-macro)
                                              (cons |%%_v5IVj1JlK_def|
                                                    |%%_v5mZlAIlK_body|))))
                                (syntax-object->datum |%%_v501o7IlK_args|))))
                     |%%_v5k8sdHlK_tmp|)
              (syntax-error |%%_v5-buMGlK_tmp|)))
          ($syntax-dispatch
            (#%annotate |%%_v5-buMGlK_tmp| value)
            (#%quote (any . any)))))
       |%%_v5EfwjGlK_y|))))
(#%program
  ((|%%_v52ShuJlK_y| . 1)
   ($syntax-dispatch . 1)
   (|%%_v5oOfXJlK_tmp| . 2)
   (syntax-error . 1)
   (|%%_v5qD9iLlK_args| . 1)
   (syntax-object->datum . 1)
   (|%%_v5Mz7LLlK_body| . 1)
   (|%%_v56w5cMlK_def| . 1)
   (|%%_v5ss3FMlK_name| . 1)
   (cons . 3)
   (|%%_v54HbRKlK_define-macro| . 1)
   (datum->syntax-object . 1)
   (apply . 2)
   (|%%_v5KKdoKlK_tmp| . 2)
   ($sc-put-cte . 1))
  ()
  ($sc-put-cte
    syntax-error
    apply
    datum->syntax-object
    syntax-object->datum
    cons
    $syntax-dispatch)
  ($sc-put-cte
    'c-defmacro
    (#%lambda #t
      (|%%_v52ShuJlK_y|)
      ()
      ((#%lambda #t
         (|%%_v5oOfXJlK_tmp|)
         ()
         ((#%lambda #t
            (|%%_v5KKdoKlK_tmp|)
            (|%%_v5oOfXJlK_tmp|)
            (#%if |%%_v5KKdoKlK_tmp|
              (apply (#%lambda #t
                       (|%%_v54HbRKlK_define-macro| |%%_v5qD9iLlK_args|)
                       ()
                       (datum->syntax-object
                         |%%_v54HbRKlK_define-macro|
                         (apply (#%lambda #t
                                  (|%%_v5ss3FMlK_name|
                                    |%%_v56w5cMlK_def|
                                    .
                                    |%%_v5Mz7LLlK_body|)
                                  ()
                                  (cons (#%quote d-define-macro)
                                        (cons (cons |%%_v5ss3FMlK_name|
                                                    |%%_v56w5cMlK_def|)
                                              |%%_v5Mz7LLlK_body|)))
                                (syntax-object->datum |%%_v5qD9iLlK_args|))))
                     |%%_v5KKdoKlK_tmp|)
              (syntax-error |%%_v5oOfXJlK_tmp|)))
          ($syntax-dispatch
            (#%annotate |%%_v5oOfXJlK_tmp| value)
            (#%quote (any . any)))))
       |%%_v52ShuJlK_y|))))
(#%program
  ((all-macros . 1)
   (hashq-ref . 1)
   (cdr . 1)
   (apply . 1)
   (|%%_v58l_yNlK_qua| . 2)
   (null? . 