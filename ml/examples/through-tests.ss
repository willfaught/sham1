(module through-tests mzscheme
  
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (file "simple.ocaml"))
  
  (provide (all-defined))
  
  (define simple-test-suite
    (test-suite
     "simple"
     (test-equal? "A" (fact 5) 120)
     
     (test-equal? "b" (even 2) #t)

     (test-equal? "c" (even 5) #f)
     
     (test-equal? "d" (sum 5) 15)
     
     (test-equal? "e" ((add 3) 4) 7)
     (test-equal? "str" strA "abcd")
     (test-equal? "strtest" strtest #t)))
  
  (require (file "tuples.ocaml"))
  
  (define tuple-test-suite 
    (test-suite
    "tuples"
    (test-equal? "a" first-test #t)
    (test-equal? "b" second-test #f)
    (test-equal? "vit" vector-import-test-1 3)
    (test-equal? "ntt" nested-tuple-test 5)
    (test-equal? "c" an-import-test3 6)
    (test-exn "d" (ep "Expected something of type \\(int \\* int\\), got: 34") (lambda () (bad1 34)))
    ))
  
  (require (file "unit.ocaml"))
  
  (define unit-test-suite
    (test-suite
     "unit"
     (test-equal? "a" unit-test1 #())
     (test-equal? "b" unit-test2 #())
     (test-equal? "c" unit-test3 #())
     (test-exn "d" (ep "Expected something of type int, got:")
               (lambda () (unit-runerr1 42)))
     (test-exn "e" (ep "Expected something of type \\(\\), got:")
               (lambda () (unit-runerr2 43)))))
  

  
  
  ;; Yikes: looks like you get different error messages for first-order imports and components of HO imports...
  
  (define ((ep regex) exn)
    (regexp-match regex (exn-message exn)))
  

  
  )
