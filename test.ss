(module test mzscheme
  (require (lib "list.ss" "srfi" "1"))
            
  (provide (all-defined))
  
  (define-struct test (name expression result))
  
  (define (run-test f)
    (lambda (test)
      (define result (f (test-expression test)))
      (if (equal? result (test-result test)) #t (format "\n~a failed\n  expected: ~a\n  actual: ~a\n" (test-name test) (test-result test) result))))
  
  (define (run-tests f tests)
    (define results (filter (lambda (x) (not (equal? x #t))) (map (run-test f) tests)))
    (if (null? results)
        (display "All passed")
        (map display results))))