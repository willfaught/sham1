(module ConvertersTest mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "Converters.ss" "sham")
           (lib "HaskellSyntax.ss" "sham" "haskell")
           (lib "Parsers.ss" "sham" "haskell"))
  
  (provide testSuite)
  
  ; eHM :: string string datum -> schemeunit-test-case
  (define (eHM name type datum)
    (test-equal? name (convertHM (parseT type) 'test) datum))
  
  ; xHM :: string string -> schemeunit-test-case
  (define (xHM name type)
    (test-exn name (lambda (x) #t) (lambda () (convertHM (parseT type) 'test))))
  
  ; testSuite :: schemeunit-test-suite
  (define testSuite
    (test-suite "Converters"
                (xHM "co1" "Char")))
  
  ; parseT :: string -> HaskellSyntax
  (define (parseT expression)
    (let ((port (open-input-string expression)))
      (port-count-lines! port)
      (tParser (lambda () (language-lexer port)))))
  
  ; tParser :: parser
  (define tParser (type-parser "test")))