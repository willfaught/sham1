(module haskell-reader mzscheme
  (require (lib "lex.ss" "parser-tools")
           (prefix : (lib "lex-sre.ss" "parser-tools"))
           (lib "yacc.ss" "parser-tools")
           (lib "readerr.ss" "syntax")
           "scheme-emitter.ss")

  (provide (rename read-haskell-syntax read-syntax))
  
  (define-empty-tokens keywords (lparen
                                 rparen
                                 bslash
                                 arrow
                                 plus
                                 minus
                                 asterisk
                                 fslash
                                 eof))
  
  (define-tokens regular (varid num))
  
  (define-lex-abbrevs
    (small (:or asc-small "_"))
    (asc-small (:/ #\a #\z))
    (large (:or asc-large))
    (asc-large (:/ #\A #\Z))
    (asc-digit (:/ #\0 #\9))
    (digit asc-digit)
    (octit (:/ #\0 #\7))
    (hexit (:or digit (:/ #\A #\F) (:/ #\a #\f)))
    (decimal (:: digit (:* digit)))
    (octal (:: octit (:* octit)))
    (hexadecimal (:: hexit (:* hexit)))
    (integer (:or decimal (:: "0o" octal) (:: "0O" octal) (:: "0x" hexadecimal) (:: "0X" hexadecimal)))
    (float (:: decimal "." decimal (:? exponent)))
    (exponent (:: (:or "e" "E") (:? (:or "+" "-")) decimal)))
  
  (define haskell-lexer (lexer-src-pos (whitespace (return-without-pos (haskell-lexer input-port)))
                                       ("(" (token-lparen))
                                       (")" (token-rparen))
                                       ("\\" (token-bslash))
                                       ("->" (token-arrow))
                                       ("+" (token-plus))
                                       ("-" (token-minus))
                                       ("*" (token-asterisk))
                                       ("/" (token-fslash))
                                       ((eof) (token-eof))
                                       ((:: small (:* (:or small large digit "'"))) (token-varid lexeme))
                                       ((:or integer float) (token-num lexeme))))
  
  (define (haskell-parser source-name) (parser (src-pos)
                                               (tokens keywords regular)
                                               (start term)
                                               (end eof)
                                               (error (lambda (token-ok token-name token-value start-pos end-pos)
                                                        (raise-read-error "parse error"
                                                                          source-name
                                                                          (position-line start-pos)
                                                                          (position-col start-pos)
                                                                          (position-offset start-pos)
                                                                          (- (position-offset end-pos) (position-offset start-pos)))))
                                               (grammar (term ((identifier) $1)
                                                              ((abstraction) $1)
                                                              ((application) $1)
                                                              ((addition) $1)
                                                              ((subtraction) $1)
                                                              ((multiplication) $1)
                                                              ((division) $1)
                                                              ((lparen term rparen) $2)
                                                              ((primitive-value) $1))
                                                        (identifier ((varid) (make-hid $1)))
                                                        (abstraction ((bslash varid arrow term) (make-hfun $2 $4)))
                                                        (application ((term term) (make-happ $1 $2)))
                                                        (addition ((plus term term) (make-hadd $2 $3)))
                                                        (subtraction ((minus term term) (make-hsub $2 $3)))
                                                        (multiplication ((asterisk term term) (make-hmul $2 $3)))
                                                        (division ((fslash term term) (make-hdiv $2 $3)))
                                                        (primitive-value ((num) (make-hnum (string->number $1)))))))
  
  (define (parse) ((haskell-parser "prompt") (lambda () (haskell-lexer (current-input-port)))))
  
  (define (prompt) (eval (emit-scheme (parse))))
  
  (define (wrap value) (vector (lambda () value) 'empty))
  
  (define (read-haskell-syntax) 'incomplete)
)