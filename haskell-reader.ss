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
    ; whitespace
    (hwhitespace (:: hwhitestuff (:* hwhitestuff)))
    (hwhitestuff (:or hwhitechar hcomment hncomment))
    (hwhitechar (:or hnewline hvertab hspace htab))
    (hnewline (:or (:: hreturn hlinefeed) hreturn hlinefeed hformfeed))
    (hreturn (string #\return))
    (hlinefeed (string #\newline))
    (hvertab #\vtab)
    (hformfeed (string #\page))
    (hspace (string #\space))
    (htab (string #\tab))
    
    (hprogram (:* (:or item hwhitespace)))
    (hitem (:or qvarid qconid qvarsym qconsym literal special reservedop reservedid))

    (hliteral (:or hinteger hfloat hchar hstring))
    
    ; numeric literals
    (hinteger (:or hdecimal (:: "0o" hoctal) (:: "0O" hoctal) (:: "0x" hhexadecimal) (:: "0X" hhexadecimal)))
    (hdecimal (:: hdigit (:* hdigit)))
    (hdigit (:/ #\0 #\9))
    (hoctal (:: hoctit (:* hoctit)))
    (hoctit (:/ #\0 #\7))
    (hhexadecimal (:: hhexit (:* hhexit)))
    (hhexit (:or hdigit (:/ #\A #\F) (:/ #\a #\f)))
    (hfloat (:: hdecimal "." hdecimal (:? hexponent)))
    (hexponent (:: (:or "e" "E") (:? (:or "+" "-")) hdecimal))
    
    ; character and string literals
    (hchar (:: "'" (:or (:- hgraphic (:or "'" (string #\\))) hspace (:- hescape (:: (string #\\) "&")) "'")))
    (hgraphic (:or hsmall hlarge hsymbol hspecial hdigit ":" (string #\") "'"))
    (hsmall (:or (:/ #\a #\z) "_"))
    (hlarge (:or (:/ #\A #\Z)))
    (hsymbol (:or "~" "!" "@" "#" "$" "%" "^" "&" "*" "-" "+" "=" (string #\\) "|" "." "/" "<" ">" "?"))
    (hspecial (:or "(" ")" "," ";" "[" "]" "`" "{" "}"))
    (hescape (:: (string #\\) (:or "a" "b" "f" "n" "r" "t" "v" (string #\\) (string #\") "'" "&")))
    (hstring (:: (string #\") (:* (:or (:- hgraphic (:or (string #\") (string #\\))) hspace hescape hgap)) (string #\")))
    (hgap (:: (string #\\) hwhitechar (:* hwhitechar) (string #\\)))
    
    ; comments
    (hcomment (:: hdashes (:? (:- hany hsymbol) (:* hany)) hnewline))
    (hdashes (:: "--" (:* "-")))
    (hopencom "{-")
    (hclosecom "-}")
    (hncomment (:: hopencom hANYseq (:* hncomment hANYseq) hclosecom))
    (hANYseq (:- hANY (:: (:* hANY) (:* (:: hopencom hclosecom)) (:* hANY))))
    (hANY (:or hgraphic hwhitechar))
    (hany (:or hgraphic hspace htab))
    
    ; identifiers and operators
    (hvarid (:- (:: hsmall (:* (:or hsmall hlarge hdigit "'"))) hreservedid))
    (hreservedid (:or "case"
                     "data"
                     "else"
                     "if"
                     "in"
                     "let"
                     "newtype"
                     "of"
                     "then"
                     "type"
                     "where"
                     "_"))
    (hconid (hlarge (:* (:or hsmall hlarge hdigit "'"))))
    (hvarsym (:- (:: hsymbol (:* (:or hsymbol ":"))) (:or hreservedop hdashes)))
    (hreservedop (:or ":" "::" "=" (string #\\) "|" "->"))
    (hconsym (:- (:: ":" (:* (:or hsymbol ":"))) hreservedop))
    (htyvar hvarid)
    (htycon hconid)
    
    ; variables
    (exp (:or (:: (string #\\) (:: apat (:* apat)) "->" exp)
              (:: "let" decls "in" exp)
              (:: "if" exp "then" exp "else" exp)
              (:: "case" exp "of" "{" alts "}")
              fexp))
    (fexp (:: (:? fexp) aexp))
    (aexp (:or qvar gcon literal (:: "(" exp ")") (:: "(" exp (:+ exp) ")") (:: "[" (:+ exp) "]")))
    (gcon (:or (:: "(" ")") (:: "[" "]") (:: "(" "," (:* ",") ")") qcon))

    (hvar (:or hvarid (:: "(" hvarsym ")")))
    (hcon (:or hconid (:: "(" hconsym ")")))
    (hvarop (:or hvarsym (:: "`" hvarid "`")))
    (hconop (:or hconsym (:: "`" hconid "`")))
    (hop (:or hvarop hconop))
    (gconsym (:or ":" qconsym))
    (fexp (:: (:? fexp) aexp))
    (hexp (:or (:: (string #\\) (:+ apat) "->" hexp) (:: hexp qop hexp) (:: "-" qop) (:: "if" hexp "then" hexp "else" hexp)))
    )
    
  
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