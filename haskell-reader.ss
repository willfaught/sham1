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
    (program (:* (:or item ws)))
    (item (:or qvarid qconid qvarsym qconsym literal special reservedop reservedid))

    (literal (:or integer float char string))
    
    ; whitespace
    (ws (:: whitestuff (:* whitestuff)))
    (whitestuff (:or whitechar comment ncomment))
    (whitechar (:or newline vertab space tab))
    (newline (:or (:: return linefeed) return linefeed formfeed))
    (return (string #\return))
    (linefeed (string #\newline))
    (vertab #\vtab)
    (formfeed (string #\page))
    (space (string #\space))
    (tab (string #\tab))
    
    ; comments
    (comment (:: dashes (:? (:- any symbol) (:* any)) newline))
    (dashes (:: "--" (:* "-")))
    (opencom "{-")
    (closecom "-}")
    (ncomment (:: opencom ANYseq (:* ncomment ANYseq) closecom))
    (ANYseq (:- ANY (:: (:* ANY) (:* (:: opencom closecom)) (:* ANY))))
    (ANY (:or graphic whitechar))
    (any (:or graphic space tab))
    (graphic (:or small large symbol digit special ":" "#\"" "'"))
    (special (:or "(" ")" "," ";" "[" "]" "`" "{" "}"))
    
    ; characters
    (small (:or asc-small "_"))
    (asc-small (:/ #\a #\z))
    (large (:or asc-large))
    (asc-large (:/ #\A #\Z))
    (symbol (:or asc-symbol))
    (asc-symbol (:or "!" "#" "$" "%" "&" "*" "+" "." "/" "<" "=" ">" "?" "@" (string #\\) "^" "|" "-" "~"))
    (digit (:or asc-digit))
    (asc-digit (:/ #\0 #\9))
    (octit (:/ #\0 #\7))
    (hexit (:or digit (:/ #\A #\F) (:/ #\a #\f)))
    (decimal (:: digit (:* digit)))
    (octal (:: octit (:* octit)))
    (hexadecimal (:: hexit (:* hexit)))
    (integer (:or decimal (:: "0o" octal) (:: "0O" octal) (:: "0x" hexadecimal) (:: "0X" hexadecimal)))
    (float (:: decimal "." decimal (:? exponent)))
    (exponent (:: (:or "e" "E") (:? (:or "+" "-")) decimal))
    (varid (:- (:: small (:* (:or small large digit "'"))) reservedid))
    (conid (large (:* (:or small large digit "'"))))
    (reservedid (:or "case"
                     "class"
                     "data"
                     "default"
                     "deriving"
                     "do"
                     "else"
                     "if"
                     "import"
                     "in"
                     "infix"
                     "infixl"
                     "infixr"
                     "instance"
                     "let"
                     "module"
                     "newtype"
                     "of"
                     "then"
                     "type"
                     "where"
                     "_"))
    (varsym (:- (:: symbol (:* (:or symbol ":"))) (:or reservedop dashes)))
    (consym (:- (:: ":" (:* (:or symbol ":"))) reservedop))
    (reservedop (:or ".." ":" "::" "=" (string #\\) "|" "<-" "->" "@" "~" "=>"))
    (tyvar varid)
    (tycon conid)
    (tycls conid)
    (modid conid)
    (qvarid (:: (:? modid ".") varid))
    (qconid (:: (:? modid ".") conid))
    (qtycon (:: (:? modid ".") tycon))
    (qtycls (:: (:? modid ".") tycls))
    (qvarsym (:: (:? modid ".") varsym))
    (qconsym (:: (:? modid ".") consym))
    (char (:: "'" (:or (:- graphic (:or "'" (string #\\))) space (:- escape (:: (string #\\) "&")) "'")))
    (string (:: (string #\") (:or (:- graphic (:or (string #\") (string #\\))) space escape gap) (string #\")))
    (escape (:: (string #\\) (:or charesc ascii decimal (:: "o" octal) (:: "x" hexadecimal))))
    (charesc (:or "a" "b" "f" "n" "r" "t" "v" (string #\\) (string #\") "'" "&"))
    (ascii (:or (:: "^" cntrl) "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN" "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"))
    (cntrl (:or asc-large "@" "[" (space #\\) "]" "ˆ" "_"))
    (gap (:: (space #\\) whitechar (:* whitechar) (space #\\)))
    (aexp (:or qvar gcon literal ()))
    (gcon (:or (:: "(" ")") (:: "[" "]") (:: "(" "," (:* ",") ")") qcon))
    (var (:or varid (:: "(" varsym ")")))
    (qvar (:or qvarid (:: "(" qvarsym ")")))
    (con (:or conid (:: "(" consym ")")))
    (qcon (:or qconid (:: "(" gconsym ")")))
    (varop (:or varsym (:: "`" varid "`")))
    (qvarop (:or qvarsym (:: "`" qvarid "`")))
    (conop (:or consym (:: "`" conid "`")))
    (qconop (:or gconsym (:: "`" qconid "`")))
    (op (:or varop conop))
    (qop (:or qvarop qconop))
    (gconsym (:or ":" qconsym))
    (fexp (:: (:? fexp) aexp))
    (exp (:or (:: (string #\\) (:+ apat) "->" exp) (:: exp qop exp) (:: "-" qop) (:: "if" exp "then" exp "else" exp)))
    (qop (:or qvarop qconop))
    (
    
  
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