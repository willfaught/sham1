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
    (hprogram (:* (:or hlexeme hwhitespace)))
    (hlexeme (:or hqvarid hqconid hqvarsym hqconsym hliteral hspecial hreservedop hreservedid))
    (hliteral (:or hinteger hfloat hchar hstring))
    (hspecial (:or "(" ")" "," ";" "[" "]" "`" "{" "}"))
    
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
    
    (hcomment (:: hdashes (:? (:- hany hsymbol) (:* hany)) hnewline))
    (hdashes (:: "--" (:* "-")))
    (hopencom "{-")
    (hclosecom "-}")
    (hncomment (:: hopencom hANYseq (:* (:: hncomment hANYseq)) hclosecom))
    (hANYseq (:- hANY (:: (:* hANY) (:* (:: hopencom hclosecom)) (:* hANY))))
    (hANY (:or hgraphic hwhitechar))
    (hany (:or hgraphic hspace htab))
    (hgraphic (:or hsmall hlarge hsymbol hdigit hspecial ":" (string #\") "'"))

    (hsmall (:or (:/ #\a #\z) "_"))
    
    (hlarge (:or (:/ #\A #\Z)))
    (hsymbol (:or "~" "!" "@" "#" "$" "%" "^" "&" "*" "-" "+" "=" (string #\\) "|" "." "/" "<" ">" "?"))
    
    (hdigit (:/ #\0 #\9))
    (hoctit (:/ #\0 #\7))
    (hhexit (:or hdigit (:/ #\A #\F) (:/ #\a #\f)))
    
    (hvarid (:- (:: hsmall (:* (:or hsmall hlarge hdigit "'"))) hreservedid))
    (hconid (hlarge (:* (:or hsmall hlarge hdigit "'"))))
    (hreservedid (:or "case" "class" "data" "default" "deriving" "do" "else" "if" "import" "in" "infix" "infixl" "infixr" "instance" "let" "module" "newtype" "of" "then" "type" "where" "_"))
    
    (hvarsym (:- (:: hsymbol (:* (:or hsymbol ":"))) (:or hreservedop hdashes)))
    (hconsym (:- (:: ":" (:* (:or hsymbol ":"))) hreservedop))
    (hreservedop (:or ":" "::" "=" (string #\\) "|" "->"))
    
    (htyvar hvarid)
    (htycon hconid)
    (htycls hconid)
    (hmodid hconid)
    
    (hqvarid (:: (:: (:? hmodid) ".") hvarid))
    (hqconid (:: (:: (:? hmodid) ".") hconid))
    (hqtycon (:: (:: (:? hmodid) ".") htycon))
    (hqtycls (:: (:: (:? hmodid) ".") htycls))
    (hqvarsym (:: (:: (:? hmodid) ".") hvarsym))
    (hqconsym (:: (:: (:? hmodid) ".") hconsym))
    
    (hdecimal (:: hdigit (:* hdigit)))
    (hoctal (:: hoctit (:* hoctit)))
    (hhexadecimal (:: hhexit (:* hhexit)))
    
    (hinteger (:or hdecimal (:: "0o" hoctal) (:: "0O" hoctal) (:: "0x" hhexadecimal) (:: "0X" hhexadecimal)))
    (hfloat (:: hdecimal "." hdecimal (:? hexponent)))
    (hexponent (:: (:or "e" "E") (:? (:or "+" "-")) hdecimal))
    
    (hchar (:: "'" (:or (:- hgraphic (:or "'" (string #\\))) hspace (:- hescape (:: (string #\\) "&")) "'")))
    (hstring (:: (string #\") (:* (:or (:- hgraphic (:or (string #\") (string #\\))) hspace hescape hgap)) (string #\")))
    (hescape (:: (string #\\) (:or hcharesc hascii hdecimal (:: "o" hoctal) (:: "x" hhexadecimal))))
    (hcharesc (:or "a" "b" "f" "n" "r" "t" "v" (string #\\) (string #\") "'" "&"))
    (hascii (:or (:: "^" hcntrl) "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN" "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"))
    (hcntrl (:or hlarge "@" "[" (string #\\) "]" "^" "_"))
    (hgap (:: (string #\\) hwhitechar (:* hwhitechar) (string #\\)))
    
    (hmodule (:or (:: "module" hmodid (:? hexports) "where" hbody) hbody))
    (hbody (:or (:: "{" himpdecls ";" htopdecls "}") (:: "{" himpdecls "}") (:: "{" htopdecls "}")))
    
    (himpdecls (:: himpdecl (:* (:: ";" himpdecl))))
    
    (hexports (:: "(" hexport (:* (:: "," hexport)) (:? ",") ")"))
    
    (hexport (:or hqvar (:: hqtycon (:? (:or (:: "(" ".." ")") (:: "(" hcname (:* (:: "," hcname)) ")")))) (:: hqtycls (:? (:or (:: "(" ".." ")") (:: "(" hqvar (:* (:: "," hqvar)))))) (:: "module" hmodid)))
    
    (himpdecl (:or (:: "import" (:? "qualified") hmodid (:? "as" hmodid) (:? himpspec)) nothing))
    
    (himpspec (:or (:: "(" himport (:* (:: "," himport)) (:? himport) ")") (:: "hiding" (:: "(" himport (:* (:: "," himport)) (:? himport) ")"))))
    
    (himport (:or hvar (:: htycon (:? (:or (:: "(" ".." ")") (:: "(" hcname (:* (:: "," hcname)) ")")))) (:: htycls (:? (:or (:: "(" ".." ")") (:: "(" hvar (:* (:: "," hvar))))))))
    (cname (:or hvar hcon))
    
    (htopdecls (:? (:: htopdecl (:* (:: ";" htopdecl)))))
    (htopdecl (:or (:: "type" hsimpletype "=" htype)
                   (:: "data" (:? (:: hcontext "=>")) hsimpletype "=" hconstrs (:? hderiving))
                   (:: "newtype" (:? (:: hcontext "=>")) hsimpletype "=" hnewconstr (:? hderiving))
                   (:: "class" (:? (:: hscontext "=>")) htycls htyvar (:? (:: "where" hcdecls)))
                   (:: "instance" (:? (:: hscontext "=>")) hqtycls hinst (:? (:: "where" hidecls)))
                   (:: "default" "(" (:? htype (:* (:: "," htype))) ")")
                   hdecl))
    
    (hdecls (:: "{" (:? hdecl (:* (:: "," hdecl))) "}"))
    (hdecl (:or hgendecl (:: (:or hfunlhs hpat) hrhs)))
    
    (hcdecls (:: "{" (:? hcdecl (:* (:: "," hcdecl))) "}"))
    (hcdecl (:or hgendecl (:: (:or hfunlhs hvar) hrhs)))
    
    (hgendecl (:or (:: hvars "::" (:? (:: hcontext "=>")) htype) (:: hfixity (:? hinteger) hops) nothing))
    
    (hops (:: hop (:* (:: "," hop))))
    (hvars (:: hvar (:* (:: "," hvar))))
    (hfixity (:or "infixl" "infixr" "infix"))
    
    (htype (:: hbtype (:? (:: "->" htype))))
    
    (hbtype (:: (:? hbtype) hatype))
    
    (hatype (:or hgtycon htyvar (:: "(" htype (:+ (:: "," htype)) ")") (:: "[" htype "]") (:: "(" htype ")")))
    
    (hgtycon (:or hqtycon "()" "[]" (:: "(" "->" ")") (:: "(" (:+ ",") ")")))
    
    (hcontext (:or hclass (:: "(" (:? (:: hclass (:* (:: "," hclass)))) ")")))
    (hclass (:or (:: hqtycls htyvar) (:: hqtycls "(" htyvar hatype (:* (:: "," hatype)) ")")))
    (hscontext (:or hsimpleclass (:: "(" (:? (:: hsimpleclass (:* (:: "," hsimpleclass)))) ")")))
    (hsimpleclass (:or hqtycls htyvar))
    
    (hsimpletype (:: htycon (:* htyvar)))
    
    (hconstrs (:: hconstr (:* (:: "|" hconstr))))
    
    (hconstr (:or (:: hcon (:* (:: (:? "!") hatype)))
                  (:: (:or hbtype (:: "!" hatype)) hconop (:or hbtype (:: "!" hatype)))
                  (:: hcon "{" (:? hfielddecl (:* (:: "," hfielddecl "}"))))))
    (hnewconstr (:or (:: hcon hatype) (:: hcon "{" hvar "::" htype "}")))
    (hfielddecl (:: hvars "::" (:or htype (:: "!" hatype))))
    (hderiving (:: "deriving" (:or hdclass (:: "(" hdclass (:* (:: "," hdclass)) ")"))))
    (hdclass hqtycls)
    
    (hinst (:or hgtycon (:: "(" hgtycon (:* htyvar) ")") (:: "(" htyvar (:+ (:: "," htyvar)) ")") (:: "[" htyvar "]") (:: "(" htyvar "->" htyvar)))
    
    (hfunlhs (:or (:: hvar hapat "{" hapat "}") (:: hpat hvarop hpat) (:: hlpat hvarop hpat) (:: hpat hvarop hrpat) (:: "(" hfunlhs ")" hapat "{" hapat "}")))
    
    (hrhs (:or (:: "=" hexp (:? (:: "where" hdecls))) (:: hgdrhs (:? (:: "where" hdecls)))))
    
    (hgdrhs (:: hgd "=" hexp (:? hgdrhs)))
    
    (hgd (:: "|" hexp))
    
    (hexp (:or (:: hexp "::" (:? (:: hcontext "=>")) htype)
               hexp
               (:: hexp (:? (:: hqop hexp))) ;exp^i
               hlexp ;exp^i
               hrexp)) ;exp^i
    (hlexp (:: (:or hlexp hexp) hqop hexp)
                  
    (hvar (:or hvarid (:: "(" hvarsym ")")))
    (hcon (:or hconid (:: "(" hconsym ")")))
    (hvarop (:or hvarsym (:: "`" hvarid "`")))
    (hconop (:or hconsym (:: "`" hconid "`")))
    (hop (:or hvarop hconop))
    (hgconsym (:or ":" hconsym))
    
    (haexp (:or hvar hgcon hliteral (:: "(" hexp ")") (:: "(" hexp (:+ (:: "," hexp)) ")") (:: "[" hexp (:+ (:: "," hexp)) "]")))
    (hgcon (:or (:: "(" ")") (:: "[" "]") (:: "(" (:+ ",") ")") hcon))
    (hfexp (:: (:? hfexp) haexp))
    (hexp (:or (:: (string #\\) (:: hapat (:* hapat)) "->" hexp)
               (:: "if" hexp "then" hexp "else" hexp)
               (:: "let" hdecls "in" hexp)
               (:: "case" hexp "of" "{" halts "}")
               fexp
               (:: hexp hop hexp)
               (:: "-" hexp)))
    (halts (:: halt (:* (::";" halt))))
    (halt (:: hpat "->" hexp (:? (:: "where" hdecls))))
    

    
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