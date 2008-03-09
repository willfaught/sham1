(module haskell-reader mzscheme
  (require (lib "lex.ss" "parser-tools")
           (prefix : (lib "lex-sre.ss" "parser-tools"))
           (lib "yacc.ss" "parser-tools")
           (lib "readerr.ss" "syntax")
           "scheme-emitter.ss")

  (provide (rename read-haskell-syntax read-syntax))
  
  (define-empty-tokens keywords (eof t-as t-backtick t-bang t-class t-colon t-coloncolon t-comma t-darrow t-data t-default t-periodperiod t-equal t-hiding t-import t-instance t-lcbracket t-lrbracket t-lsbracket t-module t-newtype t-ocrbrackets t-ocsbrackets t-period t-pipe t-qualified t-rcbracket t-rrbracket t-rsbracket t-sarrow t-semicolon t-type t-where))
  
  (define-tokens regular (t-conid t-consym t-varid t-varsym))
  
  (define haskell-lexer (lexer-src-pos ("as" (token-t-as))
                                       ("`" (token-t-backtick))
                                       ("!" (token-t-bang))
                                       ("class" (token-t-class))
                                       (":" (token-t-colon))
                                       ("::" (token-t-coloncolon))
                                       ("," (token-t-comma))
                                       ("=>" (token-t-darrow))
                                       ("data" (token-t-data))
                                       ("default" (token-t-default))
                                       (".." (token-t-periodperiod))
                                       ("=" (token-t-equal))
                                       ("hiding" (token-t-hiding))
                                       ("import" (token-t-import))
                                       ("instance" (token-t-instance))
                                       ("{" (token-t-lcbracket))
                                       ("(" (token-t-lrbracket))
                                       ("[" (token-t-lsbracket))
                                       ("module" (token-t-module))
                                       ("newtype" (token-t-newtype))
                                       ("()" (token-t-ocrbrackets))
                                       ("[]" (token-t-ocsbrackets))
                                       ("." (token-t-period))
                                       ("|" (token-t-pipe))
                                       ("qualified" (token-t-qualified))
                                       ("}" (token-t-rcbracket))
                                       (")" (token-t-rrbracket))
                                       ("]" (token-t-rsbracket))
                                       ("->" (token-t-sarrow))
                                       (";" (token-t-semicolon))
                                       ("type" (token-t-type))
                                       ("where" (token-t-where))
                                       ((:: hlarge (:* (:or hsmall hlarge hdigit "'"))) (token-t-conid lexeme))
                                       ((:- (:: ":" (:* (:or hsymbol ":"))) hreservedop) (token-t-consym))
                                       ((:- (:: hsmall (:* (:or hsmall hlarge hdigit "'"))) hreservedid) (token-t-varid lexeme))
                                       ((:- (:: hsymbol (:* (:or hsymbol ":"))) (:or hreservedop hdashes)) (token-t-varsym lexeme))
                                       (hwhitespace (return-without-pos (haskell-lexer input-port)))
                                       ((eof) (token-eof))))

  (define (haskell-parser source-name) (parser (src-pos)
                                               ;(debug "debug.txt")
                                               (tokens keywords regular)
                                               (start nt-module)
                                               (end eof)
                                               (error (lambda (token-ok token-name token-value start-pos end-pos)
                                                        (raise-read-error (format "parser: malformed token: source ~a, line ~a, column ~a: ~a: ~a"
                                                                                  source-name
                                                                                  (position-line start-pos)
                                                                                  (position-col start-pos)
                                                                                  token-name
                                                                                  token-value)
                                                                          source-name
                                                                          (position-line start-pos)
                                                                          (position-col start-pos)
                                                                          (position-offset start-pos)
                                                                          (- (position-offset end-pos) (position-offset start-pos)))))
                                               (grammar (nt-module ((t-module nt-modid nt-module-2 t-where nt-body) null)
                                                                   ((nt-body) null))
                                                        (nt-module-2 (() null)
                                                                     ((nt-exports) null))
                                                        (nt-modid ((t-conid) null))
                                                        (nt-exports ((t-lrbracket nt-exports-2 nt-exports-3 t-rrbracket) null))
                                                        (nt-exports-2 (() null)
                                                                     ((nt-export nt-exports-2-2) null))
                                                        (nt-exports-2-2 (() null)
                                                                       ((t-comma nt-export nt-exports-2-2) null))
                                                        (nt-exports-3 (() null)
                                                                      ((t-comma) null))
                                                        (nt-export ((nt-qvar) null)
                                                                   ((nt-qtycon nt-export-2) null)
                                                                   ((nt-qtycls nt-export-3) null)
                                                                   ((t-module nt-modid) null))
                                                        (nt-export-2 (() null)
                                                                     ((t-lrbracket t-periodperiod t-rrbracket) null)
                                                                     ((t-lrbracket nt-export-2-2 t-rrbracket) null))
                                                        (nt-export-2-2 (() null)
                                                                       ((nt-cname nt-export-2-2-2) null))
                                                        (nt-export-2-2-2 (() null)
                                                                         ((t-comma nt-cname nt-export-2-2-2) null))
                                                        (nt-export-3 (() null)
                                                                     ((t-lrbracket t-periodperiod t-rrbracket) null)
                                                                     ((t-lrbracket nt-export-3-2 t-rrbracket) null))
                                                        (nt-export-3-2 (() null)
                                                                       ((nt-qvar nt-export-3-2-2) null))
                                                        (nt-export-3-2-2 (() null)
                                                                         ((t-comma nt-qvar nt-export-3-2-2) null))
                                                        (nt-qvar ((nt-qvarid) null)
                                                                 ((t-lrbracket nt-qvarsym t-rrbracket) null))
                                                        (nt-qvarid ((nt-qvarid-2 t-varid) null))
                                                        (nt-qvarid-2 (() null)
                                                                     ((nt-modid t-period) null))
                                                        (nt-qvarsym ((nt-qvarsym-2 t-varsym) null))
                                                        (nt-qvarsym-2 (() null)
                                                                      ((nt-modid t-period) null))
                                                        (nt-cname ((nt-var) null)
                                                                  ((nt-con) null))
                                                        (nt-var ((t-varid) null)
                                                                ((t-lrbracket t-varsym t-rrbracket) null))
                                                        (nt-con ((t-conid) null)
                                                                ((t-lrbracket t-consym t-rrbracket) null))
                                                        (nt-conop ((t-consym) null)
                                                                  ((t-backtick t-conid t-backtick) null))
                                                        (nt-body ((t-lcbracket nt-impdecls t-semicolon nt-topdecls t-rcbracket) null)
                                                                 ((t-lcbracket nt-impdecls t-rcbracket) null)
                                                                 ((t-lcbracket nt-topdecls t-rcbracket) null))
                                                        (nt-impdecls ((nt-impdecl nt-impdecls-2) null))
                                                        (nt-impdecls-2 (() null)
                                                                       ((t-semicolon nt-impdecl nt-impdecls-2) null))
                                                        (nt-impdecl ((t-import nt-impdecl-2 nt-modid nt-impdecl-3 nt-impdecl-4) null)
                                                                    (() null))
                                                        (nt-impdecl-2 (() null)
                                                                      ((t-qualified) null))
                                                        (nt-impdecl-3 (() null)
                                                                      ((t-as nt-modid) null))
                                                        (nt-impdecl-4 (() null)
                                                                      ((nt-impspec) null))
                                                        (nt-impspec ((nt-impspec-2) null)
                                                                    ((t-hiding nt-impspec-2) null))
                                                        (nt-impspec-2 ((t-lrbracket nt-impspec-2-2 nt-impspec-2-3 t-rrbracket) null))
                                                        (nt-impspec-2-2 (() null)
                                                                        ((nt-import nt-impspec-2-2-2) null))
                                                        (nt-impspec-2-2-2 (() null)
                                                                          ((t-comma nt-import nt-impspec-2-2-2) null))
                                                        (nt-impspec-2-3 (() null)
                                                                        ((t-comma) null))
                                                        (nt-import ((nt-var) null)
                                                                   ((nt-tycon nt-import-2) null)
                                                                   ((nt-tycls nt-import-3) null))
                                                        (nt-import-2 (() null)
                                                                     ((t-lrbracket t-periodperiod t-rrbracket) null)
                                                                     ((t-lrbracket nt-import-2-2 t-rrbracket) null))
                                                        (nt-import-2-2 (() null)
                                                                       ((nt-cname nt-import-2-2-2) null))
                                                        (nt-import-2-2-2 (() null)
                                                                         ((t-comma nt-cname nt-import-2-2-2) null))
                                                        (nt-import-3 (() null)
                                                                     ((t-lrbracket t-periodperiod t-rrbracket) null)
                                                                     ((t-lrbracket nt-import-3-2 t-rrbracket) null))
                                                        (nt-import-3-2 (() null)
                                                                       ((nt-var nt-import-3-2-2) null))
                                                        (nt-import-3-2-2 (() null)
                                                                         ((t-comma nt-var nt-import-3-2-2) null))
                                                        (nt-tycon ((t-conid) null))
                                                        (nt-tycls ((t-conid) null))
                                                        (nt-topdecls (() null)
                                                                     ((nt-topdecl nt-topdecls-2) null))
                                                        (nt-topdecls-2 (() null)
                                                                       ((t-semicolon nt-topdecl nt-topdecls-2) null))
                                                        (nt-topdecl ((t-type nt-simpletype t-equal nt-type) null)
                                                                    ((t-data nt-topdecl-2 nt-simpletype t-equal nt-constrs nt-topdecl-3) null)
                                                                    ((t-newtype nt-topdecl-2 nt-simpletype t-equal nt-newconstr nt-topdecl-3) null)
                                                                    ((t-class nt-topdecl-4 nt-tycls nt-tyvar nt-topdecl-5) null)
                                                                    ((t-instance nt-topdecl-4 nt-qtycls nt-inst nt-topdecl-6) null)
                                                                    ((t-default t-lrbracket nt-topdecl-7 t-rrbracket) null)
                                                                    ((nt-decl) null))
                                                        (nt-topdecl-2 (() null)
                                                                      ((nt-context t-darrow) null))
                                                        (nt-topdecl-3 (() null)
                                                                      ((nt-deriving) null))
                                                        (nt-topdecl-4 (() null)
                                                                      ((nt-scontext t-darrow) null))
                                                        (nt-topdecl-5 (() null)
                                                                      ((t-where nt-cdecls) null))
                                                        (nt-topdecl-6 (() null)
                                                                      ((t-where nt-idecls) null))
                                                        (nt-topdecl-7 (() null)
                                                                      ((nt-type nt-topdecl-8) null))
                                                        (nt-topdecl-8 (() null)
                                                                      ((t-comma nt-type nt-topdecl-8) null))
                                                        (nt-simpletype ((nt-tycon nt-simpletype-2) null))
                                                        (nt-simpletype-2 (() null)
                                                                         ((nt-tyvar nt-simpletype-2) null))
                                                        (nt-tyvar ((t-varid) null))
                                                        (nt-type ((nt-btype nt-type-2) null))
                                                        (nt-type-2 (() null)
                                                                   ((t-sarrow nt-type) null))
                                                        (nt-btype ((nt-btype-2 nt-atype) null))
                                                        (nt-btype-2 (() null)
                                                                    ((nt-btype) null))
                                                        (nt-atype ((nt-gtycon) null)
                                                                  ((nt-tyvar) null)
                                                                  ((t-lrbracket nt-type t-comma nt-type nt-atype-2 t-rrbracket) null)
                                                                  ((t-lsbracket nt-type t-rsbracket) null)
                                                                  ((t-lrbracket nt-type t-rrbracket) null))
                                                        (nt-atype-2 (() null)
                                                                    ((t-comma nt-type nt-atype-2) null))
                                                        (nt-gtycon ((nt-qtycon) null)
                                                                   ((t-ocrbrackets) null)
                                                                   ((t-ocsbrackets) null)
                                                                   ((t-lrbracket t-sarrow t-rrbracket) null)
                                                                   ((t-lrbracket t-comma nt-gtycon-2 t-rrbracket) null))
                                                        (nt-gtycon-2 (() null)
                                                                     ((t-comma nt-gtycon-2) null))
                                                        (nt-qtycon ((nt-qtycon-2 nt-tycon) null))
                                                        (nt-qtycon-2 (() null)
                                                                     ((nt-modid t-period) null))
                                                        (nt-qtycls ((nt-qtycls-2 nt-tycls) null))
                                                        (nt-qtycls-2 (() null)
                                                                     ((nt-modid t-period) null))
                                                        (nt-constrs ((nt-constr nt-constrs-2) null))
                                                        (nt-constrs-2 (() null)
                                                                      ((t-pipe nt-constr nt-constrs-2) null))
                                                        (nt-constr ((nt-con nt-constr-2) null)
                                                                   ((nt-constr-3 nt-conop nt-constr-3) null)
                                                                   ((nt-con t-lcbracket nt-constr-4 t-rcbracket) null))
                                                        (nt-constr-2 (() null)
                                                                     ((nt-constr-2-2 nt-atype nt-constr-2) null))
                                                        (nt-constr-2-2 (() null)
                                                                       ((t-bang) null))
                                                        (nt-constr-3 ((nt-btype) null)
                                                                     ((t-bang nt-atype) null))
                                                        (nt-constr-4 (() null)
                                                                     ((nt-fielddecl nt-constr-4-2) null))
                                                        (nt-constr-4-2 (() null)
                                                                       ((t-comma nt-fielddecl nt-constr-4-2) null))
                                                        (nt-fielddecl ((nt-vars t-coloncolon nt-fielddecl-2) null))
                                                        (nt-fielddecl-2 ((nt-type) null)
                                                                        ((t-bang nt-atype) null))
                                                        (nt-vars ((nt-var nt-vars-2) null))
                                                        (nt-vars-2 (() null)
                                                                   ((t-comma nt-var nt-vars-2) null))
                                                        (nt-newconstr ((nt-con nt-atype) null)
                                                                      ((nt-con t-lcbracket nt-var t-coloncolon nt-type t-rcbracket) null))
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        ; todo: context, scontext, qtycls, cdecls, idecls
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        (nt-qcon ((nt-qconid) null)
                                                                 ((t-lrbracket nt-gconsym t-rrbracket) null))
                                                        (nt-qconid ((nt-modid t-period t-conid) null)
                                                                   ((t-conid) null))
                                                        (nt-gconsym ((t-colon) null)
                                                                    ((nt-qconsym) null))
                                                        (nt-qconsym ((nt-qconsym-2 t-consym) null))
                                                        (nt-qconsym-2 (() null)
                                                                      ((nt-modid t-period) null))
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        ;(hvarop (:or hvarsym (:: "`" hvarid "`")))
                                                        ;(hqvarop (:or hqvarsym (:: "`" hqvarid "`")))
                                                        
                                                        ;(hqconop (:or hgconsym (:: "`" hqconid "`")))
                                                        ;(hop (:or hvarop hconop))
                                                        ;(hqop (:or hqvarop hqconop))
                                                        
                                                        
                                                        
                                                        )))
  
  
    
  
  
  (define-lex-abbrevs
    (hliteral (:or hinteger hfloat hchar hstring))
    (hspecial (:or "(" ")" "," ";" "[" "]" "`" "{" "}"))
    
    (hwhitespace (:: hwhitestuff (:* hwhitestuff)))
    (hwhitestuff (:or hwhitechar hcomment hncomment))
    (hwhitechar (:or hnewline hvertab hspace htab))
    (hnewline (:or (:: hreturn hlinefeed) hreturn hlinefeed hformfeed))
    (hreturn #\return)
    (hlinefeed #\newline)
    (hvertab #\vtab)
    (hformfeed #\page)
    (hspace #\space)
    (htab #\tab)
    
    (hcomment (:: hdashes (:? (:- hany hsymbol) (:* hany)) hnewline))
    (hdashes (:: "--" (:* "-")))
    (hopencom "{-")
    (hclosecom "-}")
    (hncomment nothing);(:: hopencom hANYseq (:* (:: hncomment hANYseq)) hclosecom))
    (hANYseq (:- hANY (:: (:* hANY) (:* (:: hopencom hclosecom)) (:* hANY))))
    (hANY (:or hgraphic hwhitechar))
    (hany (:or hgraphic hspace htab))
    (hgraphic (:or hsmall hlarge hsymbol hdigit hspecial ":" #\" "'"))

    (hsmall (:or (:/ #\a #\z) "_"))
    
    (hlarge (:or (:/ #\A #\Z)))
    (hsymbol (:or "~" "!" "@" "#" "$" "%" "^" "&" "*" "-" "+" "=" #\\ "|" "." "/" "<" ">" "?"))
    
    (hdigit (:/ #\0 #\9))
    (hoctit (:/ #\0 #\7))
    (hhexit (:or hdigit (:/ #\A #\F) (:/ #\a #\f)))
    
    (hvarid (:- (:: hsmall (:* (:or hsmall hlarge hdigit "'"))) hreservedid))

    (hreservedid (:or "case" "class" "data" "default" "deriving" "do" "else" "if" "import" "in" "infix" "infixl" "infixr" "instance" "let" "module" "newtype" "of" "then" "type" "where" "_"))
    

    (hreservedop (:or ":" "::" "=" #\\ "|" "->"))
    
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
    
    
    
    (hdecls (:: "{" (:? hdecl (:* (:: "," hdecl))) "}"))
    (hdecl (:or hgendecl (:: (:or hfunlhs hpat/0) hrhs)))
    
    (hcdecls (:: "{" (:? hcdecl (:* (:: "," hcdecl))) "}"))
    (hcdecl (:or hgendecl (:: (:or hfunlhs hvar) hrhs)))
    
    (hgendecl (:or (:: hvars "::" (:? (:: hcontext "=>")) htype) (:: hfixity (:? hinteger) hops) nothing))
    
    (hops (:: hop (:* (:: "," hop))))
    
    (hfixity (:or "infixl" "infixr" "infix"))
    
    
    
    
    
    
    
    (hcontext (:or hclass (:: "(" (:? (:: hclass (:* (:: "," hclass)))) ")")))
    (hclass (:or (:: hqtycls htyvar) (:: hqtycls "(" htyvar hatype (:* (:: "," hatype)) ")")))
    (hscontext (:or hsimpleclass (:: "(" (:? (:: hsimpleclass (:* (:: "," hsimpleclass)))) ")")))
    (hsimpleclass (:or hqtycls htyvar))
    
    
    
    
    
    
    (hderiving (:: "deriving" (:or hdclass (:: "(" hdclass (:* (:: "," hdclass)) ")"))))
    (hdclass hqtycls)
    
    (hinst (:or hgtycon (:: "(" hgtycon (:* htyvar) ")") (:: "(" htyvar (:+ (:: "," htyvar)) ")") (:: "[" htyvar "]") (:: "(" htyvar "->" htyvar)))
    
    (hfunlhs (:or (:: hvar hapat "{" hapat "}")
                  (:: hpat/i+1 hvarop/a/i hpat/i+1)
                  (:: hlpat/i hvarop/l/i hpat/i+1)
                  (:: hpat/i+1 hvarop/r/i hrpat/i)
                  (:: "(" hfunlhs ")" hapat "{" hapat "}")))
    
    (hrhs (:or (:: "=" hexp (:? (:: "where" hdecls))) (:: hgdrhs (:? (:: "where" hdecls)))))
    
    (hgdrhs (:: hgd "=" hexp (:? hgdrhs)))
    
    (hgd (:: "|" hexp/0))
    
    (hexp (:or (:: hexp/0 "::" (:? (:: hcontext "=>")) htype) hexp/0))
    (hexp/i (:or (:: hexp/i+1 (:? (:: hqop/n/i hexp/i+1))) hlexp/i hrexp/i))
    (hexp/0 hexp)
    (hexp/7 hexp)
    (hexp/10 (:or (:: (string #\\) (:: hapat (:* hapat)) "->" hexp)
                  (:: "let" hdecls "in" hexp)
                  (:: "if" hexp "then" hexp "else" hexp)
                  (:: "case" hexp "of" "{" halts "}")
                  (:: "do" "{" hstmts "}")
                  hfexp
                  (:: hexp hop hexp)
                  (:: "-" hexp)))
    (hlexp/i (:: (:or hlexp/i hexp/i+1) hqop/l/i hexp/i+1))
    (hlexp/6 (:: "-" hexp/7))
    (hrexp/i (:: hexp/i+1 hqop/r/i (:or hrexp/i hexp/i+1)))
    (hfexp (:: (:? hfexp) haexp))
    
    (haexp (:or hqvar
                hgcon
                hliteral
                (:: "(" hexp ")")
                (:: "(" hexp (:+ (:: "," hexp)) ")")
                (:: "[" hexp (:* (:: "," hexp)) "]")
                (:: "[" hexp (:? (:: "," hexp)) ".." (:? hexp))
                (:: "[" hexp "|" hqual (:* (:: "," hqual)) "]")
                (:: "(" hexp/i+1 hqop/a/i ")")
                (:: "(" hlexp/i hqop/l/i ")")
                (:: "(" (:- hqop/a/i "-") hexp/i+1 ")")
                (:: "(" (:- hqop/r/i "-") hrexp/i ")")
                (:: hqcon "{" (:? (:: hfbind (:* (:: "," hfbind)))) "}")
                (:: (:- haexp hqcon) "{" hfbind (:* (:: "," hfbind)) "}")))
    
    (hqual (:or (:: hpat "<-" hexp) (:: "let" hdecls) hexp))
    
    (halts (:: halt (:* (::";" halt))))
    (halt (:or (:: hpat "->" hexp (:? (:: "where" hdecls))) (:: hpat hgdpat (:? (:: "where" hdecls))) nothing))
    
    (hgdpat (:: hgd "->" hexp (:? hgdpat)))
    
    (hstmts (:: (:* hstmt) hexp (:? ";")))
    (hstmt (:or (:: hexp ";") (:: hpat "<-" hexp ";") (:: "let" hdecls ";") ";"))
    
    (hfbind (:: hqvar "=" hexp))
    
    (hpat (:or (:: hvar "+" hinteger) hpat/0))
    (hpat/i (:or (:: hpat/i+1 (:? (:: hqconop/n/i hpat/i+1))) hlpat/i hrpat/i))
    (hpat/0 hpat)
    (hpat/10 (:or hapat (:: hgcon (:+ hapat))))
    (hlpat/i (:: (:or hlpat/i hpat/i+1) hqconop/l/i hpat/i+1))
    (hlpat/6 (:: "-" (:or hinteger hfloat)))
    (hrpat/i (:: hpat/i+1 hqconop/r/i (:or hrpat/i hpat/i+1)))
    
    (hapat (:or (:: hvar (:? (:: "@" hapat)))
                gcon
                (:: hqcon "{" (:? hfpat (:* (:: "," hfpat ))) "}" )
                hliteral
                "_"
                (:: "(" hpat ")")
                (:: "(" hpat (:+ (:: "," hpat)) ")")
                (:: "[" hpat (:* (:: "," hpat)) "]")
                (:: "~" hapat)))
    
    (hfpat (:: hqvar "=" hpat))
    
    (hgcon (:or (:: "(" ")") (:: "[" "]") (:: "(" (:+ ",") ")") hqcon))
    
    (hvarop (:or hvarsym (:: "`" hvarid "`")))
    (hqvarop (:or hqvarsym (:: "`" hqvarid "`")))
    (hconop (:or hconsym (:: "`" hconid "`")))
    (hqconop (:or hgconsym (:: "`" hqconid "`")))
    (hop (:or hvarop hconop))
    (hqop (:or hqvarop hqconop))
    )
  
  
  
  
  
  (define (parse) ((haskell-parser "prompt") (lambda () (haskell-lexer (current-input-port)))))
  
  (define (prompt) (eval (emit-scheme (parse))))
  
  (define (wrap value) (vector (lambda () value) 'empty))
  
  (define (read-haskell-syntax) 'incomplete)
)