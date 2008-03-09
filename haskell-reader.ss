(module haskell-reader mzscheme
  (require (lib "lex.ss" "parser-tools")
           (prefix : (lib "lex-sre.ss" "parser-tools"))
           (lib "yacc.ss" "parser-tools")
           (lib "readerr.ss" "syntax")
           "scheme-emitter.ss")

  (provide (rename read-haskell-syntax read-syntax))
  
  (define-empty-tokens keywords (eof t-as t-colon t-comma t-dotdot t-hiding t-import t-lcbracket t-lrbracket t-module t-period t-qualified t-rcbracket t-rrbracket t-semicolon t-where))
  
  (define-tokens regular (t-conid t-consym t-varid t-varsym))
  
  (define haskell-lexer (lexer-src-pos ("as" (token-t-as))
                                       (":" (token-t-colon))
                                       ("," (token-t-comma))
                                       (".." (token-t-dotdot))
                                       ("hiding" (token-t-hiding))
                                       ("import" (token-t-import))
                                       ("{" (token-t-lcbracket))
                                       ("(" (token-t-lrbracket))
                                       ("module" (token-t-module))
                                       ("." (token-t-period))
                                       ("qualified" (token-t-qualified))
                                       ("}" (token-t-rcbracket))
                                       (")" (token-t-rrbracket))
                                       (";" (token-t-semicolon))
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
                                               (grammar (nt-module ((t-module nt-modid nt-exports t-where nt-body) null)
                                                                   ((t-module nt-modid t-where nt-body) null)
                                                                   ((nt-body) null))
                                                        (nt-exports ((t-lrbracket nt-export nt-exports-2 t-rrbracket) null)
                                                                    ((t-lrbracket nt-export nt-exports-2 t-comma t-rrbracket) null))
                                                        (nt-exports-2 (() null)
                                                                      ((t-comma nt-export nt-exports-2) null))
                                                        (nt-export ((nt-qvar) null)
                                                                   ((nt-qtycon) null)
                                                                   ((nt-qtycon t-lrbracket t-dotdot t-rrbracket) null)
                                                                   ((nt-qtycon t-lrbracket nt-cname nt-export-2 t-rrbracket) null)
                                                                   ((nt-qtycls) null)
                                                                   ((nt-qtycls t-lrbracket t-dotdot t-rrbracket) null)
                                                                   ((nt-qtycls t-lrbracket nt-qvar nt-export-3 t-rrbracket) null)
                                                                   ((t-module nt-modid) null))
                                                        (nt-export-2 (() null)
                                                                     ((t-comma nt-cname nt-export-2) null))
                                                        (nt-export-3 (() null)
                                                                     ((t-comma nt-qvar nt-export-3) null))
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
                                                        (nt-impspec-2 ((t-lrbracket nt-import nt-impspec-3 nt-impspec-4 t-rrbracket) null))
                                                        (nt-impspec-3 (() null)
                                                                      ((t-comma nt-import nt-impspec-3) null))
                                                        (nt-impspec-4 (() null)
                                                                      ((t-comma) null))
                                                        (nt-import ((nt-var) null)
                                                                   ((nt-tycon nt-import-2) null)
                                                                   ((nt-tycls nt-import-3) null))
                                                        (nt-import-2 (() null)
                                                                     ((t-lrbracket t-dotdot t-rrbracket) null)
                                                                     ((t-lrbracket nt-cname nt-import-2-2 t-rrbracket) null))
                                                        (nt-import-2-2 (() null)
                                                                       ((t-comma nt-cname) null))
                                                        (nt-import-3 (() null)
                                                                     ((t-lrbracket t-dotdot t-rrbracket) null)
                                                                     ((t-lrbracket nt-var nt-import-3-2 t-rrbracket) null))
                                                        (nt-import-3-2 (() null)
                                                                       ((t-comma nt-var) null))
                                                        
                                                        (nt-topdecls (() null))
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        (nt-tyvar ((t-varid) null))
                                                        (nt-tycon ((t-conid) null))
                                                        (nt-tycls ((t-conid) null))
                                                        (nt-modid ((t-conid) null))
                                                        
                                                        (nt-cname ((nt-var) null)
                                                                  ((nt-con) null))
                                                        (nt-var ((t-varid) null)
                                                                ((t-lrbracket t-varsym t-rrbracket) null))
                                                        (nt-con ((t-conid) null)
                                                                ((t-lrbracket t-consym t-rrbracket) null))
                                                        
                                                        (nt-qtycon ((nt-modid t-period nt-tycon) null)
                                                                   ((nt-tycon) null))
                                                        (nt-qtycls ((nt-modid t-period nt-tycls) null)
                                                                   ((nt-tycls) null))
                                                        
                                                        (nt-qvar ((nt-qvarid) null)
                                                                 ((t-lrbracket nt-qvarsym t-rrbracket) null))
                                                        (nt-qvarid ((nt-modid t-period t-varid) null)
                                                                   ((t-varid) null))
                                                        (nt-qvarsym ((nt-modid t-period t-varsym) null)
                                                                    ((t-varsym) null))
                                                        
                                                        (nt-qcon ((nt-qconid) null)
                                                                 ((t-lrbracket nt-gconsym t-rrbracket) null))
                                                        (nt-qconid ((nt-modid t-period t-conid) null)
                                                                   ((t-conid) null))
                                                        (nt-gconsym ((t-colon) null)
                                                                    ((nt-qconsym) null))
                                                        (nt-qconsym ((nt-modid t-period t-consym) null)
                                                                    ((t-consym) null))
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        ;(hvarop (:or hvarsym (:: "`" hvarid "`")))
                                                        ;(hqvarop (:or hqvarsym (:: "`" hqvarid "`")))
                                                        ;(hconop (:or hconsym (:: "`" hconid "`")))
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
    (hconid (:: hlarge (:* (:or hsmall hlarge hdigit "'"))));done
    (hreservedid (:or "case" "class" "data" "default" "deriving" "do" "else" "if" "import" "in" "infix" "infixl" "infixr" "instance" "let" "module" "newtype" "of" "then" "type" "where" "_"))
    
    (hvarsym (:- (:: hsymbol (:* (:or hsymbol ":"))) (:or hreservedop hdashes))); done
    (hconsym (:- (:: ":" (:* (:or hsymbol ":"))) hreservedop)); done
    (hreservedop (:or ":" "::" "=" #\\ "|" "->"))
    
    (htyvar hvarid);done
    (htycon hconid);done
    (htycls hconid);done
    (hmodid hconid);done
    
    (hqvarid (:: (:: (:? hmodid) ".") hvarid)); done
    (hqconid (:: (:: (:? hmodid) ".") hconid)); done
    (hqtycon (:: (:: (:? hmodid) ".") htycon)); done
    (hqtycls (:: (:: (:? hmodid) ".") htycls)); done
    (hqvarsym (:: (:: (:? hmodid) ".") hvarsym)); done
    (hqconsym (:: (:: (:? hmodid) ".") hconsym)); done
    
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
    
    (hmodule (:or (:: "module" hmodid (:? hexports) "where" hbody) hbody));done
    (hbody (:or (:: "{" himpdecls ";" htopdecls "}") (:: "{" himpdecls "}") (:: "{" htopdecls "}")));done
    
    (himpdecls (:: himpdecl (:* (:: ";" himpdecl))));done
    
    (hexports (:: "(" hexport (:* (:: "," hexport)) (:? ",") ")"));done
    
    (hexport (:or hqvar (:: hqtycon (:? (:or (:: "(" ".." ")") (:: "(" hcname (:* (:: "," hcname)) ")")))) (:: hqtycls (:? (:or (:: "(" ".." ")") (:: "(" hqvar (:* (:: "," hqvar)))))) (:: "module" hmodid)));done
    
    (himpdecl (:or (:: "import" (:? "qualified") hmodid (:? "as" hmodid) (:? himpspec)) nothing));done
    
    (himpspec (:or (:: "(" himport (:* (:: "," himport)) (:? himport) ")") (:: "hiding" (:: "(" himport (:* (:: "," himport)) (:? himport) ")"))));done
    
    (himport (:or hvar (:: htycon (:? (:or (:: "(" ".." ")") (:: "(" hcname (:* (:: "," hcname)) ")")))) (:: htycls (:? (:or (:: "(" ".." ")") (:: "(" hvar (:* (:: "," hvar))))))));done
    (hcname (:or hvar hcon));done
    
    (htopdecls (:? (:: htopdecl (:* (:: ";" htopdecl)))))
    (htopdecl (:or (:: "type" hsimpletype "=" htype)
                   (:: "data" (:? (:: hcontext "=>")) hsimpletype "=" hconstrs (:? hderiving))
                   (:: "newtype" (:? (:: hcontext "=>")) hsimpletype "=" hnewconstr (:? hderiving))
                   (:: "class" (:? (:: hscontext "=>")) htycls htyvar (:? (:: "where" hcdecls)))
                   (:: "instance" (:? (:: hscontext "=>")) hqtycls hinst (:? (:: "where" hidecls)))
                   (:: "default" "(" (:? htype (:* (:: "," htype))) ")")
                   hdecl))
    
    (hdecls (:: "{" (:? hdecl (:* (:: "," hdecl))) "}"))
    (hdecl (:or hgendecl (:: (:or hfunlhs hpat/0) hrhs)))
    
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
    
    (hvar (:or hvarid (:: "(" hvarsym ")"))); done
    (hqvar (:or hqvarid (:: "(" hqvarsym ")"))); done
    (hcon (:or hconid (:: "(" hconsym ")"))); done
    (hqcon (:or hqconid (:: "(" hgconsym ")"))); done
    (hvarop (:or hvarsym (:: "`" hvarid "`"))); done
    (hqvarop (:or hqvarsym (:: "`" hqvarid "`"))); done
    (hconop (:or hconsym (:: "`" hconid "`"))); done
    (hqconop (:or hgconsym (:: "`" hqconid "`"))); done
    (hop (:or hvarop hconop)); done
    (hqop (:or hqvarop hqconop)); done
    (hgconsym (:or ":" hqconsym)); done
    )
  
  
  
  
  
  (define (parse) ((haskell-parser "prompt") (lambda () (haskell-lexer (current-input-port)))))
  
  (define (prompt) (eval (emit-scheme (parse))))
  
  (define (wrap value) (vector (lambda () value) 'empty))
  
  (define (read-haskell-syntax) 'incomplete)
)