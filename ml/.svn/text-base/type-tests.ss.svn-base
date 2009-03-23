(module type-tests mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (provide type-test-suite)
  
  (define prelude
#<<---
#reader(lib "reader.ss" "smootxes")

#module ("bogus-name")
#include ()
---
    )
  
  (define first-def "let first = (fin :G (('a * 'b) -> 'a));;\n")
  
  (define s-a string-append)
  
  (define type-test-suite
    (test-suite
     "typechecking"
     (test-exn "a" (ep #px"Unbound variable") (te "let a = zbx;;"))
     (test-not-exn "a" (te "let a = (zbx :G int);;"))
     (test-not-exn "first" (te (s-a first-def "let y = first (3,4);;")))
     (test-exn "lump" (ep #px"Incompatible types: lump and int.")
               (te (s-a first-def "let second = (sin :G (('a * 'b) -> 'b));;
let vector-import-test-2 = 
  let v = (vector1 :G (int * (lump -> lump))) in
    let b = first v in
      let t = second v in
        t b;;")))
     ;; unification failure tuple vs. int:
     (test-exn "a"
               (ep #px"Incompatible types: \\(_tyvar_[0-9]* \\* _tyvar_[0-9]*\\) and int")
               (te (s-a first-def 
                        "let blah2 = first 3;;")))
     ;; let-polymorphism
     (test-not-exn "let-poly"
                   (te "let ans = let f x y = y in ((f 3 f) true 3) + 4;;"))
     (test-exn "let-poly-err"
               (ep "Incompatible types: bool and int.")
               (te "let ans = let f x y = y in ((f 3 f) 3 true) + 4;;"))
     ;; polymorphic recursion
     (test-exn "polymorphic-let"
               (ep "Incompatible types: int and bool.")
               (te "let f g = (if (g 3) then ((g 4) + 5) else 9);;"))
     ;; huh?
     (test-exn "should not typecheck: looks like a lexer problem."
               (ep "Incompatible types: int and int->int.")
               (te "let f = ((g :G (int -> int)) +5);;"))
     ;; let's try strings
     (test-not-exn "str1"
                   (te "let f = \"abc\";;"))
     ;; units
     (test-exn "unit + int"
               (ep "Incompatible types: int and \\(\\)")
               (te "let unit-err1 = 3 + ();;"))
     (test-exn "apply unit"
               (ep "Incompatible types: \\(\\) and \\(\\)->_tyvar_[0-9]*")
               (te "let unit-err2 = () ();;"))
     (test-exn "if unit"
               (ep "Incompatible types: \\(\\) and bool")
               (te "let unit-err3 = if () then () else ();;"))
     ;; polymorphic equals
     (test-exn "polymorphic equals"
               (ep "Incompatible types: string and int.")
               (te "let t1 = (3 = \"abc\");;"))
     ;; usertype name collisions
     (test-exn "type collisions"
               (ep "duplicate"))
     ))
  
  (define ((ep regex) exn)
    (regexp-match regex (exn-message exn)))
  
  (define ((te str))
    (parameterize ([read-accept-reader #t])
      (read-syntax "smootxy-test" (open-input-string (string-append prelude str)))))

)