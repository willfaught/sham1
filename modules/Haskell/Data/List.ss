#reader (lib "Reader.ss" "sham" "haskell")

module List () where {

x ++ y = if null x then y else (:) (head x) ((++) (tail x) y);

--tail

--init

--length x = if null x then 0 else 

map f x = if null x then [] else (:) (f (head x)) (tail x);

reverse x = 

}