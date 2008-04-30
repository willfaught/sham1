#reader (lib "haskell.ss" "haskell")

module Prelude where {

error = :scheme [Char] -> a "prelude:error"

trace = :scheme a -> b -> b "prelude:trace"

(+) = :scheme Int -> Int -> Int "prelude:int-add"

(-) = :scheme Int -> Int -> Int "prelude:int-subtract"

(*) = :scheme Int -> Int -> Int "prelude:int-multiply"

(/) = :scheme Int -> Int -> Int "prelude:int-divide"

(==) = :scheme Int -> Int -> Bool "prelude:int-equal"

(/=) = :scheme Int -> Int -> Bool "prelude:int-not-equal"

(:) = :scheme a -> [a] -> [a] "prelude:list-cons"

head = :scheme [a] -> a "prelude:list-head"

tail = :scheme [a] -> [a] "prelude:list-tail"

null = :scheme [a] -> Bool "prelude:list-null"

fst = :scheme (a, b) -> a "prelude:tuple-first"

snd = :scheme (a, b) -> b "prelude:tuple-second"

True = :scheme Bool "prelude:boolean-true"

False = :scheme Bool "prelude:boolean-false"

(&&) = :scheme Bool -> Bool -> Bool "prelude:boolean-and"

(||) = :scheme Bool -> Bool -> Bool "prelude:boolean-or"

not = :scheme Bool -> Bool "prelude:boolean-not"

}