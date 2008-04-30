#reader (lib "haskell.ss" "haskell")

module Prelude where {

error = :scheme [Char] -> a "error"

trace = :scheme a -> b -> b "trace"

(+) = :scheme Int -> Int -> Int "int-add"

(-) = :scheme Int -> Int -> Int "int-subtract"

(*) = :scheme Int -> Int -> Int "int-multiply"

(/) = :scheme Int -> Int -> Int "int-divide"

(==) = :scheme Int -> Int -> Bool "int-equal"

(/=) = :scheme Int -> Int -> Bool "int-not-equal"

(:) = :scheme a -> [a] -> [a] "list-cons"

head = :scheme [a] -> a "list-head"

tail = :scheme [a] -> [a] "list-tail"

null = :scheme [a] -> Bool "list-null"

fst = :scheme (a, b) -> a "tuple-first"

snd = :scheme (a, b) -> b "tuple-second"

True = :scheme Bool "boolean-true"

False = :scheme Bool "boolean-false"

(&&) = :scheme Bool -> Bool -> Bool "boolean-and"

(||) = :scheme Bool -> Bool -> Bool "boolean-or"

not = :scheme Bool -> Bool "boolean-not"

}