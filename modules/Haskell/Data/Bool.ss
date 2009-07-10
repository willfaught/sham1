#reader (lib "Reader.ss" "sham" "haskell")

module Bool where {

x && y = if Haskell.Prelude.isTrue x then y else x;

x || y = if Haskell.Prelude.isTrue x then x else y;

not x = if Haskell.Prelude.isTrue x then Haskell.Prelude.False else Haskell.Prelude.True;

otherwise = Haskell.Prelude.True;

boolEqual x = if Haskell.Prelude.isTrue x then Haskell.Prelude.isTrue else Haskell.Prelude.isFalse

}