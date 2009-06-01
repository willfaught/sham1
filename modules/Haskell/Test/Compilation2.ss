#reader (lib "Reader.ss" "sham" "haskell")

module Compilation2 (res1) where {

import haskell Haskell.Data.List (map :: (a -> b) -> [a] -> [b]);

res1 = Haskell.Data.List.map (\x -> (Haskell.Prelude.+) x 1) [1, 2, 3];

res2 = (Haskell.Prelude./) 6 3

}