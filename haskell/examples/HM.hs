#reader (lib "Reader.ss" "sham" "haskell")

module MLTest where {

import "ml.ml";

ml1 = :ml Bool "mlBoolean1";
ml2 = :ml Bool "mlBoolean2";
ml3 = :ml a -> a "mlFunction1";
ml4 = :ml (a, b) -> (b, a) "mlFunction2";
ml5 = :ml Int -> Int "mlFunction3";
ml6 = :ml Int "mlInteger";
ml7 = :ml [Char] "mlString";
ml8 = :ml (Bool, [Char]) "mlTuple";
ml9 = :ml () "mlUnit"

}