#reader (lib "reader.ss" "haskell")

module MLTest where {

import "test.ocaml";

ml1 = :ml Bool "mlBoolean1";
ml2 = :ml Bool "mlBoolean2";
ml3 = :ml a -> a "mlFunction1";
ml4 = :ml (a, (b, (c, (d, e)))) -> (e, (d, (c, (b, a)))) "mlFunction2";
ml5 = :ml Int -> Int "mlFunction3";
ml6 = :ml Int "mlInt";
ml7 = :ml [Char] "mlString";
ml8 = :ml (Bool, (Bool, (a -> a, (Int, [Char])))) "mlTuple";
ml9 = :ml () "mlUnit"

}