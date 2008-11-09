#reader (lib "Reader.ss" "sham" "haskell")

module HM where {

import "ml.ml";

hmBoolean1 = :ml Bool "mlBoolean1";

hmBoolean2 = :ml Bool "mlBoolean2";

hmFunction1 = :ml a -> a "mlFunction1";

hmFunction2 = :ml (a, b) -> (b, a) "mlFunction2";

hmFunction3 = :ml Int -> Int "mlFunction3";

hmInteger = :ml Int "mlInteger";

hmString = :ml String "mlString";

hmTuple = :ml (Bool, [Char]) "mlTuple";

hmUnit = :ml () "mlUnit"

}