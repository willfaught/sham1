#reader (lib "reader.ss" "haskell")

module SchemeTest where {

import "scheme.ss";

sc1 = :scheme Bool "schemeBoolean1";
sc2 = :scheme Bool "schemeBoolean2";
sc3 = :scheme Char "schemeCharacter";
sc4 = :scheme Float "schemeFloat";
sc5 = :scheme a -> a "schemeFunction1";
sc6 = :scheme Int "schemeInteger";
sc7 = :scheme [a] "schemeList1";
sc8 = :scheme [Int] "schemeList2";
sc9 = :scheme (Int, Int) "schemeTuple"

}