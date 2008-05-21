#reader (lib "reader.ss" "smootxes")

#module ("ml")

#include ()

let mlBoolean1 = false;;

let mlBoolean2 = true;;

let mlFunction1 = fun x -> x;;

let mlFunction2 = fun (x, y) -> (y, x);;

let rec mlFunction3 n = if n = 0 then 0 else n + mlFunction3 (n - 1);;

let mlInteger = 1;;

let mlString = "ab";;

let mlTuple = (false, "ab");;

let mlUnit = ();;