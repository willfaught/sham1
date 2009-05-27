#reader (lib "Reader.ss" "sham" "haskell")

module Function (id, const, (.), flip, ($), fix, on) where {

id x = x;

const x y = x;

(.) x y z = x (y z);

flip f x y = f y x;

($) x y = x y;

fix f = f (fix f);

on f g x y = f (g x) (g y)

}