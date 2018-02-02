#include <stdlib/all.hy>

isCorrect = fn a b : { c = (- (- 1000 a) b) in == (* c c) (+ (* a a) (* b b)) }

base = listRangeFromTo 1 998 1
triplets = listAp (listMap (tuple 2) base) base
main = listFilter (fn t : t isCorrect) triplets
