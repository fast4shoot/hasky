import Builtin
import Std.List
import Std.Tuple

isCorrect = fn a b : { c = (- (- 1000 a) b) in == (* c c) (+ (* a a) (* b b)) }

base = rangeFromTo 1 998 1
triplets = ap (map (tuple 2) base) base
main = filter (fn t : t isCorrect) triplets
