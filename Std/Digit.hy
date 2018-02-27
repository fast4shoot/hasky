import Builtin
import Std.List

` working with digits of a number - can be useful for some things
digits = { go = fn x base : cons x (go (* x base) base) in go 1 }
hasDigitsAtOrBelow = fn d x : != 0 (/ x d)
getDigit = fn base d x : / (% x (* d base)) d
getDigits = fn base x : map (fn d : getDigit base d x) (takeWhile (fn d : hasDigitsAtOrBelow d x) (digits base))
