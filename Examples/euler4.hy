import Builtin
import Std.List
import Std.Misc

digits = { go = fn x : cons x (go (* x 10)) in go 1 }
hasDigit = fn x d : != 0 (/ x d)
digitsFor = fn x : takeWhile (hasDigit x) digits
getDigit = fn x d : (/ (% x (* d 10)) d)
isPalindrome = fn x : {
    digits2 = digitsFor x
    match = fn d1 d2 : == (getDigit x d1) (getDigit x d2)
    in all id (zip match digits2 (reverse digits2))
}

base = rangeFromTo 100 1000 1
numbers = ap (map * base) base
getMax = fn a b : > a b a b

main = fold getMax 0 (filter isPalindrome numbers)
