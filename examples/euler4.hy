#include <stdlib/all.hy>

digits = { go = fn x : cons x (go (* x 10)) in go 1 }
hasDigit = fn x d : != 0 (/ x d)
digitsFor = fn x : listTakeWhile (hasDigit x) digits
getDigit = fn x d : (/ (% x (* d 10)) d)
isPalindrome = fn x : {
    digits = digitsFor x
    match = fn d1 d2 : == (getDigit x d1) (getDigit x d2)
    in listAll id (listZip match digits (listReverse digits))
}

base = listRangeFromTo 100 1000 1
numbers = listAp (listMap * base) base
getMax = fn a b : > a b a b

main = listFold getMax 0 (listFilter isPalindrome numbers)
