#include <stdlib/all.hy>

square = fn x : * x x
nums = listRangeFromTo 1 100 1
sum = listFold + 0
main = - (square (sum nums)) (sum (listMap square nums))
