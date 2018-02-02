#include <stdlib/all.hy>

`tohle je shit, protože se to dá udělat v O(1) čase, ale takhle
`je to zajímavější

allnums = listRangeFromTo 1 999 1
isMultipleOf = fn n x : == 0 (% x n)
isMultipleOf3Or5 = fn x : or (isMultipleOf 3 x) (isMultipleOf 5 x)
multiples = listFilter isMultipleOf3Or5 allnums
sum = listFold + 0 multiples

main = sum
