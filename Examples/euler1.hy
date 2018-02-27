import Builtin
import Std.Bool
import Std.List

`tohle je shit, protože se to dá udělat v O(1) čase, ale takhle
`je to zajímavější

allnums = rangeFromTo 1 999 1
isMultipleOf = fn n x : == 0 (% x n)
isMultipleOf3Or5 = fn x : or (isMultipleOf 3 x) (isMultipleOf 5 x)
multiples = filter isMultipleOf3Or5 allnums
sum = fold + 0 multiples

main = sum
