import Builtin
import Std.List

square = fn x : * x x
nums = rangeFromTo 1 100 1
sum = fold + 0
main = - (square (sum nums)) (sum (map square nums))
