import Builtin
import Std.List

`idgaf, brute force baby
numbers = cons 19 (cons 9 (cons 17 (cons 16 (cons 7 (cons 13 (cons 11 []))))))
candidates = rangeFrom 20 20 

isDivisible = fn x y : == 0 (% x y)

main = head (filter (fn x : all (isDivisible x) numbers) candidates)

