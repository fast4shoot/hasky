import Std.List
import Builtin

list = cons 5 (cons 4 (cons 3 (cons 2 (cons 1 []))))
sum = fold + 0
main = sum (map (* 3) list)
