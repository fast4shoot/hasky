import Builtin
import Std.List

fibs = cons 1 (cons 1 (zip + fibs (tail fibs)))
isEven = fn x : == 0 (% x 2)
main = fold + 0 (filter isEven (takeWhile (> 4000000) fibs))
