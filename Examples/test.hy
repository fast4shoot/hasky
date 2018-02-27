import Builtin
import Std.Bool
import Std.Misc

foo = and (not true) (or (not false) (id true))
bar = foo 9001 42
baz = + 1300 37
qux = fn x y: . (+x) (+y) y

main = qux bar baz 
