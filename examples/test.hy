. = fn g f x: g (f x)
id = fn x: x

not = fn b: b false true
and = fn a b: a b false
or = fn a b: a true b

foo = and (not true) (or (not false) (id true))
bar = foo 9001 42
baz = + 1300 37
qux = fn x y: . (+x) (+y) y

main = qux bar baz 
