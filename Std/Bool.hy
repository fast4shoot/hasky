` boolean stuff
true = fn a b : a
false = fn a b : b

not = fn b : b false true
and = fn a b : a b false
or = fn a b : a true b
