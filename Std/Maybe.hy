import Std.Misc (.)

` maybe
just = fn x a _ : a x
nothing = fn _ b : b

map = fn f m : m (. just f) nothing
filter = fn p m : (fn x: p x m nothing) nothing
ap = fn ma mb : ma (fn f: map f mb) nothing
