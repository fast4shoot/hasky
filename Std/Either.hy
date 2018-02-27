import Std.Misc (id)

` either
left = fn l f _ : f l
right = fn r _ f : f r

eitherMap = fn f e: e id f
