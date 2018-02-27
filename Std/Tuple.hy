import Builtin (pred ==)
import Std.Misc (id)

` tuply
tuple2 = fn e n: == n 0 e (fn x: tuple2 (fn f: e f x) (pred n))
tuple = tuple2 id
