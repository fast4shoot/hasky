import Builtin
import Std.Tuple

` some basic line continuations
num1 = \
    + 1 2

num2 \
= \
+ \
2 \
3

num3 = + \
3 \
4 

` something more complex
num4 = + (+ \
    (+ 2 3) \
    (+ 2 4) \
) \
42

` line continuations with empty lines inbetween
num5 = + \
1 \
2

` continuations with comments
num6 = \
` this is a comment
+ 4 2

main = tuple 6 num1 num2 num3 num4 num5 num6
