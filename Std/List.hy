import Builtin
import Std.Bool
import Std.Misc

` list
cons = fn x list f _ : f x list
[] = fn _ a: a

head = fn list: list (fn x _ : x) undefined
tail = fn list: list (fn _ l : l) undefined
map = fn f l: l (fn h t: cons (f h) (map f t)) [] ` tohle asi jde napsat přes fold jednodušeji
filter = fn p l: l (fn h t: p h (cons h) id (filter p t)) []
fold = fn f i l: l (fn h t: f h (fold f i t)) i
append = fn la lb: la (fn h t: cons h (append t lb)) lb
concat = fold append [] ` zhruba, asi
ap = fn la lb: concat (map (fn f: map f lb) la)
rangeFrom = fn from step : cons from (rangeFrom (+ from step) step)
rangeFromTo = fn from to step : (> step 0 > <) from to [] (cons from (rangeFromTo (+ from step) to step))
zip = fn f l1 l2 : l1 (fn h1 t1 : l2 (fn h2 t2: cons (f h1 h2) (zip f t1 t2) ) []) []
takeWhile = fn p l : l (fn h t : p h (cons h (takeWhile p t)) []) []
all = fn p l : l (fn h t : p h (all p t) false) true
reverse = fn ll : { go = fn l acc : l (fn h t : go t (cons h acc)) acc in go ll [] }
take = fn n l : == n 0 [] (l (fn h t : cons h (take (- n 1) t)) [])
drop = fn n l : == n 0 l (l (fn _ t : drop (- n 1) t) [])
repeat = fn e : cons e (repeat e)
tails = fn l : { go = fn xs : cons xs (xs (fn _ t : go t) []) in go l }
window = fn n l : fold (zip cons) (repeat []) (take n (tails l))
max = fn cmp l : fold (fn a b : cmp a b a b) (head l) l
