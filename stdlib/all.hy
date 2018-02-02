#ifndef STDLIB_ALL
#define STDLIB_ALL

` se hodí

undefined = undefined

` funkce

. = fn g f x : g (f x)
id = fn x : x
fix = fn f : f (fix f) ` nevim, tohle by asi šlo

` booleany

true = fn a b : a
false = fn a b : b

not = fn b : b false true
and = fn a b : a b false
or = fn a b : a true b

` lel, if nepotřebuješ, ale pokud ho nutně chceš, tak:
if = id

` maybe

just = fn x a _ : a x
nothing = fn _ b : b

maybeMap = fn f m : m (. just f) nothing
maybeFilter = fn p m : (fn x: p x m nothing) nothing
maybeAp = fn ma mb : ma (fn f: maybeMap f mb) nothing

` either

left = fn l f _ : f l
right = fn r _ f : f r

eitherMap = fn f e: e id f

` listy

cons = fn x list f _ : f x list
[] = fn _ a: a

listHead = fn list: list (fn x _ : x) undefined
listTail = fn list: list (fn _ l : l) undefined
listMap = fn f l: l (fn h t: cons (f h) (listMap f t)) [] ` tohle asi jde napsat přes fold jednodušeji
listFilter = fn p l: l (fn h t: p h (cons h) id (listFilter p t)) []
listFold = fn f i l: l (fn h t: f h (listFold f i t)) i
listAppend = fn la lb: la (fn h t: cons h (listAppend t lb)) lb
listConcat = listFold listAppend [] ` zhruba, asi
listAp = fn la lb: listConcat (listMap (fn f: listMap f lb) la)
listRangeFrom = fn from step : cons from (listRangeFrom (+ from step) step)
listRangeFromTo = fn from to step : (> step 0 > <) from to [] (cons from (listRangeFromTo (+ from step) to step))
listZip = fn f l1 l2 : l1 (fn h1 t1 : l2 (fn h2 t2: cons (f h1 h2) (listZip f t1 t2) ) []) []
listTakeWhile = fn p l : l (fn h t : p h (cons h (listTakeWhile p t)) []) []
listAll = fn p l : l (fn h t : p h (listAll p t) false) true
listReverse = fn l : { go = fn l acc : l (fn h t : go t (cons h acc)) acc in go l [] }
listTake = fn n l : == n 0 [] (l (fn h t : cons h (listTake (- n 1) t)) [])
listDrop = fn n l : == n 0 l (l (fn _ t : listDrop (- n 1) t) [])
listRepeat = fn e : cons e (listRepeat e)
listTails = fn l : { go = fn xs : cons xs (xs (fn _ t : go t) []) in go l }
listWindow = fn n l : listFold (listZip cons) (listRepeat []) (listTake n (listTails l))
listMax = fn > l : listFold (fn a b : > a b a b) (listHead l) l

` tuply
` předpokládám rozumně fungující == a pred na číslech
tuple2 = fn e n: == n 0 e (fn x: tuple2 (fn f: e f x) (pred n))
tuple = tuple2 id

` čísla - někdy se hodí se na ně dívat jako na seznamy číslic
digits = { go = fn x base : cons x (go (* x base) base) in go 1 }
hasDigitsAtOrBelow = fn d x : != 0 (/ x d)
getDigit = fn base d x : / (% x (* d base)) d
getDigits = fn base x : listMap (fn d : getDigit base d x) (listTakeWhile (fn d : hasDigitsAtOrBelow d x) (digits base))

#endif
