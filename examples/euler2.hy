#include <stdlib/all.hy>

fibs = cons 1 (cons 1 (listZip + fibs (listTail fibs)))
isEven = fn x : == 0 (% x 2)
main = listFold + 0 (listFilter isEven (listTakeWhile (> 4000000) fibs))
