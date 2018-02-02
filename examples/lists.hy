#include <stdlib/all.hy>

list = cons 5 (cons 4 (cons 3 (cons 2 (cons 1 []))))
listSum = listFold + 0
main = listSum (listMap (* 3) list)
