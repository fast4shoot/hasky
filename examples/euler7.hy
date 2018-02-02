#include <stdlib/all.hy>

isDivisible = fn x y: == 0 (% x y)
isNotDivisible = fn x y : not (isDivisible x y)
lessThanSqrt = fn x y: <= (* y y) x
candidateFactorsFor = fn x : listTakeWhile (lessThanSqrt x) primes
primes = cons 2 (cons 3 (listFilter primesFlt (listRangeFrom 5 2)))
primesFlt = fn x : listAll (isNotDivisible x) (candidateFactorsFor x)

main = listHead (listDrop 10000 primes)
