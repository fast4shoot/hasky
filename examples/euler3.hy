#include <stdlib/all.hy>

isDivisible = fn x y: == 0 (% x y)
isNotDivisible = fn x y : not (isDivisible x y)
lessThanSqrt = fn x y: <= (* y y) x
candidateFactorsFor = fn x : listTakeWhile (lessThanSqrt x) primes
primesFlt = fn x : listAll (isNotDivisible x) (candidateFactorsFor x)
primes = cons 2 (cons 3 (listFilter primesFlt (listRange 5 10000 2)))
factorsFor = fn x : listFilter (isDivisible x) (candidateFactorsFor x)

number = 600851475143
main = factorsFor number
