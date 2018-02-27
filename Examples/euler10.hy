import Builtin
import Std.Bool
import Std.List

isDivisible = fn x y: == 0 (% x y)
isNotDivisible = fn x y : not (isDivisible x y)
lessThanSqrt = fn x y: <= (* y y) x
candidateFactorsFor = fn x : takeWhile (lessThanSqrt x) primes
primes = cons 2 (cons 3 (filter primesFlt (rangeFrom 5 2)))
primesFlt = fn x : all (isNotDivisible x) (candidateFactorsFor x)

main = fold + 0 (takeWhile (> 2000000) primes)
