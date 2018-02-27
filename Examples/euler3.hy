import Builtin
import Std.Bool
import Std.List

isDivisible = fn x y: == 0 (% x y)
isNotDivisible = fn x y : not (isDivisible x y)
lessThanSqrt = fn x y: <= (* y y) x
candidateFactorsFor = fn x : takeWhile (lessThanSqrt x) primes
primesFlt = fn x : all (isNotDivisible x) (candidateFactorsFor x)
primes = cons 2 (cons 3 (filter primesFlt (rangeFromTo 5 10000 2)))
factorsFor = fn x : filter (isDivisible x) (candidateFactorsFor x)

number = 600851475143
main = factorsFor number
