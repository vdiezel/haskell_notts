-- lazy evaluation
-- innermost vs outermost evaluation
-- innermost will not always terminate: fst (0, infinity) will yield (fst (0, 1 + inifnity)
-- outermost fst (0, infinity) yields 0
-- the catch is the number of reduction steps (less efficient), which can be improved using pointers

-- lazy evaluation: outermost evaluation + sharing of arguments
--
-- generating primes using the Sieve of Aristosthenes

primes = sieve [2..]
sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]

-- love it!
