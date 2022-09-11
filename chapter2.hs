double x    = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns  = sum ns `div` length ns

-- fixed syntax

myF = a `div` length xs
      where
        a  = 10
        xs = [1, 2, 3, 4, 5]

-- 3: Write a custom "last" function

customLast ns = ns !! (length ns - 1)

customInit1 ns = reverse (drop 1 (reverse ns))

-- WOW: the linter is able to recognize that this can be done better using init!
customInit2 ns = reverse (tail (reverse ns))
