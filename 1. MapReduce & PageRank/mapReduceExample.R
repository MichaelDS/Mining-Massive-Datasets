# map function takes an integer i and produces the list of pairs (p,i) such that p is a prime divisor of i
# v - a vector of integers to which to apply the map function
p4.map <- function(v) {
  library(gmp)
  library(plyr)
  primes <- as.vector(sapply(v, function(x) as.numeric(unique(factorize(x))))) # compute a list of vectors of prime divisors; the vectors correspond to the elements of v
  keyVals <- mapply(function(x, y) t(sapply(y, function(z) c(z, x))), v, primes) # compute a list of n x 2 matrices, corresponding to the elements of v, such that each row represents a pair (p,i) such that p is a prime divisor of i and i is the corresponding element of v   
  rbind.fill(lapply(keyVals, data.frame)) # coerce the list of key-value matrices into a list of key-value data frames and rbind them together 
}

# Group by keys and apply the reduce function to produce the final key-value pairs
# x - n x 2 data frame produced by the map function where each row represents a (key, value) tuple
p4.reduce <- function(x) {
  tapply(x[, 2], x[, 1], function(x) Reduce('+', x)) # grouping step accomplished implicitly by tapply
}

# Problem 4
p4.reduce(p4.map(c(15, 21, 24, 30, 49)))