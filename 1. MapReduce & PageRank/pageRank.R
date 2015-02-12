# M - column-stochastic transition matrix modeling probabilities (or flow) at each node
# beta - probability of continuing random walk (as opposed to teleporting)
# N - normalization factor; the elements of the rank vector will sum to N
# eps - goal for power iteration algorithm
page.ranks <- function(M, beta, N = 1, eps = 0.00001, maxIter = Inf) {
  n <- nrow(M)
  A <- beta*M + (1 - beta)*(1/n)*outer(rep(1, n), rep(1, n))
  r_0 = c(N/n, N/n, N/n) # normalize page rank vector to sum to N
  iter = 0
  repeat {
    r <- A %*% r_0
    iter = iter + 1 
    if(norm(r - r_0, type = 'F') < eps | iter >= maxIter)
      return(as.vector(r))
    r_0 <- r
  }
}

# Problem 1
M <- matrix(c(0, 1/2, 1/2, 0, 0, 1, 0, 0, 1), ncol = 3) 
page.ranks(M, beta = 0.7, N = 3)

# Problem 2
M <- matrix(c(0, 1/2, 1/2, 0, 0, 1, 1, 0, 0), ncol = 3)  
A <- 0.85*M + (1 - 0.85)*(1/3)*outer(rep(1, 3), rep(1, 3))
A[1,]  # these are the coefficients of the equation for calculating the rank of node a

# Problem 3
M <- matrix(c(0, 1/2, 1/2, 0, 0, 1, 1, 0, 0), ncol = 3)
page.ranks(M, beta = 1.0, N = 3, maxIter = 5)
page.ranks(M, beta = 1.0, N = 3)

