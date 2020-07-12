
# Chi-squared distance from Legendre and Legendre (2012) Numerical Ecology

D16 <- function(X){
  ss <- sqrt(sum(X)) # square root of sum
  rs <- rowSums(X) # row sums
  cs <- colSums(X) # column sums
  
  n.pt <- dim(X)[1]
  dist.mat <- matrix(0, nrow = n.pt, ncol = n.pt)
  for(i in 1:(n.pt - 1)){
    for(j in (i + 1):n.pt){
      dist.mat[i,j] <- 
        dist.mat[j,i] <- 
        ss * sqrt(sum((X[i,] / rs[i] - X[j,] / rs[j])^2 / cs))
    }
  }
  
  return(dist.mat)
}

X <- matrix(rgeom(n = 5 * 3, 
                  prob = 0.2), 
            ncol = 3)
D16(X)
