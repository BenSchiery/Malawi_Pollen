# a function for 1-D linear interpolation (and limited extrapolation)

linterp <- function(x, y, x.out){
  bad <- is.na(x) | is.na(y)
  X <- x[!bad]
  Y <- y[!bad]
  Y <- Y[order(X)]
  X <- X[order(X)]
  a <- coef(lm(Y ~ X))[2] # slope
  
  data.length <- length(X)
  X.min <- X[1]
  X.max <- X[data.length]
  Y.min <- Y[1]
  Y.max <- Y[data.length]
  
  n.out <- length(x.out)
  y.out <- numeric(n.out)
  for(i in 1:n.out){
    x0 <- x.out[i]
    if(x0 < X.min){
      y.out[i] <- a * (x0 - X.min) + Y.min
    }else if(x0 > X.max){
      y.out[i] <- a * (x0 - X.max) + Y.max
    }else if(any(X == x0)){
      y.out[i] <- mean(Y[which(X == x0)])
    }else{
      j <- max(which(X < x0))
      y.out[i] <- (Y[j + 1] - Y[j]) / (X[j + 1] - X[j]) * (x0 - X[j]) + Y[j]
    }
  }
  
  return(y.out)
}

# test case 1: x values out of order, some missing x and y values
shuf <- sample(100)
na.x <- sample(100, size = 5)
na.y <- sample(100, size = 5)
x <- sort(c(runif(100, max = 10)))[shuf]
y <- cumsum(rnorm(100))[shuf]
x[na.x] <- NA
y[na.y] <- NA

x.linterp <- seq(-1, 11, length.out = 1000)
y.linterp <- linterp(x = x, y = y, x.out = x.linterp)

plot(y[order(x)] ~ x[order(x)],
     type = "l",
     lwd = 3, 
     xlim = range(x.linterp))
lines(y = y.linterp[order(x.linterp)], 
      x = x.linterp[order(x.linterp)], 
      col = "red")

# test case 2
x <- c(1,2,2,3,3,4,4,5,5)
y <- c(1,1,2,2,2,3,4,5,6)

x.linterp <- seq(0, 6, length.out = 1000)
y.linterp <- linterp(x = x, y = y, x.out = x.linterp)

plot(y[order(x)] ~ x[order(x)],
     type = "l",
     lwd = 3, 
     xlim = range(x.linterp))
lines(y = y.linterp[order(x.linterp)], 
      x = x.linterp[order(x.linterp)], 
      col = "red")
