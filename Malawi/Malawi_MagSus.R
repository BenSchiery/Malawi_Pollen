rm(list = ls())
graphics.off()

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
  Y.max <- Y[data.length] # which.max takes the first instance, so we have to reverse
  
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

magSus <- read.table(file = "./data/magSus.csv", header = T, sep = ",")
colnames(magSus) <- c("depth", "age", "gammaDensity", "MS")

char <- read.table(file = "./data/magSus_charcoal.csv", header = T, sep = ",")
colnames(char) <- c("age", "char", "lake")

MSdownsampled <- cbind("age" = char$age,
                       "MS_downsampled" = linterp(x = magSus$age,
                                                  y = magSus$MS, 
                                                  x.out = char$age))

write.table(x = MSdownsampled, file = "./data/MagSusDownsampled.csv", row.names = F, sep = ",")
