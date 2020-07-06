rm(list = ls())
graphics.off()

library(sads)
set.seed(2587)

###############################
#### Baby's First Evenness ####
###############################

s1 <- rsad(S = 1000,
           frac = 1, 
           sad = "geom", 
           coef = list(prob = 0.9),
           zeroes = T, 
           ssize = 1)
# this is virtually the same as:
# #s1 <- rgeom(10, prob = 0.9)
s1

# simpson's measure of evenness
D <- sum((s1 / sum(s1))^2)
(1/D) / length(s1)
1 - D 

# simpson's "other" equation
D_p <- sum((s1 * (s1 - 1)) / (sum(s1) * (sum(s1) - 1)))
(1/D_p) / length(s1)
1 - D_p

######################
#### Evenness Sim ####
######################

niter <- 100
max.spec <- 1000
rich <- seq(0, max.spec, by = 10)[-1]
prob <- seq(0.1, 0.99, length.out = 50)
for(k in 1:niter){
  s1 <- array(NA, dim = c(length(prob), max.spec, length(rich)))
  for (i in 1:length(rich)){
    for(j in 1:length(prob)){
      cat("\r                                     \r", 
          "iter no", j, "of", i, "within", k)
      
      s <- rsad(S = rich[i], 
                frac = 1, 
                sad = "geom",
                coef = list(prob = prob[j]),
                zeroes = FALSE, 
                ssize = 1)
      
      if(length(s) < max.spec){
        s <- c(s, rep(NA, max.spec-length(s)))
      }
      s1[j,,i] <- s
    }
  }
  
  oneover <- array(NA, 
                   dim = c(dim(s1)[1], dim(s1)[3], niter), 
                   dimnames = list(prob, rich, 1:niter))
  oneminus <- array(NA,
                    dim = c(dim(s1)[1], dim(s1)[3], niter), 
                    dimnames = list(prob, rich, 1:niter))
  oneover_p <- array(NA,
                     c(dim(s1)[1], dim(s1)[3], niter), 
                     dimnames = list(prob, rich, 1:niter))
  oneminus_p <- array(NA,
                      c(dim(s1)[1], dim(s1)[3], niter), 
                      dimnames = list(prob, rich, 1:niter))
  D <- array(NA,
             c(dim(s1)[1], dim(s1)[3], niter), 
             dimnames = list(prob, rich, 1:niter))
  D_p <- array(NA,
               c(dim(s1)[1], dim(s1)[3], niter), 
               dimnames = list(prob, rich, 1:niter))
  
  for(m in 1:dim(s1)[3]){
    D[,m,k] <- rowSums((s1[,,m]/rowSums(s1[,,m], na.rm = TRUE))^2, na.rm = TRUE)
    D_p[,m,k] <- rowSums((s1[,,m]*(s1[,,m]-1)) / (rowSums(s1[,,m], na.rm = TRUE) * (rowSums(s1[,,m], na.rm = TRUE)-1)), na.rm = TRUE)
    oneover[,m,k] <- (1/D[,m,k]) / rowSums(!is.na(s1[,,m]))
    oneminus[,m,k] <- 1 - D[,m,k]
    oneover_p[,m,k] <- (1/D_p[,m,k]) / rowSums(!is.na(s1[,,m]))
    oneminus_p[,m,k] <- 1 - D_p[,m,k]
  }
}

dev.new()
par(mfrow = c(3,2))
plot(prob, 
     apply(oneover, c(1,2), mean, na.rm = TRUE)[,1],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D",
     main = "Low Richness NO 0s")
plot(prob,
     apply(oneover_p,c(1,2), mean, na.rm = TRUE)[,1],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D PRIME", 
     main = "Low Richness NO 0s")
plot(prob,
     apply(oneover,c(1,2),mean, na.rm=TRUE)[,50],
     type="p",
     xlab="Evenness (DECREASES left to right)",
     ylab="E=1/D",
     main="Medium Richness NO 0s")
plot(prob,
     apply(oneover_p,c(1,2),mean, na.rm=TRUE)[,50],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D PRIME",
     main = "Medium Richness NO 0s")
plot(prob,
     apply(oneover,c(1,2),mean, na.rm=TRUE)[,100],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D",
     main = "High Richness NO 0s")
plot(prob,
     apply(oneover_p,c(1,2),mean, na.rm=TRUE)[,100],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D PRIME",
     main = "High Richness NO 0s")

dev.new()
par(mfrow = c(3,2))
plot(prob,
     apply(oneminus,c(1,2),mean, na.rm=TRUE)[,1],
     type = "p",
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D",
     main = "Low Richness NO 0s")
plot(prob,
     apply(oneminus_p,c(1,2),mean, na.rm=TRUE)[,1],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D PRIME",
     main = "Low Richness NO 0s")
plot(prob,
     apply(oneminus,c(1,2),mean, na.rm=TRUE)[,50],
     type = "p",
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D",
     main = "Medium Richness NO 0s")
plot(prob,
     apply(oneminus_p,c(1,2),mean, na.rm=TRUE)[,50],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D PRIME", 
     main = "Medium Richness NO 0s")
plot(prob,
     apply(oneminus,c(1,2),mean, na.rm=TRUE)[,100],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D", 
     main = "High Richness NO 0s")
plot(prob,
     apply(oneminus_p,c(1,2),mean, na.rm=TRUE)[,100],
     type = "p",
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D PRIME",
     main = "High Richness NO 0s")

set.seed(2587)
for(k in 1:niter){
  s1 <- array(NA, dim = c(length(prob), max.spec, length(rich)))
  for (i in 1:length(rich)){
    for(j in 1:length(prob)){
      cat("iter no", j, "of", i, "within",k, "\n")
      
      s <- rsad(S = rich[i], frac = 1, sad = "geom",
                coef = list(prob = prob[j]),
                zeroes = TRUE, ssize =1)
      
      if(length(s) < max.spec){
        s <- c(s, rep(NA, max.spec-length(s)))
      }
      s1[j,,i] <- s
    }
  }
  
  oneover <- array(NA,
                   dim = c(dim(s1)[1], dim(s1)[3], niter), 
                   dimnames = list(prob, rich, 1:niter))
  oneminus <- array(NA,
                    dim = c(dim(s1)[1], dim(s1)[3], niter), 
                    dimnames = list(prob, rich, 1:niter))
  oneover_p <- array(NA,
                     dim = c(dim(s1)[1], dim(s1)[3], niter), 
                     dimnames = list(prob, rich, 1:niter))
  oneminus_p <- array(NA,
                      dim = c(dim(s1)[1], dim(s1)[3], niter), 
                      dimnames = list(prob, rich, 1:niter))
  D <- array(NA,
             dim = c(dim(s1)[1], dim(s1)[3], niter), 
             dimnames = list(prob, rich, 1:niter))
  D_p <- array(NA,
               dim = c(dim(s1)[1], dim(s1)[3], niter), 
               dimnames = list(prob, rich, 1:niter))
  
  for(m in 1:dim(s1)[3]){
    D[,m,k] <- rowSums((s1[,,m]/rowSums(s1[,,m], na.rm = TRUE))^2, na.rm = TRUE)
    D_p[,m,k] <- rowSums((s1[,,m]*(s1[,,m]-1))/(rowSums(s1[,,m], na.rm = TRUE) * (rowSums(s1[,,m], na.rm = TRUE)-1)), na.rm = TRUE)
    oneover[,m,k] <- (1/D[,m,k])/rowSums(!is.na(s1[,,m]))
    oneminus[,m,k] <- 1 - D[,m,k]
    oneover_p[,m,k] <- (1/D_p[,m,k])/rowSums(!is.na(s1[,,m]))
    oneminus_p[,m,k] <- 1 - D_p[,m,k]
  }
}

dev.new()
par(mfrow = c(3,2))
plot(prob,
     apply(oneover, c(1,2), mean, na.rm = TRUE)[,1],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D", 
     main = "Low Richness including 0s")
plot(prob,
     apply(oneover_p, c(1,2), mean, na.rm = TRUE)[,1],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D PRIME", 
     main = "Low Richness including 0s")
plot(prob,
     apply(oneover, c(1,2), mean, na.rm = TRUE)[,50],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D", 
     main = "Medium Richness including 0s")
plot(prob,
     apply(oneover_p, c(1,2), mean, na.rm = TRUE)[,50],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D PRIME", 
     main = "Medium Richness including 0s")
plot(prob,
     apply(oneover, c(1,2), mean, na.rm = TRUE)[,100],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D",
     main = "High Richness including 0s")
plot(prob,
     apply(oneover_p, c(1,2), mean, na.rm = TRUE)[,100],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "E=1/D PRIME",
     main = "High Richness including 0s")

dev.new()
par(mfrow = c(3,2))
plot(prob,
     apply(oneminus, c(1,2), mean, na.rm = TRUE)[,1],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D", 
     main = "Low Richness including 0s")
plot(prob,
     apply(oneminus_p, c(1,2), mean, na.rm = TRUE)[,1],
     type = "p",
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D PRIME",
     main = "Low Richness including 0s")
plot(prob,
     apply(oneminus, c(1,2), mean, na.rm = TRUE)[,50],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D", 
     main = "Medium Richness including 0s")
plot(prob,
     apply(oneminus_p, c(1,2), mean, na.rm = TRUE)[,50],
     type = "p",
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D PRIME",
     main = "Medium Richness including 0s")
plot(prob,
     apply(oneminus, c(1,2), mean, na.rm = TRUE)[,100],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D", 
     main = "High Richness including 0s")
plot(prob,
     apply(oneminus_p, c(1,2), mean, na.rm = TRUE)[,100],
     type = "p", 
     xlab = "Evenness (DECREASES left to right)",
     ylab = "1-D PRIME", 
     main = "High Richness including 0s")

