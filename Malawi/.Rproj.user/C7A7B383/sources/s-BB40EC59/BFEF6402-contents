rm(list = ls())
graphics.off()
options(scipen = 17)

library(dismo)
library(rgdal)
library(vegan)
library(geomorph)

# read in lrb data 
lrb <- read.csv("./data/lrbforenvdat.csv", header = T, row.names = 1)
dim(lrb)
colnames(lrb)
head(lrb)
envvars <- lrb[,4:7] # MAT, MAT sd, TAP, TAP sd
geog <- lrb[,8:9]
forag <- lrb[,1:3]

# e <- raster::extent(-170, 170, -70, 70)
# r <- dismo::gmap(e, lonlat = F, type = 'terrain')
# plot(r, interpolate = TRUE, main = "Map")
# points(dismo::Mercator(geog), pch = 19, cex = 0.55, col = "red")

forag.dist <- dist(vegan::decostand(x = forag, 
                                    method = "chi.square")) # approximate chi-square distance
env.dist <- dist(envvars)
geog.dist <- dist(geog) # design matrix from groups
plot(env.dist, forag.dist, pch = 21, cex = 1.5, bg = "black") # LOTS of variation

# 1: Mantel Test
vegan::mantel(xdis = forag.dist,
              ydis = env.dist)
vegan::mantel(xdis = forag.dist, 
              ydis = geog.dist)   
vegan::mantel(xdis = env.dist,
              ydis = geog.dist) # Mantel test using design matrix
vegan::mantel.partial(xdis = forag.dist, 
                      ydis = env.dist,
                      zdis = geog.dist, 
                      permutations = 999) # 3-way Mantel

# 2: Canonical Correlation Analysis (CCorA)
res <- vegan::CCorA(Y = forag,
                    X = envvars, 
                    permutations = 1000) #canonical correlation w/ 1000 permutions	
plot(x = res$Cx[,1],
     y = res$Cy[,1],
     xlab = "CanCor1 Environment", 
     ylab = "CanCor1 Forag", 
     asp = 1, pch = 21, cex = 1.5, bg = "black")
cor(res$Cx[,1], res$Cy[,1])

## 2-Block Partial Least Squares (PLS)
pls <- two.b.pls(A1 = envvars, 
                 A2 = forag, 
                 iter = 999,
                 seed = NULL)
pls
plot(pls)
# NOTE: can also use function plsr() in library(pls)

# PLS 'by hand'
n <- nrow(envvars)
envvars <- as.matrix(envvars)
forag <- as.matrix(forag)
px <- ncol(envvars)
py <- ncol(forag)
pmin <- min(px, py)
dat <- cbind(envvars, forag)
S <- var(dat)
S12 <- matrix(S[1:px, -(1:px)], px, py)
pls <- La.svd(S12, pmin, pmin)
U <- pls$u
V <- t(pls$vt)
XScores <- envvars %*% U
YScores <- forag %*% V
r.pls <- cor(XScores[, 1], YScores[, 1])
r.pls 

## Escofier's RV coefficient
dat <- cbind(envvars, forag)
S <- var(dat)
S
px <- ncol(envvars)
py <- ncol(forag)
S11 <- S[1:px, 1:px]
S22 <- S[-(1:px), -(1:px)]
S12 <- matrix(S[1:px, -(1:px)], px, py)
RV <- sum(colSums(S12^2)) / sqrt(sum(S11^2) * sum(S22^2))
RV

nsims <- 10000
RVobs <- RV
RVperm <- numeric(nsims)
RVperm[1] <- RVobs
for(i in 2:nsims){
  cat("\r                 \r", 
      "sim no.", i)
  datperm <- cbind(envvars[sample(1:nrow(forag)),], forag)
  S <- var(datperm)
  # S
  px <- ncol(envvars)
  py <- ncol(forag)
  S11 <- S[1:px, 1:px]
  S22 <- S[-(1:px), -(1:px)]
  S12 <- matrix(S[1:px, -(1:px)], px, py)
  RVp <- sum(colSums(S12^2)) / sqrt(sum(S11^2) * sum(S22^2))
  RVperm[i] <- RVp
}
hist(RVperm, breaks = 100)
abline(v = RVobs, col = "red")

# RDA
geog.m <- as.matrix(geog)
envgeo.rda <- vegan::rda(envvars ~ geog.m)
envgeo.rda
plot(envgeo.rda)

# CCA
foraenv.cca <- vegan::rda(forag ~ envvars)
foraenv.cca
plot(foraenv.cca)

# vector fitting
forag.dist <- dist(vegan::decostand(x = forag,
                                    method = "chi.square")) # approximate chi-square distance
env.dist <- dist(envvars)
cmd <- cmdscale(d = forag.dist, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)
cmd

lrb.fit <- vegan::envfit(ord = cmd, 
                         env = forag, 
                         permutations = 10000) 
lrb.fit

env.fit <- vegan::envfit(ord = cmd, 
                         env = envvars, 
                         permutations = 10000) 
env.fit

plot(cmd$points, type = "n")
text(cmd$points, label = row.names(forag), cex = 0.55, col = "gray")
plot(lrb.fit, col = "red", lwd = 3, cex = 1.3)
plot(env.fit, col = "blue", lwd = 3, cex = 1.3)
# try using 
# forag.dist <- dist(forag, method = "canberra")





