rm(list = ls())
graphics.off()
options(scipen = 17)

library(vegan)

# read in lrb data 
lrb <- read.csv("./data/LRB_CFR_alldata.csv",header=T)
dim(lrb)
colnames(lrb)
head(lrb)
envvars <- scale(cbind("MAT" = lrb$bio.1, 
                       "MATseas" = lrb$bio.4,
                       "TAP" = lrb$bio.12, 
                       "TAPseas" = lrb$bio.15), 
                 center = T,
                 scale = T) # MAT, MAT sd, TAP, TAP sd
geog <- cbind("lon" = lrb$longitude,
              "lat" = lrb$latitude)

# use the ranking data
forag <- lrb[,86:88] / 100
forag <- forag + abs(rnorm(dim(forag)[1], sd = 0.0001))
# include the distillery name as the matrix rowname
row.names(forag) <- lrb$name
# lrb.dat <- as.matrix(lrb.dat[lrb$wldsec == "asia",])
forag.dist <- dist(vegan::decostand(x = forag, 
                                    method = "chi.square")) # approximate chi-square distance
# forag.dist <- dist(x = forag, method = "canberra")
env.dist <- dist(envvars)
cmd <- cmdscale(forag.dist, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)
cmd
summary(cmd)
lrb.fit <- vegan::envfit(ord = cmd, 
                         env = forag,
                         permutations = 10000) 
lrb.fit
env.fit <- vegan::envfit(ord = cmd, 
                         env = envvars, 
                         permutations = 10000) 
env.fit
prey.fit <- vegan::envfit(ord = cmd, 
                          env = lrb$lexprey,
                          permutations = 10000) 
prey.fit

plot(cmd$points, type = "n")
text(cmd$points, label = row.names(forag), cex = 0.55, col = "gray")
plot(lrb.fit, col = "red", lwd = 3, cex = 1.3)
plot(env.fit, col = "blue", lwd = 3, cex = 1.3)
plot(prey.fit)
