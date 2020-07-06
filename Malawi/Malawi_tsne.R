rm(list = ls())
graphics.off()

library(tsne)
library(animation)

rot <- function(x, y, z){
  rx <- rbind(c(1, 0, 0),
              c(0, cos(x), -sin(x)),
              c(0, sin(x), cos(x)))
  ry <- rbind(c(cos(y), 0, sin(y)),
              c(0, 1, 0),
              c(-sin(y), 0, cos(y)))
  rz <- rbind(c(cos(z), -sin(z), 0),
              c(sin(z), cos(z), 0),
              c(0, 0, 1))
  return(rz %*% ry %*% rx)
}

eco <- read.csv("./data/Last600_Eco2.csv")

X <- eco[,c("age", "lake", "char", "rich")]
young <- eco$age <= 85
old <- eco$age > 85
eco.cex <- old + 2 * young
eco.colors <- c("red", "black")[1 + old]
age.scaled <- eco$age
age.scaled <- age.scaled - min(age.scaled)
age.scaled <- age.scaled / max(age.scaled)
eco.colors.grad <- sapply(X = age.scaled,
                          FUN = function(x){
                            y <- colorRamp(c("red", "blue"))(x) / 255
                            return(rgb(y[1], y[2], y[3]))
                          })

# eco.tsne.ageIn.2 <- tsne::tsne(X = X, k = 2)
# eco.tsne.ageIn.3 <- tsne::tsne(X = X, k = 3)
# eco.tsne.ageOut.2 <- tsne::tsne(X = X[,-1], k = 2)
# eco.tsne.ageOut.3 <- tsne::tsne(X = X[,-1], k = 3)

# write.csv(x = eco.tsne.ageIn.2, file = "./data/eco_tsne_ageIn_2.csv", row.names = F)
# write.csv(x = eco.tsne.ageIn.3, file = "./data/eco_tsne_ageIn_3.csv", row.names = F)
# write.csv(x = eco.tsne.ageOut.2, file = "./data/eco_tsne_ageOut_2.csv", row.names = F)
# write.csv(x = eco.tsne.ageOut.3, file = "./data/eco_tsne_ageOut_3.csv", row.names = F)

# load data from previous runs of t-SNE
eco.tsne.ageIn.2 <- as.matrix(read.csv("./data/eco_tsne_ageIn_2.csv"))
eco.tsne.ageIn.3 <- as.matrix(read.csv("./data/eco_tsne_ageIn_3.csv"))
eco.tsne.ageOut.2 <- as.matrix(read.csv("./data/eco_tsne_ageOut_2.csv"))
eco.tsne.ageOut.3 <- as.matrix(read.csv("./data/eco_tsne_ageOut_3.csv"))

# video parameters
n.frame <- 200
x.seq <- seq(0, 2 * pi, length.out = n.frame)

######################
#### Age Included ####
######################

# young = red, old = black
png(width = 480, 
    height = 480, 
    filename = "./pictures/eco_tsne_ageIn.png")
plot(eco.tsne.ageIn.2, 
     col = eco.colors,
     xlab = "t-SNE X",
     ylab = "t-SNE Y",
     pch = 16)
dev.off()

# young = red, old = blue
png(width = 480, 
    height = 480, 
    filename = "./pictures/eco_tsne_ageIn_gradient.png")
plot(eco.tsne.ageIn.2, 
     col = eco.colors.grad,
     xlab = "t-SNE X",
     ylab = "t-SNE Y",
     pch = 16)
dev.off()

# video
data.3d <- eco.tsne.ageIn.3
x.lim <- range(data.3d[,1])
y.lim <- range(data.3d[,2])
saveVideo(expr = {
  for(frame in 1:n.frame){
    rot.mat <- rot(x.seq[frame], 0, 0)
    xyz <- (data.3d %*% t(rot.mat))
    z.ord <- order(xyz[,3])
    xy <- xyz[z.ord,1:2]
    plot(xy, 
         col = eco.colors[z.ord],
         xlim = x.lim,
         ylim = y.lim,
         xlab = NA,
         ylab = NA,
         pch = 16)
  }
}, interval = 1/10, video.name = "./videos/eco_tsne_ageIn.mp4")

######################
#### Age Excluded ####
######################

# young = red, old = black
png(width = 480, 
    height = 480, 
    filename = "./pictures/eco_tsne_ageOut.png")
plot(eco.tsne.ageOut.2, 
     col = eco.colors,
     xlab = "t-SNE X",
     ylab = "t-SNE Y",
     pch = 16)
dev.off()

# young = red, old = blue
png(width = 480, 
    height = 480, 
    filename = "./pictures/eco_tsne_ageOut_gradient.png")
plot(eco.tsne.ageOut.2, 
     col = eco.colors.grad,
     xlab = "t-SNE X",
     ylab = "t-SNE Y",
     pch = 16)
dev.off()

# video
data.3d <- eco.tsne.ageOut.3
x.lim <- range(data.3d[,1])
y.lim <- range(data.3d[,2])
saveVideo(expr = {
  for(frame in 1:n.frame){
    rot.mat <- rot(x.seq[frame], 0, 0)
    xyz <- (data.3d %*% t(rot.mat))
    z.ord <- order(xyz[,3])
    xy <- xyz[z.ord,1:2]
    plot(xy, 
         col = eco.colors[z.ord],
         xlim = x.lim,
         ylim = y.lim,
         xlab = NA,
         ylab = NA,
         pch = 16)
  }
}, interval = 1/10, video.name = "./videos/eco_tsne_ageOut.mp4")
