rm(list = ls())
graphics.off()

library(kernlab)
library(animation)

# a function to make a 3D rotation matrix
# executes a rotation by `x` rad about the x-axis, then y, then z
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
young <- X$age <= 85
old <- X$age > 85
eco.colors <- c("red", "black")[1 + old]
eco.cex <- old + 2 * young

eco.kpca.ageIn.2 <- pcv(kernlab::kpca(~., data = X, features = 2))
eco.kpca.ageIn.3 <- pcv(kernlab::kpca(~., data = X, features = 3))
eco.kpca.ageOut.2 <- pcv(kernlab::kpca(~., data = X[,-1], features = 2))
eco.kpca.ageOut.3 <- pcv(kernlab::kpca(~., data = X[,-1], features = 3))

# write.csv(x = eco.kpca.ageIn.2, file = "./data/eco_kpca_ageIn_2.csv")
# write.csv(x = eco.kpca.ageIn.3, file = "./data/eco_kpca_ageIn_3.csv")
# write.csv(x = eco.kpca.ageOut.2, file = "./data/eco_kpca_ageOut_2.csv")
# write.csv(x = eco.kpca.ageOut.3, file = "./data/eco_kpca_ageOut_3.csv")

n.frame <- 200 # number of frames to draw
x.seq <- seq(0, 2 * pi, length.out = n.frame) # sequence of angles by which to rotate the 3D point cloud

######################
#### Age Included ####
######################

data.3d <- eco.kpca.ageIn.3 # 3D data points to animate
x.lim <- range(data.3d[,1]) # extent of plot window
y.lim <- range(data.3d[,2])
saveVideo(expr = {
  for(frame in 1:n.frame){
    rot.mat <- rot(x.seq[frame], 0, 0) # for rotating the points in 3D
    xyz <- (data.3d %*% t(rot.mat)) # perform the rotation
    z.ord <- order(xyz[,3]) # lower z = farther from "camera"
    xy <- xyz[z.ord,1:2] # sort so points "in the back" are plotted first; project onto xy-plane
    plot(xy, 
         col = eco.colors[z.ord], # sort colors to match order of points
         cex = eco.cex[z.ord], # sort cexs to match order of points
         xlim = x.lim, 
         ylim = y.lim,
         xlab = NA, 
         ylab = NA,
         pch = 1)
  }
}, interval = 1/10, video.name = "./videos/Malawi_kpca_ageIn.mp4")

######################
#### Age Excluded ####
######################

data.3d <- eco.kpca.ageOut.3
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
         cex = eco.cex[z.ord],
         xlim = x.lim, 
         ylim = y.lim,
         xlab = NA, 
         ylab = NA,
         pch = 16)
  }
}, interval = 1/10, video.name = "./videos/Malawi_kpca_ageOut.mp4")
