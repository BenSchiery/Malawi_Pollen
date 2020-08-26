# We have lake level data, pollen count data, and charcoal data, all sampled at different
# time points. Pollen is sampled the most sparsely of these, so we will downsample the
# other two to match its time points. We will do this by Krigging, by cubic splines, and
# by linear interpolation to see which is best.

rm(list = ls())
graphics.off()

# library(fields) # for Krigging functions

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

pollenData <- read.csv("./data/20200722_pollen.csv")
lakeData <- read.csv("./data/20200722_lake.csv")
charcoalData <- read.csv("./data/20200722_charcoal.csv")

colnames(pollenData)[1] <- "age"
colnames(lakeData) <- c("age", "lake")
colnames(charcoalData) <- c("age", "charcoal")

# make sure data are in chronological order
pollenData <- pollenData[order(pollenData$age),]
lakeData <- lakeData[order(lakeData$age),]
charcoalData <- charcoalData[order(charcoalData$age),]

tail(pollenData)
tail(lakeData)
tail(charcoalData)

#################################
#### interpolate by Krigging ####
#################################

# index of the smallest charcoal age greater than the greatest pollen age
# last.age.char <- min(which(charcoalData$age > max(pollenData$age)))
# truncate the charcoal data to just what is needed
# char.trunc <- charcoalData[1:last.age.char,]
# make a 1-D Krig model of the charcoal data
# char.Krig <- fields::Krig(x = matrix(char.trunc$age, ncol = 1), 
#                           Y = matrix(char.trunc$charcoal, ncol = 1))
# predict the charcoal values at the pollen ages
# char.Krig.pred <- fields::predict.Krig(object = char.Krig,
#                                        x = pollenData$age)

# plot(char.trunc, type = "l", lwd = 2, main = "Charcoal Krig Interpolation")
# lines(char.Krig.pred ~ pollenData$age, lwd = 1, col = "red")

# index of the smallest lake level age greater than the greatest pollen age
# last.age.lake <- min(which(lakeData$age > max(pollenData$age)))
# truncate the lake level data to just what is needed
# lake.trunc <- lakeData[1:last.age.lake,]
# make a 1-D Krig model of the lake level data
# lake.Krig <- fields::Krig(x = matrix(lake.trunc$age, ncol = 1),
#                           Y = matrix(lake.trunc$lake, ncol = 1))
# predict the lake level values at pollen ages
# lake.Krig.pred <- fields::predict.Krig(object = lake.Krig, 
#                                        x = pollenData$age)

# plot(lake.trunc, type = "l", lwd = 2, main = "Lake Level Krig Interpolation")
# lines(lake.Krig.pred ~ pollenData$age, lwd = 1, col = "red")

######################################
#### interpolate by cubic splines ####
######################################

# index of the smallest charcoal age greater than the greatest pollen age
# last.age.char <- min(which(charcoalData$age > max(pollenData$age)))
# truncate the charcoal data to just what is needed
# char.trunc <- charcoalData[1:last.age.char,]
# interpolate by a cubic spline
# char.spline.pred <- spline(x = char.trunc, xout = pollenData$age)[[2]]

# plot(char.trunc, type = "l", lwd = 2, main = "Charcoal Spline Interpolation")
# lines(char.spline.pred ~ pollenData$age, lwd = 1, col = "red")

# index of the smallest lake level age greater than the greatest pollen age
# last.age.lake <- min(which(lakeData$age > max(pollenData$age)))
# truncate the lake level data to just what is needed
# lake.trunc <- lakeData[1:last.age.lake,]
# interpolate by a cubic spline
# lake.spline.pred <- spline(x = lake.trunc, xout = pollenData$age)[[2]]

# plot(lake.trunc, type = "l", lwd = 2, main = "Lake Level Spline Iterpolation")
# lines(lake.spline.pred ~ pollenData$age, lwd = 1, col = "red")

##############################
#### interpolate linearly ####
##############################

# index of the smallest charcoal age greater than the greatest pollen age
last.age.char <- min(which(charcoalData$age > max(pollenData$age)))
# truncate the charcoal data to just what is needed
char.trunc <- charcoalData[1:last.age.char,]
# interpolate linearly
char.linterp.pred <- linterp(x = char.trunc$age, 
                             y = char.trunc$charcoal, 
                             x.out = pollenData$age)

plot(char.trunc, type = "l", lwd = 2, main = "Charcoal Linear Interpolation")
lines(char.linterp.pred ~ pollenData$age, lwd = 1, col = "red")

# index of the smallest lake level age greater than the greatest pollen age
last.age.lake <- min(which(lakeData$age > max(pollenData$age)))
# truncate the lake level data to just what is needed
lake.trunc <- lakeData[1:last.age.lake,]
# interpolate linearly
lake.linterp.pred <- linterp(x = lake.trunc$age, 
                             y = lake.trunc$lake, 
                             x.out = pollenData$age)

plot(lake.trunc, type = "l", lwd = 2, main = "Charcoal Linear Interpolation")
lines(lake.linterp.pred ~ pollenData$age, lwd = 1, col = "red")

#############################
#### save interpolations #### 
#############################
# splines didn't work well, so we won't save those results

charcoalData.linterp <- data.frame("age" = pollenData$age,
                                   "charcoal" = char.linterp.pred)
write.table(x = charcoalData.linterp, 
            file = "./data/20200722_char_linterp.csv", 
            row.names = F, 
            col.names = T, 
            sep = ",")

# charcoalData.Krig <- data.frame("age" = pollenData$age,
#                                 "charcoal" = char.Krig.pred)
# write.table(x = charcoalData.Krig, 
#             file = "./data/char_Krig.csv", 
#             row.names = F, 
#             col.names = T, 
#             sep = ",")

lakeData.linterp <- data.frame("age" = pollenData$age,
                               "lake" = lake.linterp.pred)
write.table(x = lakeData.linterp, 
            file = "./data/20200722_lake_linterp.csv", 
            row.names = F, 
            col.names = T, 
            sep = ",")

# lakeData.Krig <- data.frame("age" = pollenData$age,
#                             "lake" = lake.Krig.pred)
# write.table(x = lakeData.Krig, 
#             file = "./data/lake_Krig.csv", 
#             row.names = F, 
#             col.names = T, 
#             sep = ",")
