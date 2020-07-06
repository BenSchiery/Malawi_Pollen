require(rstan)
require(ggplot2)
require(dplyr)
require(bayesplot)
require(lme4)
require(brms)

xa <- rnorm(50)
ya <- .5 + .65*xa +rnorm(50)

xb <- rnorm(50)
yb <- 1.5 + 1.5*xb + rnorm(50)

x<-c(xa,xb)
y<-c(ya,yb)

ID<-rep(c(1,2), each=50)
ID
plot(x,y, type="n")
points(x,y, col=ID, pch=19)

data<-data.frame(x,y,ID)

mod3<-brm(y ~ x 
          +  (1 + x|r|ID), 
          data = data, family = "gaussian", control = list(adapt_delta=.99))
plot(mod3)
print(mod3)
cf<-coefficients(mod3)
names(cf)
cf

# extract posterior samples of population-level effects 
s <- posterior_samples(mod3)
head(s)
names(s)

plot(x,y, type="p")
points(x,y, col=ID, pch=19)


for (i in 1:length(s$b_Intercept)){
  pred1<- s$b_Intercept[i]  +    s$b_x[i]*x[ID==1] +  s$'r_ID[1,Intercept]'[i] + s$"r_ID[1,x]"[i] * x[ID==1]
  pred2<- s$b_Intercept[i]  +    s$b_x[i]*x[ID==2] +  s$'r_ID[2,Intercept]'[i] + s$"r_ID[2,x]"[i] * x[ID==2]
  points(x[ID==1],pred1, col="green")
  points(x[ID==2],pred2, col="orange")
  }

