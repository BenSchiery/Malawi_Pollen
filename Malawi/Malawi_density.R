rm(list = ls())
graphics.off()

library(kdensity)

eco <- read.csv("./data/Last600_Eco2.csv")

age.threshold <- 87
charcoal.young <- eco$char[eco$age <= age.threshold]
charcoal.old <- eco$char[eco$age > age.threshold]

char.dens <- kdensity::kdensity(x = charcoal.old)
integrate(char.dens, lower = -Inf, upper = Inf) # to verify this is 1

probs.young <- sapply(X = charcoal.young,
                      FUN = function(x){
                        integrate(f = char.dens, 
                                  lower = x, 
                                  upper = Inf)$value
                      })
likelihoods.young <- sapply(X = charcoal.young,
                            FUN = char.dens)

cbind(charcoal.young, 
      probs.young)

first.spike <- max(eco$char[eco$age > 100])
integrate(f = char.dens, 
          lower = first.spike, 
          upper = Inf)$value

second.spike <- max(eco$char)
integrate(f = char.dens, 
          lower = second.spike, 
          upper = Inf)$value
