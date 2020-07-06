rm(list = ls())
graphics.off()

library(rstanarm)
library(bayesplot)

eco <- read.csv("./data/Last600_Eco2.csv")
eco$Age <- NULL

threshold <- 85
eco.yng <- eco[eco$age <= threshold,]
eco.old <- eco[eco$age > threshold,]

plot(char ~ age, data = eco.yng, pch = 16)
plot(char ~ age, data = eco.old, pch = 16)

mod <- rstanarm::stan_glm(char ~ age, data = eco)
mod.yng <- rstanarm::stan_glm(char ~ age, data = eco.yng)
mod.old <- rstanarm::stan_glm(char ~ age, data = eco.old)

mod$stanfit
mod.yng
mod.old
