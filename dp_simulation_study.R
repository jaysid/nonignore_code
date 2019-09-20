# simulation study

rm(list=ls())

setwd("C:/Users/jsi894/Box Sync/measurement_error_grant/nonignorable/nonignore_code") #laptop

library(runjags)
library(rjags)
library(coda)

#tell runjags where JAGS executable is located
#Also allow more than 50 parameters to be saved
runjags.options(jagspath="C:/Users/jsi894/AppData/Local/JAGS/JAGS-4.3.0/x64/bin/jags-terminal.exe", force.summary=TRUE)

y1 <- data.frame(rnorm(1000, mean=-2, sd=3))
y2 <- data.frame(rnorm(2000, mean=0, sd=1))
y3 <- data.frame(rnorm(500, mean=3, sd=.5))
names(y1) <- "y"
names(y2) <- "y"
names(y3) <- "y"

sim.data <- rbind(y1, y2, y3)



template.jags(y ~ 1,data=sim.data, family="gaussian")

fit <- run.jags('JAGSmodel_sim.txt')

K <- 20

# Create a function to generate starting values
inits <- function() {
  list(intercept=rnorm(K, mean=1), regression_precision=rnorm(K, mean=8))
}

fit <- run.jags('JAGSmodel_sim.txt', inits=inits, n.chains=1)

fit.mcmc <- as.mcmc.list(fit)

summary(fit.mcmc)
traceplot(fit.mcmc)