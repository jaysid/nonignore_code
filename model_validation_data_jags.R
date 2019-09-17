## Code for using JAGS on validation datasets##
## To do:
# 1. Embed JAGS code into this R file
# 2. Code to generate starting values like Gelman and Hill
# 3. Incorporate DPP prior
# 4. Use all datasets not just OPEN


rm(list=ls())

setwd("C:/Users/jsi894/Box Sync/measurement_error_grant/nonignorable") #laptop

library(runjags)
library(rjags)
library(coda)
runjags.options(jagspath="C:/Users/jsi894/AppData/Local/JAGS/JAGS-4.3.0/x64/bin/jags-terminal.exe")

testjags()

load(file="R:/PrevMed/Projects/Merror/Analysis/data/formatted/open_outliers_removed_kcal.Rdata")

names(open)

#center covariates
open$c.self <- open$log.self01 - mean(open$log.self01)
open$c.age  <- open$age - mean(open$age)
open$c.bmi  <- open$log.bmi - mean(open$log.bmi)

template.jags(log.urine01 ~ c.self + c.age + c.bmi + male, data=open, family="gaussian")

fit <- run.jags('JAGSmodel.txt')
fit
plot(fit)


plot(fit, vars="c", layout=c(3,3))

traceplot(fit)

fit.mcmc <- as.mcmc.list(fit)

summary(fit.mcmc)
traceplot(fit.mcmc)

K <- 20
inits <- function() {
  list(regression_precision=rnorm(K, mean=8))
}

fit <- run.jags('JAGSmodel_dprior.txt', inits=inits, n.chains=2)




