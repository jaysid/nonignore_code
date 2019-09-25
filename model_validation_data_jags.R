## Code for using JAGS on validation datasets##


## TO DO:
# Simulate bivarariate Normal data. Make sure works. Look at pfizer model.
# Then run bivariate model 
# Think about label switching
# Regression log.urine on log.self separately by study and sex just to look at parameters
# Figure out which fits better: log.urine, log.self, log.bmi, or non-transformed. 
# Use a K less than 20
# Create separate programs for diagnostics

## Low Priority ##
# Think about starting values set seed
# How to asssess convergence using mutliple chains?
# Consider using TOPH as a validation dataset since has two baseline urines
# Consider dropping WHI because only 1 baseline urine. But good sample.
# Embed JAGS code into this R file

# DONE:
# Code to generate starting values like Gelman and Hill 
# Incorporate DPP prior 
# Simulate some data just to make sure code is working
# Use the template.jags function to get sample code for a bivariate model DOESN'T WORK
# David Aaby will incorporate PREMIER age into datasets 
# RUN ON COMPLETE CASE DATA FIGURE OUT HOW TO DO MISSING COVARIATES.
# Use all datasets not just OPEN: Need to harmonize variables to do this.
# Maybe don't use log.bmi since don't need to assume linearity?
# Maybe separate by gender. DPP takes care of this automatically?
# Look at distribution of study (and sex) by class. Note: Use the C[i] variable

# Notes:
# Need to look at parameters separately by chain 
# because each chain can select different classes
# One class solution for men with log self and log bmi.
# One class solution for women with log self and log bmi
# Urinary sodium primary determinant of class (at least with one urine, not two)

# PREMIER
# Premier has n=810. 121 (15%) missing at 6 months.
# 53 dropout after baseline, 31 dropout after 6 months. Dropout rate is 10%
# 1 urine at baseline, 6, and 18. 2 recalls at baseline, 6, and 18.
# load(file="R:/PrevMed/Projects/Merror/Analysis/data/formatted/premier_outliers_removed_kcal.Rdata")

rm(list=ls())

setwd("C:/Users/jsi894/Box Sync/measurement_error_grant/nonignorable/nonignore_code") #laptop

library(runjags)
library(rjags)
library(coda)

#tell runjags where JAGS executable is located
#Also allow more than 50 parameters to be saved
runjags.options(jagspath="C:/Users/jsi894/AppData/Local/JAGS/JAGS-4.3.0/x64/bin/jags-terminal.exe", force.summary=TRUE)

testjags()

# load the combined TOPH, WHI, OPEN, AMPM validation datasets
load(file="R:/PrevMed/Projects/Merror/Analysis/data/formatted/all_vals_outliers_removed_kcal.Rdata")

N <- nrow(vals)
K <- 20 #max number of latent classes

val.data <- list(N=N, K=K, log.urine01=vals$log.urine01, log.urine02=vals$log.urine02, 
             c.self=vals$c.self, c.age=vals$c.age, c.bmi=vals$c.bmi, 
             c.male=vals$c.male, c.white=vals$c.white)


# Create a function to generate starting values
inits <- function() {
  list(intercept=rnorm(K, mean=1), c.self_coefficient=rnorm(K, mean=1), c.age_coefficient=rnorm(K, mean=1),
       c.bmi_coefficient=rnorm(K, mean=1), c.male_coefficient=rnorm(K, mean=1), 
       c.white_coefficient=rnorm(K, mean=1),
       regression_precision=rnorm(K, mean=10))
}

fit <- run.jags('JAGSmodel_dprior.txt', data=val.data, inits=inits, n.chains=1)

save(fit, file="one_urine_all_covariates_both_sexes_no_transformations.Rdata")
load(file="one_urine_all_covariates_both_sexes.Rdata")

load(file="one_urine_all_covariates_both_sexes_no_transformations.Rdata")

# make an mcmc object for coda
fit.mcmc <- as.mcmc.list(fit)

summary(fit.mcmc)
traceplot(fit.mcmc)

# put parameters into a dataframe
params <- data.frame(as.matrix(fit.mcmc))
params$iter <- 1: nrow(params)

# keep just the cluster assignments
members <- params[ , grepl( "C." , names( params ) ) ]

# Find modal cluster assignment
modefunc <- function(x){
  tabresult <- tabulate(x)
  themode <- which(tabresult == max(tabresult))
  if(sum(tabresult == max(tabresult))>1) themode <- NA
  return(themode)
}

# create dataframe of just cluster modes
clust.mode <- data.frame(apply(members, 2, modefunc))
names(clust.mode) <- "cluster"
clust.mode$id <- 1:nrow(clust.mode)

# create an ID variable for validation studies
vals$id <- 1:nrow(vals)

#merge cluster indicators with validation data
all <- merge(vals, clust.mode, by="id")

# look at categorical characteristics of clusters
CrossTable(all$male, all$cluster, prop.r=FALSE, prop.chisq=FALSE, prop.t=FALSE)
CrossTable(all$study, all$cluster, prop.r=FALSE, prop.chisq=FALSE, prop.t=FALSE)
CrossTable(all$white, all$cluster, prop.r=FALSE, prop.chisq=FALSE, prop.t=FALSE)

# look at continuous characteristics of clusters
boxplot(log.bmi~cluster, data=all)
boxplot(age~cluster, data=all)
boxplot(log.urine01~cluster, data=all)
boxplot(log.self~cluster, data=all)


# scatter plots of urine by self by cluster.
plot(log.urine01 ~ log.self, data=all[all$cluster==17,], col="red", pch=19, ylim=c(0, 14000), xlim=c(0, 14000))
abline(lm(log.urine01 ~ log.self, data=all[all$cluster==17,]), col="red")
points(log.urine01 ~ log.self, data=all[all$cluster==19,], col="blue", pch=19)
abline(lm(log.urine01 ~ log.self, data=all[all$cluster==19,]), col="blue")
points(log.urine01 ~ log.self, data=all[all$cluster==20,], col="green", pch=19)
abline(lm(log.urine01 ~ log.self, data=all[all$cluster==20,]), col="green")







#############################
# Code to get a template
template.jags(log.urine01 ~ c.self + c.age + c.bmi + c.male + c.white, data=vals, family="gaussian")

fit <- run.jags('JAGSmodel.txt')
###############################

