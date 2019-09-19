## Code for using JAGS on validation datasets##


## TO DO:
# Use all datasets not just OPEN
# Use the template.jags function to get sample code for a bivariate model
# Embed JAGS code into this R file or tag variables
# Think about label switching
# Think about starting values set seed

# DONE:
# Code to generate starting values like Gelman and Hill 
# Incorporate DPP prior 

rm(list=ls())

setwd("C:/Users/jsi894/Box Sync/measurement_error_grant/nonignorable/nonignore_code") #laptop

library(runjags)
library(rjags)
library(coda)

#tell runjags where JAGS executable is located
#Also allow more than 50 parameters to be saved
runjags.options(jagspath="C:/Users/jsi894/AppData/Local/JAGS/JAGS-4.3.0/x64/bin/jags-terminal.exe", force.summary=TRUE)

testjags()

# load open data
# for OPEN, second 24-hour recall was 3 months later. So only use
# Both 24-hour urine, and first (log.self01) self report.
load(file="R:/PrevMed/Projects/Merror/Analysis/data/formatted/open_outliers_removed_kcal.Rdata")

# load WHI data
# WHI only had one urine on full sample
# second and third 24-hour recalls were 1 and 2 months after first. 
# just use log.urine01.p and log.self01p
load(file="R:/PrevMed/Projects/Merror/Analysis/data/formatted/whi_outliers_removed_kcal.Rdata")

# load AMPM data
# AMPM received a total of 3 24-hour recalls. First two were during the same two weeks
# as the 2 24-hour urines. So use first 2 self reports and two urines. 
load(file="R:/PrevMed/Projects/Merror/Analysis/data/formatted/ampm_outliers_removed_kcal.Rdata")

# tohp
# Just use sodium group !is.na(trtna)
# For trtna, 1 is treatment group, 2 is control
# TOHP has two urines at baseline between 10-60 days apart.
# Total of n=744 in TOHP. 56 dropout after 6 months, 40 dropout after baseline.
# Dropout rate of 13%. 
# 64 missing at 6 months (8.6%)
load(file="R:/PrevMed/Projects/Merror/Analysis/data/formatted/tohp_outliers_removed_kcal.Rdata")
tohp.sodium <- tohp[!is.na(tohp$trtna),]


# PREMIER
# Premier has n=810. 121 (15%) missing at 6 months.
# 53 dropout after baseline, 31 dropout after 6 months. Dropout rate is 10%
# 1 urine at baseline, 6, and 18. 2 recalls at baseline, 6, and 18.
load(file="R:/PrevMed/Projects/Merror/Analysis/data/formatted/premier_outliers_removed_kcal.Rdata")



##Summary of all datasets###
#OPEN: 2 urine, 1 self
#WHI: 1 urine, 1 self
#AMPM: 2 urine, 2 self
#PREMIER 1 urine, 2 self at baseline, 6, and 18.
#TOHP: 2 urine baseline, 1 at 6, self at base and 6.

load(file="R:/PrevMed/Projects/Merror/Analysis/data/formatted/premier_outliers_removed_kcal.Rdata")



names(open)

#center covariates
open$c.self <- open$log.self01 - mean(open$log.self01)
open$c.age  <- open$age - mean(open$age)
open$c.bmi  <- open$log.bmi - mean(open$log.bmi)
open$c.male <- open$male - mean(open$male)

#############################
# Code to get a template
template.jags(log.urine01 ~ c.self + c.age + c.bmi + c.male, data=open, family="gaussian")

fit <- run.jags('JAGSmodel.txt')
###############################




K <- 20

# Create a function to generate starting values
inits <- function() {
  list(intercept=rnorm(K, mean=1), c.self_coefficient=rnorm(K, mean=1), c.age_coefficient=rnorm(K, mean=1),
       c.bmi_coefficient=rnorm(K, mean=1), c.male_coefficient=rnorm(K, mean=1),
       regression_precision=rnorm(K, mean=8))
}

fit <- run.jags('JAGSmodel_dprior.txt', inits=inits, n.chains=2)


fit.mcmc <- as.mcmc.list(fit)

summary(fit.mcmc)
traceplot(fit.mcmc)

