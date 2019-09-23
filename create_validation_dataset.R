# This program combines the OPEN, WHI, AMPM and TOPH datasets.
# To create a cross-sectional validation dataset

#Summary of all datasets:
#OPEN: 2 urine, 1 self
#WHI: 1 urine, 1 self
#AMPM: 2 urine, 2 self
#TOHP: 2 urine baseline, 1 self at baseline

# kcal outliers have been removed from all these datasets.

rm(list=ls())

setwd("R:/PrevMed/Projects/Merror/Analysis/data/formatted")


# load open data
# for OPEN, second 24-hour recall was 3 months later. So only use
# Both 24-hour urine, and first (log.self01) self report.
load(file="open_outliers_removed_kcal.Rdata")
open <- open[,c("male", "age", "log.bmi", "white", "log.self01", "log.urine01", "log.urine02", "study")]
open$log.self <- open$log.self01
open$log.self01 <- NULL

# load WHI data
# WHI only had one urine on full sample
# second and third 24-hour recalls were 1 and 2 months after first. 
# just use log.urine01.p and log.self01p
load(file="whi_outliers_removed_kcal.Rdata")
whi <- whi[,c("male", "age", "log.bmi", "white", "log.self01.p", "log.urine01.p", "study")]
whi$log.self <- whi$log.self01.p
whi$log.urine01 <- whi$log.urine01.p
whi$log.urine02 <- NA
whi$log.self01.p <- whi$log.urine01.p <- NULL

# load AMPM data
# AMPM received a total of 3 24-hour recalls. First two were during the same two weeks
# as the 2 24-hour urines. So use first 2 self reports and two urines. 
load(file="ampm_outliers_removed_kcal.Rdata")
ampm <- ampm[,c("male", "age", "log.bmi", "white", "log.self0.avg", "log.urine01", "log.urine02", "study")]
ampm$log.self <- ampm$log.self0.avg
ampm$log.self0.avg <- NULL

# tohp
# Just use sodium group !is.na(trtna)
# For trtna, 1 is treatment group, 2 is control
# TOHP has two urines at baseline between 10-60 days apart.
# Total of n=744 in TOHP. 56 dropout after 6 months, 40 dropout after baseline.
# Dropout rate of 13%. 
# 64 missing at 6 months (8.6%)
load(file="tohp_outliers_removed_kcal.Rdata")
tohp.sodium <- tohp[!is.na(tohp$trtna),]
tohp.sodium <- tohp.sodium[, c("male", "age", "log.bmi", "white", "log.self01", 
                               "log.urine01", "log.urine02", "study")]
tohp.sodium$log.self <- tohp.sodium$log.self01
tohp.sodium$log.self01 <- NULL


vals <- rbind(open, ampm, whi, tohp.sodium)

#center covariates
vals$c.self <- vals$log.self - mean(vals$log.self)
vals$c.age  <- vals$age - mean(vals$age)
vals$c.bmi  <- vals$log.bmi - mean(vals$log.bmi)
vals$c.male <- vals$male - mean(vals$male)
vals$c.white <- vals$white - mean(vals$white)

save(vals, file="all_vals_outliers_removed_kcal.Rdata")




