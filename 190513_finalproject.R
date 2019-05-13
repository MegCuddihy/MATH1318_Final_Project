##########################################
# PROJECT       Time Series Analysis Final Project
# DATE          13th May 2019
# AUTHOR        Meg Cuddihy, Sam Holt, Elleni Toumpas
# VERSION       1.0
# THIS SCRIPT   BitCoin Analysis
##########################################



#### PACKAGES ---------
library(TSA)
library(FSAdata)
library(lmtest)


#### SET WORKING DIRECTORY ---------
# Elleni's working directory
# setwd("~/STUDYING/COURSES/MASTERS-ANALYTICS/03-SEMESTER-01-2019/TIME-SERIES-ANALYSIS/MATH1318_Final_Project")

# Meg's working directory
# setwd("")

# Sam's working directory
# setwd("")



#### FUNCTIONS ---------
source("functions.R")



### IMPORT DATA ---------
url <- 'https://raw.githubusercontent.com/MegCuddihy/MATH1318_Final_Project/master/docs/Bitcoin_Historical_Price.csv?token=AIIPMCRZOCCZ55YOP24RDOS43EGV6'
bitcoin <- read.csv(url)
bitcoin.ts <- ts(bitcoin$Close, start=c(2013,04,27), end = c(2019,02,24), frequency = 365)



#### MODEL SPECIFICATION ---------
fig_num = 1
plot(bitcoin.ts, type='o', main=paste0("Figure ",fig_num,": A timeseries plot of BitCoin value"), xlab="Year", ylab="$", cex.main=0.8)
fig_num = fig_num + 1

# Testing Autocorrelation
fig_num <- ms.autocorrelation(bitcoin.ts,1,"Autocorrelation of BitCoin series","","",fig_num)

# Testing Seasonality/ Cyclicity
fig_num <- ms.seasonality(bitcoin.ts, "Seasonality ", "Year", "", 5, 15,fig_num)



#### CHECKING STATIONARY  ---------

# ADF TEST
test_adf(bitcoin.ts, "funitroot")

# Boxcox - Plot Lambda Likelihood
BoxCox.ar(bitcoin.ts)

# Find Lambda
lambda <- boxcox_lambda(bitcoin.ts)

# Transform data
bitcoin.ts.bc <- boxcox(bitcoin.ts, lambda)

# Plot transformed series
plot(bitcoin.ts.bc, type='o', main=paste0("Figure ",fig_num,": A timeseries plot of transformed BitCoin series"), xlab="Year", ylab="$", cex.main=0.8)
fig_num = fig_num + 1

# First Difference 
bitcoin.ts.bc.diff <- diff(bitcoin.ts.bc, difference = 1)

# Visually testing differenced series
fig_num <- test_diff(bitcoin.ts.bc.diff, "", fig_num)

# Testing normality in the series
fig_num <- ts_normality_test(bitcoin.ts.bc.diff,"",fig_num)



#### MODEL FITTING  --------
#### COEFFICIENT ESTIMATION  ---------
#### DIAGNOSTIC TESTING  ---------
#### OVERFITTING  ---------
#### FORECASTING  ---------