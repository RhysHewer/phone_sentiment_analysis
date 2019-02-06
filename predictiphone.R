#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1
# Purpose: To apply sentiment model to data
#------------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

#load data (variable data can be used)
load("output/ipLarge.RDS")

##### APPLY SENTIMENT CLASSIFICATION MODEL ####################################
load("output/iphonemodel.RDS")

#Predictions
preds <- predict(iphonemodel, ipLarge)
ipLarge$ipSentPreds <- preds

summary(preds)

save(ipLarge, file = "output/ipLarge.RDS")
