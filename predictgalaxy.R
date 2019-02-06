#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1
# Purpose: To apply sentiment model to data
#------------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

#load data (variable data can be used)
load("output/galLarge.RDS")

##### APPLY SENTIMENT CLASSIFICATION MODEL ####################################
load("output/galaxymodel.RDS")

#Predictions
preds <- predict(galaxymodel, galLarge)
galLarge$galSentPreds <- preds

summary(preds)

save(galLarge, file = "output/galLarge.RDS")
