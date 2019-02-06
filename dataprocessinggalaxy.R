#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1    
# Purpose: prepare galaxy data for modelling 
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")

#load data
galData <- read.csv("data/galaxy_smallmatrix_labeled_8d.csv")

##### PREPROCESSING ###########################################################

#remove rows where no info beyond iphone being mentioned
galData <- galData[rowSums(galData) > galData$samsunggalaxy,]

#recode dependent variable
galData$galaxysentiment <- recode(galData$galaxysentiment, 
                                 '0' = 1, 
                                 '1' = 1, 
                                 '2' = 2, 
                                 '3' = 3, 
                                 '4' = 4, 
                                 '5' = 4)

##### Collate Positive/Unclear/Negative sentiments ############################
galData$galaxyPos <- galData %>% 
        select(starts_with("samsung")) %>% 
        select(contains("pos")) %>% 
        rowSums()

galData$galaxyUnc <- galData %>% 
        select(starts_with("samsung")) %>% 
        select(contains("unc")) %>% 
        rowSums()                   

galData$galaxyNeg <- galData %>% 
        select(starts_with("samsung")) %>% 
        select(contains("neg")) %>% 
        rowSums()

save(galData, file = "output/galData.RDS")

