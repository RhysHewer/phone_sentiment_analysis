#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1    
# Purpose: prepare iphone data for modelling 
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")

#load data
ipData <- read.csv("data/iphone_smallmatrix_labeled_8d.csv")

##### PREPROCESSING ###########################################################

#remove rows where no info beyond iphone being mentioned
ipData <- ipData[rowSums(ipData) > ipData$iphone,]

#recode dependent variable
ipData$iphonesentiment <- recode(ipData$iphonesentiment, 
                                    '0' = 1, 
                                    '1' = 1, 
                                    '2' = 2, 
                                    '3' = 3, 
                                    '4' = 4, 
                                    '5' = 4)

#remove near zero variance features
nzv <- nearZeroVar(ipData)
ipData <- ipData[-nzv]

##### Collate Positive/Unclear/Negative sentiments ############################
ipData$iphonePos <- ipData %>% 
        select(starts_with("iphone")) %>% 
        select(contains("pos")) %>% 
        rowSums()

ipData$iphoneUnc <- ipData %>% 
        select(starts_with("iphone")) %>% 
        select(contains("unc")) %>% 
        rowSums()                   

ipData$iphoneNeg <- ipData %>% 
        select(starts_with("iphone")) %>% 
        select(contains("neg")) %>% 
        rowSums()

save(ipData, file = "output/ipData.RDS")
