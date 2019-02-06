#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1    
# Purpose: Collate and concatenate csv files from data collection. Split for
#          later modellisation.
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")

##### DATA COMBINATION ########################################################

#combine factor sheets and output csv.
dataFac2 <- read.csv("data/runtwo/concatenated_factors2.csv")
dataFac3 <- read.csv("data/runthree/concatenated_factors3.csv")
dataFac4 <- read.csv("data/runfour/concatenated_factors4.csv")
dataFac5 <- read.csv("data/runfive/concatenated_factors5.csv")

facData <- bind_rows(dataFac2, dataFac3, dataFac4, dataFac5)
rm(dataFac2, dataFac3, dataFac4, dataFac5)
save(facData, file = "output/facData.RDS")

write.csv(facData, file = "data/largeMatrix.csv")

##### IPHONE SUBSET FOR MODEL APPLICATION #####################################
load("output/facData.RDS")

ipLarge <- facData %>% filter(iphone != 0)


#Collate Positive/Unclear/Negative sentiments
ipLarge$iphonePos <- ipLarge %>% 
        select(starts_with("iphone")) %>% 
        select(contains("pos")) %>% 
        rowSums()

ipLarge$iphoneUnc <- ipLarge %>% 
        select(starts_with("iphone")) %>% 
        select(contains("unc")) %>% 
        rowSums()                   

ipLarge$iphoneNeg <- ipLarge %>% 
        select(starts_with("iphone")) %>% 
        select(contains("neg")) %>% 
        rowSums()

save(ipLarge, file = "output/ipLarge.RDS")

##### GALAXY SUBSET FOR MODEL APPLICATION #####################################
load("output/facData.RDS")

galLarge <- facData %>% filter(samsunggalaxy != 0)


#Collate Positive/Unclear/Negative sentiments
galLarge$galaxyPos <- galLarge %>% 
        select(starts_with("samsung")) %>% 
        select(contains("pos")) %>% 
        rowSums()

galLarge$galaxyUnc <- galLarge %>% 
        select(starts_with("samsung")) %>% 
        select(contains("unc")) %>% 
        rowSums()                   

galLarge$galaxyNeg <- galLarge %>% 
        select(starts_with("samsung")) %>% 
        select(contains("neg")) %>% 
        rowSums()

save(galLarge, file = "output/galLarge.RDS")
