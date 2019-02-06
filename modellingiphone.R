#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1
# Purpose: To train/test iphone sentiment model
#------------------------------------------------------------------------------#


#Load libraries
source("scripts/libraries.R")

#load data (variable data can be used)
load("output/ipData.RDS")
modData <- ipData
rm(ipData)
modData$iphonesentiment <- modData$iphonesentiment %>% as.factor()

##### MODELLING PREPARATION ###################################################

#Parallel processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

#Cross Validation
fitControl<- trainControl(method = "cv", 
                          number = 5, 
                          savePredictions = TRUE, 
                          allowParallel = TRUE)

#Creating Testing/Training sets
set.seed(111)
trainIndex <- createDataPartition(modData$iphonesentiment, p = 0.75, list = FALSE)
training <- modData[ trainIndex,]
iptesting  <- modData[-trainIndex,]
rm(modData)


##### SENTIMENT CLASSIFICATION ################################################

#Train model
set.seed(111)
iphonemodel <- train(iphonesentiment ~ .,
                   data = training,
                   method = "xgbTree",
                   trControl = fitControl)

iphonemodel
save(iphonemodel, file = "output/iphonemodel.RDS")

#Predictions
preds <- predict(iphonemodel, iptesting)
iptesting$preds <- preds
save(iptesting, file = "output/iptesting.RDS")

#Metrics
mets <- postResample(pred = iptesting$preds, obs = iptesting$iphonesentiment)
mets

#Confusion Matrix
confMat <- confusionMatrix(iptesting$preds, iptesting$iphonesentiment)
confMat

#Mean prediction distance
predDist <- abs((iptesting$preds %>% as.numeric()) - 
                (iptesting$iphonesentiment %>% as.numeric())) %>% 
        mean() %>% round(3)
predDist
