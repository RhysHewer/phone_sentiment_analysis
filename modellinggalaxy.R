#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1
# Purpose: To train/test samsung galaxy sentiment model
#------------------------------------------------------------------------------#


#Load libraries
source("scripts/libraries.R")

#load data (variable data can be used)
load("output/galData.RDS")
modData <- galData
rm(galData)
modData$galaxysentiment <- modData$galaxysentiment %>% as.factor()

##### MODELLING PREPARATION ###################################################

#Parallel processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

#Cross Validation
fitControl<- trainControl(method = "cv", 
                          number = 5, 
                          savePredictions = TRUE, 
                          allowParallel = TRUE)

#Creating galtesting/Training sets
set.seed(111)
trainIndex <- createDataPartition(modData$galaxysentiment, 
                                  p = 0.75, list = FALSE)
training <- modData[ trainIndex,]
galtesting  <- modData[-trainIndex,]
rm(modData)


##### SENTIMENT CLASSIFICATION ################################################

#Train model
set.seed(111)
galaxymodel <- train(galaxysentiment ~ .,
                     data = training,
                     method = "xgbTree",
                     trControl = fitControl)

galaxymodel
save(galaxymodel, file = "output/galaxymodel.RDS")

#Predictions
preds <- predict(galaxymodel, galtesting)
galtesting$preds <- preds
save(galtesting, file = "output/galtesting.RDS")

#Metrics
mets <- postResample(pred = galtesting$preds, obs = galtesting$galaxysentiment)
mets

#Confusion Matrix
confMat <- confusionMatrix(galtesting$preds, galtesting$galaxysentiment)
confMat

#Mean prediction distance
predDist <- abs((galtesting$preds %>% as.numeric()) - 
                        (galtesting$galaxysentiment %>% as.numeric())) %>% 
        mean() %>% round(3)
predDist
