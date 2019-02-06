#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1
# Purpose: Error Analysis - galaxy modelling
#------------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

#load data
load("output/galtesting.RDS")


##### GENERATE METRICS AND CONFUSION MATRIX ###################################
#Metrics
mets <- postResample(pred = galtesting$preds, obs = galtesting$galaxysentiment)
mets

#Confusion Matrix
confMat <- confusionMatrix(galtesting$preds, galtesting$galaxysentiment)
confMat

##### VISUALISE CONFUSION MATRIX ##############################################
confMatrix <- confMat$table %>% as.data.frame()

g.confMatrix <- ggplot(confMatrix, aes(Reference, Prediction, fill = Freq)) +
        geom_tile(color="black") +
        theme_bw() + 
        coord_equal() +
        scale_fill_distiller(palette = "Blues", direction=1) +
        guides(fill=F) +
        labs(title = "Confusion Matrix: Galaxy Sentiment") +
        geom_text(aes(label = Freq), color="black")
g.confMatrix


##### EXPLORING PREDICTED AS 1 ERRORS #########################################

#subset data
pred1Wrong <- galtesting %>% 
        filter(galaxysentiment != preds) %>% 
        filter(preds == "1")
pred1Wrong$clr <- ifelse(pred1Wrong$samsunggalaxy == "0", "y", "n")

g.pred1Wrong <- ggplot(pred1Wrong, aes(samsunggalaxy, fill = clr)) +
        geom_bar() +
        theme_hc() +
        ggtitle("Most Errors Relate to Zero Mentions") +
        xlab("Galaxy Mentions") +
        ylab("Count") +
        scale_fill_manual(values = c("#8E8E8E", "#15AAEA")) +
        theme(legend.position="none")
g.pred1Wrong

##### EXPLORING PREDICTED AS 4 ERRORS #########################################

#subset data
predCorrect <- galtesting %>% 
        filter(galaxysentiment == preds)

pred4Wrong <- galtesting %>% 
        filter(galaxysentiment != preds) %>% 
        filter(preds == "4")

#subset numerical features per sentiment group
pred4WrongCM <- pred4Wrong %>% select(-galaxysentiment, -preds)

predCorrect1CM <- predCorrect %>% filter(galaxysentiment == "1") %>% 
        select(-galaxysentiment, -preds)
predCorrect2CM <- predCorrect %>% filter(galaxysentiment == "2") %>% 
        select(-galaxysentiment, -preds)
predCorrect3CM <- predCorrect %>% filter(galaxysentiment == "3") %>% 
        select(-galaxysentiment, -preds)
predCorrect4CM <- predCorrect %>% filter(galaxysentiment == "4") %>% 
        select(-galaxysentiment, -preds)


#create colMeans dataframe
colMeansCollated <- colMeans(pred4WrongCM) %>% 
        as.data.frame() %>% 
        rownames_to_column()

colnames(colMeansCollated) <- c("feature","pred4WrongCM")
colMeansCollated$correctPreds1 <- colMeans(predCorrect1CM)
colMeansCollated$correctPreds3 <- colMeans(predCorrect3CM)
colMeansCollated$correctPreds4 <- colMeans(predCorrect4CM)


#Transform to long data
colMeansCollated.long <- colMeansCollated %>% 
        gather(-'feature', key = 'df', value = 'value')


g.colMeansCollated.long <- ggplot(colMeansCollated.long, 
                                  aes(feature, value, colour = df, 
                                      group = df, size = df)) +
        geom_line() +
        geom_point() +
        theme_hc() +
        ggtitle("Correct 4 and Incorrect Prediction 4 Profiles Similar") +
        xlab("Feature") +
        ylab("Mean Value") +
        
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        scale_colour_manual(values = c("#8E8E8E", 
                                       "#8E8E8E", 
                                       "#15AAEA", 
                                       "#FC440F")) +
        
        scale_size_manual(values = c(1,1,2,2)) +
        annotate("text", x = "samsungcamneg", y = 2.4, 
                 label = "Incorrect Predictions of 4",
                 colour = "#FC440F",
                 size = 5) +
        
        annotate("text", x = "samsungcamneg", y = 2.2, 
                 label = "Correct Predictions of 4",
                 colour = "#15AAEA",
                 size = 5)

g.colMeansCollated.long
