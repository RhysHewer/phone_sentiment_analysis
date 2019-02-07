#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1
# Purpose: Error Analysis - iphone modelling
#------------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

#load data
load("output/iptesting.RDS")

##### GENERATE METRICS AND CONFUSION MATRIX ###################################
#Metrics
mets <- postResample(pred = iptesting$preds, obs = iptesting$iphonesentiment)
mets

#Confusion Matrix
confMat <- confusionMatrix(iptesting$preds, iptesting$iphonesentiment)
confMat

##### VISUALISE CONFUSION MATRIX ##############################################

#convert confusion matrix to data frame and plot
confMatrix <- confMat$table %>% as.data.frame()

g.confMatrix <- ggplot(confMatrix, aes(Reference, Prediction, fill = Freq)) +
        geom_tile(color="black") +
        theme_bw() + 
        coord_equal() +
        scale_fill_distiller(palette = "Blues", direction=1) +
        guides(fill=F) +
        labs(title = "Overpredicting Sentiment 4",
             subtitle = "Confusion Matrix: iPhone Sentiment") +
        geom_text(aes(label = Freq), color="black")
g.confMatrix

##### EXPLORING PREDICTED AS 1 ERRORS #########################################

#subset data
pred1Wrong <- iptesting %>% 
        filter(iphonesentiment != preds) %>% 
        filter(preds == "1")
pred1Wrong$colour <- ifelse(pred1Wrong$iphone == "0", "y", "n")

g.pred1Wrong <- ggplot(pred1Wrong, aes(iphone, fill = colour)) +
        geom_bar() +
        theme_hc() +
        theme(legend.position="none") +
        ggtitle("Most Errors Relate to Zero Mentions") +
        xlab("iPhone Mentions") +
        ylab("Count") +
        scale_fill_manual(values = c("#8E8E8E", "#15AAEA")) +
        theme(legend.position="none")
g.pred1Wrong


##### EXPLORING PREDICTED AS 4 ERRORS #########################################

#subset data
predCorrect <- iptesting %>% 
        filter(iphonesentiment == preds)

pred4Wrong <- iptesting %>% 
        filter(iphonesentiment != preds) %>% 
        filter(preds == "4")


#csubset numerical features per sentiment group
pred4WrongCM <- pred4Wrong %>% select(-iphonesentiment, -preds)

predCorrect1CM <- predCorrect %>% filter(iphonesentiment == "1") %>% 
        select(-iphonesentiment, -preds)
predCorrect2CM <- predCorrect %>% filter(iphonesentiment == "2") %>% 
        select(-iphonesentiment, -preds)
predCorrect3CM <- predCorrect %>% filter(iphonesentiment == "3") %>% 
        select(-iphonesentiment, -preds)
predCorrect4CM <- predCorrect %>% filter(iphonesentiment == "4") %>% 
        select(-iphonesentiment, -preds)


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
        annotate("text", x = "iphoneperunc", y = 2.4, 
                 label = "Incorrect Predictions of 4",
                 colour = "#FC440F",
                 size = 5) +
        
        annotate("text", x = "iphonecamneg", y = 2.4, 
                 label = "Correct Predictions of 4",
                 colour = "#15AAEA",
                 size = 5)

g.colMeansCollated.long

