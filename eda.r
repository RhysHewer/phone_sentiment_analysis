


##### INITIAL IPHONE EDA ######################################################

load("output/iphoneSmallMatrix.RDS")

#all features are integers. Dep Var (iphonesentiment) should be factor
str(iphoneSmallMatrix)
summary(iphoneSmallMatrix)

#Plotting distribution of dep variable
g.ipDepVar <- ggplot(iphoneSmallMatrix, aes(iphonesentiment %>% as.factor())) + 
        geom_bar() +
        bbc_style()
g.ipDepVar

#plotting distribution of other variables
numericVars <- iphoneSmallMatrix %>% select(-iphonesentiment)

g.numericVars <- ggplot(gather(numericVars), aes(value)) + 
        geom_histogram(bins = 10) + 
        facet_wrap(~key, scales = 'free_x')


##### PLOTTING CORRELATION ####################################################

corData <- iphoneSmallMatrix

corData <- cor(corData)
corrplot.mixed(corData)

##### REMOVE LOW INFO OBSERVATIONS ############################################

load("output/iphoneSmallMatrix.RDS")

lowInfo <- iphoneSmallMatrix[rowSums(iphoneSmallMatrix) > iphoneSmallMatrix$iphone,]

save(lowInfo, file = "output/lowInfo.RDS")

##### IPHONE ERROR ANALYSIS ###################################################

#load data
load("output/iptesting.RDS")
load("output/iphonemodel.RDS")

##### GENERATE METRICS AND CONFUSION MATRIX ###################################
#Metrics
mets <- postResample(pred = iptesting$preds, obs = iptesting$iphonesentiment)
mets

#Confusion Matrix
confMat <- confusionMatrix(iptesting$preds, iptesting$iphonesentiment)
confMat

#varImp from model - iphone most important
varImp(iphonemodel)

#plot iphone v sentiment & iphone v preds
g.ipSent <- ggplot(iptesting) +
        geom_point(aes(iphonesentiment, iphone), 
                   colour = "red", 
                   position  = "jitter", 
                   alpha = 0.2) +
        
        geom_point(aes(preds, iphone), 
                   colour = "blue", 
                   position  = "jitter", 
                   alpha = 0.2) +
        
        theme_hc() +
        #ggtitle("Highest Sentiment Over-represented") +
        xlab("iPhone Mentions") +
        ylab("Sentiment Quartile")
        
g.ipSent





