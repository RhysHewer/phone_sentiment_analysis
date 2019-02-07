#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1
# Purpose: comparing small & large matrix datasets
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")

##### GALAXY - COMPARING DATASETS #############################################
#load data/models
load("output/galtesting.RDS")
load("output/galLarge.RDS")
load("output/galData.RDS")
load("output/galaxymodel.RDS")

#subset features from top 10 varImp
galVarImp10 <- varImp(galaxymodel)
galVarImp10 <- galVarImp10$importance %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        head(10)

galFeat <- galVarImp10$rowname

#select & colmeans of top10 varImp features from small matrix
smFeat10 <- galData %>% select(galFeat)
ColMean10 <- colMeans(smFeat10) %>% 
        as.data.frame() %>% 
        rownames_to_column()
colnames(ColMean10) <- c("feature","smallMatrix")

#select & colmeans of top10 varImp features from large matrix  
lmFeat10 <- galLarge %>% select(galFeat)
ColMean10$largeMatrix <- colMeans(lmFeat10)

#make long and plot
ColMean10.long <- ColMean10 %>% 
        gather(-'feature', key = 'df', value = 'value')

g.ColMean10.long <- ggplot(ColMean10.long, 
                           aes(feature, value, colour = df, 
                               group = df, size = df)) +
        geom_line() +
        geom_point() +
        theme_hc() +
        ggtitle("Small Matrix and Large Matrix have different distributions") +
        xlab("Feature") +
        ylab("Mean Value") +
        labs(subtitle ="Galaxy Sentiment: Features ranked by varImp()") +
        
        theme(legend.position="top", legend.title = element_blank()) +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        scale_colour_manual(values = c("#1E1E1B", "#C1330B")) +
        
        scale_size_manual(values = c(1.5,1.5)) +
        scale_x_discrete(limits = galFeat) 
        

g.ColMean10.long


##### ALTERNATIVE COMPARISON ##################################################

#box plots of distributions
smFeat10$df <- "smallMatrix"
lmFeat10$df <- "largeMatrix"

comboFeat10 <- bind_rows(smFeat10, lmFeat10)
comboFeat10.long <- comboFeat10 %>% gather(-df, key = 'feature', value = 'value')
comboFeat10.long$value <- comboFeat10.long$value %>% log10()

g.comboFeat10.long <- ggplot(comboFeat10.long, aes(feature, value, fill = df)) +
        geom_boxplot(position = "dodge") +
        theme_hc() +
        scale_x_discrete(limits = galFeat) +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        scale_fill_manual(values = c("#15AAEA", "#FC440F")) +
        ggtitle("HTC mentions more frequent in Large Matrix") +
        labs(subtitle ="Galaxy sentiment: Features ranked by varImp()") +
        xlab("Feature") +
        ylab("log10 Mean Value") +
        theme(legend.position = "top", legend.title = element_blank())
g.comboFeat10.long

