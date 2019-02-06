#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1
# Purpose: visualising sentiment distribution across datasets
#------------------------------------------------------------------------------#


##### VISUALISING SENTIMENT DISTRIBUTION - IPHONE #############################

#load Iphone data
load("output/iptesting.RDS")
load("output/ipLarge.RDS")

#relative % calculations
ipOrigSent <- iptesting %>% 
        group_by(iphonesentiment) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
ipOrigSent$df <- "small matrix"
ipOrigSent$sent <- ipOrigSent$iphonesentiment
ipOrigSent$iphonesentiment <- NULL

ipTestSent <- iptesting %>% 
        group_by(preds) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
ipTestSent$df <- "test set"
ipTestSent$sent <- ipTestSent$preds
ipTestSent$preds <- NULL

ipLargeSent <- ipLarge %>% 
        group_by(ipSentPreds) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
ipLargeSent$df <- "large matrix"
ipLargeSent$sent <- ipLargeSent$ipSentPreds
ipLargeSent$ipSentPreds <- NULL

#Combining to a dataframe
ipSentDist <- bind_rows(ipOrigSent, ipTestSent, ipLargeSent)

#Visualising relative distributions
g.ipSentDist <- ggplot(ipSentDist, aes(sent, perSent, fill = df)) + 
        geom_col(position = "dodge") +
        theme_hc() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        ggtitle("Highest Sentiment Over-represented") +
        xlab("Sentiment Quartile") +
        ylab("Percent") +
        scale_fill_manual(values = c("#FC440F", "#8E8E8E","#15AAEA"))+
        geom_text(aes(label = perSent %>% round(2)),
                  position=position_dodge(width=0.9), 
                  vjust=-0.25)
g.ipSentDist


##### VISUALISING SENTIMENT DISTRIBUTION - GALAXY #############################

#load data
load("output/galtesting.RDS")
load("output/galLarge.RDS")

#relative % calculations
galOrigSent <- galtesting %>% 
        group_by(galaxysentiment) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
galOrigSent$df <- "small matrix"
galOrigSent$sent <- galOrigSent$galaxysentiment
galOrigSent$galaxysentiment <- NULL

galTestSent <- galtesting %>% 
        group_by(preds) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
galTestSent$df <- "test set"
galTestSent$sent <- galTestSent$preds
galTestSent$preds <- NULL

galLargeSent <- galLarge %>% 
        group_by(galSentPreds) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
galLargeSent$df <- "large matrix"
galLargeSent$sent <- galLargeSent$galSentPreds
galLargeSent$galSentPreds <- NULL

#Combining to a dataframe
galSentDist <- bind_rows(galOrigSent, galTestSent, galLargeSent)

#Visualising relative distributions
g.galSentDist <- ggplot(galSentDist, aes(sent, perSent, fill = df)) + 
        geom_col(position = "dodge") +
        theme_hc() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        ggtitle("Large Difference Between Small and Large Matrix Data") +
        xlab("Sentiment Quartile") +
        ylab("Percent") +
        scale_fill_manual(values = c("#FC440F", "#8E8E8E","#15AAEA"))+
        geom_text(aes(label = perSent %>% round(2)),
                  position=position_dodge(width=0.9), 
                  vjust=-0.25)
g.galSentDist


