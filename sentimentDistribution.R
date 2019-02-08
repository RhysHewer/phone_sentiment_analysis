#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Big Data - sentiment analysis
# Version: 1
# Purpose: visualising and comparing sentiment distribution across datasets
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")

#load data
load("output/galtesting.RDS")
load("output/galLarge.RDS")
load("output/galData.RDS")

#load Iphone data
load("output/iptesting.RDS")
load("output/ipLarge.RDS")
load("output/ipData.RDS")

##### VISUALISING SENTIMENT DISTRIBUTION - IPHONE #############################

#relative % calculations
ipOrigSent <- iptesting %>% 
        group_by(iphonesentiment) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
ipOrigSent$df <- "small matrix"
ipOrigSent$sent <- ipOrigSent$iphonesentiment
ipOrigSent$iphonesentiment <- NULL

ipLargeSent <- ipLarge %>% 
        group_by(ipSentPreds) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
ipLargeSent$df <- "large matrix"
ipLargeSent$sent <- ipLargeSent$ipSentPreds
ipLargeSent$ipSentPreds <- NULL

#Combining to a dataframe
ipSentDist <- bind_rows(ipOrigSent, ipLargeSent)

#Visualising relative distributions
g.ipSentDist <- ggplot(ipSentDist, aes(sent, perSent, fill = df)) + 
        geom_col(position = "dodge") +
        theme_hc() +
        theme(legend.position = "top", legend.title = element_blank()) +
        ggtitle("Highest Sentiment Over-represented") +
        labs(subtitle = "iPhone sentiment distribution across datasets") +
        xlab("Sentiment Quartile") +
        ylab("Percent") +
        scale_fill_manual(values = c("#8E8E8E","#143B66"))+
        geom_text(aes(label = perSent %>% round(2)),
                  position=position_dodge(width = 0.9), 
                  vjust = -0.25)
g.ipSentDist


##### VISUALISING SENTIMENT DISTRIBUTION - GALAXY #############################



#relative % calculations
galOrigSent <- galtesting %>% 
        group_by(galaxysentiment) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
galOrigSent$df <- "small matrix"
galOrigSent$sent <- galOrigSent$galaxysentiment
galOrigSent$galaxysentiment <- NULL


galLargeSent <- galLarge %>% 
        group_by(galSentPreds) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
galLargeSent$df <- "large matrix"
galLargeSent$sent <- galLargeSent$galSentPreds
galLargeSent$galSentPreds <- NULL

#Combining to a dataframe
galSentDist <- bind_rows(galOrigSent, galLargeSent)

#Visualising relative distributions
g.galSentDist <- ggplot(galSentDist, aes(sent, perSent, fill = df)) + 
        geom_col(position = "dodge") +
        theme_hc() +
        theme(legend.position = "top", legend.title = element_blank()) +
        ggtitle("Large Difference Between Small and Large Matrix Data") +
        labs(subtitle = "Galaxy sentiment distribution across datasets") +
        xlab("Sentiment Quartile") +
        ylab("Percent") +
        scale_fill_manual(values = c("#1E1E1B", "#C1330B"))+
        geom_text(aes(label = perSent %>% round(2)),
                  position  = position_dodge(width = 0.9), 
                  vjust = -0.25)
g.galSentDist



##### AVG SENTIMENT COMBINED ################################################## 

#mean sentiment for each phone
numSentIp <- ipLarge$ipSentPreds %>% 
        as.integer() %>% 
        mean() %>% round(2)

numSentGal <- galLarge$galSentPreds %>% 
        as.integer() %>% 
        mean() %>% round(2)

#statistical significance between means (p < 0.5 = significant)
statSigIp <- ipLarge$ipSentPreds %>% 
        as.integer()

statSigGal <- galLarge$galSentPreds %>% 
        as.integer()

statSig <- t.test(statSigIp, statSigGal)
statSig

#combine to dataframe and plot
numSent <- c(numSentIp, numSentGal)
phoneType <- c("iPhone", "Galaxy")

meanSent <- data.frame(phoneType, numSent)

g.meanSent <- ggplot(meanSent, aes(phoneType, numSent, fill = phoneType)) +
        geom_col() +
        theme_hc() +
        theme(legend.position="none") +
        
        ggtitle("iPhone has more positive average sentiment") +
        xlab("Phone") +
        ylab("Mean Sentiment") +
        labs(subtitle ="Mean sentiment by phone type") +
        
        scale_fill_manual(values = c("#FC440F", "#15AAEA")) +
        geom_text(aes(label = numSent), vjust = -0.25)

g.meanSent

##### COMBINED SENTIMENT CATEGORIES ###########################################

#Extract % for each sentiment level, each phone and combine to dataframe
groupSentIp <- ipLarge %>% 
        group_by(ipSentPreds) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
groupSentIp$phone <- "iPhone"
groupSentIp <- groupSentIp %>% rename(sentiment = ipSentPreds)

groupSentGal <- galLarge %>% 
        group_by(galSentPreds) %>% 
        summarise(n = n()) %>% 
        mutate(perSent = n / sum(n)*100)
groupSentGal$phone <- "Galaxy"
groupSentGal <- groupSentGal %>% rename(sentiment = galSentPreds)

groupSent <- bind_rows(groupSentIp, groupSentGal)

#Plot sentiment levels for each phone

g.groupSent <- ggplot(groupSent, aes(sentiment, perSent, fill = phone)) +
        geom_col(position = "dodge") +
        theme_hc() +
        theme(legend.position = "top", legend.title = element_blank()) +
        
        ggtitle("Sentiment generally polarises positive or negative") +
        xlab("Sentiment Level") +
        ylab("% Websites") +
        labs(subtitle ="Sentiment level distribution per phone type") +
        
        scale_fill_manual(values = c("#FC440F", "#15AAEA")) +
        geom_text(aes(label = perSent %>% round(2)),
                  position  = position_dodge(width = 0.9), 
                  vjust = -0.25)
        
g.groupSent

##### Polarisation percentages ################################################

polSentIp <- ipLarge$ipSentPreds
polSentGal <- galLarge$galSentPreds

polSent <- c(polSentIp, polSentGal)

polSentPercent <- prop.table(table(polSent)) *100 

polSentMid <- polSentPercent[2] + polSentPercent[3]
polSentMid %>% round(2)

##### distribution Pos/Neg mentions ###########################################

avgPosSentIp <- median(ipLarge$iphonePos)
avgPosSentGal <- median(galLarge$galaxyPos)
avgNegSentIp <- median(ipLarge$iphoneNeg)
avgNegSentGal <- median(galLarge$galaxyNeg)

##### Information Sparseness ##################################################

#Calculate sparseness per dataset 
#(sparseness = zero info beyond smartphone mention)
sparseIpLm <- ipLarge %>% 
        select(-iphone, -id, -ipSentPreds) %>% 
        rowSums() %>% 
        table() %>% 
        prop.table() *100
sparseIpLm <- sparseIpLm[[1]]
sparseIpLm

sparseIpSm <- ipData %>% 
        select(-iphone, -iphonesentiment) %>% 
        rowSums() %>% 
        table() %>% 
        prop.table() *100
sparseIpSm <- sparseIpSm[[1]]
sparseIpSm

sparseGalLm <- galLarge %>% 
        select(-samsunggalaxy, -id, -galSentPreds) %>% 
        rowSums() %>% 
        table() %>% 
        prop.table() *100
sparseGalLm <- sparseGalLm[[1]]
sparseGalLm

sparseGalSm <- galData %>% 
        select(-samsunggalaxy, -galaxysentiment, -iphone) %>% 
        rowSums() %>% 
        table() %>% 
        prop.table() *100
sparseGalSm <- sparseGalSm[[1]]
sparseGalSm

#Combine and plot
sparsePercent <- c(sparseIpLm, sparseIpSm, sparseGalLm, 0)
sparseDf <- c("iPhone Large Matrix", "iPhone Small Matrix", 
              "Galaxy Large Matrix", "Galaxy Small Matrix")
sparseData <- data.frame(sparseDf, sparsePercent)

g.sparseData <- ggplot(sparseData, aes(sparseDf, sparsePercent, 
                                       fill = sparseDf)) +
        geom_col() +
        theme_hc() +
        theme(legend.position = "none") +
        
        ggtitle("Substantial Sparseness of Data") +
        xlab("Dataset") +
        ylab("% Sparseness") +
        labs(subtitle ="Sparseness of data per dataset") +
        
        scale_fill_manual(values = c("#FC440F", "#FC440F", 
                                     "#15AAEA", "#15AAEA")) +
        geom_text(aes(label = sparsePercent %>% round(2)),
                  position  = position_dodge(width = 0.9), 
                  vjust = -0.25) +
        theme(axis.text.x=element_text(angle= 30, hjust=1))
g.sparseData

