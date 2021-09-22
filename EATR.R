## Load libraries

library(plyr)
library(dplyr)
library(data.table)
library(datasets)
library(igraph)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(graphlayouts)
library(RColorBrewer)
library(cluster)
library(rio)
library(stringr)
library(stringi)
library(qdap)
library(sqldf)
library(lubridate)
library(rlist)
library(purrr)
library(taRifx)
library(devtools)
library(splitstackshape)
library(pbapply)
library(maps)
library(geosphere)
library(ggplot2)
library(shiny)
library(magick)
library(reshape2)
library(ggallin)
library(Hmisc)
library(BSDA)
library(marima)
library(lmtest)
library(dynlm)
library(tseries)
library(XML)
library(xml2)
library(foreach)
library(doParallel)
library(snow)
library(doSNOW)
library(fuzzyjoin)
library(shadowtext)


## import orbis data


NI <- rio::import("ImportNI.xlsx", which = "Results")
NI <- cbind(data.frame("CompanyBVDID" = c(NI$`BvD ID number`)),NI[,4:13])

Depr <- rio::import("ImportDepr.xlsx", which = "Results")
Depr <- cbind(data.frame("CompanyBVDID" = c(Depr$`BvD ID number`)),Depr[,4:13])

Tax <- rio::import("ImportTax.xlsx", which = "Results")
Tax <- cbind(data.frame("CompanyBVDID" = c(Tax$`BvD ID number`)),Tax[,4:13])

WC <- rio::import("ImportWC.xlsx", which = "Results")
WC <- cbind(data.frame("CompanyBVDID" = c(WC$`BvD ID number`)),WC[,4:13])


BvDIDNorm <- intersect(NI$CompanyBVDID, Depr$CompanyBVDID)
BvDIDNorm2 <- intersect(Tax$CompanyBVDID, WC$CompanyBVDID)
BvDIDNorm <- intersect(BvDIDNorm, BvDIDNorm2)
rm(BvDIDNorm2)

BvDIDNorm <- as.data.frame(BvDIDNorm)
names(BvDIDNorm) <- "CompanyBVDID"


NI <- left_join(BvDIDNorm, NI, by = "CompanyBVDID")
Depr <- left_join(BvDIDNorm, Depr, by = "CompanyBVDID")
Tax <- left_join(BvDIDNorm, Tax, by = "CompanyBVDID")
WC <- left_join(BvDIDNorm, WC, by = "CompanyBVDID")




CWC <- sapply(1:(ncol(WC)-1), function(y) as.numeric(WC[,y]) - as.numeric(WC[,(y+1)]) )
CWC <- cbind(CWC,c(NA))
CWC <- as.data.frame(CWC)



CF <- NI
for(i in 1:nrow(CF)) {
  for (j in 2:ncol(CF)) {
  CF[i,j] <- ifelse(!is.na(as.numeric(NI[i,j])) & !is.na(as.numeric(Depr[i,j])) & !is.na(as.numeric(CWC[i,j])), as.numeric(NI[i,j]) + as.numeric(Depr[i,j]) + as.numeric(CWC[i,j]), NA)  
  }}




##equal out samples

for(i in 1:nrow(CF)) {
  for (j in 2:ncol(CF)) {
    CF[i,j] <- ifelse(!is.na(as.numeric(CF[i,j])) & !is.na(as.numeric(Tax[i,j])),  as.numeric(CF[i,j]) , NA )
  }
}


for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(CF[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}


## Drop if <3 obs


CF <- CF[apply(CF,1,function (z) length(z[!is.na(as.numeric(z))]) > 2),]
Tax <- Tax[apply(Tax,1,function (z) length(z[!is.na(as.numeric(z))]) > 2),]



#####

EATR.List <- vector(mode = "list")
EATR.List[[1]] <- vector(mode = "list")
names(EATR.List) <- "DeDom"

EATR.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {EATR.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(EATR.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2012)


for (i in 1:length(EATR.List[["DeDom"]][["CompanyList"]])) {
  EATR.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(EATR.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- EATR.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(EATR.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(EATR.List[["DeDom"]][["CompanyList"]][[i]])))
}




###


EATR.List[["DeDom"]][["EATRunweightedCF"]] <- subset(CF, CF$CompanyBVDID %in% Temp1)

for (i in 1:nrow(EATR.List[["DeDom"]][["EATRunweightedCF"]])) {
  for (j in 3:ncol(EATR.List[["DeDom"]][["EATRunweightedCF"]])) {
    EATR.List[["DeDom"]][["EATRunweightedCF"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["DeDom"]][["EATRunweightedCF"]][i,j])) & EATR.List[["DeDom"]][["EATRunweightedCF"]][i,1] %in% EATR.List[["DeDom"]][["CompanyList"]][[(j-2)]], EATR.List[["DeDom"]][["EATRunweightedCF"]][i,j], NA)
  }}


EATR.List[["DeDom"]][["EATRunweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)

for (i in 1:nrow(EATR.List[["DeDom"]][["EATRunweightedTax"]])) {
  for (j in 3:ncol(EATR.List[["DeDom"]][["EATRunweightedTax"]])) {
    EATR.List[["DeDom"]][["EATRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["DeDom"]][["EATRunweightedTax"]][i,j])) & EATR.List[["DeDom"]][["EATRunweightedTax"]][i,1] %in% EATR.List[["DeDom"]][["CompanyList"]][[(j-2)]], EATR.List[["DeDom"]][["EATRunweightedTax"]][i,j], NA)
  }}




HelpDisc <- EATR.List[["DeDom"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List[["DeDom"]][["EATRunweightedCF"]])) {
  HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))] <- 1.05^seq(length(HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))]),1)
}


EATR.List[["DeDom"]][["EATRunweightedNPVAT"]] <- EATR.List[["DeDom"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]])) {
  for(j in 2:ncol(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]])) {
  
  
    EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][i,j])),  (as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
  }}


EATR.List[["DeDom"]][["EATRunweightedNPVBT"]] <- EATR.List[["DeDom"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]])) {
  for(j in 2:ncol(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]])) {
    EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][i,j])),  (as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][i,j]) + as.numeric(EATR.List[["DeDom"]][["EATRunweightedTax"]][i,j])  ) / as.numeric(HelpDisc[i,j]) , NA)
  }}






EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum <- sapply(1:nrow(EATR.List[["DeDom"]][["EATRunweightedCF"]]), function (y) sum(as.numeric(EATR.List[["DeDom"]][["EATRunweightedCF"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum[EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum == 0] <- NA

EATR.List[["DeDom"]][["EATRunweightedTax"]]$sum <- sapply(1:nrow(EATR.List[["DeDom"]][["EATRunweightedTax"]]), function (y) sum(as.numeric(EATR.List[["DeDom"]][["EATRunweightedTax"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRunweightedTax"]]$sum[EATR.List[["DeDom"]][["EATRunweightedTax"]]$sum == 0] <- NA

EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum <- sapply(1:nrow(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]), function (y) sum(as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum[EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum == 0] <- NA

EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum <- sapply(1:nrow(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]), function (y) sum(as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum[EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum == 0] <- NA





EATR.List[["DeDom"]][["EATRunweightedNPVBT"]] <- EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][!EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List[["DeDom"]][["EATRunweightedNPVAT"]] <- EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][!EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List[["DeDom"]][["EATRunweightedTax"]] <- EATR.List[["DeDom"]][["EATRunweightedTax"]][!EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List[["DeDom"]][["EATRunweightedCF"]] <- EATR.List[["DeDom"]][["EATRunweightedCF"]][!EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]



EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR  <-  (1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum)



EATR.List[["DeDom"]][["EATRunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                         "EATR" = mean(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR, na.rm = TRUE), 
                                                         "sd" = sd(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR, na.rm = TRUE), 
                                                         "n" = nrow(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)),
                                                         "EATRNeg" = mean(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR, na.rm = TRUE), 
                                                         "sdNeg" = sd(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR, na.rm = TRUE), 
                                                         "nNeg" = nrow(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)))

EATR.List[["DeDom"]][["EATRunweighted"]]$low95 <- EATR.List[["DeDom"]][["EATRunweighted"]]$EATR - qt(0.975, df= EATR.List[["DeDom"]][["EATRunweighted"]]$n-1) * EATR.List[["DeDom"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["DeDom"]][["EATRunweighted"]]$n)
EATR.List[["DeDom"]][["EATRunweighted"]]$high95 <- EATR.List[["DeDom"]][["EATRunweighted"]]$EATR + qt(0.975, df= EATR.List[["DeDom"]][["EATRunweighted"]]$n-1) * EATR.List[["DeDom"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["DeDom"]][["EATRunweighted"]]$n)

EATR.List[["DeDom"]][["EATRunweighted"]] <- EATR.List[["DeDom"]][["EATRunweighted"]][!is.na(EATR.List[["DeDom"]][["EATRunweighted"]]$EATR),]





#Domestic firms weighted EATR


EATR.List[["DeDom"]][["EATRweighted"]] <- data.frame("ISO" = "DEDOM")
EATR.List[["DeDom"]][["EATRweighted"]]$EATR <- 1 - sum(subset(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]], 1 -  EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum >0)$sum / sum(subset(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]], 1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum >0 )$sum))
EATR.List[["DeDom"]][["EATRweighted"]]$sd <- sqrt(wtd.var(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR, sqrt(subset(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]], 1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum >0)$sum^2), na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRweighted"]]$n <- nrow(subset(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]], 1 -  EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum > 0))

EATR.List[["DeDom"]][["EATRweighted"]]$EATRNeg <- 1 - sum(subset(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]], 1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum <0)$sum / sum(subset(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]], 1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum <0 )$sum))
EATR.List[["DeDom"]][["EATRweighted"]]$sdNeg <- sqrt(wtd.var(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR, sqrt(subset(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]], 1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum <0)$sum^2), na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRweighted"]]$nNeg <- nrow(subset(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]], 1 -  EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum < 0))

EATR.List[["DeDom"]][["EATRweighted"]]$low95 <- EATR.List[["DeDom"]][["EATRweighted"]]$EATR - qt(0.975, df = EATR.List[["DeDom"]][["EATRweighted"]]$n-1) * EATR.List[["DeDom"]][["EATRweighted"]]$sd / sqrt(EATR.List[["DeDom"]][["EATRweighted"]]$n)
EATR.List[["DeDom"]][["EATRweighted"]]$high95 <- EATR.List[["DeDom"]][["EATRweighted"]]$EATR + qt(0.975, df = EATR.List[["DeDom"]][["EATRweighted"]]$n-1) * EATR.List[["DeDom"]][["EATRweighted"]]$sd / sqrt(EATR.List[["DeDom"]][["EATRweighted"]]$n)






## Affiliates




EATR.List[["Affiliates"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {EATR.List[["Affiliates"]][["CompanyList"]][[(i-1)]] <- unique(Reduce("rbind",EdgelistByanyown[[i]]))}

names(EATR.List[["Affiliates"]][["CompanyList"]]) <- paste(2020:2012)


for (i in 1:length(EATR.List[["Affiliates"]][["CompanyList"]])) {
  Temp1 <- apply(EATR.List[["Affiliates"]][["CompanyList"]][[i]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] != "DE")))
  Temp2 <- apply(EATR.List[["Affiliates"]][["CompanyList"]][[i]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
  Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
  
  Temp4 <- EATR.List[["Affiliates"]][["CompanyList"]][[i]]
  for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
  
  for (g in 1:nrow(Temp4)) {
    for(h in 1:ncol(Temp4)) {
      Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
    }
  }
  
  Temp4 <- sapply(Temp4, function (y) y == "TRUE")
  
  EATR.List[["Affiliates"]][["CompanyList"]][[i]] <- EATR.List[["Affiliates"]][["CompanyList"]][[i]][Temp4]
}



Temp1 <- EATR.List[["Affiliates"]][["CompanyList"]][[1]]
for(i in 2:length(EATR.List[["Affiliates"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(EATR.List[["Affiliates"]][["CompanyList"]][[i]])))
}




###


EATR.List[["Affiliates"]][["EATRunweightedCF"]] <- subset(CF, CF$CompanyBVDID %in% Temp1)

for (i in 1:nrow(EATR.List[["Affiliates"]][["EATRunweightedCF"]])) {
  for (j in 3:ncol(EATR.List[["Affiliates"]][["EATRunweightedCF"]])) {
    EATR.List[["Affiliates"]][["EATRunweightedCF"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Affiliates"]][["EATRunweightedCF"]][i,j])) & EATR.List[["Affiliates"]][["EATRunweightedCF"]][i,1] %in% EATR.List[["Affiliates"]][["CompanyList"]][[(j-2)]], EATR.List[["Affiliates"]][["EATRunweightedCF"]][i,j], NA)
  }}


EATR.List[["Affiliates"]][["EATRunweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)

for (i in 1:nrow(EATR.List[["Affiliates"]][["EATRunweightedTax"]])) {
  for (j in 3:ncol(EATR.List[["Affiliates"]][["EATRunweightedTax"]])) {
    EATR.List[["Affiliates"]][["EATRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Affiliates"]][["EATRunweightedTax"]][i,j])) & EATR.List[["Affiliates"]][["EATRunweightedTax"]][i,1] %in% EATR.List[["Affiliates"]][["CompanyList"]][[(j-2)]], EATR.List[["Affiliates"]][["EATRunweightedTax"]][i,j], NA)
  }}




HelpDisc <- EATR.List[["Affiliates"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List[["Affiliates"]][["EATRunweightedCF"]])) {
  HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))] <- 1.05^seq(length(HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))]),1)
}


EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]] <- EATR.List[["Affiliates"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]])) {
  for(j in 2:ncol(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]])) {
    
    
    EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]][i,j])),  (as.numeric(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
  }}


EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]] <- EATR.List[["Affiliates"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]])) {
  for(j in 2:ncol(EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]])) {
    EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]][i,j])),  (as.numeric(EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]][i,j]) + as.numeric(EATR.List[["Affiliates"]][["EATRunweightedTax"]][i,j])  ) / as.numeric(HelpDisc[i,j]) , NA)
  }}






EATR.List[["Affiliates"]][["EATRunweightedCF"]]$sum <- sapply(1:nrow(EATR.List[["Affiliates"]][["EATRunweightedCF"]]), function (y) sum(as.numeric(EATR.List[["Affiliates"]][["EATRunweightedCF"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Affiliates"]][["EATRunweightedCF"]]$sum[EATR.List[["Affiliates"]][["EATRunweightedCF"]]$sum == 0] <- NA

EATR.List[["Affiliates"]][["EATRunweightedTax"]]$sum <- sapply(1:nrow(EATR.List[["Affiliates"]][["EATRunweightedTax"]]), function (y) sum(as.numeric(EATR.List[["Affiliates"]][["EATRunweightedTax"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Affiliates"]][["EATRunweightedTax"]]$sum[EATR.List[["Affiliates"]][["EATRunweightedTax"]]$sum == 0] <- NA

EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum <- sapply(1:nrow(EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]), function (y) sum(as.numeric(EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum[EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum == 0] <- NA

EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum <- sapply(1:nrow(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]), function (y) sum(as.numeric(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum[EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum == 0] <- NA





EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]] <- EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]][!EATR.List[["Affiliates"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]] <- EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]][!EATR.List[["Affiliates"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List[["Affiliates"]][["EATRunweightedTax"]] <- EATR.List[["Affiliates"]][["EATRunweightedTax"]][!EATR.List[["Affiliates"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List[["Affiliates"]][["EATRunweightedCF"]] <- EATR.List[["Affiliates"]][["EATRunweightedCF"]][!EATR.List[["Affiliates"]][["EATRunweightedCF"]]$sum < 0,]


EATR.List[["Affiliates"]][["EATRunweightedTax"]]$EATR  <-  1 - EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum






EATR.List[["Affiliates"]][["EATRunweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                       "EATR" = mean(subset(EATR.List[["Affiliates"]][["EATRunweightedTax"]], EATR.List[["Affiliates"]][["EATRunweightedTax"]]$EATR > 0)$EATR, na.rm = TRUE), 
                                                       "sd" = sd(subset(EATR.List[["Affiliates"]][["EATRunweightedTax"]], EATR.List[["Affiliates"]][["EATRunweightedTax"]]$EATR > 0)$EATR, na.rm = TRUE), 
                                                       "n" = nrow(subset(EATR.List[["Affiliates"]][["EATRunweightedTax"]], EATR.List[["Affiliates"]][["EATRunweightedTax"]]$EATR > 0)),
                                                       "EATRNeg" = mean(subset(EATR.List[["Affiliates"]][["EATRunweightedTax"]], EATR.List[["Affiliates"]][["EATRunweightedTax"]]$EATR < 0)$EATR, na.rm = TRUE), 
                                                       "sdNeg" = sd(subset(EATR.List[["Affiliates"]][["EATRunweightedTax"]], EATR.List[["Affiliates"]][["EATRunweightedTax"]]$EATR < 0)$EATR, na.rm = TRUE), 
                                                       "nNeg" = nrow(subset(EATR.List[["Affiliates"]][["EATRunweightedTax"]], EATR.List[["Affiliates"]][["EATRunweightedTax"]]$EATR < 0)))

EATR.List[["Affiliates"]][["EATRunweighted"]]$low95 <- EATR.List[["Affiliates"]][["EATRunweighted"]]$EATR - qt(0.975, df= EATR.List[["Affiliates"]][["EATRunweighted"]]$n-1) * EATR.List[["Affiliates"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["Affiliates"]][["EATRunweighted"]]$n)
EATR.List[["Affiliates"]][["EATRunweighted"]]$high95 <- EATR.List[["Affiliates"]][["EATRunweighted"]]$EATR + qt(0.975, df= EATR.List[["Affiliates"]][["EATRunweighted"]]$n-1) * EATR.List[["Affiliates"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["Affiliates"]][["EATRunweighted"]]$n)

EATR.List[["Affiliates"]][["EATRunweighted"]] <- EATR.List[["Affiliates"]][["EATRunweighted"]][!is.na(EATR.List[["Affiliates"]][["EATRunweighted"]]$EATR),]





#Affiliates weighted EATR


EATR.List[["Affiliates"]][["EATRweighted"]] <- data.frame("ISO" = "Affiliates")
EATR.List[["Affiliates"]][["EATRweighted"]]$EATR <- 1 - sum(subset(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]], 1 -  EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum >0)$sum / sum(subset(EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]], 1 - EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum >0 )$sum))
EATR.List[["Affiliates"]][["EATRweighted"]]$sd <- sqrt(wtd.var(subset(EATR.List[["Affiliates"]][["EATRunweightedTax"]], EATR.List[["Affiliates"]][["EATRunweightedTax"]]$EATR > 0)$EATR, sqrt(subset(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]], 1 - EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum >0)$sum^2), na.rm = TRUE ))
EATR.List[["Affiliates"]][["EATRweighted"]]$n <- nrow(subset(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]], 1 -  EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum > 0))

EATR.List[["Affiliates"]][["EATRweighted"]]$EATRNeg <- 1 - sum(subset(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]], 1 - EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum <0)$sum / sum(subset(EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]], 1 - EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum <0 )$sum))
EATR.List[["Affiliates"]][["EATRweighted"]]$sdNeg <- sqrt(wtd.var(subset(EATR.List[["Affiliates"]][["EATRunweightedTax"]], EATR.List[["Affiliates"]][["EATRunweightedTax"]]$EATR < 0)$EATR, sqrt(subset(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]], 1 - EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum <0)$sum^2), na.rm = TRUE ))
EATR.List[["Affiliates"]][["EATRweighted"]]$nNeg <- nrow(subset(EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]], 1 -  EATR.List[["Affiliates"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["Affiliates"]][["EATRunweightedNPVBT"]]$sum < 0))

EATR.List[["Affiliates"]][["EATRweighted"]]$low95 <- EATR.List[["Affiliates"]][["EATRweighted"]]$EATR - qt(0.975, df = EATR.List[["Affiliates"]][["EATRweighted"]]$n-1) * EATR.List[["Affiliates"]][["EATRweighted"]]$sd / sqrt(EATR.List[["Affiliates"]][["EATRweighted"]]$n)
EATR.List[["Affiliates"]][["EATRweighted"]]$high95 <- EATR.List[["Affiliates"]][["EATRweighted"]]$EATR + qt(0.975, df = EATR.List[["Affiliates"]][["EATRweighted"]]$n-1) * EATR.List[["Affiliates"]][["EATRweighted"]]$sd / sqrt(EATR.List[["Affiliates"]][["EATRweighted"]]$n)







## anyown





EATR.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {EATR.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(EATR.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2012)

for(i in 1:length(EATR.List[["Byanyown"]][["CompanyList"]])) {EATR.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(EATR.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(EATR.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(EATR.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}




EATR.List[["Byanyown"]][["EATRunweightedCF"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List[["Byanyown"]][["EATRunweightedCF"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List[["Byanyown"]][["EATRunweightedCF"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List[["Byanyown"]][["EATRunweightedCF"]])) {
  Temp1 <- EATR.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List[["Byanyown"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,EATR.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List[["Byanyown"]][["EATRunweightedCF"]][[i]] <- subset(CF, CF$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[i]]) == 0 ) {EATR.List[["Byanyown"]][["EATRunweightedCF"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List[["Byanyown"]][["EATRunweightedCF"]])) {
  for (i in 1:nrow(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]])) {
    for (j in 3:ncol(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]])) {
      EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][i,j])) & EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][i,1] %in% EATR.List[["Byanyown"]][["CompanyList"]][[(j-2)]][[x]], EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][i,j], NA)
    }}}



EATR.List[["Byanyown"]][["EATRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List[["Byanyown"]][["EATRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]])) {
  Temp1 <- EATR.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List[["Byanyown"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,EATR.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List[["Byanyown"]][["EATRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[i]]) == 0 ) {EATR.List[["Byanyown"]][["EATRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]])) {
  for (i in 1:nrow(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]])) {
    for (j in 3:ncol(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]])) {
      EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j])) & EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][i,1] %in% EATR.List[["Byanyown"]][["CompanyList"]][[(j-2)]][[x]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j], NA)
    }}}





for(x in 1:length(EATR.List[["Byanyown"]][["EATRunweightedCF"]])) {


HelpDisc <- EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]
for(i in 1:nrow(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]])) {
  HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))] <- 1.05^seq(length(HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))]),1)
}


EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]

for(i in 1:nrow(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]])) {
  for(j in 2:ncol(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]])) {
    EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][i,j])),  (as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
  }}



EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]

for(i in 1:nrow(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]])) {
  for(j in 2:ncol(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]])) {
    EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][i,j])),  (as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][i,j]) + as.numeric(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j])) / as.numeric(HelpDisc[i,j]) , NA)
  }}


print(paste0(x,"/",length(EATR.List[["Byanyown"]][["EATRunweightedCF"]])))

}

names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) <- names(EATR.List[["Byanyown"]][["EATRunweightedCF"]])
names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) <- names(EATR.List[["Byanyown"]][["EATRunweightedCF"]])



for(x in 1:length(EATR.List[["Byanyown"]][["EATRunweightedCF"]])) {



EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum[EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum == 0] <- NA

EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$sum[EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$sum == 0] <- NA

EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum[EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum == 0] <- NA

EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum[EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum == 0] <- NA





EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]
EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]
EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]
EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]



EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR  <- 1 - EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum

}



EATR.List[["Byanyown"]][["EATRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                       "EATR" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                       "sd" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                       "n" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) nrow(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)))),
                                                       "EATRNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                       "sdNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                       "nNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) nrow(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)))))


EATR.List[["Byanyown"]][["EATRunweighted"]]$low95 <- EATR.List[["Byanyown"]][["EATRunweighted"]]$EATR - qt(0.975, df= EATR.List[["Byanyown"]][["EATRunweighted"]]$n-1) * EATR.List[["Byanyown"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["Byanyown"]][["EATRunweighted"]]$n)
EATR.List[["Byanyown"]][["EATRunweighted"]]$high95 <- EATR.List[["Byanyown"]][["EATRunweighted"]]$EATR + qt(0.975, df= EATR.List[["Byanyown"]][["EATRunweighted"]]$n-1) * EATR.List[["Byanyown"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["Byanyown"]][["EATRunweighted"]]$n)

EATR.List[["Byanyown"]][["EATRunweighted"]] <- EATR.List[["Byanyown"]][["EATRunweighted"]][!is.na(EATR.List[["Byanyown"]][["EATRunweighted"]]$EATR) | !is.na(EATR.List[["Byanyown"]][["EATRunweighted"]]$EATRNeg),]





#anyown  weighted EATR


EATR.List[["Byanyown"]][["EATRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                          "EATR" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum / sum(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum >0 )$sum)))),
                                                          "sd" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, sqrt(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum^2), na.rm = TRUE )))),
                                                          "n" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) nrow(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum > 0)))),
                                                          "EATRNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum / sum(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum <0 )$sum)))),
                                                          "sdNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, sqrt(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum^2), na.rm = TRUE )))),
                                                          "nNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) nrow(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum < 0))))
)


EATR.List[["Byanyown"]][["EATRweighted"]]$low95 <- EATR.List[["Byanyown"]][["EATRweighted"]]$EATR - qt(0.975, df= EATR.List[["Byanyown"]][["EATRweighted"]]$n-1) * EATR.List[["Byanyown"]][["EATRweighted"]]$sd /sqrt(EATR.List[["Byanyown"]][["EATRweighted"]]$n)
EATR.List[["Byanyown"]][["EATRweighted"]]$high95 <- EATR.List[["Byanyown"]][["EATRweighted"]]$EATR + qt(0.975, df= EATR.List[["Byanyown"]][["EATRweighted"]]$n-1) * EATR.List[["Byanyown"]][["EATRweighted"]]$sd /sqrt(EATR.List[["Byanyown"]][["EATRweighted"]]$n)

EATR.List[["Byanyown"]][["EATRweighted"]]$EATR[EATR.List[["Byanyown"]][["EATRweighted"]]$EATR == 1] <- NA
EATR.List[["Byanyown"]][["EATRweighted"]]$EATRNeg[EATR.List[["Byanyown"]][["EATRweighted"]]$EATRNeg == 1] <- NA

EATR.List[["Byanyown"]][["EATRweighted"]] <- EATR.List[["Byanyown"]][["EATRweighted"]][!is.na(EATR.List[["Byanyown"]][["EATRweighted"]]$EATR) | !is.na(EATR.List[["Byanyown"]][["EATRweighted"]]$EATRNeg),]







## intermed


EATR.List[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {EATR.List[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(EATR.List[["Byintermed"]][["CompanyList"]]) <- paste(2020:2012)

for(i in 1:length(EATR.List[["Byintermed"]][["CompanyList"]])) {EATR.List[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(EATR.List[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(EATR.List[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(EATR.List[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}




EATR.List[["Byintermed"]][["EATRunweightedCF"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List[["Byintermed"]][["EATRunweightedCF"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List[["Byintermed"]][["EATRunweightedCF"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List[["Byintermed"]][["EATRunweightedCF"]])) {
  Temp1 <- EATR.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List[["Byintermed"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,EATR.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List[["Byintermed"]][["EATRunweightedCF"]][[i]] <- subset(CF, CF$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[i]]) == 0 ) {EATR.List[["Byintermed"]][["EATRunweightedCF"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List[["Byintermed"]][["EATRunweightedCF"]])) {
  for (i in 1:nrow(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]])) {
    for (j in 3:ncol(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]])) {
      EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][i,j])) & EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][i,1] %in% EATR.List[["Byintermed"]][["CompanyList"]][[(j-2)]][[x]], EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][i,j], NA)
    }}}



EATR.List[["Byintermed"]][["EATRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List[["Byintermed"]][["EATRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]])) {
  Temp1 <- EATR.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List[["Byintermed"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,EATR.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[i]]) == 0 ) {EATR.List[["Byintermed"]][["EATRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]])) {
  for (i in 1:nrow(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]])) {
    for (j in 3:ncol(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]])) {
      EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j])) & EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][i,1] %in% EATR.List[["Byintermed"]][["CompanyList"]][[(j-2)]][[x]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j], NA)
    }}}





for(x in 1:length(EATR.List[["Byintermed"]][["EATRunweightedCF"]])) {
  
  
  HelpDisc <- EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]
  for(i in 1:nrow(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]])) {
    HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))] <- 1.05^seq(length(HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))]),1)
  }
  
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]
  
  for(i in 1:nrow(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]])) {
    for(j in 2:ncol(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]])) {
      EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][i,j])),  (as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
    }}
  
  
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]
  
  for(i in 1:nrow(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]])) {
    for(j in 2:ncol(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]])) {
      EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][i,j])),  (as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][i,j]) + as.numeric(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j])) / as.numeric(HelpDisc[i,j]) , NA)
    }}
  
  
  print(paste0(x,"/",length(EATR.List[["Byintermed"]][["EATRunweightedCF"]])))
  
}

names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) <- names(EATR.List[["Byintermed"]][["EATRunweightedCF"]])
names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) <- names(EATR.List[["Byintermed"]][["EATRunweightedCF"]])



for(x in 1:length(EATR.List[["Byintermed"]][["EATRunweightedCF"]])) {
  
  
  
  EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum[EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum == 0] <- NA
  
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$sum[EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$sum == 0] <- NA
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum[EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum == 0] <- NA
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum[EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum == 0] <- NA
  
  
  
  
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  
  
  
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR  <- 1 - EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum
  
}



EATR.List[["Byintermed"]][["EATRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                          "EATR" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                          "sd" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                          "n" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) nrow(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)))),
                                                          "EATRNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                          "sdNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                          "nNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) nrow(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)))))


EATR.List[["Byintermed"]][["EATRunweighted"]]$low95 <- EATR.List[["Byintermed"]][["EATRunweighted"]]$EATR - qt(0.975, df= EATR.List[["Byintermed"]][["EATRunweighted"]]$n-1) * EATR.List[["Byintermed"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["Byintermed"]][["EATRunweighted"]]$n)
EATR.List[["Byintermed"]][["EATRunweighted"]]$high95 <- EATR.List[["Byintermed"]][["EATRunweighted"]]$EATR + qt(0.975, df= EATR.List[["Byintermed"]][["EATRunweighted"]]$n-1) * EATR.List[["Byintermed"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["Byintermed"]][["EATRunweighted"]]$n)

EATR.List[["Byintermed"]][["EATRunweighted"]] <- EATR.List[["Byintermed"]][["EATRunweighted"]][!is.na(EATR.List[["Byintermed"]][["EATRunweighted"]]$EATR) | !is.na(EATR.List[["Byintermed"]][["EATRunweighted"]]$EATRNeg),]





#intermed  weighted EATR


EATR.List[["Byintermed"]][["EATRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                        "EATR" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum / sum(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum >0 )$sum)))),
                                                        "sd" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, sqrt(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum^2), na.rm = TRUE )))),
                                                        "n" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) nrow(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum > 0)))),
                                                        "EATRNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum / sum(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum <0 )$sum)))),
                                                        "sdNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, sqrt(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum^2), na.rm = TRUE )))),
                                                        "nNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) nrow(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum < 0))))
)


EATR.List[["Byintermed"]][["EATRweighted"]]$low95 <- EATR.List[["Byintermed"]][["EATRweighted"]]$EATR - qt(0.975, df= EATR.List[["Byintermed"]][["EATRweighted"]]$n-1) * EATR.List[["Byintermed"]][["EATRweighted"]]$sd /sqrt(EATR.List[["Byintermed"]][["EATRweighted"]]$n)
EATR.List[["Byintermed"]][["EATRweighted"]]$high95 <- EATR.List[["Byintermed"]][["EATRweighted"]]$EATR + qt(0.975, df= EATR.List[["Byintermed"]][["EATRweighted"]]$n-1) * EATR.List[["Byintermed"]][["EATRweighted"]]$sd /sqrt(EATR.List[["Byintermed"]][["EATRweighted"]]$n)

EATR.List[["Byintermed"]][["EATRweighted"]]$EATR[EATR.List[["Byintermed"]][["EATRweighted"]]$EATR == 1] <- NA
EATR.List[["Byintermed"]][["EATRweighted"]]$EATRNeg[EATR.List[["Byintermed"]][["EATRweighted"]]$EATRNeg == 1] <- NA

EATR.List[["Byintermed"]][["EATRweighted"]] <- EATR.List[["Byintermed"]][["EATRweighted"]][!is.na(EATR.List[["Byintermed"]][["EATRweighted"]]$EATR) | !is.na(EATR.List[["Byintermed"]][["EATRweighted"]]$EATRNeg),]









## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown


EATR.List[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRunweighted"]], 
                                                     
                                                     data.frame("ISO" = c("Sinks"), 
                                                                "EATR" = mean(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "sd" = sd(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "n" = nrow(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)),
                                                                "EATRNeg" = mean(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "sdNeg" = sd(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "nNeg" = nrow(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



EATR.List[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRunweighted"]], 
                                                     
                                                     data.frame("ISO" = c("Conduits"), 
                                                                "EATR" = mean(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "sd" = sd(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "n" = nrow(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)),
                                                                "EATRNeg" = mean(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "sdNeg" = sd(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "nNeg" = nrow(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



EATR.List[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRunweighted"]], 
                                                     
                                                     data.frame("ISO" = c("ConduitsProxy"), 
                                                                "EATR" = mean(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "sd" = sd(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "n" = nrow(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)),
                                                                "EATRNeg" = mean(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "sdNeg" = sd(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "nNeg" = nrow(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



EATR.List[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRunweighted"]],
                                                    EATR.List[["DeDom"]][["EATRunweighted"]],
                                                    EATR.List[["Affiliates"]][["EATRunweighted"]]
)










## weighted


EATR.List[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRweighted"]], 
                                                   
                                                   data.frame("ISO" = c("Sinks"), 
                                                              "EATR" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum >0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum >0 )$sum)),
                                                              "sd"   = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum >0)$sum^2), na.rm = TRUE )),
                                                              "n"    = nrow(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum > 0)),
                                                              "EATRNeg" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum <0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum <0 )$sum)),
                                                              "sdNeg"   = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum <0)$sum^2), na.rm = TRUE )),
                                                              "nNeg"    = nrow(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum < 0)),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))




EATR.List[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRweighted"]], 
                                                   
                                                   data.frame("ISO" = c("Conduits"), 
                                                              "EATR" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0 )$sum)),
                                                              "sd"   = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum^2), na.rm = TRUE )),
                                                              "n"    = nrow(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum > 0)),
                                                              "EATRNeg" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0 )$sum)),
                                                              "sdNeg"   = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum^2), na.rm = TRUE )),
                                                              "nNeg"    = nrow(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum < 0)),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))





EATR.List[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRweighted"]], 
                                                   
                                                   data.frame("ISO" = c("ConduitsProxy"), 
                                                              "EATR" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0 )$sum)),
                                                              "sd"   = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum^2), na.rm = TRUE )),
                                                              "n"    = nrow(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum > 0)),
                                                              "EATRNeg" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0 )$sum)),
                                                              "sdNeg"   = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum^2), na.rm = TRUE )),
                                                              "nNeg"    = nrow(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum < 0)),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))




EATR.List[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRweighted"]],
                                                     EATR.List[["DeDom"]][["EATRweighted"]],
                                                   EATR.List[["Affiliates"]][["EATRweighted"]]
)








for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, ETR2, g, h, i, ISO, j, x, y , z)





