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


EBIT <- rio::import("ImportEBIT.xlsx", which = "Results")
EBIT <- cbind(data.frame("CompanyBVDID" = c(EBIT$`BvD ID number`)),EBIT[,4:13])
EBIT <- full_join(data.frame("CompanyBVDID" = EBIT$CompanyBVDID), EBIT, by = "CompanyBVDID")


Tax <- rio::import("ImportTax.xlsx", which = "Results")
Tax <- cbind(data.frame("CompanyBVDID" = c(Tax$`BvD ID number`)),Tax[,4:13])
Tax <- full_join(data.frame("CompanyBVDID" = EBIT$CompanyBVDID), Tax, by = "CompanyBVDID")



##equal out samples

for(i in 1:nrow(EBIT)) {
  for (j in 2:ncol(EBIT)) {
    EBIT[i,j] <- ifelse(!is.na(as.numeric(EBIT[i,j])) & !is.na(as.numeric(Tax[i,j])),  as.numeric(EBIT[i,j]) , NA )
  }
}


for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(EBIT[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}



## Drop company if losses in 2012

EBIT <- EBIT[EBIT[,11] > 0 | is.na(EBIT[,11]),]
EBIT <- EBIT[!is.na(EBIT$CompanyBVDID),]
Tax <- Tax[Tax$CompanyBVDID %in% EBIT$CompanyBVDID,]


## Keep 2013 - 2020

EBIT[,11] <- NA
Tax[,11] <- NA


EBIT[,2] <- NA
Tax[,2] <- NA


## Drop last year if negative profits


EBIT[,3][EBIT[,3] < 0] <- NA



for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(EBIT[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}


## Drop if <3 obs


EBIT <- EBIT[apply(EBIT,1,function (z) length(z[!is.na(as.numeric(z))]) > 2),]
Tax <- Tax[Tax$CompanyBVDID %in% EBIT$CompanyBVDID,]








ETR2.List <- vector(mode = "list")
ETR2.List[[1]] <- vector(mode = "list")
names(ETR2.List) <- "ByCSH"


#Domestic firms unweightet ETR2


ETR2.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {ETR2.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(ETR2.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2012)


for (i in 1:length(ETR2.List[["DeDom"]][["CompanyList"]])) {
  ETR2.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR2.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR2.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR2.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR2.List[["DeDom"]][["CompanyList"]][[i]])))
}

ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]])) {
  for (j in 3:ncol(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]])) {
    ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][i,j])) & ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][i,1] %in% ETR2.List[["DeDom"]][["CompanyList"]][[(j-2)]], ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][i,j], NA)
  }}



for (i in 1:length(ETR2.List[["DeDom"]][["CompanyList"]])) {
  ETR2.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR2.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR2.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR2.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR2.List[["DeDom"]][["CompanyList"]][[i]])))
}


ETR2.List[["DeDom"]][["ETR2unweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR2.List[["DeDom"]][["ETR2unweightedTax"]])) {
  for (j in 3:ncol(ETR2.List[["DeDom"]][["ETR2unweightedTax"]])) {
    ETR2.List[["DeDom"]][["ETR2unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedTax"]][i,j])) & ETR2.List[["DeDom"]][["ETR2unweightedTax"]][i,1] %in% ETR2.List[["DeDom"]][["CompanyList"]][[(j-2)]], ETR2.List[["DeDom"]][["ETR2unweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]])) {
  for(j in 2:ncol(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]])) {
    ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][i,j])) & !is.na(as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedTax"]][i,j])) ,  as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR2.List[["DeDom"]][["ETR2unweightedTax"]])) {
  for(j in 2:ncol(ETR2.List[["DeDom"]][["ETR2unweightedTax"]])) {
    ETR2.List[["DeDom"]][["ETR2unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][i,j])) & !is.na(as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedTax"]][i,j])) ,  as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedTax"]][i,j])  , NA  )
  }}



ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]]$sum <- sapply(1:nrow(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]]), function (y) sum(as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][y,2:11]) , na.rm = TRUE ))
ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]]$sum[ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]]$sum == 0] <- NA
ETR2.List[["DeDom"]][["ETR2unweightedTax"]]$sum <- sapply(1:nrow(ETR2.List[["DeDom"]][["ETR2unweightedTax"]]), function (y) sum(as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedTax"]][y,2:11]), na.rm = TRUE))
ETR2.List[["DeDom"]][["ETR2unweightedTax"]]$sum[ETR2.List[["DeDom"]][["ETR2unweightedTax"]]$sum == 0] <- NA
ETR2.List[["DeDom"]][["ETR2unweightedTax"]]$ETR2 <- sapply(1:nrow(ETR2.List[["DeDom"]][["ETR2unweightedTax"]]), function (y) ETR2.List[["DeDom"]][["ETR2unweightedTax"]][[y,12]] / ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][[y,12]])

ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]] <- ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][!ETR2.List[["DeDom"]][["ETR2unweightedTax"]]$ETR2 > 1,]
ETR2.List[["DeDom"]][["ETR2unweightedTax"]] <- ETR2.List[["DeDom"]][["ETR2unweightedTax"]][!ETR2.List[["DeDom"]][["ETR2unweightedTax"]]$ETR2 > 1,]
ETR2.List[["DeDom"]][["ETR2unweightedTax"]] <- ETR2.List[["DeDom"]][["ETR2unweightedTax"]][!ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]]$sum < 0,]
ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]] <- ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]][!ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]]$sum < 0,]



ETR2.List[["DeDom"]][["ETR2unweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                     "ETR2" = mean(ETR2.List[["DeDom"]][["ETR2unweightedTax"]][,13], na.rm = TRUE), 
                                                     "sd" = sd(ETR2.List[["DeDom"]][["ETR2unweightedTax"]][,13], na.rm = TRUE),
                                                     "n" = length(ETR2.List[["DeDom"]][["ETR2unweightedTax"]][,13][!is.na(as.numeric(ETR2.List[["DeDom"]][["ETR2unweightedTax"]][,13]))]))

ETR2.List[["DeDom"]][["ETR2unweighted"]]$low95 <- ETR2.List[["DeDom"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["DeDom"]][["ETR2unweighted"]]$n-1) * ETR2.List[["DeDom"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["DeDom"]][["ETR2unweighted"]]$n)
ETR2.List[["DeDom"]][["ETR2unweighted"]]$high95 <- ETR2.List[["DeDom"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["DeDom"]][["ETR2unweighted"]]$n-1) * ETR2.List[["DeDom"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["DeDom"]][["ETR2unweighted"]]$n)

ETR2.List[["DeDom"]][["ETR2unweighted"]] <- ETR2.List[["DeDom"]][["ETR2unweighted"]][!is.na(ETR2.List[["DeDom"]][["ETR2unweighted"]]$ETR2),]


#International firms unweightet ETR2


ETR2.List[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {ETR2.List[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(ETR2.List[["DeInt"]][["CompanyList"]]) <- paste(2020:2012)


for (i in 1:length(ETR2.List[["DeInt"]][["CompanyList"]])) {
  ETR2.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR2.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR2.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR2.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR2.List[["DeInt"]][["CompanyList"]][[i]])))
}

ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]])) {
  for (j in 3:ncol(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]])) {
    ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][i,j])) & ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][i,1] %in% ETR2.List[["DeInt"]][["CompanyList"]][[(j-2)]], ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][i,j], NA)
  }}



for (i in 1:length(ETR2.List[["DeInt"]][["CompanyList"]])) {
  ETR2.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR2.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR2.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR2.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR2.List[["DeInt"]][["CompanyList"]][[i]])))
}


ETR2.List[["DeInt"]][["ETR2unweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR2.List[["DeInt"]][["ETR2unweightedTax"]])) {
  for (j in 3:ncol(ETR2.List[["DeInt"]][["ETR2unweightedTax"]])) {
    ETR2.List[["DeInt"]][["ETR2unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedTax"]][i,j])) & ETR2.List[["DeInt"]][["ETR2unweightedTax"]][i,1] %in% ETR2.List[["DeInt"]][["CompanyList"]][[(j-2)]], ETR2.List[["DeInt"]][["ETR2unweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]])) {
  for(j in 2:ncol(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]])) {
    ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][i,j])) & !is.na(as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedTax"]][i,j])) ,  as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR2.List[["DeInt"]][["ETR2unweightedTax"]])) {
  for(j in 2:ncol(ETR2.List[["DeInt"]][["ETR2unweightedTax"]])) {
    ETR2.List[["DeInt"]][["ETR2unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][i,j])) & !is.na(as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedTax"]][i,j])) ,  as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedTax"]][i,j])  , NA  )
  }}


ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]]$sum <- sapply(1:nrow(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]]), function (y) sum(as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][y,2:11]) , na.rm = TRUE ))
ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]]$sum[ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]]$sum == 0] <- NA
ETR2.List[["DeInt"]][["ETR2unweightedTax"]]$sum <- sapply(1:nrow(ETR2.List[["DeInt"]][["ETR2unweightedTax"]]), function (y) sum(as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedTax"]][y,2:11]), na.rm = TRUE))
ETR2.List[["DeInt"]][["ETR2unweightedTax"]]$sum[ETR2.List[["DeInt"]][["ETR2unweightedTax"]]$sum == 0] <- NA
ETR2.List[["DeInt"]][["ETR2unweightedTax"]]$ETR2 <- sapply(1:nrow(ETR2.List[["DeInt"]][["ETR2unweightedTax"]]), function (y) ETR2.List[["DeInt"]][["ETR2unweightedTax"]][[y,12]] / ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][[y,12]])

ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]] <- ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][!ETR2.List[["DeInt"]][["ETR2unweightedTax"]]$ETR2 > 1,]
ETR2.List[["DeInt"]][["ETR2unweightedTax"]] <- ETR2.List[["DeInt"]][["ETR2unweightedTax"]][!ETR2.List[["DeInt"]][["ETR2unweightedTax"]]$ETR2 > 1,]
ETR2.List[["DeInt"]][["ETR2unweightedTax"]] <- ETR2.List[["DeInt"]][["ETR2unweightedTax"]][!ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]]$sum < 0,]
ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]] <- ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]][!ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]]$sum < 0,]


ETR2.List[["DeInt"]][["ETR2unweighted"]] <- data.frame("ISO" = "DeInt", 
                                                     "ETR2" = mean(ETR2.List[["DeInt"]][["ETR2unweightedTax"]][,13], na.rm = TRUE), 
                                                     "sd" = sd(ETR2.List[["DeInt"]][["ETR2unweightedTax"]][,13], na.rm = TRUE),
                                                     "n" = length(ETR2.List[["DeInt"]][["ETR2unweightedTax"]][,13][!is.na(as.numeric(ETR2.List[["DeInt"]][["ETR2unweightedTax"]][,13]))]))

ETR2.List[["DeInt"]][["ETR2unweighted"]]$low95 <- ETR2.List[["DeInt"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["DeInt"]][["ETR2unweighted"]]$n-1) * ETR2.List[["DeInt"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["DeInt"]][["ETR2unweighted"]]$n)
ETR2.List[["DeInt"]][["ETR2unweighted"]]$high95 <- ETR2.List[["DeInt"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["DeInt"]][["ETR2unweighted"]]$n-1) * ETR2.List[["DeInt"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["DeInt"]][["ETR2unweighted"]]$n)

ETR2.List[["DeInt"]][["ETR2unweighted"]] <- ETR2.List[["DeInt"]][["ETR2unweighted"]][!is.na(ETR2.List[["DeInt"]][["ETR2unweighted"]]$ETR2),]



#CSH unweighted ETR2


ETR2.List[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {ETR2.List[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(ETR2.List[["ByCSH"]][["CompanyList"]]) <- paste(2020:2012)

for(i in 1:length(ETR2.List[["ByCSH"]][["CompanyList"]])) {ETR2.List[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR2.List[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List[["ByCSH"]][["CompanyList"]][[i]])) {
    ETR2.List[["ByCSH"]][["CompanyList"]][[i]][[j]] <- ETR2.List[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR2.List[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR2.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR2.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]])) {
  Temp1 <- ETR2.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["ByCSH"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[i]]) == 0 ) {ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]])) {
  for (i in 1:nrow(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][i,j])) & ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][i,1] %in% ETR2.List[["ByCSH"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR2.List[["ByCSH"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["ByCSH"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List[["ByCSH"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]])) {
  for(i in 1:nrow(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]])) {
  ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]]$sum[ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][y,12])
  
  ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  
}



ETR2.List[["ByCSH"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                     "sd" = c(sapply(1:length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                     "n" = c(sapply(1:length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["ByCSH"]][["ETR2unweighted"]]$low95 <- ETR2.List[["ByCSH"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["ByCSH"]][["ETR2unweighted"]]$n-1) * ETR2.List[["ByCSH"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["ByCSH"]][["ETR2unweighted"]]$n)
ETR2.List[["ByCSH"]][["ETR2unweighted"]]$high95 <- ETR2.List[["ByCSH"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["ByCSH"]][["ETR2unweighted"]]$n-1) * ETR2.List[["ByCSH"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["ByCSH"]][["ETR2unweighted"]]$n)

ETR2.List[["ByCSH"]][["ETR2unweighted"]] <- ETR2.List[["ByCSH"]][["ETR2unweighted"]][!is.na(ETR2.List[["ByCSH"]][["ETR2unweighted"]]$ETR2),]



#GUO unweighted ETR2


ETR2.List[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {ETR2.List[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(ETR2.List[["ByGUO"]][["CompanyList"]]) <- paste(2020:2012)

for(i in 1:length(ETR2.List[["ByGUO"]][["CompanyList"]])) {ETR2.List[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(ETR2.List[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List[["ByGUO"]][["CompanyList"]][[i]])) {
    ETR2.List[["ByGUO"]][["CompanyList"]][[i]][[j]] <- ETR2.List[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR2.List[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR2.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR2.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]])) {
  Temp1 <- ETR2.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["ByGUO"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[i]]) == 0 ) {ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]])) {
  for (i in 1:nrow(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][i,j])) & ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][i,1] %in% ETR2.List[["ByGUO"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR2.List[["ByGUO"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["ByGUO"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List[["ByGUO"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]])) {
  for(i in 1:nrow(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]])) {
  ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]]$sum[ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][y,12])
  
  ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  
}


ETR2.List[["ByGUO"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                     "sd" = c(sapply(1:length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                     "n" = c(sapply(1:length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["ByGUO"]][["ETR2unweighted"]]$low95 <- ETR2.List[["ByGUO"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["ByGUO"]][["ETR2unweighted"]]$n-1) * ETR2.List[["ByGUO"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["ByGUO"]][["ETR2unweighted"]]$n)
ETR2.List[["ByGUO"]][["ETR2unweighted"]]$high95 <- ETR2.List[["ByGUO"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["ByGUO"]][["ETR2unweighted"]]$n-1) * ETR2.List[["ByGUO"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["ByGUO"]][["ETR2unweighted"]]$n)

ETR2.List[["ByGUO"]][["ETR2unweighted"]] <- ETR2.List[["ByGUO"]][["ETR2unweighted"]][!is.na(ETR2.List[["ByGUO"]][["ETR2unweighted"]]$ETR2),]


#anyown unweighted ETR2


ETR2.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {ETR2.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR2.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2012)

for(i in 1:length(ETR2.List[["Byanyown"]][["CompanyList"]])) {ETR2.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR2.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR2.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR2.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR2.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {ETR2.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(ETR2.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR2.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR2.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR2.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR2.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- ETR2.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]])) {
  Temp1 <- ETR2.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["Byanyown"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[i]]) == 0 ) {ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]])) {
  for (i in 1:nrow(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][i,j])) & ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][i,1] %in% ETR2.List[["Byanyown"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR2.List[["Byanyown"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["Byanyown"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List[["Byanyown"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]])) {
  for(i in 1:nrow(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]])) {
  ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]]$sum[ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][y,12])
  
  ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  
}


ETR2.List[["Byanyown"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["Byanyown"]][["ETR2unweighted"]]$low95 <- ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Byanyown"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Byanyown"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Byanyown"]][["ETR2unweighted"]]$n)
ETR2.List[["Byanyown"]][["ETR2unweighted"]]$high95 <- ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Byanyown"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Byanyown"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Byanyown"]][["ETR2unweighted"]]$n)

ETR2.List[["Byanyown"]][["ETR2unweighted"]] <- ETR2.List[["Byanyown"]][["ETR2unweighted"]][!is.na(ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ETR2),]




#intermed unweighted ETR2


ETR2.List[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {ETR2.List[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(ETR2.List[["Byintermed"]][["CompanyList"]]) <- paste(2020:2012)

for(i in 1:length(ETR2.List[["Byintermed"]][["CompanyList"]])) {ETR2.List[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR2.List[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR2.List[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR2.List[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR2.List[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {ETR2.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(ETR2.List[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR2.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR2.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR2.List[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR2.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- ETR2.List[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]])) {
  Temp1 <- ETR2.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["Byintermed"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[i]]) == 0 ) {ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]])) {
  for (i in 1:nrow(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][i,j])) & ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][i,1] %in% ETR2.List[["Byintermed"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR2.List[["Byintermed"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["Byintermed"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List[["Byintermed"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]])) {
  for(i in 1:nrow(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]])) {
  ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]]$sum[ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][y,12])
  
  ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  
}


ETR2.List[["Byintermed"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                          "sd" = c(sapply(1:length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                          "n" = c(sapply(1:length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["Byintermed"]][["ETR2unweighted"]]$low95 <- ETR2.List[["Byintermed"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Byintermed"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Byintermed"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Byintermed"]][["ETR2unweighted"]]$n)
ETR2.List[["Byintermed"]][["ETR2unweighted"]]$high95 <- ETR2.List[["Byintermed"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Byintermed"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Byintermed"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Byintermed"]][["ETR2unweighted"]]$n)

ETR2.List[["Byintermed"]][["ETR2unweighted"]] <- ETR2.List[["Byintermed"]][["ETR2unweighted"]][!is.na(ETR2.List[["Byintermed"]][["ETR2unweighted"]]$ETR2),]





#ETR2 unweighted Loops


ETR2.List[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {ETR2.List[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR2.List[["Loop"]][["CompanyList"]]) <- paste(2020:2012)


for(i in 1:length(ETR2.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(ETR2.List[["Loop"]][["CompanyList"]])) {ETR2.List[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR2.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR2.List[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR2.List[["Loop"]][["ETR2unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]])) {
  Temp1 <- ETR2.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["Loop"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[i]]) == 0 ) {ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]])) {
  for (i in 1:nrow(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][i,j])) & ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][i,1] %in% ETR2.List[["Loop"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR2.List[["Loop"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["Loop"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["Loop"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["Loop"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["Loop"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["Loop"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List[["Loop"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["Loop"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List[["Loop"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]])) {
  for(i in 1:nrow(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List[["Loop"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]])) {
  ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]]$sum[ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][y,12])
  
  ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["Loop"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  
}


ETR2.List[["Loop"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["Loop"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                    "sd" = c(sapply(1:length(ETR2.List[["Loop"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                    "n" = c(sapply(1:length(ETR2.List[["Loop"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["Loop"]][["ETR2unweighted"]]$low95 <- ETR2.List[["Loop"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Loop"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Loop"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Loop"]][["ETR2unweighted"]]$n)
ETR2.List[["Loop"]][["ETR2unweighted"]]$high95 <- ETR2.List[["Loop"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Loop"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Loop"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Loop"]][["ETR2unweighted"]]$n)

ETR2.List[["Loop"]][["ETR2unweighted"]] <- ETR2.List[["Loop"]][["ETR2unweighted"]][!is.na(ETR2.List[["Loop"]][["ETR2unweighted"]]$ETR2),]


#anysub unweighted ETR2


ETR2.List[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {ETR2.List[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR2.List[["Byanysub"]][["CompanyList"]]) <- paste(2020:2012)

for(i in 1:length(ETR2.List[["Byanysub"]][["CompanyList"]])) {ETR2.List[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR2.List[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(ETR2.List[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR2.List[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {ETR2.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(ETR2.List[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR2.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR2.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- ETR2.List[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR2.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- ETR2.List[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]])) {
  Temp1 <- ETR2.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["Byanysub"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[i]]) == 0 ) {ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]])) {
  for (i in 1:nrow(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][i,j])) & ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][i,1] %in% ETR2.List[["Byanysub"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR2.List[["Byanysub"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["Byanysub"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List[["Byanysub"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]])) {
  for(i in 1:nrow(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]])) {
  ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]]$sum[ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][y,12])
  
  ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  
}

ETR2.List[["Byanysub"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["Byanysub"]][["ETR2unweighted"]]$low95 <- ETR2.List[["Byanysub"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Byanysub"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Byanysub"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Byanysub"]][["ETR2unweighted"]]$n)
ETR2.List[["Byanysub"]][["ETR2unweighted"]]$high95 <- ETR2.List[["Byanysub"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Byanysub"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Byanysub"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Byanysub"]][["ETR2unweighted"]]$n)

ETR2.List[["Byanysub"]][["ETR2unweighted"]] <- ETR2.List[["Byanysub"]][["ETR2unweighted"]][!is.na(ETR2.List[["Byanysub"]][["ETR2unweighted"]]$ETR2),]


#anysubGER unweighted ETR2


ETR2.List[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {ETR2.List[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR2.List[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2012)

for(i in 1:length(ETR2.List[["ByanysubGER"]][["CompanyList"]])) {ETR2.List[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(ETR2.List[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    ETR2.List[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- ETR2.List[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(ETR2.List[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(ETR2.List[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]])) {
  Temp1 <- ETR2.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["ByanysubGER"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[i]]) == 0 ) {ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]])) {
  for (i in 1:nrow(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][i,j])) & ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][i,1] %in% ETR2.List[["ByanysubGER"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List[["ByanysubGER"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,ETR2.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 3:ncol(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List[["ByanysubGER"]][["CompanyList"]][[(j-2)]][[x]], ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]])) {
  for(i in 1:nrow(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]])) {
      ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]])) {
  ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]]$sum[ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][y,12])
  
  ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]] <- ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][!ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]] <- ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]][!ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[x]]$sum < 0,]
  
}


ETR2.List[["ByanysubGER"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                           "sd" = c(sapply(1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                           "n" = c(sapply(1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$low95 <- ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$n-1) * ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$n)
ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$high95 <- ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$n-1) * ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$n)

ETR2.List[["ByanysubGER"]][["ETR2unweighted"]] <- ETR2.List[["ByanysubGER"]][["ETR2unweighted"]][!is.na(ETR2.List[["ByanysubGER"]][["ETR2unweighted"]]$ETR2),]


#Affiliates  unweighted ETR2


ETR2.List[["Affiliates"]][["ETR2unweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                          "ETR2" = mean(unique(Reduce("rbind", ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]))[,13], na.rm = TRUE), 
                                                          "sd" = sd(unique(Reduce("rbind", ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]))[,13], na.rm = TRUE),
                                                          "n" = length(unique(Reduce("rbind", ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]))[,13])]))




ETR2.List[["Affiliates"]][["ETR2unweighted"]]$low95 <- ETR2.List[["Affiliates"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Affiliates"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Affiliates"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Affiliates"]][["ETR2unweighted"]]$n)
ETR2.List[["Affiliates"]][["ETR2unweighted"]]$high95 <- ETR2.List[["Affiliates"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Affiliates"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Affiliates"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Affiliates"]][["ETR2unweighted"]]$n)




#GerGUO unweighted ETR2


ETR2.List[["GerGUO"]][["ETR2unweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                      "ETR2" = mean(unique(Reduce("rbind", ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13], na.rm = TRUE), 
                                                      "sd" = sd(unique(Reduce("rbind", ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13], na.rm = TRUE),
                                                      "n" = length(unique(Reduce("rbind", ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13])]))




ETR2.List[["GerGUO"]][["ETR2unweighted"]]$low95 <- ETR2.List[["GerGUO"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["GerGUO"]][["ETR2unweighted"]]$n-1) * ETR2.List[["GerGUO"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["GerGUO"]][["ETR2unweighted"]]$n)
ETR2.List[["GerGUO"]][["ETR2unweighted"]]$high95 <- ETR2.List[["GerGUO"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["GerGUO"]][["ETR2unweighted"]]$n-1) * ETR2.List[["GerGUO"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["GerGUO"]][["ETR2unweighted"]]$n)

ETR2.List[["GerGUO"]][["ETR2unweighted"]] <- ETR2.List[["GerGUO"]][["ETR2unweighted"]][!is.na(ETR2.List[["GerGUO"]][["ETR2unweighted"]]$ETR2),]




#Domestic firms weighted ETR2


ETR2.List[["DeDom"]][["ETR2weighted"]] <- data.frame("ISO" = "DEDOM")
ETR2.List[["DeDom"]][["ETR2weighted"]]$ETR2 <- sum(ETR2.List[["DeDom"]][["ETR2unweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]]$sum, na.rm = TRUE)
ETR2.List[["DeDom"]][["ETR2weighted"]]$sd <- sqrt(wtd.var(ETR2.List[["DeDom"]][["ETR2unweightedTax"]]$ETR2, sqrt(ETR2.List[["DeDom"]][["ETR2unweightedEBIT"]]$sum^2), na.rm = TRUE ))
ETR2.List[["DeDom"]][["ETR2weighted"]]$n <- length(ETR2.List[["DeDom"]][["ETR2unweightedTax"]]$ETR2[!is.na(ETR2.List[["DeDom"]][["ETR2unweightedTax"]]$ETR2)])
ETR2.List[["DeDom"]][["ETR2weighted"]]$low95 <- ETR2.List[["DeDom"]][["ETR2weighted"]]$ETR2 - qt(0.975, df = ETR2.List[["DeDom"]][["ETR2weighted"]]$n-1) * ETR2.List[["DeDom"]][["ETR2weighted"]]$sd / sqrt(ETR2.List[["DeDom"]][["ETR2weighted"]]$n)
ETR2.List[["DeDom"]][["ETR2weighted"]]$high95 <- ETR2.List[["DeDom"]][["ETR2weighted"]]$ETR2 + qt(0.975, df = ETR2.List[["DeDom"]][["ETR2weighted"]]$n-1) * ETR2.List[["DeDom"]][["ETR2weighted"]]$sd / sqrt(ETR2.List[["DeDom"]][["ETR2weighted"]]$n)


#International firms weighted ETR2


ETR2.List[["DeInt"]][["ETR2weighted"]] <- data.frame("ISO" = "DEINT")
ETR2.List[["DeInt"]][["ETR2weighted"]]$ETR2 <- sum(ETR2.List[["DeInt"]][["ETR2unweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]]$sum, na.rm = TRUE)
ETR2.List[["DeInt"]][["ETR2weighted"]]$sd <- sqrt(wtd.var(ETR2.List[["DeInt"]][["ETR2unweightedTax"]]$ETR2, sqrt(ETR2.List[["DeInt"]][["ETR2unweightedEBIT"]]$sum^2), na.rm = TRUE ))
ETR2.List[["DeInt"]][["ETR2weighted"]]$n <- length(ETR2.List[["DeInt"]][["ETR2unweightedTax"]]$ETR2[!is.na(ETR2.List[["DeInt"]][["ETR2unweightedTax"]]$ETR2)])
ETR2.List[["DeInt"]][["ETR2weighted"]]$low95 <- ETR2.List[["DeInt"]][["ETR2weighted"]]$ETR2 - qt(0.975, df = ETR2.List[["DeInt"]][["ETR2weighted"]]$n-1) * ETR2.List[["DeInt"]][["ETR2weighted"]]$sd / sqrt(ETR2.List[["DeInt"]][["ETR2weighted"]]$n)
ETR2.List[["DeInt"]][["ETR2weighted"]]$high95 <- ETR2.List[["DeInt"]][["ETR2weighted"]]$ETR2 + qt(0.975, df = ETR2.List[["DeInt"]][["ETR2weighted"]]$n-1) * ETR2.List[["DeInt"]][["ETR2weighted"]]$sd / sqrt(ETR2.List[["DeInt"]][["ETR2weighted"]]$n)


#CSH firms weighted ETR2 

ETR2.List[["ByCSH"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                   "sd" = c(sapply(1:length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List[["ByCSH"]][["ETR2unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                   "n" = c(sapply(1:length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["ByCSH"]][["ETR2weighted"]]$low95 <- ETR2.List[["ByCSH"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List[["ByCSH"]][["ETR2weighted"]]$n-1) * ETR2.List[["ByCSH"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["ByCSH"]][["ETR2weighted"]]$n)
ETR2.List[["ByCSH"]][["ETR2weighted"]]$high95 <- ETR2.List[["ByCSH"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List[["ByCSH"]][["ETR2weighted"]]$n-1) * ETR2.List[["ByCSH"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["ByCSH"]][["ETR2weighted"]]$n)

ETR2.List[["ByCSH"]][["ETR2weighted"]] <- ETR2.List[["ByCSH"]][["ETR2weighted"]][!is.na(ETR2.List[["ByCSH"]][["ETR2weighted"]]$ETR2),]


#GUO firms weighted ETR2 

ETR2.List[["ByGUO"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                   "sd" = c(sapply(1:length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List[["ByGUO"]][["ETR2unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                   "n" = c(sapply(1:length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["ByGUO"]][["ETR2weighted"]]$low95 <- ETR2.List[["ByGUO"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List[["ByGUO"]][["ETR2weighted"]]$n-1) * ETR2.List[["ByGUO"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["ByGUO"]][["ETR2weighted"]]$n)
ETR2.List[["ByGUO"]][["ETR2weighted"]]$high95 <- ETR2.List[["ByGUO"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List[["ByGUO"]][["ETR2weighted"]]$n-1) * ETR2.List[["ByGUO"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["ByGUO"]][["ETR2weighted"]]$n)

ETR2.List[["ByGUO"]][["ETR2weighted"]] <- ETR2.List[["ByGUO"]][["ETR2weighted"]][!is.na(ETR2.List[["ByGUO"]][["ETR2weighted"]]$ETR2),]


#anyown firms weighted ETR2 

ETR2.List[["Byanyown"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["Byanyown"]][["ETR2weighted"]]$low95 <- ETR2.List[["Byanyown"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Byanyown"]][["ETR2weighted"]]$n-1) * ETR2.List[["Byanyown"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["Byanyown"]][["ETR2weighted"]]$n)
ETR2.List[["Byanyown"]][["ETR2weighted"]]$high95 <- ETR2.List[["Byanyown"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Byanyown"]][["ETR2weighted"]]$n-1) * ETR2.List[["Byanyown"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["Byanyown"]][["ETR2weighted"]]$n)

ETR2.List[["Byanyown"]][["ETR2weighted"]] <- ETR2.List[["Byanyown"]][["ETR2weighted"]][!is.na(ETR2.List[["Byanyown"]][["ETR2weighted"]]$ETR2),]


#intermed firms weighted ETR2 

ETR2.List[["Byintermed"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                        "sd" = c(sapply(1:length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                        "n" = c(sapply(1:length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["Byintermed"]][["ETR2weighted"]]$low95 <- ETR2.List[["Byintermed"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Byintermed"]][["ETR2weighted"]]$n-1) * ETR2.List[["Byintermed"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["Byintermed"]][["ETR2weighted"]]$n)
ETR2.List[["Byintermed"]][["ETR2weighted"]]$high95 <- ETR2.List[["Byintermed"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Byintermed"]][["ETR2weighted"]]$n-1) * ETR2.List[["Byintermed"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["Byintermed"]][["ETR2weighted"]]$n)

ETR2.List[["Byintermed"]][["ETR2weighted"]] <- ETR2.List[["Byintermed"]][["ETR2weighted"]][!is.na(ETR2.List[["Byintermed"]][["ETR2weighted"]]$ETR2),]


#Loops firms weighted ETR2 

ETR2.List[["Loop"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["Loop"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                  "sd" = c(sapply(1:length(ETR2.List[["Loop"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List[["Loop"]][["ETR2unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                  "n" = c(sapply(1:length(ETR2.List[["Loop"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["Loop"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["Loop"]][["ETR2weighted"]]$low95 <- ETR2.List[["Loop"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Loop"]][["ETR2weighted"]]$n-1) * ETR2.List[["Loop"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["Loop"]][["ETR2weighted"]]$n)
ETR2.List[["Loop"]][["ETR2weighted"]]$high95 <- ETR2.List[["Loop"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Loop"]][["ETR2weighted"]]$n-1) * ETR2.List[["Loop"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["Loop"]][["ETR2weighted"]]$n)

ETR2.List[["Loop"]][["ETR2weighted"]] <- ETR2.List[["Loop"]][["ETR2weighted"]][!is.na(ETR2.List[["Loop"]][["ETR2weighted"]]$ETR2),]



#anysub firms weighted ETR2 

ETR2.List[["Byanysub"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List[["Byanysub"]][["ETR2unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["Byanysub"]][["ETR2weighted"]]$low95 <- ETR2.List[["Byanysub"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Byanysub"]][["ETR2weighted"]]$n-1) * ETR2.List[["Byanysub"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["Byanysub"]][["ETR2weighted"]]$n)
ETR2.List[["Byanysub"]][["ETR2weighted"]]$high95 <- ETR2.List[["Byanysub"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Byanysub"]][["ETR2weighted"]]$n-1) * ETR2.List[["Byanysub"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["Byanysub"]][["ETR2weighted"]]$n)

ETR2.List[["Byanysub"]][["ETR2weighted"]] <- ETR2.List[["Byanysub"]][["ETR2weighted"]][!is.na(ETR2.List[["Byanysub"]][["ETR2weighted"]]$ETR2),]


#anysubGER firms weighted ETR2 

ETR2.List[["ByanysubGER"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                         "sd" = c(sapply(1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                         "n" = c(sapply(1:length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$low95 <- ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$n-1) * ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$n)
ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$high95 <- ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$n-1) * ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$n)

ETR2.List[["ByanysubGER"]][["ETR2weighted"]] <- ETR2.List[["ByanysubGER"]][["ETR2weighted"]][!is.na(ETR2.List[["ByanysubGER"]][["ETR2weighted"]]$ETR2),]


#Affiliates  weighted ETR2


ETR2.List[["Affiliates"]][["ETR2weighted"]] <- data.frame("ISO" = "Affiliates")
ETR2.List[["Affiliates"]][["ETR2weighted"]]$ETR2 <- sum(unique(Reduce("rbind", ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]]))[,12], na.rm = TRUE)
ETR2.List[["Affiliates"]][["ETR2weighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR2.List[["Byanyown"]][["ETR2unweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]])[,12])^2), na.rm = TRUE ))
ETR2.List[["Affiliates"]][["ETR2weighted"]]$n <- length(unique(Reduce("rbind", ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]))[,13])])
ETR2.List[["Affiliates"]][["ETR2weighted"]]$low95 <- ETR2.List[["Affiliates"]][["ETR2weighted"]]$ETR2 - qt(0.975, df = ETR2.List[["Affiliates"]][["ETR2weighted"]]$n-1) * ETR2.List[["Affiliates"]][["ETR2weighted"]]$sd / sqrt(ETR2.List[["Affiliates"]][["ETR2weighted"]]$n)
ETR2.List[["Affiliates"]][["ETR2weighted"]]$high95 <- ETR2.List[["Affiliates"]][["ETR2weighted"]]$ETR2 + qt(0.975, df = ETR2.List[["Affiliates"]][["ETR2weighted"]]$n-1) * ETR2.List[["Affiliates"]][["ETR2weighted"]]$sd / sqrt(ETR2.List[["Affiliates"]][["ETR2weighted"]]$n)



#GerGUO weighted ETR2


ETR2.List[["GerGUO"]][["ETR2weighted"]] <- data.frame("ISO" = "GerGUO")
ETR2.List[["GerGUO"]][["ETR2weighted"]]$ETR2 <- sum(unique(Reduce("rbind", ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]]))[,12], na.rm = TRUE)
ETR2.List[["GerGUO"]][["ETR2weighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR2.List[["ByanysubGER"]][["ETR2unweightedEBIT"]])[,12])^2), na.rm = TRUE ))
ETR2.List[["GerGUO"]][["ETR2weighted"]]$n <- length(unique(Reduce("rbind", ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR2.List[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13])])
ETR2.List[["GerGUO"]][["ETR2weighted"]]$low95 <- ETR2.List[["GerGUO"]][["ETR2weighted"]]$ETR2 - qt(0.975, df = ETR2.List[["GerGUO"]][["ETR2weighted"]]$n-1) * ETR2.List[["GerGUO"]][["ETR2weighted"]]$sd / sqrt(ETR2.List[["GerGUO"]][["ETR2weighted"]]$n)
ETR2.List[["GerGUO"]][["ETR2weighted"]]$high95 <- ETR2.List[["GerGUO"]][["ETR2weighted"]]$ETR2 + qt(0.975, df = ETR2.List[["GerGUO"]][["ETR2weighted"]]$n-1) * ETR2.List[["GerGUO"]][["ETR2weighted"]]$sd / sqrt(ETR2.List[["GerGUO"]][["ETR2weighted"]]$n)

ETR2.List[["GerGUO"]][["ETR2weighted"]] <- ETR2.List[["GerGUO"]][["ETR2weighted"]][!is.na(ETR2.List[["GerGUO"]][["ETR2weighted"]]$ETR2),]





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown

ETR2.List[["Byanyown"]][["ETR2unweighted"]] <- rbind(ETR2.List[["Byanyown"]][["ETR2unweighted"]], 
                                                   
                                                   data.frame("ISO" = c("Sinks"), "ETR2" = c(mean(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                              "sd" = c(sd(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                              "n" = c(length(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))

ETR2.List[["Byanyown"]][["ETR2unweighted"]] <- rbind(ETR2.List[["Byanyown"]][["ETR2unweighted"]], 
                                                   
                                                   data.frame("ISO" = c("Conduits"), "ETR2" = c(mean(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                              "sd" = c(sd(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                              "n" = c(length(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))



ETR2.List[["Byanyown"]][["ETR2unweighted"]] <- rbind(ETR2.List[["Byanyown"]][["ETR2unweighted"]], 
                                                   
                                                   data.frame("ISO" = c("ConduitsProxy"), "ETR2" = c(mean(unique(Reduce("rbind",ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                              "sd" = c(sd(unique(Reduce("rbind",ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                              "n" = c(length(unique(Reduce("rbind",ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))



ETR2.List[["Byanyown"]][["ETR2unweighted"]] <- rbind(ETR2.List[["Byanyown"]][["ETR2unweighted"]], 
                                                   ETR2.List[["DeInt"]][["ETR2unweighted"]],
                                                   ETR2.List[["DeDom"]][["ETR2unweighted"]],
                                                   ETR2.List[["Affiliates"]][["ETR2unweighted"]],
                                                   ETR2.List[["GerGUO"]][["ETR2unweighted"]]
)


ETR2.List[["Byanyown"]][["ETR2unweighted"]]$low95 <- ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Byanyown"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Byanyown"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Byanyown"]][["ETR2unweighted"]]$n)
ETR2.List[["Byanyown"]][["ETR2unweighted"]]$high95 <- ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Byanyown"]][["ETR2unweighted"]]$n-1) * ETR2.List[["Byanyown"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List[["Byanyown"]][["ETR2unweighted"]]$n)





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown


ETR2.List[["Byanyown"]][["ETR2weighted"]] <- rbind(ETR2.List[["Byanyown"]][["ETR2weighted"]], 
                                                 
                                                 data.frame("ISO" = c("Sinks"), "ETR2" = c(sum(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]]) %in% Taxhavens])[,12]), na.rm = TRUE)),
                                                            "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13]), sqrt(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,12])^2),  na.rm = TRUE))),
                                                            "n" = c(length(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                            "low95" = c(NA),
                                                            "high95" = c(NA)
                                                 ))

ETR2.List[["Byanyown"]][["ETR2weighted"]] <- rbind(ETR2.List[["Byanyown"]][["ETR2weighted"]], 
                                                 
                                                 data.frame("ISO" = c("Conduits"), "ETR2" = c(sum(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedEBIT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                            "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                            "n" = c(length(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR2.List[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                            "low95" = c(NA),
                                                            "high95" = c(NA)
                                                 ))



ETR2.List[["Byanyown"]][["ETR2weighted"]] <- rbind(ETR2.List[["Byanyown"]][["ETR2weighted"]], 
                                                 
                                                 data.frame("ISO" = c("ConduitsProxy"), "ETR2" = c(sum(unique(Reduce("rbind",ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]][names(ETR2.List[["Byintermed"]][["ETR2unweightedEBIT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                            "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                            "n" = c(length(unique(Reduce("rbind",ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR2.List[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                            "low95" = c(NA),
                                                            "high95" = c(NA)
                                                 ))



ETR2.List[["Byanyown"]][["ETR2weighted"]] <- rbind(ETR2.List[["Byanyown"]][["ETR2weighted"]], 
                                                 ETR2.List[["DeInt"]][["ETR2weighted"]],
                                                 ETR2.List[["DeDom"]][["ETR2weighted"]],
                                                 ETR2.List[["Affiliates"]][["ETR2weighted"]],
                                                 ETR2.List[["GerGUO"]][["ETR2weighted"]]
)


ETR2.List[["Byanyown"]][["ETR2weighted"]]$low95 <- ETR2.List[["Byanyown"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List[["Byanyown"]][["ETR2weighted"]]$n-1) * ETR2.List[["Byanyown"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["Byanyown"]][["ETR2weighted"]]$n)
ETR2.List[["Byanyown"]][["ETR2weighted"]]$high95 <- ETR2.List[["Byanyown"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List[["Byanyown"]][["ETR2weighted"]]$n-1) * ETR2.List[["Byanyown"]][["ETR2weighted"]]$sd /sqrt(ETR2.List[["Byanyown"]][["ETR2weighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, ETR2, g, h, i, ISO, j, x, y , z)





