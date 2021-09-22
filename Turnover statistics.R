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



Turnover <- rio::import("ImportTurnover.xlsx", which = "Results")

Turnover <- Turnover[,3:13]

colnames(Turnover) <- c("CompanyBVDID", colnames(Turnover[,2:11]))


Turnover.List <- vector(mode = "list")

TurnoverSum <- sum(apply(Turnover,1,function (y) mean(as.numeric(y), na.rm = TRUE)), na.rm = TRUE)

TurnoverSumEstimate <- TurnoverSum * ( nrow(Edgelist.GerSub[[1]]) / nrow(Turnover[apply(Turnover,1,function(y) any(!is.na(as.numeric(y)))),]) )


##domestic


Turnover.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {Turnover.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(Turnover.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2013)


for (i in 1:length(Turnover.List[["DeDom"]][["CompanyList"]])) {
  Turnover.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(Turnover.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- Turnover.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(Turnover.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(Turnover.List[["DeDom"]][["CompanyList"]][[i]])))
}

Turnover.List[["DeDom"]][["Turnover"]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)


for (i in 1:nrow(Turnover.List[["DeDom"]][["Turnover"]])) {
  for (j in 3:ncol(Turnover.List[["DeDom"]][["Turnover"]])) {
    Turnover.List[["DeDom"]][["Turnover"]][i,j] <- ifelse(!is.na(as.numeric(Turnover.List[["DeDom"]][["Turnover"]][i,j])) & Turnover.List[["DeDom"]][["Turnover"]][i,1] %in% Turnover.List[["DeDom"]][["CompanyList"]][[(j-2)]], Turnover.List[["DeDom"]][["Turnover"]][i,j], NA)
  }}




Turnover.List[["DeDom"]][["Turnover"]]$Average <- apply(Turnover.List[["DeDom"]][["Turnover"]],1,function (y) mean(as.numeric(y), na.rm = TRUE))




Turnover.List[["DeDom"]][["TurnoverStats"]] <- data.frame("ISO" = "DEDOM", 
                                                     "Turnover" = sum(Turnover.List[["DeDom"]][["Turnover"]]$Average, na.rm = TRUE), 
                                                     "Sample_Share" = sum(Turnover.List[["DeDom"]][["Turnover"]]$Average, na.rm = TRUE) / TurnoverSum,
                                                     "n" = length(Turnover.List[["DeDom"]][["Turnover"]]$Average[!is.na(as.numeric(Turnover.List[["DeDom"]][["Turnover"]]$Average))]))


Turnover.List[["DeDom"]][["TurnoverStats"]]$perFirm <- Turnover.List[["DeDom"]][["TurnoverStats"]]$Turnover / Turnover.List[["DeDom"]][["TurnoverStats"]]$n

Turnover.List[["DeDom"]][["TurnoverStats"]]$nAll <- nrow(EdgelistDeDom[[1]])












# by anyown


Turnover.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {Turnover.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(Turnover.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2013)

for(i in 1:length(Turnover.List[["Byanyown"]][["CompanyList"]])) {Turnover.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(Turnover.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(Turnover.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(Turnover.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(Turnover.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(Turnover.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {Turnover.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(Turnover.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(Turnover.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(Turnover.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(Turnover.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- Turnover.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    Turnover.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- Turnover.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



Turnover.List[["Byanyown"]][["Turnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {Turnover.List[["Byanyown"]][["Turnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(Turnover.List[["Byanyown"]][["Turnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(Turnover.List[["Byanyown"]][["Turnover"]])) {
  Temp1 <- Turnover.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(Turnover.List[["Byanyown"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,Turnover.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  Turnover.List[["Byanyown"]][["Turnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(Turnover.List[["Byanyown"]][["Turnover"]][[i]]) == 0 ) {Turnover.List[["Byanyown"]][["Turnover"]][[i]][1,] <- NA}
}

for (x in 1:length(Turnover.List[["Byanyown"]][["Turnover"]])) {
  for (i in 1:nrow(Turnover.List[["Byanyown"]][["Turnover"]][[x]])) {
    for (j in 3:ncol(Turnover.List[["Byanyown"]][["Turnover"]][[x]])) {
      Turnover.List[["Byanyown"]][["Turnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(Turnover.List[["Byanyown"]][["Turnover"]][[x]][i,j])) & Turnover.List[["Byanyown"]][["Turnover"]][[x]][i,1] %in% Turnover.List[["Byanyown"]][["CompanyList"]][[(j-2)]][[x]], Turnover.List[["Byanyown"]][["Turnover"]][[x]][i,j], NA)
    }}}



for (x in 1:length(Turnover.List[["Byanyown"]][["Turnover"]])) {Turnover.List[["Byanyown"]][["Turnover"]][[x]]$Average <- apply(Turnover.List[["Byanyown"]][["Turnover"]][[x]],1,function (y) mean(as.numeric(y), na.rm = TRUE)) }


  



Turnover.List[["Byanyown"]][["TurnoverStats"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                             "Turnover" = c(sapply(1:length(Turnover.List[["Byanyown"]][["Turnover"]]),function(y) sum(Turnover.List[["Byanyown"]][["Turnover"]][[y]]$Average, na.rm = TRUE))), 
                                                             "Sample_Share" = c(sapply(1:length(Turnover.List[["Byanyown"]][["Turnover"]]),function(y) sum(Turnover.List[["Byanyown"]][["Turnover"]][[y]]$Average, na.rm = TRUE) / TurnoverSum)),
                                                             "n" = c(sapply(1:length(Turnover.List[["Byanyown"]][["Turnover"]]),function(y) length(Turnover.List[["Byanyown"]][["Turnover"]][[y]]$Average[!is.na(as.numeric(Turnover.List[["Byanyown"]][["Turnover"]][[y]]$Average))]))))



Turnover.List[["Byanyown"]][["TurnoverStats"]]$perFirm <- Turnover.List[["Byanyown"]][["TurnoverStats"]]$Turnover / Turnover.List[["Byanyown"]][["TurnoverStats"]]$n

Turnover.List[["Byanyown"]][["TurnoverStats"]]$nAll <- sapply(Turnover.List[["Byanyown"]][["TurnoverStats"]]$ISO, function(z) nrow(EdgelistByanyown.Gersub[[1]][[z]]))









# by intermed


Turnover.List[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:10) {Turnover.List[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(Turnover.List[["Byintermed"]][["CompanyList"]]) <- paste(2020:2013)

for(i in 1:length(Turnover.List[["Byintermed"]][["CompanyList"]])) {Turnover.List[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(Turnover.List[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(Turnover.List[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(Turnover.List[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(Turnover.List[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(Turnover.List[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {Turnover.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(Turnover.List[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(Turnover.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(Turnover.List[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(Turnover.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- Turnover.List[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    Turnover.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- Turnover.List[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



Turnover.List[["Byintermed"]][["Turnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {Turnover.List[["Byintermed"]][["Turnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(Turnover.List[["Byintermed"]][["Turnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(Turnover.List[["Byintermed"]][["Turnover"]])) {
  Temp1 <- Turnover.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(Turnover.List[["Byintermed"]][["CompanyList"]])) {
    Temp1 <- unique(c(Temp1,Turnover.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  Turnover.List[["Byintermed"]][["Turnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(Turnover.List[["Byintermed"]][["Turnover"]][[i]]) == 0 ) {Turnover.List[["Byintermed"]][["Turnover"]][[i]][1,] <- NA}
}

for (x in 1:length(Turnover.List[["Byintermed"]][["Turnover"]])) {
  for (i in 1:nrow(Turnover.List[["Byintermed"]][["Turnover"]][[x]])) {
    for (j in 3:ncol(Turnover.List[["Byintermed"]][["Turnover"]][[x]])) {
      Turnover.List[["Byintermed"]][["Turnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(Turnover.List[["Byintermed"]][["Turnover"]][[x]][i,j])) & Turnover.List[["Byintermed"]][["Turnover"]][[x]][i,1] %in% Turnover.List[["Byintermed"]][["CompanyList"]][[(j-2)]][[x]], Turnover.List[["Byintermed"]][["Turnover"]][[x]][i,j], NA)
    }}}



for (x in 1:length(Turnover.List[["Byintermed"]][["Turnover"]])) {Turnover.List[["Byintermed"]][["Turnover"]][[x]]$Average <- apply(Turnover.List[["Byintermed"]][["Turnover"]][[x]],1,function (y) mean(as.numeric(y), na.rm = TRUE)) }






Turnover.List[["Byintermed"]][["TurnoverStats"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                             "Turnover" = c(sapply(1:length(Turnover.List[["Byintermed"]][["Turnover"]]),function(y) sum(Turnover.List[["Byintermed"]][["Turnover"]][[y]]$Average, na.rm = TRUE))), 
                                                             "Sample_Share" = c(sapply(1:length(Turnover.List[["Byintermed"]][["Turnover"]]),function(y) sum(Turnover.List[["Byintermed"]][["Turnover"]][[y]]$Average, na.rm = TRUE) / TurnoverSum)),
                                                             "n" = c(sapply(1:length(Turnover.List[["Byintermed"]][["Turnover"]]),function(y) length(Turnover.List[["Byintermed"]][["Turnover"]][[y]]$Average[!is.na(as.numeric(Turnover.List[["Byintermed"]][["Turnover"]][[y]]$Average))]))))



Turnover.List[["Byintermed"]][["TurnoverStats"]]$perFirm <- Turnover.List[["Byintermed"]][["TurnoverStats"]]$Turnover / Turnover.List[["Byintermed"]][["TurnoverStats"]]$n

Turnover.List[["Byintermed"]][["TurnoverStats"]]$nAll <- sapply(Turnover.List[["Byintermed"]][["TurnoverStats"]]$ISO, function(z) nrow(EdgelistByintermed.Gersub[[1]][[z]]))





## append special rows






Turnover.List[["Byanyown"]][["TurnoverStats"]] <- rbind(Turnover.List[["Byanyown"]][["TurnoverStats"]], 
                                                   
                                                   data.frame("ISO" = c("Sinks"), 
                                                              "Turnover" = c(sum(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% Taxhavens]))$Average, na.rm = TRUE)),
                                                              "Sample_Share" = c(sum(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% Taxhavens]))$Average, na.rm = TRUE)) / TurnoverSum,
                                                              "n" = length(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% Taxhavens]))$Average[!is.na(as.numeric(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% Taxhavens]))$Average))]),
                                                              "perFirm" = c(sum(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% Taxhavens]))$Average, na.rm = TRUE)) / length(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% Taxhavens]))$Average[!is.na(as.numeric(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% Taxhavens]))$Average))]),
                                                              "nAll" = nrow(unique(Reduce("rbind",EdgelistByanyown[[1]][names(EdgelistByanyown[[1]]) %in% Taxhavens])))))
                                                              



Turnover.List[["Byanyown"]][["TurnoverStats"]] <- rbind(Turnover.List[["Byanyown"]][["TurnoverStats"]], 
                                                        
                                                        data.frame("ISO" = c("Conduits"), 
                                                                   "Turnover" = c(sum(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% TaxhavensEU]))$Average, na.rm = TRUE)),
                                                                   "Sample_Share" = c(sum(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% TaxhavensEU]))$Average, na.rm = TRUE)) / TurnoverSum,
                                                                   "n" = length(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% TaxhavensEU]))$Average[!is.na(as.numeric(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% TaxhavensEU]))$Average))]),
                                                                   "perFirm" = c(sum(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% TaxhavensEU]))$Average, na.rm = TRUE)) / length(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% TaxhavensEU]))$Average[!is.na(as.numeric(unique(Reduce("rbind",Turnover.List[["Byanyown"]][["Turnover"]][names(Turnover.List[["Byanyown"]][["Turnover"]]) %in% TaxhavensEU]))$Average))]),
                                                                   "nAll" = nrow(unique(Reduce("rbind",EdgelistByanyown[[1]][names(EdgelistByanyown[[1]]) %in% TaxhavensEU])))))


Turnover.List[["Byanyown"]][["TurnoverStats"]] <- rbind(Turnover.List[["Byanyown"]][["TurnoverStats"]], 
                                                        
                                                        data.frame("ISO" = c("ConduitsProxy"), 
                                                                   "Turnover" = c(sum(unique(Reduce("rbind",Turnover.List[["Byintermed"]][["Turnover"]][names(Turnover.List[["Byintermed"]][["Turnover"]]) %in% TaxhavensEU]))$Average, na.rm = TRUE)),
                                                                   "Sample_Share" = c(sum(unique(Reduce("rbind",Turnover.List[["Byintermed"]][["Turnover"]][names(Turnover.List[["Byintermed"]][["Turnover"]]) %in% TaxhavensEU]))$Average, na.rm = TRUE)) / TurnoverSum,
                                                                   "n" = length(unique(Reduce("rbind",Turnover.List[["Byintermed"]][["Turnover"]][names(Turnover.List[["Byintermed"]][["Turnover"]]) %in% TaxhavensEU]))$Average[!is.na(as.numeric(unique(Reduce("rbind",Turnover.List[["Byintermed"]][["Turnover"]][names(Turnover.List[["Byintermed"]][["Turnover"]]) %in% TaxhavensEU]))$Average))]),
                                                                   "perFirm" = c(sum(unique(Reduce("rbind",Turnover.List[["Byintermed"]][["Turnover"]][names(Turnover.List[["Byintermed"]][["Turnover"]]) %in% TaxhavensEU]))$Average, na.rm = TRUE)) / length(unique(Reduce("rbind",Turnover.List[["Byintermed"]][["Turnover"]][names(Turnover.List[["Byintermed"]][["Turnover"]]) %in% TaxhavensEU]))$Average[!is.na(as.numeric(unique(Reduce("rbind",Turnover.List[["Byintermed"]][["Turnover"]][names(Turnover.List[["Byintermed"]][["Turnover"]]) %in% TaxhavensEU]))$Average))]),
                                                                   "nAll" = nrow(unique(Reduce("rbind",EdgelistByintermed[[1]][names(EdgelistByintermed[[1]]) %in% TaxhavensEU])))))



Turnover.List[["Byanyown"]][["TurnoverStats"]] <- rbind(Turnover.List[["Byanyown"]][["TurnoverStats"]], 
                                                   Turnover.List[["DeDom"]][["TurnoverStats"]]
)





TurnoverStats <- Turnover.List[["Byanyown"]][["TurnoverStats"]]


TurnoverStats <- subset(TurnoverStats, Turnover > 0)


TurnoverStats$Estimate <- TurnoverStats$perFirm * as.numeric(TurnoverStats$nAll)

TurnoverStats$ShareEstimate <- sapply(TurnoverStats$ISO, function(y) TurnoverStats[TurnoverStats$ISO == y,]$Estimate / TurnoverSumEstimate )




rio::export(TurnoverStats, "TurnoverStats.xlsx")











## Turnover NBFI 



NBFITurnover <- data.frame(
  
  
  "TurnoverNBFI" = sum(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFIOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE)),na.rm = TRUE),
  
  "TurnoverNBFInoholding" = sum(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFInoholdingOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE)), na.rm = TRUE),
  
  "TurnoverNBFIConduits" = sum(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFITaxhavenEUOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE)), na.rm = TRUE),
  
  "TurnoverNBFIConduitsnoholding" = sum(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFInoholdingTaxhavenEUOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE)), na.rm = TRUE),
  
  "TurnoverNBFISinks" = sum(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFITaxhavenOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE)), na.rm = TRUE),
  
  "TurnoverNBFISinksnoholding" = sum(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFInoholdingTaxhavenOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE)), na.rm = TRUE)
  
)

NBFITurnover[2,] <- NBFITurnover[1,] / TurnoverSum

NBFITurnover[3,] <- 
  
  c(length(na.omit(as.numeric(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFIOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE))))),
    length(na.omit(as.numeric(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFInoholdingOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE))))),
    length(na.omit(as.numeric(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFITaxhavenEUOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE))))),
    length(na.omit(as.numeric(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFInoholdingTaxhavenEUOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE))))),
    length(na.omit(as.numeric(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFITaxhavenOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE))))),
    length(na.omit(as.numeric(apply(reduce(pblapply(1:10, function(x) subset(Turnover, CompanyBVDID %in% apply(NBFI.List[["NBFInoholdingTaxhavenOwner"]][[x]],1,function(y) last(y[!is.na(y)])))[,(c(1,(x+1)))]), dplyr::full_join, by = "CompanyBVDID"),1,function(y) mean(as.numeric(y), na.rm = TRUE)))))
  )


NBFITurnover[4,] <- NBFITurnover[1,] / NBFITurnover[3,]

NBFITurnover[5,] <- c(nrow(NBFI.List[["NBFIOwner"]][[1]]),
                      nrow(NBFI.List[["NBFInoholdingOwner"]][[1]]),
                      nrow(NBFI.List[["NBFITaxhavenEUOwner"]][[1]]),
                      nrow(NBFI.List[["NBFInoholdingTaxhavenEUOwner"]][[1]]),
                      nrow(NBFI.List[["NBFITaxhavenOwner"]][[1]]),
                      nrow(NBFI.List[["NBFInoholdingTaxhavenOwner"]][[1]])
)


NBFITurnover[6,] <-  NBFITurnover[4,] * NBFITurnover[5,]



NBFITurnover[7,] <- NBFITurnover[6,] / TurnoverSumEstimate



rownames(NBFITurnover) <- colnames(TurnoverStats[,2:ncol(TurnoverStats)])




rio::export(NBFITurnover, "NBFITurnover.xlsx")

















