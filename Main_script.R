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


## define notin function

'%notin%' <- Negate('%in%')



## Import raw data from Orbis

OrbisCompanies <- rio::import("Orbis companies 2021/Orbis Export Companies 01.08.2021.xlsx", which = "Results")




## Export CSH ID's that are not in company list to Orbis (to get all dates of incorporation)

CompanyBViDs <- OrbisCompanies$CompanyBvDID
CSHBViDs <- OrbisCompanies$CSHBvDID
CSHBViDs2 <- dplyr::distinct(as.data.frame(setdiff(CSHBViDs, CompanyBViDs)))

rio::export(CSHBViDs2, "CSHList.csv")



## Import CSH list to R

OrbisCSH <- rio::import("Orbis companies 2021/Orbis Export CSH 01.08.2021.xlsx", which = "Results")



## cleanup

rm(CSHBViDs2)
rm(CSHBViDs)
rm(CompanyBViDs)



## create edgelist of 2021 by concatenating CSHs


Edgelist2021 <- data.frame(unique(OrbisCompanies$CompanyBvDID))

for (i in 1:15) {
  Edgelist2021 <- cbind(subset(OrbisCompanies, OrbisCompanies$CSHLevel == i)$CSHBvDID[match(Edgelist2021$unique.OrbisCompanies.CompanyBvDID., subset(OrbisCompanies, OrbisCompanies$CSHLevel == i)$CompanyBvDID)],Edgelist2021)
}

Edgelist2021 <- t(apply(Edgelist2021,1,function(x) {c(x[!is.na(x)],x[is.na(x)])}))
Edgelist2021 <- as.data.frame(Edgelist2021)
Edgelist2021 <- Edgelist2021[!sapply(Edgelist2021, function(x) all(is.na(x)))]
Edgelist2021[is.na(Edgelist2021)] <- ""

rm(i)



## create nodelist of 2021 by merging companies and chs' 

Nodelist2021 <- rbind(OrbisCompanies[1:12], OrbisCSH) %>%
                      dplyr::group_by(CompanyBvDID) %>%
                      dplyr::summarize(
                                CompanyName = first(CompanyName),
                                CompanyISO = first(CompanyISO),
                                CompanyNACECore = first(CompanyNACECore),
                                CompanyNACE = toString(unique(na.omit(CompanyNACE))),
                                CompanyType = toString(unique(na.omit(CompanyType))),
                                CompanyPostcode = first(CompanyPostcode),
                                CompanyCity = first(CompanyCity),
                                CompanyState = first(CompanyState),
                                CompanyStart = first(CompanyStart),
                                CompanyStatus = first(CompanyStatus),
                                CompanyEnd = first(CompanyEnd),
                                )


## Import M&A Events

Deals1 <- rio::import("Orbis deals list level 1/ExportDeals1.1.xlsx")
Deals2 <- rio::import("Orbis deals list level 1/ExportDeals1.2.xlsx")
Deals3 <- rio::import("Orbis deals list level 1/ExportDeals1.3.xlsx")
Deals4 <- rio::import("Orbis deals list level 1/ExportDeals1.4.xlsx")

Deals <- rbind(Deals1,Deals2,Deals3,Deals4)

for (i in 1:4){
  rm(list=paste0("Deals",i))
}

rm(i)


## create nested list of deals by year / deal number


Deals$Year <- year(Deals$`Completed date`)

Deals$Year <- as.numeric(Deals$Year)

Deals.List <- split(Deals, Deals$`Deal Number`)


Deals.List.Byyear <- vector(mode = "list")


for (i in min(Deals$Year, na.rm = TRUE):max(Deals$Year, na.rm = TRUE)) {
  

Deals.List.Byyear[[((max(Deals$Year, na.rm = TRUE)+1)-i)]] <- list.filter(Deals.List, any(Year == i))

}

names(Deals.List.Byyear) <- pblapply(1:length(Deals.List.Byyear), function(x) paste0("Deals",(2022-x)))




## Pre-filter Deals List


Deals.List.Byyear <- pblapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, "Minority") == FALSE)))

Deals.List.Byyear <- pblapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, "Share buyback") == FALSE)))

Deals.List.Byyear <- pblapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, "Capital Increase") == FALSE)))
Deals.List.Byyear <- pblapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, "Capital increase") == FALSE)))
Deals.List.Byyear <- pblapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], all(is.na(`Deal type`)) == FALSE))

for(i in 50:100) {
Deals.List.Byyear <- pblapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, paste0("Acquisition increased from ",i,".*")) == FALSE)))
}

for(i in 0:49) {
    Deals.List.Byyear <- pblapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, paste0("Acquisition increased .* to",i,"\\.*")) == FALSE)))
}

for (i in 0:49) {
Deals.List.Byyear <- pblapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, paste0("Acquisition ",i,"\\.")) == FALSE)))
} 


names(Deals.List.Byyear) <- pblapply(1:length(Deals.List.Byyear), function(x) paste0("Deals",(2022-x)))





## check if acquirer is still owner 



Edgelist.List <- vector(mode = "list")

Edgelist.List[[1]] <- as.matrix(Edgelist2021)


for (i in 1:length(Deals.List.Byyear)) {
  
  Temp1 <- cbind(c(""),Edgelist.List[[i]])
  
  for (j in 1:length(Deals.List.Byyear[[i]])) {
    
  Temp2 <- sapply(2:ncol(Temp1), function(x) 
           Temp1[,x] %in% Deals.List.Byyear[[i]][[j]][,8] & Temp1[,(x-1)] %in% Deals.List.Byyear[[i]][[j]][,2])
  
  Temp2 <- cbind(Temp2,c(FALSE))
  

  Temp1[Temp2] <- head(Deals.List.Byyear[[i]][[j]][,5], n = 1, na.rm = TRUE)
  Temp1[is.na(Temp1)] <- "END"
  Temp3 <- cbind(Temp2[,(-1)],c(FALSE))
  Temp1[Temp3] <- "APPEND"
  }
  
    for (j in 1:nrow(Temp1)) {
      for (k in 2:ncol(Temp1)) {
        if (Temp1[j,k] == "END" | Temp1[j,k] == "APPEND")
        {Temp1[j,(1:(k-1))] <- ""}
      }
    }


Temp1[] <- t(apply(Temp1,1,function(x) {c(x[!x == ""],x[x == ""])}))

Temp4 <- as.data.frame(Temp1)
Temp4 <- Temp4[!sapply(Temp4, function(x) all(x == ""))]

Edgelist.List[[(i+1)]] <- as.matrix(Temp4)


}

names(Edgelist.List) <- pblapply(1:length(Edgelist.List), function(x) paste0("Edgelist",(2022-x)))



for (i in 1:4) {
rm(list=paste0("Temp",i))
}
rm(i,j,k)






## check if Vendor not  the owner anymore  


for (i in 1:length(Deals.List.Byyear)) {
  
  Temp1 <- cbind(c(""),Edgelist.List[[i]])
  
  for (j in 1:length(Deals.List.Byyear[[i]])) {
    
    Temp2 <- sapply(2:ncol(Temp1), function(x) 
      Temp1[,x] %in% Deals.List.Byyear[[i]][[j]][,8] & Temp1[,(x-1)] %notin% Deals.List.Byyear[[i]][[j]][,5])
    
    Temp2 <- cbind(Temp2,c(FALSE))
    
    
    Temp1[Temp2] <- head(Deals.List.Byyear[[i]][[j]][,5], n = 1, na.rm = TRUE)
    Temp1[is.na(Temp1)] <- "END"
    Temp3 <- cbind(Temp2[,(-1)],c(FALSE))
    Temp1[Temp3] <- "APPEND"
  }
  
  for (j in 1:nrow(Temp1)) {
    for (k in 2:ncol(Temp1)) {
      if (Temp1[j,k] == "END" | Temp1[j,k] == "APPEND")
      {Temp1[j,(1:(k-1))] <- ""}
    }
  }
  
  
  Temp1[] <- t(apply(Temp1,1,function(x) {c(x[!x == ""],x[x == ""])}))
  
  Temp4 <- as.data.frame(Temp1)
  Temp4 <- Temp4[!sapply(Temp4, function(x) all(x == ""))]
  
  Edgelist.List[[(i+1)]] <- as.matrix(Temp4)
  
  
}

names(Edgelist.List) <- pblapply(1:length(Edgelist.List), function(x) paste0("Edgelist",(2022-x)))


for (i in 1:4) {
  rm(list=paste0("Temp",i))
}
rm(i,j,k)


## Standardize column names

Edgelist.List <- pblapply(1:length(Edgelist.List), function(x) as.data.frame(Edgelist.List[[x]]))
Edgelist.List <- pblapply(1:length(Edgelist.List), function(x) Edgelist.List[[x]][!sapply(Edgelist.List[[x]], function(y) all(y == ""))])
Edgelist.List <- pblapply(1:length(Edgelist.List), function(x) setNames(Edgelist.List[[x]],c(paste0("X",1:ncol(Edgelist.List[[x]])))))
names(Edgelist.List) <- pblapply(1:length(Edgelist.List), function(x) paste0("Edgelist",(2022-x)))



## Export new companies to merge to Orbis


Temp1 <- pblapply(1:length(Edgelist.List),function (x) Edgelist.List[[x]]$X2[Edgelist.List[[x]]$X1 == "APPEND"])

TempControlByYear <- Temp1



NewBvDIDs <- vector(mode = "character")



for (i in 2:length(Temp1)) {
NewBvDIDs <- c(NewBvDIDs,Temp1[[i]])
}

rm(i)

NewBvDIDs <- unique(NewBvDIDs)


BvIDexport <- as.data.frame(NewBvDIDs)


rio::export(BvIDexport,"BvIDexport.xlsx")


## Import new companies to merge from Orbis -> we save the Orbis company List resulting from a BvDID search using BvIDexport.xlsx as "Merge1"


Merge1 <- rio::import("Merge1.xlsx", which = "Results")
Merge1 <- Merge1[,-1]
colnames(Merge1) <- colnames(OrbisCompanies)

Merge1 <- Merge1 %>%
                  fill(CompanyBvDID, CompanyName)




## create ownership chains for new companies to merge at "APPEND" for each year 



Merge1Edges <- data.frame(unique(Merge1$CompanyBvDID))

for (i in 1:15) {
  Merge1Edges <- cbind(subset(Merge1, Merge1$CSHLevel == i)$CSHBvDID[match(Merge1Edges$unique.Merge1.CompanyBvDID., subset(Merge1, Merge1$CSHLevel == i)$CompanyBvDID)],Merge1Edges)
}

Merge1Edges <- t(apply(Merge1Edges,1,function(x) {c(x[!is.na(x)],x[is.na(x)])}))
Merge1Edges <- as.data.frame(Merge1Edges)
Merge1Edges <- Merge1Edges[!sapply(Merge1Edges, function(x) all(is.na(x)))]
Merge1Edges[is.na(Merge1Edges)] <- ""

rm(i)

Merge1Edges <- cbind(Merge1Edges, apply(Merge1Edges,1, function(x) last(x[x!=""])))

colnames(Merge1Edges) <- paste0("X", 1:ncol(Merge1Edges))



##Export new companies using first level merge chains to get level 2 deal list


Temp1 <- vector(mode = "character")

Temp2 <- as.matrix(Merge1Edges)

for (i in 1:nrow(Temp2)) {
  Temp1 <- c(Temp1,Temp2[i,])
}

NewBvDIDs <- unique(Temp1)

for(i in 1:2) {
  rm(list= paste0("Temp",i))
}

rm(i)


BvIDexport2 <- as.data.frame(NewBvDIDs)


rio::export(BvIDexport2,"BvIDexport2.xlsx")



## Import M&A Events


Deals1L2 <- rio::import("Orbis deals list level 2/ExportDeals2.1.xlsx")

Deals1L2[,1] <- NULL

Deals1L2 <- Deals1L2 %>%
  fill(`Deal Number`)


Deals1L2$Year <- year(Deals1L2$`Completed date`)

Deals1L2$Year <- as.numeric(Deals1L2$Year)

Deals1L2 <- Deals1L2 %>% subset(Deals1L2$`Deal Number` %notin% Deals$`Deal Number`) 

Deals.ListL2 <- split(Deals1L2, Deals1L2$`Deal Number`)

Deals.List.ByyearL2 <- vector(mode = "list")



for (i in min(Deals1L2$Year, na.rm = TRUE):max(Deals1L2$Year, na.rm = TRUE)) {
  
  
  Deals.List.ByyearL2[[((max(Deals1L2$Year, na.rm = TRUE)+1)-i)]] <- list.filter(Deals.ListL2, any(Year == i))
  
}

names(Deals.List.ByyearL2) <- pblapply(1:length(Deals.List.ByyearL2), function(x) paste0("Deals",(2022-x)))




## Pre-filter Deals List


Deals.List.ByyearL2 <- pblapply(1:length(Deals.List.ByyearL2), function(x) list.filter(Deals.List.ByyearL2[[x]], any(str_detect(`Deal type`, "Minority") == FALSE)))

Deals.List.ByyearL2 <- pblapply(1:length(Deals.List.ByyearL2), function(x) list.filter(Deals.List.ByyearL2[[x]], any(str_detect(`Deal type`, "Share buyback") == FALSE)))

Deals.List.ByyearL2 <- pblapply(1:length(Deals.List.ByyearL2), function(x) list.filter(Deals.List.ByyearL2[[x]], any(str_detect(`Deal type`, "Capital Increase") == FALSE)))
Deals.List.ByyearL2 <- pblapply(1:length(Deals.List.ByyearL2), function(x) list.filter(Deals.List.ByyearL2[[x]], any(str_detect(`Deal type`, "Capital increase") == FALSE)))
Deals.List.ByyearL2 <- pblapply(1:length(Deals.List.ByyearL2), function(x) list.filter(Deals.List.ByyearL2[[x]], all(is.na(`Deal type`)) == FALSE))

for(i in 50:100) {
  Deals.List.ByyearL2 <- pblapply(1:length(Deals.List.ByyearL2), function(x) list.filter(Deals.List.ByyearL2[[x]], any(str_detect(`Deal type`, paste0("Acquisition increased from ",i,".*")) == FALSE)))
}

for(i in 1:49) {
  Deals.List.ByyearL2 <- pblapply(1:length(Deals.List.ByyearL2), function(x) list.filter(Deals.List.ByyearL2[[x]], any(str_detect(`Deal type`, paste0("Acquisition increased .* to",i,"\\.*")) == FALSE)))
}

for (i in 1:49) {
  Deals.List.ByyearL2 <- pblapply(1:length(Deals.List.ByyearL2), function(x) list.filter(Deals.List.ByyearL2[[x]], any(str_detect(`Deal type`, paste0("Acquisition ",i,"\\.")) == FALSE)))
} 


names(Deals.List.ByyearL2) <- pblapply(1:length(Deals.List.ByyearL2), function(x) paste0("Deals",(2022-x)))




## check if acquirer is still owner 



Mergelist1.List <- vector(mode = "list")

Mergelist1.List[[1]] <- as.matrix(Merge1Edges[,-ncol(Merge1Edges)])

for (i in 1:length(Deals.List.ByyearL2)) {
  
  Temp1 <- cbind(c(""),Mergelist1.List[[i]])
  
  for (j in 1:length(Deals.List.ByyearL2[[i]])) {
    
    Temp2 <- sapply(2:ncol(Temp1), function(x) 
      Temp1[,x] %in% Deals.List.ByyearL2[[i]][[j]][,8] & Temp1[,(x-1)] %in% Deals.List.ByyearL2[[i]][[j]][,2])
    
    Temp2 <- cbind(Temp2,c(FALSE))
    
    
    Temp1[Temp2] <- head(Deals.List.ByyearL2[[i]][[j]][,5], n = 1, na.rm = TRUE)
    Temp1[is.na(Temp1)] <- "END"
    Temp3 <- cbind(Temp2[,(-1)],c(FALSE))
    Temp1[Temp3] <- "APPEND"
  }
  
  for (j in 1:nrow(Temp1)) {
    for (k in 2:ncol(Temp1)) {
      if (Temp1[j,k] == "END" | Temp1[j,k] == "APPEND")
      {Temp1[j,(1:(k-1))] <- ""}
    }
  }
  
  
  Temp1[] <- t(apply(Temp1,1,function(x) {c(x[!x == ""],x[x == ""])}))
  
  Temp4 <- as.data.frame(Temp1)
  Temp4 <- Temp4[!sapply(Temp4, function(x) all(x == ""))]
  
  Mergelist1.List[[(i+1)]] <- as.matrix(Temp4)
  
  
}

names(Mergelist1.List) <- pblapply(1:length(Mergelist1.List), function(x) paste0("Mergelist1-",(2022-x)))


for (i in 1:4) {
  rm(list=paste0("Temp",i))
}
rm(i,j,k)



## check if Vendor not  the owner anymore  



for (i in 1:length(Deals.List.ByyearL2)) {
  
  Temp1 <- cbind(c(""),Mergelist1.List[[i]])
  
  for (j in 1:length(Deals.List.ByyearL2[[i]])) {
    
    Temp2 <- sapply(2:ncol(Temp1), function(x) 
      Temp1[,x] %in% Deals.List.ByyearL2[[i]][[j]][,8] & Temp1[,(x-1)] %notin% Deals.List.ByyearL2[[i]][[j]][,5])
    
    Temp2 <- cbind(Temp2,c(FALSE))
    
    
    Temp1[Temp2] <- head(Deals.List.ByyearL2[[i]][[j]][,5], n = 1, na.rm = TRUE)
    Temp1[is.na(Temp1)] <- "END"
    Temp3 <- cbind(Temp2[,(-1)],c(FALSE))
    Temp1[Temp3] <- "APPEND"
  }
  
  for (j in 1:nrow(Temp1)) {
    for (k in 2:ncol(Temp1)) {
      if (Temp1[j,k] == "END" | Temp1[j,k] == "APPEND")
      {Temp1[j,(1:(k-1))] <- ""}
    }
  }
  
  
  Temp1[] <- t(apply(Temp1,1,function(x) {c(x[!x == ""],x[x == ""])}))
  
  Temp4 <- as.data.frame(Temp1)
  Temp4 <- Temp4[!sapply(Temp4, function(x) all(x == ""))]
  
  Mergelist1.List[[(i+1)]] <- as.matrix(Temp4)
  
  
}

names(Mergelist1.List) <- pblapply(1:length(Mergelist1.List), function(x) paste0("Mergelist-",(2022-x)))


for (i in 1:4) {
  rm(list=paste0("Temp",i))
}
rm(i,j,k)



## Standardize column names

Mergelist1.List <- pblapply(1:length(Mergelist1.List), function(x) as.data.frame(Mergelist1.List[[x]]))
Mergelist1.List <- pblapply(1:length(Mergelist1.List), function(x) Mergelist1.List[[x]][!sapply(Mergelist1.List[[x]], function(y) all(y == ""))])
Mergelist1.List <- pblapply(1:length(Mergelist1.List), function(x) setNames(Mergelist1.List[[x]],c(paste0("X",1:ncol(Mergelist1.List[[x]])))))
names(Mergelist1.List) <- pblapply(1:length(Mergelist1.List), function(x) paste0("Mergelist-",(2022-x)))




## Export new companies to append (from deals) to Orbis


Temp1 <- pblapply(1:length(Mergelist1.List),function (x) Mergelist1.List[[x]]$X2[Mergelist1.List[[x]]$X1 == "APPEND"])

TempControlByYear <- Temp1



NewBvDIDs <- vector(mode = "character")



for (i in 2:length(Temp1)) {
  NewBvDIDs <- c(NewBvDIDs,Temp1[[i]])
}

rm(i)

NewBvDIDs <- unique(NewBvDIDs)


BvIDexportL2.1 <- as.data.frame(NewBvDIDs)


rio::export(BvIDexportL2.1,"BvIDexportL2.1.xlsx")




## Import new companies to merge from Orbis -> we save the Orbis company List resulting from a BvDID search using BvIDexport.xlsx as "Merge2"


Merge2 <- rio::import("Merge2.xlsx", which = "Results")
Merge2 <- Merge2[,-1]
colnames(Merge2) <- colnames(OrbisCompanies)

Merge2 <- Merge2 %>%
  fill(CompanyBvDID, CompanyName)




## create ownership chains for new companies to merge at "APPEND" for each year 


Merge2Edges <- data.frame(unique(Merge2$CompanyBvDID))

for (i in 1:15) {
  Merge2Edges <- cbind(subset(Merge2, Merge2$CSHLevel == i)$CSHBvDID[match(Merge2Edges$unique.Merge2.CompanyBvDID., subset(Merge2, Merge2$CSHLevel == i)$CompanyBvDID)],Merge2Edges)
}

Merge2Edges <- t(apply(Merge2Edges,1,function(x) {c(x[!is.na(x)],x[is.na(x)])}))
Merge2Edges <- as.data.frame(Merge2Edges)
Merge2Edges <- Merge2Edges[!sapply(Merge2Edges, function(x) all(is.na(x)))]
Merge2Edges[is.na(Merge2Edges)] <- ""

rm(i)

Merge2Edges <- cbind(Merge2Edges, apply(Merge2Edges,1, function(x) last(x[x!=""])))

colnames(Merge2Edges) <- paste0("X", 1:ncol(Merge2Edges))




##Export new companies using second level merge chains to get level 3 deal list


Temp1 <- vector(mode = "character")

Temp2 <- as.matrix(Merge2Edges)

for (i in 1:nrow(Temp2)) {
  Temp1 <- c(Temp1,Temp2[i,])
}

NewBvDIDs <- unique(Temp1)

for(i in 1:2) {
  rm(list= paste0("Temp",i))
}

rm(i)


BvIDexportL2.2 <- as.data.frame(NewBvDIDs)

rio::export(BvIDexportL2.2,"BvIDexportL2.2.xlsx")





## Import M&A Events

Deals1L3 <- rio::import("Orbis deals list level 3/ExportDeals3.1.xlsx")

Deals1L3[,1] <- NULL

Deals1L3 <- Deals1L3 %>%
  fill(`Deal Number`)


Deals1L3$Year <- year(Deals1L3$`Completed date`)

Deals1L3$Year <- as.numeric(Deals1L3$Year)

Deals1L3 <- Deals1L3 %>% subset(Deals1L3$`Deal Number` %notin% Deals$`Deal Number`) 

Deals.ListL3 <- split(Deals1L3, Deals1L3$`Deal Number`)

Deals.List.ByyearL3 <- vector(mode = "list")



for (i in min(Deals1L3$Year, na.rm = TRUE):max(Deals1L3$Year, na.rm = TRUE)) {
  
  
  Deals.List.ByyearL3[[((max(Deals1L3$Year, na.rm = TRUE)+1)-i)]] <- list.filter(Deals.ListL3, any(Year == i))
  
}

names(Deals.List.ByyearL3) <- pblapply(1:length(Deals.List.ByyearL3), function(x) paste0("Deals",(2022-x)))





## Pre-filter Deals List


Deals.List.ByyearL3 <- pblapply(1:length(Deals.List.ByyearL3), function(x) list.filter(Deals.List.ByyearL3[[x]], any(str_detect(`Deal type`, "Minority") == FALSE)))

Deals.List.ByyearL3 <- pblapply(1:length(Deals.List.ByyearL3), function(x) list.filter(Deals.List.ByyearL3[[x]], any(str_detect(`Deal type`, "Share buyback") == FALSE)))

Deals.List.ByyearL3 <- pblapply(1:length(Deals.List.ByyearL3), function(x) list.filter(Deals.List.ByyearL3[[x]], any(str_detect(`Deal type`, "Capital Increase") == FALSE)))
Deals.List.ByyearL3 <- pblapply(1:length(Deals.List.ByyearL3), function(x) list.filter(Deals.List.ByyearL3[[x]], any(str_detect(`Deal type`, "Capital increase") == FALSE)))
Deals.List.ByyearL3 <- pblapply(1:length(Deals.List.ByyearL3), function(x) list.filter(Deals.List.ByyearL3[[x]], all(is.na(`Deal type`)) == FALSE))

for(i in 50:100) {
  Deals.List.ByyearL3 <- pblapply(1:length(Deals.List.ByyearL3), function(x) list.filter(Deals.List.ByyearL3[[x]], any(str_detect(`Deal type`, paste0("Acquisition increased from ",i,".*")) == FALSE)))
}

for(i in 1:49) {
  Deals.List.ByyearL3 <- pblapply(1:length(Deals.List.ByyearL3), function(x) list.filter(Deals.List.ByyearL3[[x]], any(str_detect(`Deal type`, paste0("Acquisition increased .* to",i,"\\.*")) == FALSE)))
}

for (i in 1:49) {
  Deals.List.ByyearL3 <- pblapply(1:length(Deals.List.ByyearL3), function(x) list.filter(Deals.List.ByyearL3[[x]], any(str_detect(`Deal type`, paste0("Acquisition ",i,"\\.")) == FALSE)))
} 


names(Deals.List.ByyearL3) <- pblapply(1:length(Deals.List.ByyearL3), function(x) paste0("Deals",(2022-x)))



## check if acquirer is still owner 


Mergelist2.List <- vector(mode = "list")

Mergelist2.List[[1]] <- as.matrix(Merge2Edges[,-ncol(Merge2Edges)])

for (i in 1:length(Deals.List.ByyearL3)) {
  
  Temp1 <- cbind(c(""),Mergelist2.List[[i]])
  
  for (j in 1:length(Deals.List.ByyearL3[[i]])) {
    
    Temp2 <- sapply(2:ncol(Temp1), function(x) 
      Temp1[,x] %in% Deals.List.ByyearL3[[i]][[j]][,8] & Temp1[,(x-1)] %in% Deals.List.ByyearL3[[i]][[j]][,2])
    
    Temp2 <- cbind(Temp2,c(FALSE))
    
    
    Temp1[Temp2] <- head(Deals.List.ByyearL3[[i]][[j]][,5], n = 1, na.rm = TRUE)
    Temp1[is.na(Temp1)] <- "END"
    Temp3 <- cbind(Temp2[,(-1)],c(FALSE))
    Temp1[Temp3] <- "APPEND"
  }
  
  for (j in 1:nrow(Temp1)) {
    for (k in 2:ncol(Temp1)) {
      if (Temp1[j,k] == "END" | Temp1[j,k] == "APPEND")
      {Temp1[j,(1:(k-1))] <- ""}
    }
  }
  
  
  Temp1[] <- t(apply(Temp1,1,function(x) {c(x[!x == ""],x[x == ""])}))
  
  Temp4 <- as.data.frame(Temp1)
  Temp4 <- Temp4[!sapply(Temp4, function(x) all(x == ""))]
  
  Mergelist2.List[[(i+1)]] <- as.matrix(Temp4)
  
  
}

names(Mergelist2.List) <- pblapply(1:length(Mergelist2.List), function(x) paste0("Mergelist2-",(2022-x)))


for (i in 1:4) {
  rm(list=paste0("Temp",i))
}
rm(i,j,k)



## check if Vendor not  the owner anymore  


for (i in 1:length(Deals.List.ByyearL3)) {
  
  Temp1 <- cbind(c(""),Mergelist2.List[[i]])
  
  for (j in 1:length(Deals.List.ByyearL3[[i]])) {
    
    Temp2 <- sapply(2:ncol(Temp1), function(x) 
      Temp1[,x] %in% Deals.List.ByyearL3[[i]][[j]][,8] & Temp1[,(x-1)] %notin% Deals.List.ByyearL3[[i]][[j]][,5])
    
    Temp2 <- cbind(Temp2,c(FALSE))
    
    
    Temp1[Temp2] <- head(Deals.List.ByyearL3[[i]][[j]][,5], n = 1, na.rm = TRUE)
    Temp1[is.na(Temp1)] <- "END"
    Temp3 <- cbind(Temp2[,(-1)],c(FALSE))
    Temp1[Temp3] <- "APPEND"
  }
  
  for (j in 1:nrow(Temp1)) {
    for (k in 2:ncol(Temp1)) {
      if (Temp1[j,k] == "END" | Temp1[j,k] == "APPEND")
      {Temp1[j,(1:(k-1))] <- ""}
    }
  }
  
  
  Temp1[] <- t(apply(Temp1,1,function(x) {c(x[!x == ""],x[x == ""])}))
  
  Temp4 <- as.data.frame(Temp1)
  Temp4 <- Temp4[!sapply(Temp4, function(x) all(x == ""))]
  
  Mergelist2.List[[(i+1)]] <- as.matrix(Temp4)
  
  
}

names(Mergelist2.List) <- pblapply(1:length(Mergelist2.List), function(x) paste0("Mergelist-",(2022-x)))


for (i in 1:4) {
  rm(list=paste0("Temp",i))
}
rm(i,j,k)



## Standardize column names

Mergelist2.List <- pblapply(1:length(Mergelist2.List), function(x) as.data.frame(Mergelist2.List[[x]]))
Mergelist2.List <- pblapply(1:length(Mergelist2.List), function(x) Mergelist2.List[[x]][!sapply(Mergelist2.List[[x]], function(y) all(y == ""))])
Mergelist2.List <- pblapply(1:length(Mergelist2.List), function(x) setNames(Mergelist2.List[[x]],c(paste0("X",1:ncol(Mergelist2.List[[x]])))))
names(Mergelist2.List) <- pblapply(1:length(Mergelist2.List), function(x) paste0("Mergelist-",(2022-x)))





## here, we find no companies which ownership changed through deals anymore, we can therefore start with the merges. We first create an appending point at the end of each data frame in the level 2 merge list


Mergelist2.List <- pblapply(1:length(Mergelist2.List), function(x) cbind(Mergelist2.List[[x]], apply(Mergelist2.List[[x]],1, function(y) last(y[y!=""]))))
Mergelist2.List <- pblapply(1:length(Mergelist2.List), function(x) setNames(Mergelist2.List[[x]],c(paste0("X",1:ncol(Mergelist2.List[[x]])))))
names(Mergelist2.List) <- pblapply(1:length(Mergelist2.List), function(x) paste0("Mergelist-",(2022-x)))




## merge Mergelist1 with Mergelist2 from 2020 to 1996



for (i in 2:length(Mergelist1.List)) {
  
  
  Temp1 <- as.data.frame(Mergelist1.List[[i]])
  Temp2 <- as.data.frame(Mergelist2.List[[i]]) 
  
  
  Temp1$rownumbers <- rownames(Temp1)
  
  
  Temp3 <- Temp1 %>%
    subset (Temp1$X2 %in% Temp2[,ncol(Temp2)] & Temp1$X1 == "APPEND")
  
  
  Temp4 <- Temp2 %>%
    subset(Temp2[,ncol(Temp2)] %in% Temp3$X2)
  
  
  setDT(Temp3)
  setDT(Temp4)
  
  setkey(Temp3,X2)
  setkeyv(Temp4,paste0("X",ncol(Temp4)))
  
  
  Temp5 <- Temp4[Temp3]
  
  if(nrow(Temp5) == 0) next
  
  Temp5[Temp5 == "APPEND"] <- NA
  
  Temp5[Temp5 == ""] <- NA
  
  
  Temp5 <- Temp5[,which(unlist(pblapply(Temp5, function(x) !all(is.na(x))))),with=F]
  
  
  Temp6 <- apply(Temp1,1, function(x) x["rownumbers"] %in% Temp5$rownumbers)
  
  
  Temp7 <- merge(Temp1[!Temp6,],Temp5, by = "rownumbers", all = TRUE)
  
  
  Temp7$rownumbers <- as.numeric(Temp7$rownumbers)
  
  
  Temp7 <- Temp7[order(Temp7$rownumbers),]
  
  
  rownames(Temp7) <- Temp7$rownumbers
  
  
  Temp7$rownumbers <- NULL
  
  
  Temp7[is.na(Temp7)] <- ""
  
  Temp7[] <- t(apply(Temp7,1,function(x) {c(x[!x == ""],x[x == ""])}))
  
  Temp7 <- Temp7[!sapply(Temp7, function(x) all(x == ""))]
  
  Temp7[,(-1)][sapply(2:ncol(Temp7), function(x) Temp7[,x] == Temp7[,(x-1)])] <- ""
  
  Temp7[] <- t(apply(Temp7,1,function(x) {c(x[!x == ""],x[x == ""])}))
  
  Temp7 <- Temp7[!sapply(Temp7, function(x) all(x == ""))]
  
  colnames(Temp7) <- c(paste0("X",1:ncol(Temp7)))
  
  
  Mergelist1.List[[i]] <- Temp7

}




for(i in 1:7) {
  rm(list= paste0("Temp",i))
}

rm(i)

names(Mergelist1.List) <- pblapply(1:length(Mergelist1.List), function(x) paste0("Mergelist-",(2022-x)))




## again create appending points at merge list. this time level 1


Mergelist1.List <- pblapply(1:length(Mergelist1.List), function(x) cbind(Mergelist1.List[[x]], apply(Mergelist1.List[[x]],1, function(y) last(y[y!=""]))))
Mergelist1.List <- pblapply(1:length(Mergelist1.List), function(x) setNames(Mergelist1.List[[x]],c(paste0("X",1:ncol(Mergelist1.List[[x]])))))
names(Mergelist1.List) <- pblapply(1:length(Mergelist1.List), function(x) paste0("Mergelist-",(2022-x)))




## merge original ownership chains with Mergelist1 from 2020 to 1996



for (i in 2:length(Edgelist.List)) {
  
  
Temp1 <- as.data.frame(Edgelist.List[[i]])
Temp2 <- as.data.frame(Mergelist1.List[[i]]) 


Temp1$rownumbers <- rownames(Temp1)


Temp3 <- Temp1 %>%
                    subset (Temp1$X2 %in% Temp2[,ncol(Temp2)] & Temp1$X1 == "APPEND")


Temp4 <- Temp2 %>%
                    subset(Temp2[,ncol(Temp2)] %in% Temp3$X2)


setDT(Temp3)
setDT(Temp4)

setkey(Temp3,X2)
setkeyv(Temp4,paste0("X",ncol(Temp4)))


Temp5 <- Temp4[Temp3]

if(nrow(Temp5) == 0) next

Temp5[Temp5 == "APPEND"] <- NA

Temp5[Temp5 == ""] <- NA
  

  
Temp5 <- Temp5[,which(unlist(pblapply(Temp5, function(x) !all(is.na(x))))),with=F]


Temp6 <- apply(Temp1,1, function(x) x["rownumbers"] %in% Temp5$rownumbers)


Temp7 <- merge(Temp1[!Temp6,],Temp5, by = "rownumbers", all = TRUE)


Temp7$rownumbers <- as.numeric(Temp7$rownumbers)


Temp7 <- Temp7[order(Temp7$rownumbers),]


rownames(Temp7) <- Temp7$rownumbers


Temp7$rownumbers <- NULL




Temp7[is.na(Temp7)] <- ""

Temp7[] <- t(apply(Temp7,1,function(x) {c(x[!x == ""],x[x == ""])}))

Temp7 <- Temp7[!sapply(Temp7, function(x) all(x == ""))]

Temp7[,(-1)][sapply(2:ncol(Temp7), function(x) Temp7[,x] == Temp7[,(x-1)])] <- ""

Temp7[] <- t(apply(Temp7,1,function(x) {c(x[!x == ""],x[x == ""])}))

Temp7 <- Temp7[!sapply(Temp7, function(x) all(x == ""))]

colnames(Temp7) <- c(paste0("X",1:ncol(Temp7)))


Edgelist.List[[i]] <- Temp7


}


for(i in 1:7) {
rm(list= paste0("Temp",i))
}

rm(i)

names(Edgelist.List) <- pblapply(1:length(Edgelist.List), function(x) paste0("Edgelist",(2022-x)))



## Standardize column names

Edgelist.List <- pblapply(1:length(Edgelist.List), function(x) as.data.frame(Edgelist.List[[x]]))
Edgelist.List <- pblapply(1:length(Edgelist.List), function(x) Edgelist.List[[x]][!sapply(Edgelist.List[[x]], function(y) all(y == ""))])
Edgelist.List <- pblapply(1:length(Edgelist.List), function(x) setNames(Edgelist.List[[x]],c(paste0("X",1:ncol(Edgelist.List[[x]])))))
names(Edgelist.List) <- pblapply(1:length(Edgelist.List), function(x) paste0("Edgelist",(2022-x)))


## after those 1105 lines of code, The historic ownership chains should be reconstructed as good as possible (at least to my capabilities) next, I will control for dates of incorporation and (if available) dates of discontinuation









