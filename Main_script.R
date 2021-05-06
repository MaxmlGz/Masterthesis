## Load libraries

library(dplyr)
library(plyr)
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
library(foreach)
library(doParallel)
library(devtools)




## define clusters and configure parallezation

c1 <- detectCores() -2
registerDoParallel(c1)


## Import raw data from Orbis

OrbisCompanies1 <- rio::import("Orbis companies 2021/Export 13_03_2021 11_56.xlsx", which = "Results")
OrbisCompanies2 <- rio::import("Orbis companies 2021/Export 13_03_2021 12_04.xlsx", which = "Results")
OrbisCompanies3 <- rio::import("Orbis companies 2021/Export 13_03_2021 12_10.xlsx", which = "Results")
OrbisCompanies4 <- rio::import("Orbis companies 2021/Export 13_03_2021 12_16.xlsx", which = "Results")
OrbisCompanies5 <- rio::import("Orbis companies 2021/Export 13_03_2021 12_32.xlsx", which = "Results")
OrbisCompanies6 <- rio::import("Orbis companies 2021/Export 13_03_2021 12_35.xlsx", which = "Results")



## Merge tables (append)

OrbisCompanies <- rbind(OrbisCompanies1, OrbisCompanies2, OrbisCompanies3, OrbisCompanies4, OrbisCompanies5, OrbisCompanies6)



## Cleanup 

for (i in 1:6){
  rm(list=paste0("OrbisCompanies",i))
}  

rm(i)



## Export CSH ID's that are not in company list to Orbis (to get all dates of incorporation)

CompanyBViDs <- OrbisCompanies$CompanyBvDID
CSHBViDs <- OrbisCompanies$CSHBvDID
CSHBViDs2 <- distinct(as.data.frame(setdiff(CSHBViDs, CompanyBViDs)))

rio::export(CSHBViDs2, "CSHList.csv")



## Import CSH list to R

OrbisCSH <- rio::import("Orbis CSH companies 2021/Export 01_04_2021 14_19.xlsx", which = "Results")



## cleanup

rm(CSHBViDs2)
rm(CSHBViDs)
rm(CompanyBViDs)



## create edgelist of 2021 by concatenating CSHs

Edgelist2021Help1 <-  dplyr::group_by(OrbisCompanies, CompanyBvDID) %>%
                      dplyr::summarize(Path = paste2(CSHBvDID, sep = ",", trim = TRUE)) 
                      

Edgelist2021Help2 <- data.frame("Path" = Edgelist2021Help1$Path, "Company" = Edgelist2021Help1$CompanyBvDID)


Edgelist2021Help3 <- as.data.frame(ifelse(is.na(Edgelist2021Help2$Path) == TRUE,
                                                                                            Edgelist2021Help2$Company,
                                                                                            paste2(Edgelist2021Help2[1:2], sep = ",")))
colnames(Edgelist2021Help3) <- c("Path")


Edgelist2021 <- data.frame(str_split_fixed(Edgelist2021Help3$Path, "," ,15))



## cleanup

for (i in 1:3){
  rm(list=paste0("Edgelist2021Help",i))
}  

rm(i)



## create nodelist of 2021 by merging companies and chs' 

Nodelist2021 <- rbind(OrbisCompanies[1:11], OrbisCSH) %>%
                      dplyr::group_by(CompanyBvDID) %>%
                      dplyr::summarize(
                                CompanyName = first(CompanyName),
                                CompanyISO = first(CompanyISO),
                                CompanyNACECore = first(CompanyNACECore),
                                CompanyNACE = paste2(CompanyNACE, sep = ",", trim = TRUE),
                                CompanyPostcode = first(CompanyPostcode),
                                CompanyCity = first(CompanyCity),
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



## Import alternative M&A list (with filters)

Deals1alt <- rio::import("Orbis deals list level 1 alternative/ExportDeals1.1ALT.xlsx")
Deals2alt <- rio::import("Orbis deals list level 1 alternative/ExportDeals1.2ALT.xlsx")
Deals3alt <- rio::import("Orbis deals list level 1 alternative/ExportDeals1.3ALT.xlsx")
Deals4alt <- rio::import("Orbis deals list level 1 alternative/ExportDeals1.4ALT.xlsx")

DealsALT <- rbind(Deals1alt,Deals2alt, Deals3alt,Deals4alt)

for (i in 1:4){
  rm(list=paste0("Deals",i,"alt"))
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

names(Deals.List.Byyear) <- lapply(1:length(Deals.List.Byyear), function(x) paste0("Deals",(2022-x)))




## Pre-filter Deals List


Deals.List.Byyear <- lapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, "Minority") == FALSE)))

Deals.List.Byyear <- lapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, "Share buyback") == FALSE)))

Deals.List.Byyear <- lapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, "Capital Increase") == FALSE)))
Deals.List.Byyear <- lapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, "Capital increase") == FALSE)))
Deals.List.Byyear <- lapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], all(is.na(`Deal type`)) == FALSE))

for(i in 50:100) {
Deals.List.Byyear <- lapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, paste0("Acquisition increased from ",i,".*")) == FALSE)))
}

for(i in 1:49) {
    Deals.List.Byyear <- lapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, paste0("Acquisition increased .* to",i,"\\.*")) == FALSE)))
}

for (i in 1:49) {
Deals.List.Byyear <- lapply(1:length(Deals.List.Byyear), function(x) list.filter(Deals.List.Byyear[[x]], any(str_detect(`Deal type`, paste0("Acquisition ",i,"\\.")) == FALSE)))
} 


names(Deals.List.Byyear) <- lapply(1:length(Deals.List.Byyear), function(x) paste0("Deals",(2022-x)))





## check if acquirer is still owner 
##Scheme:  SELECT * FROM Edgelist2021 WHERE Deals2021$Acquirer LEFT OF Deals2021$Target



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

names(Edgelist.List) <- lapply(1:length(Edgelist.List), function(x) paste0("Edgelist",(2022-x)))



for (i in 1:4) {
rm(list=paste0("Temp",i))
}
rm(i,j,k)






## check if Vendor not  the owner anymore  
##Scheme:  SELECT * FROM Edgelist2021 WHERE Deals2021$Acquirer LEFT OF Deals2021$Target


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

names(Edgelist.List) <- lapply(1:length(Edgelist.List), function(x) paste0("Edgelist",(2022-x)))


for (i in 1:4) {
  rm(list=paste0("Temp",i))
}
rm(i,j,k)


## Standardize column names

Edgelist.List <- lapply(1:length(Edgelist.List), function(x) as.data.frame(Edgelist.List[[x]]))
Edgelist.List <- lapply(1:length(Edgelist.List), function(x) Edgelist.List[[x]][!sapply(Edgelist.List[[x]], function(y) all(y == ""))])
Edgelist.List <- lapply(1:length(Edgelist.List), function(x) setNames(Edgelist.List[[x]],c(paste0("X",1:ncol(Edgelist.List[[x]])))))
names(Edgelist.List) <- lapply(1:length(Edgelist.List), function(x) paste0("Edgelist",(2022-x)))


## Add new companies to company list


Temp1 <- lapply(1:length(Edgelist.List),function (x) Edgelist.List[[x]][,2][Edgelist.List[[x]][,1] == "APPEND"])


NewBvDIDs <- vector(mode = "character")



for (i in 2:length(Temp1)) {
NewBvDIDs <- c(Temp1[[i]], Temp1[[(i-1)]])
}

NewBvDIDs <- unique(NewBvDIDs)


BvIDexport <- as.data.frame(NewBvDIDs)


rio::export(BvIDexport,"BvIDexport.xlsx")

Merge1 <- rio::import("Merge1.xlsx", which = "Results")
Merge1 <- Merge1[,-1]
colnames(Merge1) <- colnames(OrbisCompanies)

Merge1 <- Merge1 %>%
                  fill(CompanyBvDID, CompanyName)

Merge1Edges <- Merge1 %>%
                dplyr::group_by(CompanyBvDID) %>%
                dplyr::summarize(Path = paste2(CSHBvDID, sep = ",", trim = TRUE))


colnames(Merge1Edges) <- c("Company","Path")

Merge1Edges <- as.data.frame(ifelse(is.na(Merge1Edges$Path) == TRUE,
                                                    Merge1Edges$Company,
                                                    paste2(Merge1Edges[1:2], sep = ",")))
colnames(Merge1Edges) <- c("Path")

Merge1Edges <- data.frame(str_split_fixed(Merge1Edges$Path, "," ,15))

Merge1Edges <- Merge1Edges[!sapply(Merge1Edges, function(x) all(x == ""))]

Merge1Edges$X6 <- apply(Merge1Edges,1, function(x) last(x[x!=""]))


Test1 <- setDT(as.data.frame(Edgelist.List[[3]]))
Test2 <- setDT(Merge1Edges) 

setkey(Test1,X2)
setkey(Test2,X6)

Test3 <- Test2[Test1]


check <- Edgelist.List[[5]]
check2 <- Edgelist.ListBU[[9]]  


Edgelist.ListBU <- Edgelist.List
Edgelist.List <- Edgelist.ListBU
