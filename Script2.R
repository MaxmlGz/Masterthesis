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



## to filter the edgelists by date of incorporation, I first extract all unique BvDID numbers for all years


BvDIDALL <- pblapply(1:length(Edgelist.List), function (x) as.character(unique(unlist(Edgelist.List[[x]]))))

BvDIDALLExport <- vector(mode = "character")

for (i in 2:length(BvDIDALL)) {
BvDIDALLExport <- c(BvDIDALLExport,BvDIDALL[[i]])
}

BvDIDALLExport <- as.data.frame(unique(BvDIDALLExport))

rio::export(BvDIDALLExport, "BvDALLExport.xlsx")

NodelistALL <- rio::import("NodelistALL.xlsx", which = "Results")[-c(1)]
colnames(NodelistALL) <- colnames(OrbisCompanies[1:12])


NodelistALL <- NodelistALL %>%
  fill(CompanyBvDID, CompanyName) %>%
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


## because some BvDID numbers were removed from orbis or have been changed, we merge all company Data that we have at this point

NodelistMissing <- subset(OrbisCompanies[,13:ncol(OrbisCompanies)], OrbisCompanies$CSHBvDID %notin% OrbisCompanies$CompanyBvDID & OrbisCompanies$CSHBvDID %notin% NodelistALL$CompanyBvDID & OrbisCompanies$CSHBvDID %notin% Merge1$CompanyBvDID & OrbisCompanies$CSHBvDID %notin% Merge2$CompanyBvDID)
NodelistMissing <- rbind(NodelistMissing, subset(Merge1[,13:ncol(Merge1)], Merge1$CSHBvDID %notin% OrbisCompanies$CompanyBvDID & Merge1$CSHBvDID %notin% NodelistALL$CompanyBvDID & Merge1$CSHBvDID %notin% Merge1$CompanyBvDID & Merge1$CSHBvDID %notin% Merge2$CompanyBvDID))
NodelistMissing <- rbind(NodelistMissing, subset(Merge2[,13:ncol(OrbisCompanies)], Merge2$CSHBvDID %notin% OrbisCompanies$CompanyBvDID & Merge2$CSHBvDID %notin% NodelistALL$CompanyBvDID & Merge2$CSHBvDID %notin% Merge1$CompanyBvDID & Merge2$CSHBvDID %notin% Merge2$CompanyBvDID))
NodelistMissing <- unique(NodelistMissing)

NodelistMissing <- NodelistMissing[!is.na(NodelistMissing$CSHBvDID),]
NodelistMissing$CSHLevel <- NULL

names(NodelistMissing) = c("CompanyName", "CompanyBvDID", "CompanyISO", "CompanyState","CompanyCity","CompanyNACECore","CompanyType","CompanyPostcode")



NodelistMissing <- NodelistMissing %>%
  dplyr::group_by(CompanyBvDID) %>%
  dplyr::summarize(
    CompanyName = first(CompanyName),
    CompanyISO = first(CompanyISO),
    CompanyNACECore = first(CompanyNACECore),
    CompanyType = toString(unique(na.omit(CompanyType))),
    CompanyPostcode = first(CompanyPostcode),
    CompanyCity = first(CompanyCity),
    CompanyState = first(CompanyState)
  )




Merge1 <- Merge1 <- Merge1 %>%
  fill(CompanyBvDID, CompanyName) %>%
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


Merge2 <- Merge2 <- Merge1 %>%
  fill(CompanyBvDID, CompanyName) %>%
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





NodelistALL <- rbind(NodelistALL, subset(Nodelist2021, Nodelist2021$CompanyBvDID %notin% NodelistALL$CompanyBvDID), subset(Merge1[1:12],Merge1[1:12]$CompanyBvDID %notin% NodelistALL$CompanyBvDID), subset(Merge2[1:12], Merge2[1:12]$CompanyBvDID %notin% NodelistALL$CompanyBvDID))


NodelistALL <- rbind.fill(NodelistALL, subset(NodelistMissing, NodelistMissing$CompanyBvDID %notin% NodelistALL$CompanyBvDID))


NodelistALL <- unique(NodelistALL)

NodelistALL$CompanyEnd <- substring(NodelistALL$CompanyEnd,1,4)

NodelistALL <- NodelistALL[!is.na(NodelistALL$CompanyBvDID),]


## here i create a list that filters the historical edgelists by their companies' date of Incorporation. I first replace the companies BvDID numbers by their founding dates

NodelistALL$CompanyStart <- as.numeric(NodelistALL$CompanyStart)
Date.Of.Incorporation.List <- vector(mode = "list")
Date.Of.Incorporation.List <- pblapply(1:length(Edgelist.List), function (x) {as.data.frame(sapply(Edgelist.List[[x]], function (y) NodelistALL$CompanyStart [match(y, NodelistALL$CompanyBvDID)]))})



## for dates before 1996 (last year of the merger data set) i use a loop to reconstruct earlier ownership chains by their companies date of incorporation

for (i in length(Date.Of.Incorporation.List):(2022-min(as.numeric(NodelistALL$CompanyStart), na.rm = TRUE))) {
  Date.Of.Incorporation.List[[i+1]] <- as.data.frame(sapply(Edgelist.List[[length(Edgelist.List)]], function (y) NodelistALL$CompanyStart [match(y, NodelistALL$CompanyBvDID)]))
}

## the filter works by simply turning the founding dates into TRUE/FALSE values for every year smaller than the respective founding dates in the list

Filter.Incorporation <- pblapply(1:length(Date.Of.Incorporation.List), function (x) {as.data.frame(sapply(1:ncol(Date.Of.Incorporation.List[[x]]), function (y) Date.Of.Incorporation.List[[x]][,y] <= (2022-x)))})

for (i in 1:length(Filter.Incorporation)) {
  Filter.Incorporation[[i]][is.na(Filter.Incorporation[[i]])] <- TRUE
}

## all companies that were found after the year of the respective list have their BvDID number replaced by a marker named "NOTINC"

Edgelist.List.Filtered <- pblapply(1:length(Edgelist.List), function (x) {as.data.frame(ifelse (as.matrix(Filter.Incorporation[[x]]), as.matrix(Edgelist.List[[x]]), "NOTINC"))})
Edgelist.List.Filtered2 <- pblapply((length(Edgelist.List)+1):length(Filter.Incorporation), function (x) {as.data.frame(ifelse (as.matrix(Filter.Incorporation[[x]]), as.matrix(Edgelist.List[[length(Edgelist.List)]]), "NOTINC"))})
Edgelist.List.Filtered <- c(Edgelist.List.Filtered, Edgelist.List.Filtered2)


rm(Edgelist.List.Filtered2)
rm(Date.Of.Incorporation.List)   ##   since this file is very large and eats up a lot of memory, it is deleted after application
rm(Filter.Incorporation) ## since this file is very large and eats up a lot of memory, it is deleted after application


## I repeat the same process for dates of Discontinuation, I use the "last status date" orbis column as discontinuation date, excluding the status dates of companies still flagged as "active"

DateOfDiscontinuationTemp <- subset(NodelistALL, str_detect(CompanyStatus, "Active") == FALSE)
Date.Of.Discontinuation.List <- vector(mode = "list")
Date.Of.Discontinuation.List <- pblapply(1:length(Edgelist.List), function (x) {as.data.frame(sapply(Edgelist.List[[x]], function (y) DateOfDiscontinuationTemp$CompanyEnd [match(y, DateOfDiscontinuationTemp$CompanyBvDID)]))})


for (i in length(Date.Of.Discontinuation.List):(2022-min(as.numeric(DateOfDiscontinuationTemp$CompanyEnd), na.rm = TRUE))) {
  Date.Of.Discontinuation.List[[i+1]] <- as.data.frame(sapply(Edgelist.List[[length(Edgelist.List)]], function (y) DateOfDiscontinuationTemp$CompanyEnd [match(y, DateOfDiscontinuationTemp$CompanyBvDID)]))
}

rm(DateOfDiscontinuationTemp)


## the filter for discontinuation dates is as well created the same way as the incorporation filter

Filter.Discontinuation <- pblapply(1:length(Date.Of.Discontinuation.List), function (x) {as.data.frame(sapply(1:ncol(Date.Of.Discontinuation.List[[x]]), function (y) Date.Of.Discontinuation.List[[x]][,y] >= (2022-x)))})

for (i in 1:length(Filter.Discontinuation)) {
  Filter.Discontinuation[[i]][is.na(Filter.Discontinuation[[i]])] <- TRUE
}


Edgelist.List.Filtered[1:length(Filter.Discontinuation)] <- pblapply(1:length(Filter.Discontinuation), function (x) {as.data.frame(ifelse (as.matrix(Filter.Discontinuation[[x]]), as.matrix(Edgelist.List.Filtered[[x]]), "DISCONT"))})

rm(Date.Of.Discontinuation.List)   ##   since this file is very large and eats up a lot of memory, it is deleted after application


## check for empty rows

Edgelist.List.Filtered <- pblapply(1:length(Edgelist.List.Filtered), function (x) dplyr::mutate_all(Edgelist.List.Filtered[[x]], list(~na_if(.,""))))
Edgelist.List.Filtered <- pblapply(1:length(Edgelist.List.Filtered), function (x) Edgelist.List.Filtered[[x]][!apply(Edgelist.List.Filtered[[x]],1, function (y) {all(str_detect(y,"DISCONT|END|NOTINC") | is.na(y))}),])


## split chains at gaps - if a company inside an ownership chain did not exist in the respective year, the chain is split up at that gap into two separate ownership chains

Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered), function (x) cbind(Edgelist.List.Filtered[[x]][0], Path=do.call(paste, c(Edgelist.List.Filtered[[x]], sep=","))))
Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered.Temp), function (x) cSplit(Edgelist.List.Filtered.Temp[[x]], "Path", sep = "DISCONT", direction = "long"))
Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered.Temp), function (x) cSplit(Edgelist.List.Filtered.Temp[[x]], "Path", sep = "NOTINC", direction = "long"))
Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered.Temp), function (x) data.frame(str_split_fixed(data.frame(Edgelist.List.Filtered.Temp[[x]])[,1], "," ,15)))


## check for empty rows

Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered.Temp), function (x) dplyr::mutate_all(Edgelist.List.Filtered.Temp[[x]], list(~na_if(.,""))))
Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered.Temp), function (x) dplyr::mutate_all(Edgelist.List.Filtered.Temp[[x]], list(~na_if(.,"NA"))))
Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered.Temp), function (x) dplyr::mutate_all(Edgelist.List.Filtered.Temp[[x]], list(~na_if(.,"END"))))
Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered.Temp), function (x) dplyr::mutate_all(Edgelist.List.Filtered.Temp[[x]], list(~na_if(.,"APPEND")))) ## this line might seem odd, but i have manually controlled the Edgelists and the only company left with an "APPEND" marker is DE2350060811, which is a GUO company
Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered.Temp), function (x) Edgelist.List.Filtered.Temp[[x]][!apply(Edgelist.List.Filtered.Temp[[x]],1, function (y) {all  (is.na(y))}),])


## trim list

Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered.Temp), function(x)  {t(apply(Edgelist.List.Filtered.Temp[[x]],1,function(y) {c(y[!is.na(y)],y[is.na(y)])}))})
Edgelist.List.Filtered.Temp <- pblapply(1:length(Edgelist.List.Filtered.Temp), function(x) Edgelist.List.Filtered.Temp[[x]][,!apply(Edgelist.List.Filtered.Temp[[x]],2, function(y) all(is.na(y)))])


## remove duplicates, to remove list elements that are entirely equal, we convert the data frames to characters and remove all characters that are 100% equal to each other

Control.Duplicates.Temp <- pblapply(2:length(Edgelist.List.Filtered.Temp), function(x) as.character(Edgelist.List.Filtered.Temp[[x]]) != as.character(Edgelist.List.Filtered.Temp[[(x-1)]]))
Control.Duplicates.Temp2 <- pblapply(1:length(Control.Duplicates.Temp), function(x) any(Control.Duplicates.Temp[[x]] == TRUE))
Control.Duplicates.Temp3 <- vector(mode = "list")
Control.Duplicates.Temp3[[1]] <- TRUE
Control.Duplicates.Temp2 <- do.call(c, list(Control.Duplicates.Temp3, Control.Duplicates.Temp2))


Edgelist.List.Filtered.Temp2 <- Edgelist.List.Filtered.Temp[unlist(Control.Duplicates.Temp2)]


## check for ownership chains not containing any German energy companies. Since this was the criteria for the analysis from the beginning, we modify all Edgelists so that each chain contains at least one German energy company

NodelistGermanEnergy <- NodelistALL %>%
  subset(str_detect(CompanyNACECore, "^35")) %>%
  subset(str_detect(CompanyISO, "^DE"))


setDT(NodelistALL)                ## data.tables proved to be faster with lookup functions 
setDT(NodelistGermanEnergy)


Edgelist.List.Filtered.Temp2 <- pblapply(1:length(Edgelist.List.Filtered.Temp2), function (x) as.data.frame(Edgelist.List.Filtered.Temp2[[x]]))
Edgelist.List.Filtered.Temp2 <- pblapply(1:length(Edgelist.List.Filtered.Temp2), function (x) setDT(Edgelist.List.Filtered.Temp2[[x]]))
Edgelist.List.Filtered.Temp3 <- pblapply(1:length(Edgelist.List.Filtered.Temp2), function (x) {apply(Edgelist.List.Filtered.Temp2[[x]],1, function (y) {any(y %in% NodelistGermanEnergy$CompanyBvDID)})})

Edgelist.List.Filtered.Temp4 <- pblapply(1:length(Edgelist.List.Filtered.Temp2), function (x) Edgelist.List.Filtered.Temp2[[x]][Edgelist.List.Filtered.Temp3[[x]],])


##remove all chains that do not have at least one company with a known date of incorporation that is a German energy company. I actually thought a lot about this step. But after some trial and error i found this to be the best method to create
##a consistent and comparable dataset. Since i use date of incorporation as a filter criteria, it only makes sense to kick out companies with no available incorporation date. By reducing this filter to German energy companies, i prevent
##to filter out GUOs that are e.g. located in tax havens and do not reveal their company details. I also filter out branches that are, for whatever reason, not assigned to their owner by orbis while keeping those that are correctly assigned
##since those stand in the same row as their owning non-branch company which in almost every case has a valid date of incorporation. This also prevents double-counting of branches.

Edgelist.List.Filtered.Temp5 <- pblapply(1:length(Edgelist.List.Filtered.Temp4), function (x) setDT(Edgelist.List.Filtered.Temp4[[x]]))
Edgelist.List.Filtered.Temp5 <- Edgelist.List.Filtered.Temp5[sapply(Edgelist.List.Filtered.Temp5, function(x) dim(x)[1]) > 0]
Edgelist.List.Filtered.Temp6 <- pblapply(1:length(Edgelist.List.Filtered.Temp5), function(x) {sapply(Edgelist.List.Filtered.Temp5[[x]], function (y) !is.na(NodelistALL$CompanyStart[match(y, NodelistALL$CompanyBvDID)]))}) 
Edgelist.List.Filtered.Temp7 <- pblapply(1:length(Edgelist.List.Filtered.Temp5), function(x) {sapply(Edgelist.List.Filtered.Temp5[[x]], function (y) y %in% NodelistGermanEnergy$CompanyBvDID)}) 
Edgelist.List.Filtered.Temp8 <- pblapply(1:length(Edgelist.List.Filtered.Temp5), function(x) Edgelist.List.Filtered.Temp6[[x]] & Edgelist.List.Filtered.Temp7[[x]])
Edgelist.List.Filtered.Temp9 <- pblapply(1:length(Edgelist.List.Filtered.Temp8), function(x) {apply(Edgelist.List.Filtered.Temp8[[x]],1,function(y) any(y == TRUE))})
  
Edgelist.List.Filtered.Temp10 <- pblapply(1:length(Edgelist.List.Filtered.Temp5), function(x) Edgelist.List.Filtered.Temp5[[x]][Edgelist.List.Filtered.Temp9[[x]],])


## remove empty elements from list

Edgelist.List.Filtered.Temp10 <- Edgelist.List.Filtered.Temp10[sapply(Edgelist.List.Filtered.Temp10, function(x) dim(x)[1]) > 0]


## trim list and standardize column names of dataframes

Edgelist.List.Filtered.Temp11 <- pblapply(1:length(Edgelist.List.Filtered.Temp10), function(x) as.data.frame(Edgelist.List.Filtered.Temp10[[x]]))
Edgelist.List.Filtered.Temp11 <- pblapply(1:length(Edgelist.List.Filtered.Temp11), function(x) Edgelist.List.Filtered.Temp11[[x]][!sapply(Edgelist.List.Filtered.Temp11[[x]], function(y) all(is.na(y)))])
Edgelist.List.Filtered.Temp11 <- pblapply(1:length(Edgelist.List.Filtered.Temp11), function(x) setNames(Edgelist.List.Filtered.Temp11[[x]],c(paste0("X",1:ncol(Edgelist.List.Filtered.Temp11[[x]])))))


## check again for duplicate elements in list

Control.Duplicates.Temp11 <- pblapply(1:(length(Edgelist.List.Filtered.Temp11)-1), function(x) as.character(Edgelist.List.Filtered.Temp11[[(x+1)]]) != as.character(Edgelist.List.Filtered.Temp11[[(x)]]))
Control.Duplicates.Temp12 <- pblapply(1:length(Control.Duplicates.Temp11), function(x) any(Control.Duplicates.Temp11[[x]] == TRUE))
Control.Duplicates.Temp13 <- vector(mode = "list")
Control.Duplicates.Temp13[[1]] <- TRUE
Control.Duplicates.Temp12 <- do.call(c, list(Control.Duplicates.Temp13, Control.Duplicates.Temp12))

Edgelist.List.Filtered.Temp12 <- Edgelist.List.Filtered.Temp11[unlist(Control.Duplicates.Temp12)]


## name each unique Edgelist by the latest founding date of its companies and remove duplicate rows

Edgelist.List.Filtered <- Edgelist.List.Filtered.Temp12
Edgelist.List.Filtered <- pblapply(1:length(Edgelist.List.Filtered), function(x) unique(Edgelist.List.Filtered[[x]]))
names(Edgelist.List.Filtered) <- pblapply(1:length(Edgelist.List.Filtered), function (x) {max(sapply(Edgelist.List.Filtered[[x]], function (y) NodelistALL$CompanyStart[match(y, NodelistALL$CompanyBvDID)]), na.rm = TRUE)})


##cleanup

rm(Edgelist.List.Filtered.Temp)
for (i in 1:12) {
rm(list = paste0("Edgelist.List.Filtered.Temp",i))
}

rm(Control.Duplicates.Temp)
for (i in 1:13) {
  rm(list = paste0("Control.Duplicates.Temp",i))
}

rm(Filter.Discontinuation)







