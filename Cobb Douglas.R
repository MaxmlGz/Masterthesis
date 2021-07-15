


EBT <- rio::import("ImportEBT.xlsx", which = "Results")
EBT <- cbind(data.frame("CompanyBVDID" = c(EBT$`BvD ID number`)),EBT[,4:13])

Assets <- rio::import("ImportAssets.xlsx", which = "Results")
Assets <- cbind(data.frame("CompanyBVDID" = c(Assets$`BvD ID number`)),Assets[,4:13])

EmployeeCost <- rio::import("ImportEC.xlsx", which = "Results")
EmployeeCost <- cbind(data.frame("CompanyBVDID" = c(EmployeeCost$`BvD ID number`)),EmployeeCost[,4:13])


BvDIDNorm <- intersect(EBT$CompanyBVDID, Assets$CompanyBVDID)
BvDIDNorm2 <- intersect(BvDIDNorm, EmployeeCost$CompanyBVDID)

BvDIDNorm <- intersect(BvDIDNorm, BvDIDNorm2)

BvDIDNorm <- data.frame("CompanyBVDID" = c(BvDIDNorm))

EBT <- left_join(BvDIDNorm, EBT, by = "CompanyBVDID")
Assets <- left_join(BvDIDNorm, Assets, by = "CompanyBVDID")
EmployeeCost <- left_join(BvDIDNorm, EmployeeCost, by = "CompanyBVDID")



for(i in 1:nrow(EBT)) {
  for (j in 2:ncol(EBT)) {
    EBT[i,j] <- ifelse(!is.na(as.numeric(EBT[i,j])) & !is.na(as.numeric(Assets[i,j])) & !is.na(as.numeric(EmployeeCost[i,j])),  as.numeric(EBT[i,j]) , NA )
  }
}


for(i in 1:nrow(Assets)) {
  for (j in 2:ncol(Assets)) {
    Assets[i,j] <- ifelse(!is.na(as.numeric(EBT[i,j])) & !is.na(as.numeric(Assets[i,j])) & !is.na(as.numeric(EmployeeCost[i,j])),  as.numeric(Assets[i,j]) , NA )
  }
}


for(i in 1:nrow(EmployeeCost)) {
  for (j in 2:ncol(EmployeeCost)) {
    EmployeeCost[i,j] <- ifelse(!is.na(as.numeric(EBT[i,j])) & !is.na(as.numeric(Assets[i,j])) & !is.na(as.numeric(EmployeeCost[i,j])),  as.numeric(EmployeeCost[i,j]) , NA )
  }
}




EmployeeCost$sum <- apply(EmployeeCost,1,function(y) sum(as.numeric(y), na.rm = TRUE))
EBT$sum <- apply(EBT,1,function(y) sum(as.numeric(y), na.rm = TRUE))
Assets$sum <- apply(Assets,1,function(y) sum(as.numeric(y), na.rm = TRUE))



CDEstimate <- lm(EBT$sum ~  Assets$sum + EmployeeCost$sum)







CobDou.List <- vector(mode = "list")
CobDou.List[[1]] <- vector(mode = "list")
names(CobDou.List) <- "ByCSH"


#Domestic firms unweightet CobDou


CobDou.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CobDou.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(CobDou.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(CobDou.List[["DeDom"]][["CompanyList"]])) {
  CobDou.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(CobDou.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- CobDou.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(CobDou.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(CobDou.List[["DeDom"]][["CompanyList"]][[i]])))
}

CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)

for (i in 1:nrow(CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]])) {
  for (j in 2:ncol(CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]])) {
    CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]][i,j])) & CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]][i,1] %in% CobDou.List[["DeDom"]][["CompanyList"]][[(j-1)]], CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]][i,j], NA)
  }}



CobDou.List[["DeDom"]][["CobDouunweightedEBT"]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]])) {
  for (j in 2:ncol(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]])) {
    CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][i,j])) & CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][i,1] %in% CobDou.List[["DeDom"]][["CompanyList"]][[(j-1)]], CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][i,j], NA)
  }}



CobDou.List[["DeDom"]][["CobDouunweightedAssets"]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(CobDou.List[["DeDom"]][["CobDouunweightedAssets"]])) {
  for (j in 2:ncol(CobDou.List[["DeDom"]][["CobDouunweightedAssets"]])) {
    CobDou.List[["DeDom"]][["CobDouunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["DeDom"]][["CobDouunweightedAssets"]][i,j])) & CobDou.List[["DeDom"]][["CobDouunweightedAssets"]][i,1] %in% CobDou.List[["DeDom"]][["CompanyList"]][[(j-1)]], CobDou.List[["DeDom"]][["CobDouunweightedAssets"]][i,j], NA)
  }}



CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]]$sum <- sapply(1:nrow(CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]]), function (y) sum(as.numeric(CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]][y,2:11]) , na.rm = TRUE ))
CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]]$sum[CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]]$sum == 0] <- NA
CobDou.List[["DeDom"]][["CobDouunweightedEBT"]]$sum <- sapply(1:nrow(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]]), function (y) sum(as.numeric(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][y,2:11]), na.rm = TRUE))
CobDou.List[["DeDom"]][["CobDouunweightedEBT"]]$sum[CobDou.List[["DeDom"]][["CobDouunweightedEBT"]]$sum == 0] <- NA
CobDou.List[["DeDom"]][["CobDouunweightedAssets"]]$sum <- sapply(1:nrow(CobDou.List[["DeDom"]][["CobDouunweightedAssets"]]), function (y) sum(as.numeric(CobDou.List[["DeDom"]][["CobDouunweightedAssets"]][y,2:11]), na.rm = TRUE))
CobDou.List[["DeDom"]][["CobDouunweightedAssets"]]$sum[CobDou.List[["DeDom"]][["CobDouunweightedAssets"]]$sum == 0] <- NA

CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]]$LProf <- sapply(1:nrow(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]]), function (y) CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][[y,12]] / CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]][[y,12]])
CobDou.List[["DeDom"]][["CobDouunweightedAssets"]]$CProf <- sapply(1:nrow(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]]), function (y) CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][[y,12]] / CobDou.List[["DeDom"]][["CobDouunweightedAssets"]][[y,12]])







CobDou.List[["DeDom"]][["CobDouunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                         "LProf" = mean(CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]][,13], na.rm = TRUE), 
                                                         "CProf" = mean(CobDou.List[["DeDom"]][["CobDouunweightedAssets"]][,13], na.rm = TRUE),
                                                         "EBT" = mean(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][,12], na.rm = TRUE),
                                                         "EBTCD" = mean(CD[1] + CD[2]*CobDou.List[["DeDom"]][["CobDouunweightedAssets"]]$sum + CD[3]*CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]]$sum, na.rm = TRUE),
                                                         "n" = length(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]]$sum[!is.na(as.numeric(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]]$sum))])
)







#International firms unweightet CobyDobby


CobDou.List[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CobDou.List[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(CobDou.List[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(CobDou.List[["DeInt"]][["CompanyList"]])) {
  CobDou.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(CobDou.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- CobDou.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(CobDou.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(CobDou.List[["DeInt"]][["CompanyList"]][[i]])))
}

CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)

for (i in 1:nrow(CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]])) {
  for (j in 2:ncol(CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]])) {
    CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]][i,j])) & CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]][i,1] %in% CobDou.List[["DeInt"]][["CompanyList"]][[(j-1)]], CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]][i,j], NA)
  }}



CobDou.List[["DeInt"]][["CobDouunweightedEBT"]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]])) {
  for (j in 2:ncol(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]])) {
    CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][i,j])) & CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][i,1] %in% CobDou.List[["DeInt"]][["CompanyList"]][[(j-1)]], CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][i,j], NA)
  }}



CobDou.List[["DeInt"]][["CobDouunweightedAssets"]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(CobDou.List[["DeInt"]][["CobDouunweightedAssets"]])) {
  for (j in 2:ncol(CobDou.List[["DeInt"]][["CobDouunweightedAssets"]])) {
    CobDou.List[["DeInt"]][["CobDouunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["DeInt"]][["CobDouunweightedAssets"]][i,j])) & CobDou.List[["DeInt"]][["CobDouunweightedAssets"]][i,1] %in% CobDou.List[["DeInt"]][["CompanyList"]][[(j-1)]], CobDou.List[["DeInt"]][["CobDouunweightedAssets"]][i,j], NA)
  }}



CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]]$sum <- sapply(1:nrow(CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]]), function (y) sum(as.numeric(CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]][y,2:11]) , na.rm = TRUE ))
CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]]$sum[CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]]$sum == 0] <- NA
CobDou.List[["DeInt"]][["CobDouunweightedEBT"]]$sum <- sapply(1:nrow(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]]), function (y) sum(as.numeric(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][y,2:11]), na.rm = TRUE))
CobDou.List[["DeInt"]][["CobDouunweightedEBT"]]$sum[CobDou.List[["DeInt"]][["CobDouunweightedEBT"]]$sum == 0] <- NA
CobDou.List[["DeInt"]][["CobDouunweightedAssets"]]$sum <- sapply(1:nrow(CobDou.List[["DeInt"]][["CobDouunweightedAssets"]]), function (y) sum(as.numeric(CobDou.List[["DeInt"]][["CobDouunweightedAssets"]][y,2:11]), na.rm = TRUE))
CobDou.List[["DeInt"]][["CobDouunweightedAssets"]]$sum[CobDou.List[["DeInt"]][["CobDouunweightedAssets"]]$sum == 0] <- NA

CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]]$LProf <- sapply(1:nrow(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]]), function (y) CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][[y,12]] / CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]][[y,12]])
CobDou.List[["DeInt"]][["CobDouunweightedAssets"]]$CProf <- sapply(1:nrow(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]]), function (y) CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][[y,12]] / CobDou.List[["DeInt"]][["CobDouunweightedAssets"]][[y,12]])







CobDou.List[["DeInt"]][["CobDouunweighted"]] <- data.frame("ISO" = "DEINT", 
                                                           "LProf" = mean(CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]][,13], na.rm = TRUE), 
                                                           "CProf" = mean(CobDou.List[["DeInt"]][["CobDouunweightedAssets"]][,13], na.rm = TRUE),
                                                           "EBT" = mean(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][,12], na.rm = TRUE),
                                                           "EBTCD" = mean(CD[1] + CD[2]*CobDou.List[["DeInt"]][["CobDouunweightedAssets"]]$sum + CD[3]*CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]]$sum, na.rm= TRUE),
                                                           "n" = length(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]]$sum[!is.na(as.numeric(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]]$sum))])
)




#CSH unweighted LProf


CobDou.List[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CobDou.List[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(CobDou.List[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CobDou.List[["ByCSH"]][["CompanyList"]])) {CobDou.List[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CobDou.List[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(CobDou.List[["ByCSH"]][["CompanyList"]][[i]])) {
    CobDou.List[["ByCSH"]][["CompanyList"]][[i]][[j]] <- CobDou.List[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(CobDou.List[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(CobDou.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(CobDou.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(CobDou.List[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]])) {
  Temp1 <- CobDou.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[i]]) == 0 ) {CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]])) {
  for (i in 1:nrow(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]])) {
      CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]][i,j])) & CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]][i,1] %in% CobDou.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]])) {
  Temp1 <- CobDou.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[i]]) == 0 ) {CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]])) {
  for (i in 1:nrow(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]])) {
      CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]][i,j])) & CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]][i,1] %in% CobDou.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]][i,j], NA)
    }}}


CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]])) {
  Temp1 <- CobDou.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[i]]) == 0 ) {CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]])) {
  for (i in 1:nrow(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]])) {
      CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]][i,j])) & CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]][i,1] %in% CobDou.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]][i,j], NA)
    }}}








for (x in 1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]])) {
  CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]]$sum[CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]]), function (y) sum(as.numeric(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]]$sum[CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]]$sum == 0] <- NA
  CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]]), function (y) sum(as.numeric(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]]$sum[CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]]$sum == 0] <- NA

  CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]]$LProf <- sapply(1:nrow(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[x]][y,12])
  CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]]$CProf <- sapply(1:nrow(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[x]][y,12])
}



CobDou.List[["ByCSH"]][["CobDouunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                           "LProf" = c(sapply(1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[y]][,13], na.rm = TRUE))), 
                                                           "CProf" = c(sapply(1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[y]][,13], na.rm = TRUE))), 
                                                           "EBT" = c(sapply(1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))), 
                                                           "EBTCD" = c(sapply(1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]),function(y) mean(CD[1] + CD[2]*CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[y]][,12]  + CD[3]*CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                          "n" = c(sapply(1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[y]][,12]))]))))







#GUO unweighted LProf


CobDou.List[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CobDou.List[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(CobDou.List[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CobDou.List[["ByGUO"]][["CompanyList"]])) {CobDou.List[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(CobDou.List[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(CobDou.List[["ByGUO"]][["CompanyList"]][[i]])) {
    CobDou.List[["ByGUO"]][["CompanyList"]][[i]][[j]] <- CobDou.List[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(CobDou.List[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(CobDou.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(CobDou.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(CobDou.List[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]])) {
  Temp1 <- CobDou.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[i]]) == 0 ) {CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]])) {
  for (i in 1:nrow(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]])) {
      CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]][i,j])) & CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]][i,1] %in% CobDou.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]])) {
  Temp1 <- CobDou.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[i]]) == 0 ) {CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]])) {
  for (i in 1:nrow(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]])) {
      CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]][i,j])) & CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]][i,1] %in% CobDou.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]][i,j], NA)
    }}}


CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]])) {
  Temp1 <- CobDou.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[i]]) == 0 ) {CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]])) {
  for (i in 1:nrow(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]])) {
      CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]][i,j])) & CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]][i,1] %in% CobDou.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]][i,j], NA)
    }}}






for (x in 1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]])) {
  CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]]$sum[CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]]), function (y) sum(as.numeric(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]]$sum[CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]]$sum == 0] <- NA
  CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]]), function (y) sum(as.numeric(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]]$sum[CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]]$sum == 0] <- NA
  
  CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]]$LProf <- sapply(1:nrow(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[x]][y,12])
  CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]]$CProf <- sapply(1:nrow(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[x]][y,12])
}



CobDou.List[["ByGUO"]][["CobDouunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                           "LProf" = c(sapply(1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[y]][,13], na.rm = TRUE))), 
                                                           "CProf" = c(sapply(1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[y]][,13], na.rm = TRUE))), 
                                                           "EBT" = c(sapply(1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))), 
                                                           "EBTCD" = c(sapply(1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]),function(y) mean(CD[1] + CD[2]*CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[y]][,12]  + CD[3]*CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                           "n" = c(sapply(1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[y]][,12]))]))))











#anyown unweighted CobDou


CobDou.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CobDou.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(CobDou.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CobDou.List[["Byanyown"]][["CompanyList"]])) {CobDou.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CobDou.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(CobDou.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CobDou.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(CobDou.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CobDou.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {CobDou.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(CobDou.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CobDou.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CobDou.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CobDou.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- CobDou.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CobDou.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- CobDou.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]])) {
  Temp1 <- CobDou.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[i]]) == 0 ) {CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]])) {
  for (i in 1:nrow(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]])) {
      CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]][i,j])) & CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]][i,1] %in% CobDou.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]])) {
  Temp1 <- CobDou.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[i]]) == 0 ) {CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]])) {
  for (i in 1:nrow(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]])) {
      CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]][i,j])) & CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]][i,1] %in% CobDou.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]][i,j], NA)
    }}}




CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]])) {
  Temp1 <- CobDou.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[i]]) == 0 ) {CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]])) {
  for (i in 1:nrow(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]])) {
      CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]][i,j])) & CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]][i,1] %in% CobDou.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]][i,j], NA)
    }}}





for (x in 1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]])) {
  CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]]$sum[CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]]$sum[CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]]$sum == 0] <- NA
  CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]]$sum[CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]]$sum == 0] <- NA
  
  CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]]$LProf <- sapply(1:nrow(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[x]][y,12])
  CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]]$CProf <- sapply(1:nrow(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[x]][y,12])
}



CobDou.List[["Byanyown"]][["CobDouunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                           "LProf" = c(sapply(1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[y]][,13], na.rm = TRUE))), 
                                                           "CProf" = c(sapply(1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[y]][,13], na.rm = TRUE))), 
                                                           "EBT" = c(sapply(1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))), 
                                                           "EBTCD" = c(sapply(1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]),function(y) mean(CD[1] + CD[2]*CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[y]][,12]  + CD[3]*CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                           "n" = c(sapply(1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[y]][,12]))]))))








#intermed unweighted LProf


CobDou.List[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CobDou.List[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(CobDou.List[["Byintermed"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CobDou.List[["Byintermed"]][["CompanyList"]])) {CobDou.List[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CobDou.List[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(CobDou.List[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CobDou.List[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(CobDou.List[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CobDou.List[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {CobDou.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(CobDou.List[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CobDou.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CobDou.List[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CobDou.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- CobDou.List[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CobDou.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- CobDou.List[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]])) {
  Temp1 <- CobDou.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[i]]) == 0 ) {CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]])) {
  for (i in 1:nrow(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]])) {
      CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]][i,j])) & CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]][i,1] %in% CobDou.List[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]])) {
  Temp1 <- CobDou.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[i]]) == 0 ) {CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]])) {
  for (i in 1:nrow(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]])) {
      CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]][i,j])) & CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]][i,1] %in% CobDou.List[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]][i,j], NA)
    }}}




CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]])) {
  Temp1 <- CobDou.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[i]]) == 0 ) {CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]])) {
  for (i in 1:nrow(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]])) {
      CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]][i,j])) & CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]][i,1] %in% CobDou.List[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]][i,j], NA)
    }}}





for (x in 1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]])) {
  CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]]$sum[CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]]$sum[CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]]$sum == 0] <- NA
  CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]]$sum[CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]]$sum == 0] <- NA
  
  CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]]$LProf <- sapply(1:nrow(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[x]][y,12])
  CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]]$CProf <- sapply(1:nrow(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[x]][y,12])
}



CobDou.List[["Byintermed"]][["CobDouunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                              "LProf" = c(sapply(1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[y]][,13], na.rm = TRUE))), 
                                                              "CProf" = c(sapply(1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[y]][,13], na.rm = TRUE))), 
                                                              "EBT" = c(sapply(1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))), 
                                                              "EBTCD" = c(sapply(1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]),function(y) mean(CD[1] + CD[2]*CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[y]][,12]  + CD[3]*CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                              "n" = c(sapply(1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[y]][,12]))]))))





#LProf unweighted Loops


CobDou.List[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CobDou.List[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(CobDou.List[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(CobDou.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(CobDou.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]] <- CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(CobDou.List[["Loop"]][["CompanyList"]])) {CobDou.List[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CobDou.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(CobDou.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CobDou.List[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]] <- CobDou.List[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]])) {
  Temp1 <- CobDou.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[i]]) == 0 ) {CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]])) {
  for (i in 1:nrow(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]])) {
      CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]][i,j])) & CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]][i,1] %in% CobDou.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



CobDou.List[["Loop"]][["CobDouunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Loop"]][["CobDouunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]])) {
  Temp1 <- CobDou.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[i]]) == 0 ) {CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]])) {
  for (i in 1:nrow(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]])) {
      CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]][i,j])) & CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]][i,1] %in% CobDou.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]][i,j], NA)
    }}}


CobDou.List[["Loop"]][["CobDouunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Loop"]][["CobDouunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Loop"]][["CobDouunweightedAssets"]])) {
  Temp1 <- CobDou.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[i]]) == 0 ) {CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Loop"]][["CobDouunweightedAssets"]])) {
  for (i in 1:nrow(CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]])) {
      CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]][i,j])) & CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]][i,1] %in% CobDou.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]][i,j], NA)
    }}}







for (x in 1:length(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]])) {
  CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]]$sum[CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]]$sum[CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]]$sum == 0] <- NA
  CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]]$sum[CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]]$sum == 0] <- NA
  
  CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]]$LProf <- sapply(1:nrow(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[x]][y,12])
  CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]]$CProf <- sapply(1:nrow(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[x]][y,12])
}



CobDou.List[["Loop"]][["CobDouunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                                "LProf" = c(sapply(1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[y]][,13], na.rm = TRUE))), 
                                                                "CProf" = c(sapply(1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[y]][,13], na.rm = TRUE))), 
                                                                "EBT" = c(sapply(1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))), 
                                                                "EBTCD" = c(sapply(1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]]),function(y) mean(CD[1] + CD[2]*CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[y]][,12]  + CD[3]*CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                                "n" = c(sapply(1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[y]][,12]))]))))


#anysub unweighted LProf


CobDou.List[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CobDou.List[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(CobDou.List[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CobDou.List[["Byanysub"]][["CompanyList"]])) {CobDou.List[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CobDou.List[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(CobDou.List[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(CobDou.List[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CobDou.List[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {CobDou.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(CobDou.List[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CobDou.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CobDou.List[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CobDou.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- CobDou.List[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CobDou.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- CobDou.List[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]])) {
  Temp1 <- CobDou.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[i]]) == 0 ) {CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]])) {
  for (i in 1:nrow(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]])) {
      CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]][i,j])) & CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]][i,1] %in% CobDou.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]])) {
  Temp1 <- CobDou.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[i]]) == 0 ) {CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]])) {
  for (i in 1:nrow(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]])) {
      CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]][i,j])) & CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]][i,1] %in% CobDou.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]][i,j], NA)
    }}}


CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]])) {
  Temp1 <- CobDou.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[i]]) == 0 ) {CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]])) {
  for (i in 1:nrow(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]])) {
      CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]][i,j])) & CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]][i,1] %in% CobDou.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]][i,j], NA)
    }}}







for (x in 1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]])) {
  CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]]$sum[CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]]$sum[CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]]$sum == 0] <- NA
  CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]]), function (y) sum(as.numeric(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]]$sum[CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]]$sum == 0] <- NA
  
  CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]]$LProf <- sapply(1:nrow(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[x]][y,12])
  CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]]$CProf <- sapply(1:nrow(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[x]][y,12])
}



CobDou.List[["Byanysub"]][["CobDouunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                          "LProf" = c(sapply(1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[y]][,13], na.rm = TRUE))), 
                                                          "CProf" = c(sapply(1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[y]][,13], na.rm = TRUE))), 
                                                          "EBT" = c(sapply(1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))), 
                                                          "EBTCD" = c(sapply(1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]),function(y) mean(CD[1] + CD[2]*CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[y]][,12]  + CD[3]*CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                          "n" = c(sapply(1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[y]][,12]))]))))



#anysubGER unweighted LProf


CobDou.List[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CobDou.List[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(CobDou.List[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CobDou.List[["ByanysubGER"]][["CompanyList"]])) {CobDou.List[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(CobDou.List[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(CobDou.List[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    CobDou.List[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- CobDou.List[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(CobDou.List[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(CobDou.List[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]])) {
  Temp1 <- CobDou.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[i]]) == 0 ) {CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]])) {
  for (i in 1:nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]])) {
      CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]][i,j])) & CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]][i,1] %in% CobDou.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]])) {
  Temp1 <- CobDou.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[i]]) == 0 ) {CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]])) {
  for (i in 1:nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]])) {
      CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]][i,j])) & CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]][i,1] %in% CobDou.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]][i,j], NA)
    }}}




CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]])) {
  Temp1 <- CobDou.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CobDou.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[i]]) == 0 ) {CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]])) {
  for (i in 1:nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]])) {
      CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]][i,j])) & CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]][i,1] %in% CobDou.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]][i,j], NA)
    }}}






for (x in 1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]])) {
  CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]]$sum[CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]]), function (y) sum(as.numeric(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]]$sum[CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]]$sum == 0] <- NA
  CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]]), function (y) sum(as.numeric(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]][y,2:11]), na.rm = TRUE))
  CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]]$sum[CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]]$sum == 0] <- NA
  
  CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]]$LProf <- sapply(1:nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[x]][y,12])
  CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]]$CProf <- sapply(1:nrow(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]]), function (y) CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[x]][[y,12]] / CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[x]][y,12])
}



CobDou.List[["ByanysubGER"]][["CobDouunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                              "LProf" = c(sapply(1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[y]][,13], na.rm = TRUE))), 
                                                              "CProf" = c(sapply(1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[y]][,13], na.rm = TRUE))), 
                                                              "EBT" = c(sapply(1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]),function(y) mean(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))), 
                                                              "EBTCD" = c(sapply(1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]),function(y) mean(CD[1] + CD[2]*CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[y]][,12]  + CD[3]*CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                              "n" = c(sapply(1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[y]][,12]))]))))





#Affiliates  unweighted CobDou


CobDou.List[["Affiliates"]][["CobDouunweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                              "LProf" = mean(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]))[,12] / unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]))[,12], na.rm = TRUE), 
                                                              "CProf" = mean(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]))[,12] / unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]))[,12], na.rm = TRUE), 
                                                              "EBT"   = mean(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]))[,12], na.rm = TRUE), 
                                                              "EBTCD" = mean(   CD[1] +  CD[2] * unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]))[,12] + CD[3] * unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]))[,12],na.rm = TRUE),
                                                              "n" = length(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]))[,12][!is.na(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]))[,12])]))







#GerGUO unweighted CobDou



CobDou.List[["GerGUO"]][["CobDouunweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                                "LProf" = mean(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]))[,12] / unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]]))[,12], na.rm = TRUE), 
                                                                "CProf" = mean(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]))[,12] / unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]]))[,12], na.rm = TRUE), 
                                                                "EBT"   = mean(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]))[,12], na.rm = TRUE), 
                                                                "EBTCD" = mean(   CD[1] +  CD[2] * unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]]))[,12] + CD[3] * unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]]))[,12],na.rm = TRUE),
                                                                   "n" = length(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]))[,12][!is.na(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]))[,12])]))


#Domestic firms weighted LProf


CobDou.List[["DeDom"]][["CobDouweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                           "LProf" = sum(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][,12], na.rm = TRUE) / sum(CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]][,12], na.rm = TRUE),
                                                           "CProf" = sum(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][,12], na.rm = TRUE) / sum(CobDou.List[["DeDom"]][["CobDouunweightedAssets"]][,12], na.rm = TRUE),
                                                           "EBT" = sum(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]][,12], na.rm = TRUE),
                                                           "EBTCD" = sum(CD[1] + CD[2]*CobDou.List[["DeDom"]][["CobDouunweightedAssets"]]$sum + CD[3]*CobDou.List[["DeDom"]][["CobDouunweightedEmployeeCost"]]$sum, na.rm = TRUE),
                                                           "n" = length(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]]$sum[!is.na(as.numeric(CobDou.List[["DeDom"]][["CobDouunweightedEBT"]]$sum))])
)






#International firms weighted LProf


CobDou.List[["DeInt"]][["CobDouweighted"]] <- data.frame("ISO" = "DEINT", 
                                                         "LProf" = sum(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][,12], na.rm = TRUE) / sum(CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]][,12], na.rm = TRUE),
                                                         "CProf" = sum(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][,12], na.rm = TRUE) / sum(CobDou.List[["DeInt"]][["CobDouunweightedAssets"]][,12], na.rm = TRUE),
                                                         "EBT" = sum(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]][,12], na.rm = TRUE),
                                                         "EBTCD" = sum(CD[1] + CD[2]*CobDou.List[["DeInt"]][["CobDouunweightedAssets"]]$sum + CD[3]*CobDou.List[["DeInt"]][["CobDouunweightedEmployeeCost"]]$sum, na.rm = TRUE),
                                                         "n" = length(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]]$sum[!is.na(as.numeric(CobDou.List[["DeInt"]][["CobDouunweightedEBT"]]$sum))])
)



#CSH firms weighted LProf 

CobDou.List[["ByCSH"]][["CobDouweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                         "LProf" = c(sapply(1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "CProf" = c(sapply(1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "EBT" = c(sapply(1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))),
                                                         "EBTCD" = c(sapply(1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]), function(y) sum(CD[1] + CD[2] * CobDou.List[["ByCSH"]][["CobDouunweightedAssets"]][[y]][,12] + CD[3] * CobDou.List[["ByCSH"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]]), function(y) length(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["ByCSH"]][["CobDouunweightedEBT"]][[y]][,12]))])))
)







#GUO firms weighted LProf 

CobDou.List[["ByGUO"]][["CobDouweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                         "LProf" = c(sapply(1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "CProf" = c(sapply(1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "EBT" = c(sapply(1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))),
                                                         "EBTCD" = c(sapply(1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]), function(y) sum(CD[1] + CD[2] * CobDou.List[["ByGUO"]][["CobDouunweightedAssets"]][[y]][,12] + CD[3] * CobDou.List[["ByGUO"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["ByGUO"]][["CobDouunweightedEBT"]][[y]][,12]))])))
)



#anyown firms weighted LProf 

CobDou.List[["Byanyown"]][["CobDouweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                         "LProf" = c(sapply(1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "CProf" = c(sapply(1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "EBT" = c(sapply(1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))),
                                                         "EBTCD" = c(sapply(1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]), function(y) sum(CD[1] + CD[2] * CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][[y]][,12] + CD[3] * CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][[y]][,12]))])))
)



#intermed firms weighted LProf 

CobDou.List[["Byintermed"]][["CobDouweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                         "LProf" = c(sapply(1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "CProf" = c(sapply(1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "EBT" = c(sapply(1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))),
                                                         "EBTCD" = c(sapply(1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]), function(y) sum(CD[1] + CD[2] * CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][[y]][,12] + CD[3] * CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE)),
                                                          "n" = c(sapply(1:length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][[y]][,12]))]))))
)



#Loops firms weighted LProf 

CobDou.List[["Loop"]][["CobDouweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                         "LProf" = c(sapply(1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "CProf" = c(sapply(1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "EBT" = c(sapply(1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Loop"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))),
                                                         "EBTCD" = c(sapply(1:length(CobDou.List[["Loop"]][["CobDouunweightedEBT"]]), function(y) sum(CD[1] + CD[2] * CobDou.List[["Loop"]][["CobDouunweightedAssets"]][[y]][,12] + CD[3] * CobDou.List[["Loop"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(CobDou.List[["ByLoop"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["ByLoop"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["ByLoop"]][["CobDouunweightedEBT"]][[y]][,12]))])))
)




#anysub firms weighted LProf 

CobDou.List[["Byanysub"]][["CobDouweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                         "LProf" = c(sapply(1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "CProf" = c(sapply(1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "EBT" = c(sapply(1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))),
                                                         "EBTCD" = c(sapply(1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]), function(y) sum(CD[1] + CD[2] * CobDou.List[["Byanysub"]][["CobDouunweightedAssets"]][[y]][,12] + CD[3] * CobDou.List[["Byanysub"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["Byanysub"]][["CobDouunweightedEBT"]][[y]][,12]))])))
)



#anysubGER firms weighted LProf 

CobDou.List[["ByanysubGER"]][["CobDouweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                         "LProf" = c(sapply(1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "CProf" = c(sapply(1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "EBT" = c(sapply(1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]),function(y) sum(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[y]][,12], na.rm = TRUE))),
                                                         "EBTCD" = c(sapply(1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]), function(y) sum(CD[1] + CD[2] * CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]][[y]][,12] + CD[3] * CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]),function(y) length(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[y]][,12][!is.na(as.numeric(CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]][[y]][,12]))])))
)



#Affiliates  weighted LProf



CobDou.List[["Affiliates"]][["CobDouweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                         "LProf" = sum(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]))[,12], na.rm = TRUE) / sum(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]))[,12], na.rm = TRUE),
                                                         "CProf" = sum(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]))[,12], na.rm = TRUE) / sum(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]))[,12], na.rm = TRUE),
                                                         "EBT" = sum(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]))[,12], na.rm = TRUE),
                                                         "EBTCD" = sum(CD[1] + CD[2]*unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]))$sum + CD[3]*unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]))$sum, na.rm = TRUE),
                                                         "n" = length(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]))$sum[!is.na(as.numeric(unique(Reduce("rbind", CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]))$sum))])
)





#GerGUO weighted CobDou


CobDou.List[["GerGUO"]][["CobDouweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                              "LProf" = sum(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]))[,12], na.rm = TRUE) / sum(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]]))[,12], na.rm = TRUE),
                                                              "CProf" = sum(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]))[,12], na.rm = TRUE) / sum(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]]))[,12], na.rm = TRUE),
                                                              "EBT" = sum(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]))[,12], na.rm = TRUE),
                                                              "EBTCD" = sum(CD[1] + CD[2]*unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedAssets"]]))$sum + CD[3]*unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEmployeeCost"]]))$sum, na.rm = TRUE),
                                                              "n" = length(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]))$sum[!is.na(as.numeric(unique(Reduce("rbind", CobDou.List[["ByanysubGER"]][["CobDouunweightedEBT"]]))$sum))])
)





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, EBT Havens and EU EBT havens to anyown

CobDou.List[["Byanyown"]][["CobDouunweighted"]] <- rbind(CobDou.List[["Byanyown"]][["CobDouunweighted"]], 
                                                       
                                                       data.frame("ISO" = "TaxHavens", 
                                                                  "LProf" = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% Taxhavens])[,12]) / unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]) %in% Taxhavens])[,12]), na.rm = TRUE), 
                                                                  "CProf" = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% Taxhavens])[,12]) / unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]) %in% Taxhavens])[,12]), na.rm = TRUE), 
                                                                  "EBT" = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% Taxhavens])[,12]), na.rm = TRUE),
                                                                  "EBTCD" = mean(CD[1] + CD[2]*unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]) %in% Taxhavens]))$sum + CD[3]*unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]) %in% Taxhavens]))$sum, na.rm = TRUE),
                                                                  "n" = length(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% Taxhavens])$sum[!is.na(as.numeric(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% Taxhavens])$sum)))]))
                                                       ))



CobDou.List[["Byanyown"]][["CobDouunweighted"]] <- rbind(CobDou.List[["Byanyown"]][["CobDouunweighted"]], 
                                                         
                                                         data.frame("ISO" = "TaxHavensEU", 
                                                                    "LProf" = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU])[,12]) / unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE), 
                                                                    "CProf" = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU])[,12]) / unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE), 
                                                                    "EBT" = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE),
                                                                    "EBTCD" = mean(CD[1] + CD[2]*unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]) %in% TaxhavensEU]))$sum + CD[3]*unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]) %in% TaxhavensEU]))$sum, na.rm = TRUE),
                                                                    "n" = length(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU])$sum[!is.na(as.numeric(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU])$sum)))]))
                                                         ))



CobDou.List[["Byanyown"]][["CobDouunweighted"]] <- rbind(CobDou.List[["Byanyown"]][["CobDouunweighted"]], 
                                                         
                                                         data.frame("ISO" = "TaxHavensEUProxy", 
                                                                    "LProf" = mean(unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU])[,12]) / unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE), 
                                                                    "CProf" = mean(unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU])[,12]) / unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE), 
                                                                    "EBT" = mean(unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE),
                                                                    "EBTCD" = mean(CD[1] + CD[2]*unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]]) %in% TaxhavensEU]))$sum + CD[3]*unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]]) %in% TaxhavensEU]))$sum, na.rm = TRUE),
                                                                    "n" = length(unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU])$sum[!is.na(as.numeric(unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU])$sum)))]))
                                                         ))



CobDou.List[["Byanyown"]][["CobDouunweighted"]] <- rbind(CobDou.List[["Byanyown"]][["CobDouunweighted"]], 
                                                       CobDou.List[["DeInt"]][["CobDouunweighted"]],
                                                       CobDou.List[["DeDom"]][["CobDouunweighted"]],
                                                       CobDou.List[["Affiliates"]][["CobDouunweighted"]],
                                                       CobDou.List[["GerGUO"]][["CobDouunweighted"]]
)







## Append rows for DeInt, DeDom, Affiliates, GermanGUO, EBT Havens and EU EBT havens to anyown


CobDou.List[["Byanyown"]][["CobDouweighted"]] <- rbind(CobDou.List[["Byanyown"]][["CobDouweighted"]],
                                                       data.frame("ISO" = "TaxHavens", 
                                                                  "LProf" = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% Taxhavens]))[,12] / unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]) %in% Taxhavens]))[,12], na.rm = TRUE), 
                                                                  "CProf" = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% Taxhavens]))[,12] / unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]) %in% Taxhavens]))[,12], na.rm = TRUE), 
                                                                  "EBT"   = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% Taxhavens]))[,12], na.rm = TRUE), 
                                                                  "EBTCD" = mean(   CD[1] +  CD[2] * unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]) %in% Taxhavens]))[,12] + CD[3] * unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]) %in% Taxhavens]))[,12],na.rm = TRUE),
                                                                  "n" = length(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% Taxhavens]))[,12][!is.na(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% Taxhavens]))[,12])]))
)




CobDou.List[["Byanyown"]][["CobDouweighted"]] <- rbind(CobDou.List[["Byanyown"]][["CobDouweighted"]],
                                                        data.frame("ISO" = "TaxHavensEU", 
                                                                "LProf" = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU]))[,12] / unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]) %in% TaxhavensEU]))[,12], na.rm = TRUE), 
                                                                "CProf" = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU]))[,12] / unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]) %in% TaxhavensEU]))[,12], na.rm = TRUE), 
                                                                "EBT"   = mean(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU]))[,12], na.rm = TRUE), 
                                                                "EBTCD" = mean(   CD[1] +  CD[2] * unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedAssets"]]) %in% TaxhavensEU]))[,12] + CD[3] * unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEmployeeCost"]]) %in% TaxhavensEU]))[,12],na.rm = TRUE),
                                                                "n" = length(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU]))[,12][!is.na(unique(Reduce("rbind",CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byanyown"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU]))[,12])]))
)


CobDou.List[["Byanyown"]][["CobDouweighted"]] <- rbind(CobDou.List[["Byanyown"]][["CobDouweighted"]],
                                                       data.frame("ISO" = "TaxHavensEUProxy", 
                                                                  "LProf" = mean(unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU]))[,12] / unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]]) %in% TaxhavensEU]))[,12], na.rm = TRUE), 
                                                                  "CProf" = mean(unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU]))[,12] / unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]]) %in% TaxhavensEU]))[,12], na.rm = TRUE), 
                                                                  "EBT"   = mean(unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU]))[,12], na.rm = TRUE), 
                                                                  "EBTCD" = mean(   CD[1] +  CD[2] * unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedAssets"]]) %in% TaxhavensEU]))[,12] + CD[3] * unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEmployeeCost"]]) %in% TaxhavensEU]))[,12],na.rm = TRUE),
                                                                  "n" = length(unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU]))[,12][!is.na(unique(Reduce("rbind",CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]][names(CobDou.List[["Byintermed"]][["CobDouunweightedEBT"]]) %in% TaxhavensEU]))[,12])]))
)







CobDou.List[["Byanyown"]][["CobDouweighted"]] <- rbind(CobDou.List[["Byanyown"]][["CobDouweighted"]], 
                                                     CobDou.List[["DeInt"]][["CobDouweighted"]],
                                                     CobDou.List[["DeDom"]][["CobDouweighted"]],
                                                     CobDou.List[["Affiliates"]][["CobDouweighted"]],
                                                     CobDou.List[["GerGUO"]][["CobDouweighted"]]
)






for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, LProf, g, h, i, ISO, j, x, y , z)









