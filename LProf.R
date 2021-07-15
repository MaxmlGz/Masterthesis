

EBT <- rio::import("ImportEBT.xlsx", which = "Results")
EBT <- cbind(data.frame("CompanyBVDID" = c(EBT$`BvD ID number`)),EBT[,4:13])

EmployeeCost <- rio::import("ImportEC.xlsx", which = "Results")
EmployeeCost <- cbind(data.frame("CompanyBVDID" = c(EmployeeCost$`BvD ID number`)),EmployeeCost[,4:13])

BvDIDNorm <- intersect(EBT$CompanyBVDID, EmployeeCost$CompanyBVDID)
BvDIDNorm <- data.frame("CompanyBVDID" = c(BvDIDNorm))

EBT <- left_join(BvDIDNorm, EBT, by = "CompanyBVDID")
EmployeeCost <- left_join(BvDIDNorm, EmployeeCost, by = "CompanyBVDID")



##equal out samples

for(i in 1:nrow(EmployeeCost)) {
  for (j in 2:ncol(EmployeeCost)) {
    EmployeeCost[i,j] <- ifelse(!is.na(as.numeric(EmployeeCost[i,j])) & !is.na(as.numeric(EBT[i,j])),  as.numeric(EmployeeCost[i,j]) , NA )
  }
}


for(i in 1:nrow(EBT)) {
  for (j in 2:ncol(EBT)) {
    
    EBT[i,j] <- ifelse(!is.na(as.numeric(EBT[i,j])) & !is.na(as.numeric(EmployeeCost[i,j])),  as.numeric(EBT[i,j]) , NA )
    
  }
}





## Drop if <3 obs


EmployeeCost <- EmployeeCost[apply(EmployeeCost,1,function (z) length(z[!is.na(as.numeric(z))]) > 2),]
EBT <- EBT[EBT$CompanyBVDID %in% EmployeeCost$CompanyBVDID,]






LProf.List <- vector(mode = "list")
LProf.List[[1]] <- vector(mode = "list")
names(LProf.List) <- "ByCSH"


#Domestic firms unweightet LProf


LProf.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProf.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(LProf.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(LProf.List[["DeDom"]][["CompanyList"]])) {
  LProf.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(LProf.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- LProf.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(LProf.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(LProf.List[["DeDom"]][["CompanyList"]][[i]])))
}

LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)

for (i in 1:nrow(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]])) {
  for (j in 2:ncol(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]])) {
    LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]][i,j])) & LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]][i,1] %in% LProf.List[["DeDom"]][["CompanyList"]][[(j-1)]], LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]][i,j], NA)
  }}



for (i in 1:length(LProf.List[["DeDom"]][["CompanyList"]])) {
  LProf.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(LProf.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- LProf.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(LProf.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(LProf.List[["DeDom"]][["CompanyList"]][[i]])))
}


LProf.List[["DeDom"]][["LProfunweightedEBT"]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(LProf.List[["DeDom"]][["LProfunweightedEBT"]])) {
  for (j in 2:ncol(LProf.List[["DeDom"]][["LProfunweightedEBT"]])) {
    LProf.List[["DeDom"]][["LProfunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["DeDom"]][["LProfunweightedEBT"]][i,j])) & LProf.List[["DeDom"]][["LProfunweightedEBT"]][i,1] %in% LProf.List[["DeDom"]][["CompanyList"]][[(j-1)]], LProf.List[["DeDom"]][["LProfunweightedEBT"]][i,j], NA)
  }}


for(i in 1:nrow(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]])) {
  for(j in 2:ncol(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]])) {
    LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]][i,j])) & !is.na(as.numeric(LProf.List[["DeDom"]][["LProfunweightedEBT"]][i,j])) ,  as.numeric(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]][i,j])  , NA  )
  }}


for(i in 1:nrow(LProf.List[["DeDom"]][["LProfunweightedEBT"]])) {
  for(j in 2:ncol(LProf.List[["DeDom"]][["LProfunweightedEBT"]])) {
    LProf.List[["DeDom"]][["LProfunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]][i,j])) & !is.na(as.numeric(LProf.List[["DeDom"]][["LProfunweightedEBT"]][i,j])) ,  as.numeric(LProf.List[["DeDom"]][["LProfunweightedEBT"]][i,j])  , NA  )
  }}



LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]]$sum <- sapply(1:nrow(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]]), function (y) sum(as.numeric(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]][y,2:11]) , na.rm = TRUE ))
LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]]$sum[LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]]$sum == 0] <- NA
LProf.List[["DeDom"]][["LProfunweightedEBT"]]$sum <- sapply(1:nrow(LProf.List[["DeDom"]][["LProfunweightedEBT"]]), function (y) sum(as.numeric(LProf.List[["DeDom"]][["LProfunweightedEBT"]][y,2:11]), na.rm = TRUE))
LProf.List[["DeDom"]][["LProfunweightedEBT"]]$sum[LProf.List[["DeDom"]][["LProfunweightedEBT"]]$sum == 0] <- NA
LProf.List[["DeDom"]][["LProfunweightedEBT"]]$LProf <- sapply(1:nrow(LProf.List[["DeDom"]][["LProfunweightedEBT"]]), function (y) LProf.List[["DeDom"]][["LProfunweightedEBT"]][[y,12]] / LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]][[y,12]])



LProf.List[["DeDom"]][["LProfunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                         "LProf" = mean(LProf.List[["DeDom"]][["LProfunweightedEBT"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(LProf.List[["DeDom"]][["LProfunweightedEBT"]][,13], na.rm = TRUE),
                                                         "n" = length(LProf.List[["DeDom"]][["LProfunweightedEBT"]][,13][!is.na(as.numeric(LProf.List[["DeDom"]][["LProfunweightedEBT"]][,13]))]))

LProf.List[["DeDom"]][["LProfunweighted"]]$low95 <- LProf.List[["DeDom"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["DeDom"]][["LProfunweighted"]]$n-1) * LProf.List[["DeDom"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["DeDom"]][["LProfunweighted"]]$n)
LProf.List[["DeDom"]][["LProfunweighted"]]$high95 <- LProf.List[["DeDom"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["DeDom"]][["LProfunweighted"]]$n-1) * LProf.List[["DeDom"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["DeDom"]][["LProfunweighted"]]$n)

LProf.List[["DeDom"]][["LProfunweighted"]] <- LProf.List[["DeDom"]][["LProfunweighted"]][!is.na(LProf.List[["DeDom"]][["LProfunweighted"]]$LProf),]


#International firms unweightet LProf


LProf.List[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProf.List[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(LProf.List[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(LProf.List[["DeInt"]][["CompanyList"]])) {
  LProf.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(LProf.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- LProf.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(LProf.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(LProf.List[["DeInt"]][["CompanyList"]][[i]])))
}

LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)

for (i in 1:nrow(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]])) {
  for (j in 2:ncol(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]])) {
    LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]][i,j])) & LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]][i,1] %in% LProf.List[["DeInt"]][["CompanyList"]][[(j-1)]], LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]][i,j], NA)
  }}



for (i in 1:length(LProf.List[["DeInt"]][["CompanyList"]])) {
  LProf.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(LProf.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- LProf.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(LProf.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(LProf.List[["DeInt"]][["CompanyList"]][[i]])))
}


LProf.List[["DeInt"]][["LProfunweightedEBT"]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(LProf.List[["DeInt"]][["LProfunweightedEBT"]])) {
  for (j in 2:ncol(LProf.List[["DeInt"]][["LProfunweightedEBT"]])) {
    LProf.List[["DeInt"]][["LProfunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["DeInt"]][["LProfunweightedEBT"]][i,j])) & LProf.List[["DeInt"]][["LProfunweightedEBT"]][i,1] %in% LProf.List[["DeInt"]][["CompanyList"]][[(j-1)]], LProf.List[["DeInt"]][["LProfunweightedEBT"]][i,j], NA)
  }}


for(i in 1:nrow(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]])) {
  for(j in 2:ncol(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]])) {
    LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]][i,j])) & !is.na(as.numeric(LProf.List[["DeInt"]][["LProfunweightedEBT"]][i,j])) ,  as.numeric(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]][i,j])  , NA  )
  }}


for(i in 1:nrow(LProf.List[["DeInt"]][["LProfunweightedEBT"]])) {
  for(j in 2:ncol(LProf.List[["DeInt"]][["LProfunweightedEBT"]])) {
    LProf.List[["DeInt"]][["LProfunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]][i,j])) & !is.na(as.numeric(LProf.List[["DeInt"]][["LProfunweightedEBT"]][i,j])) ,  as.numeric(LProf.List[["DeInt"]][["LProfunweightedEBT"]][i,j])  , NA  )
  }}


LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]]$sum <- sapply(1:nrow(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]]), function (y) sum(as.numeric(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]][y,2:11]) , na.rm = TRUE ))
LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]]$sum[LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]]$sum == 0] <- NA
LProf.List[["DeInt"]][["LProfunweightedEBT"]]$sum <- sapply(1:nrow(LProf.List[["DeInt"]][["LProfunweightedEBT"]]), function (y) sum(as.numeric(LProf.List[["DeInt"]][["LProfunweightedEBT"]][y,2:11]), na.rm = TRUE))
LProf.List[["DeInt"]][["LProfunweightedEBT"]]$sum[LProf.List[["DeInt"]][["LProfunweightedEBT"]]$sum == 0] <- NA
LProf.List[["DeInt"]][["LProfunweightedEBT"]]$LProf <- sapply(1:nrow(LProf.List[["DeInt"]][["LProfunweightedEBT"]]), function (y) LProf.List[["DeInt"]][["LProfunweightedEBT"]][[y,12]] / LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]][[y,12]])


LProf.List[["DeInt"]][["LProfunweighted"]] <- data.frame("ISO" = "DeInt", 
                                                         "LProf" = mean(LProf.List[["DeInt"]][["LProfunweightedEBT"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(LProf.List[["DeInt"]][["LProfunweightedEBT"]][,13], na.rm = TRUE),
                                                         "n" = length(LProf.List[["DeInt"]][["LProfunweightedEBT"]][,13][!is.na(as.numeric(LProf.List[["DeInt"]][["LProfunweightedEBT"]][,13]))]))

LProf.List[["DeInt"]][["LProfunweighted"]]$low95 <- LProf.List[["DeInt"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["DeInt"]][["LProfunweighted"]]$n-1) * LProf.List[["DeInt"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["DeInt"]][["LProfunweighted"]]$n)
LProf.List[["DeInt"]][["LProfunweighted"]]$high95 <- LProf.List[["DeInt"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["DeInt"]][["LProfunweighted"]]$n-1) * LProf.List[["DeInt"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["DeInt"]][["LProfunweighted"]]$n)

LProf.List[["DeInt"]][["LProfunweighted"]] <- LProf.List[["DeInt"]][["LProfunweighted"]][!is.na(LProf.List[["DeInt"]][["LProfunweighted"]]$LProf),]



#CSH unweighted LProf


LProf.List[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProf.List[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(LProf.List[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProf.List[["ByCSH"]][["CompanyList"]])) {LProf.List[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(LProf.List[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(LProf.List[["ByCSH"]][["CompanyList"]][[i]])) {
    LProf.List[["ByCSH"]][["CompanyList"]][[i]][[j]] <- LProf.List[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(LProf.List[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(LProf.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(LProf.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProf.List[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]])) {
  Temp1 <- LProf.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[i]]) == 0 ) {LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]][i,1] %in% LProf.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProf.List[["ByCSH"]][["LProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["ByCSH"]][["LProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]])) {
  Temp1 <- LProf.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[i]]) == 0 ) {LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]])) {
  for (i in 1:nrow(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]][i,j])) & LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]][i,1] %in% LProf.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]])) {
  for(i in 1:nrow(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]])) {
  LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]]$sum[LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]]$sum[LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]]$sum == 0] <- NA
  LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]]$LProf <- sapply(1:nrow(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]]), function (y) LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[x]][[y,12]] / LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[x]][y,12])
  

}



LProf.List[["ByCSH"]][["LProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]]),function(y) mean(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]]),function(y) sd(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["ByCSH"]][["LProfunweighted"]]$low95 <- LProf.List[["ByCSH"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["ByCSH"]][["LProfunweighted"]]$n-1) * LProf.List[["ByCSH"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["ByCSH"]][["LProfunweighted"]]$n)
LProf.List[["ByCSH"]][["LProfunweighted"]]$high95 <- LProf.List[["ByCSH"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["ByCSH"]][["LProfunweighted"]]$n-1) * LProf.List[["ByCSH"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["ByCSH"]][["LProfunweighted"]]$n)

LProf.List[["ByCSH"]][["LProfunweighted"]] <- LProf.List[["ByCSH"]][["LProfunweighted"]][!is.na(LProf.List[["ByCSH"]][["LProfunweighted"]]$LProf),]



#GUO unweighted LProf


LProf.List[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProf.List[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(LProf.List[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProf.List[["ByGUO"]][["CompanyList"]])) {LProf.List[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(LProf.List[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(LProf.List[["ByGUO"]][["CompanyList"]][[i]])) {
    LProf.List[["ByGUO"]][["CompanyList"]][[i]][[j]] <- LProf.List[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(LProf.List[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(LProf.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(LProf.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProf.List[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]])) {
  Temp1 <- LProf.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[i]]) == 0 ) {LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]][i,1] %in% LProf.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProf.List[["ByGUO"]][["LProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["ByGUO"]][["LProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]])) {
  Temp1 <- LProf.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[i]]) == 0 ) {LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]])) {
  for (i in 1:nrow(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]][i,j])) & LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]][i,1] %in% LProf.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]])) {
  for(i in 1:nrow(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]])) {
  LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]]$sum[LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]]$sum[LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]]$sum == 0] <- NA
  LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]]$LProf <- sapply(1:nrow(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]]), function (y) LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[x]][[y,12]] / LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[x]][y,12])
  

}


LProf.List[["ByGUO"]][["LProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]]),function(y) mean(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]]),function(y) sd(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["ByGUO"]][["LProfunweighted"]]$low95 <- LProf.List[["ByGUO"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["ByGUO"]][["LProfunweighted"]]$n-1) * LProf.List[["ByGUO"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["ByGUO"]][["LProfunweighted"]]$n)
LProf.List[["ByGUO"]][["LProfunweighted"]]$high95 <- LProf.List[["ByGUO"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["ByGUO"]][["LProfunweighted"]]$n-1) * LProf.List[["ByGUO"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["ByGUO"]][["LProfunweighted"]]$n)

LProf.List[["ByGUO"]][["LProfunweighted"]] <- LProf.List[["ByGUO"]][["LProfunweighted"]][!is.na(LProf.List[["ByGUO"]][["LProfunweighted"]]$LProf),]


#anyown unweighted LProf


LProf.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProf.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(LProf.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProf.List[["Byanyown"]][["CompanyList"]])) {LProf.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(LProf.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(LProf.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(LProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(LProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(LProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {LProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(LProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(LProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProf.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(LProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- LProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    LProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- LProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]])) {
  Temp1 <- LProf.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[i]]) == 0 ) {LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]][i,1] %in% LProf.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProf.List[["Byanyown"]][["LProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]])) {
  Temp1 <- LProf.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[i]]) == 0 ) {LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]])) {
  for (i in 1:nrow(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]][i,j])) & LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]][i,1] %in% LProf.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]])) {
  for(i in 1:nrow(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]])) {
  LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]]$sum[LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]]$sum[LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]]$sum == 0] <- NA
  LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]]$LProf <- sapply(1:nrow(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]]), function (y) LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[x]][[y,12]] / LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[x]][y,12])
  
}


LProf.List[["Byanyown"]][["LProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]),function(y) mean(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]),function(y) sd(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["Byanyown"]][["LProfunweighted"]]$low95 <- LProf.List[["Byanyown"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["Byanyown"]][["LProfunweighted"]]$n-1) * LProf.List[["Byanyown"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Byanyown"]][["LProfunweighted"]]$n)
LProf.List[["Byanyown"]][["LProfunweighted"]]$high95 <- LProf.List[["Byanyown"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["Byanyown"]][["LProfunweighted"]]$n-1) * LProf.List[["Byanyown"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Byanyown"]][["LProfunweighted"]]$n)

LProf.List[["Byanyown"]][["LProfunweighted"]] <- LProf.List[["Byanyown"]][["LProfunweighted"]][!is.na(LProf.List[["Byanyown"]][["LProfunweighted"]]$LProf),]




#intermed unweighted LProf


LProf.List[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProf.List[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(LProf.List[["Byintermed"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProf.List[["Byintermed"]][["CompanyList"]])) {LProf.List[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(LProf.List[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(LProf.List[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(LProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(LProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(LProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {LProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(LProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(LProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProf.List[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(LProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- LProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    LProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- LProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]])) {
  Temp1 <- LProf.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[i]]) == 0 ) {LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]][i,1] %in% LProf.List[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProf.List[["Byintermed"]][["LProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]])) {
  Temp1 <- LProf.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[i]]) == 0 ) {LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]])) {
  for (i in 1:nrow(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]][i,j])) & LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]][i,1] %in% LProf.List[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]])) {
  for(i in 1:nrow(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]])) {
  LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]]$sum[LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]]$sum[LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]]$sum == 0] <- NA
  LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]]$LProf <- sapply(1:nrow(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]]), function (y) LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[x]][[y,12]] / LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[x]][y,12])
  

}


LProf.List[["Byintermed"]][["LProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]),function(y) mean(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                              "sd" = c(sapply(1:length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]),function(y) sd(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                              "n" = c(sapply(1:length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["Byintermed"]][["LProfunweighted"]]$low95 <- LProf.List[["Byintermed"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["Byintermed"]][["LProfunweighted"]]$n-1) * LProf.List[["Byintermed"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Byintermed"]][["LProfunweighted"]]$n)
LProf.List[["Byintermed"]][["LProfunweighted"]]$high95 <- LProf.List[["Byintermed"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["Byintermed"]][["LProfunweighted"]]$n-1) * LProf.List[["Byintermed"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Byintermed"]][["LProfunweighted"]]$n)

LProf.List[["Byintermed"]][["LProfunweighted"]] <- LProf.List[["Byintermed"]][["LProfunweighted"]][!is.na(LProf.List[["Byintermed"]][["LProfunweighted"]]$LProf),]





#LProf unweighted Loops


LProf.List[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProf.List[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(LProf.List[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(LProf.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(LProf.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(LProf.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    LProf.List[["Loop"]][["CompanyList"]][[i]][[j]] <- LProf.List[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(LProf.List[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(LProf.List[["Loop"]][["CompanyList"]])) {LProf.List[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(LProf.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(LProf.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(LProf.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(LProf.List[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(LProf.List[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {LProf.List[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(LProf.List[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(LProf.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProf.List[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(LProf.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- LProf.List[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    LProf.List[["Loop"]][["CompanyList"]][[i]][[j]] <- LProf.List[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]])) {
  Temp1 <- LProf.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[i]]) == 0 ) {LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]][i,1] %in% LProf.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProf.List[["Loop"]][["LProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["Loop"]][["LProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["Loop"]][["LProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["Loop"]][["LProfunweightedEBT"]])) {
  Temp1 <- LProf.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["Loop"]][["LProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["Loop"]][["LProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["Loop"]][["LProfunweightedEBT"]][[i]]) == 0 ) {LProf.List[["Loop"]][["LProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["Loop"]][["LProfunweightedEBT"]])) {
  for (i in 1:nrow(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]][i,j])) & LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]][i,1] %in% LProf.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProf.List[["Loop"]][["LProfunweightedEBT"]])) {
  for(i in 1:nrow(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]])) {
  LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]]$sum[LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]]$sum[LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]]$sum == 0] <- NA
  LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]]$LProf <- sapply(1:nrow(LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]]), function (y) LProf.List[["Loop"]][["LProfunweightedEBT"]][[x]][[y,12]] / LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[x]][y,12])
  

}


LProf.List[["Loop"]][["LProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["Loop"]][["LProfunweightedEBT"]]),function(y) mean(LProf.List[["Loop"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(LProf.List[["Loop"]][["LProfunweightedEBT"]]),function(y) sd(LProf.List[["Loop"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(LProf.List[["Loop"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["Loop"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["Loop"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["Loop"]][["LProfunweighted"]]$low95 <- LProf.List[["Loop"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["Loop"]][["LProfunweighted"]]$n-1) * LProf.List[["Loop"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Loop"]][["LProfunweighted"]]$n)
LProf.List[["Loop"]][["LProfunweighted"]]$high95 <- LProf.List[["Loop"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["Loop"]][["LProfunweighted"]]$n-1) * LProf.List[["Loop"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Loop"]][["LProfunweighted"]]$n)

LProf.List[["Loop"]][["LProfunweighted"]] <- LProf.List[["Loop"]][["LProfunweighted"]][!is.na(LProf.List[["Loop"]][["LProfunweighted"]]$LProf),]


#anysub unweighted LProf


LProf.List[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProf.List[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(LProf.List[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProf.List[["Byanysub"]][["CompanyList"]])) {LProf.List[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(LProf.List[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(LProf.List[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(LProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(LProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {LProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(LProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(LProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProf.List[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(LProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- LProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    LProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- LProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]])) {
  Temp1 <- LProf.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[i]]) == 0 ) {LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]][i,1] %in% LProf.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProf.List[["Byanysub"]][["LProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["Byanysub"]][["LProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]])) {
  Temp1 <- LProf.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[i]]) == 0 ) {LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]])) {
  for (i in 1:nrow(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]][i,j])) & LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]][i,1] %in% LProf.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]])) {
  for(i in 1:nrow(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]])) {
  LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]]$sum[LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]]$sum[LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]]$sum == 0] <- NA
  LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]]$LProf <- sapply(1:nrow(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]]), function (y) LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[x]][[y,12]] / LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[x]][y,12])
  
}

LProf.List[["Byanysub"]][["LProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]]),function(y) mean(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]]),function(y) sd(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["Byanysub"]][["LProfunweighted"]]$low95 <- LProf.List[["Byanysub"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["Byanysub"]][["LProfunweighted"]]$n-1) * LProf.List[["Byanysub"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Byanysub"]][["LProfunweighted"]]$n)
LProf.List[["Byanysub"]][["LProfunweighted"]]$high95 <- LProf.List[["Byanysub"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["Byanysub"]][["LProfunweighted"]]$n-1) * LProf.List[["Byanysub"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Byanysub"]][["LProfunweighted"]]$n)

LProf.List[["Byanysub"]][["LProfunweighted"]] <- LProf.List[["Byanysub"]][["LProfunweighted"]][!is.na(LProf.List[["Byanysub"]][["LProfunweighted"]]$LProf),]


#anysubGER unweighted LProf


LProf.List[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProf.List[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(LProf.List[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProf.List[["ByanysubGER"]][["CompanyList"]])) {LProf.List[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(LProf.List[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(LProf.List[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    LProf.List[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- LProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(LProf.List[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(LProf.List[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]])) {
  Temp1 <- LProf.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[i]]) == 0 ) {LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]][i,1] %in% LProf.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]])) {
  Temp1 <- LProf.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProf.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[i]]) == 0 ) {LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]])) {
  for (i in 1:nrow(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]][i,j])) & LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]][i,1] %in% LProf.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]])) {
      LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]])) {
  for(i in 1:nrow(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]])) {
      LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]])) {
  LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]]$sum[LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
  LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]]$sum[LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]]$sum == 0] <- NA
  LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]]$LProf <- sapply(1:nrow(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]]), function (y) LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[x]][[y,12]] / LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[x]][y,12])
  
}


LProf.List[["ByanysubGER"]][["LProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]),function(y) mean(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                               "sd" = c(sapply(1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]),function(y) sd(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                               "n" = c(sapply(1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["ByanysubGER"]][["LProfunweighted"]]$low95 <- LProf.List[["ByanysubGER"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["ByanysubGER"]][["LProfunweighted"]]$n-1) * LProf.List[["ByanysubGER"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["ByanysubGER"]][["LProfunweighted"]]$n)
LProf.List[["ByanysubGER"]][["LProfunweighted"]]$high95 <- LProf.List[["ByanysubGER"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["ByanysubGER"]][["LProfunweighted"]]$n-1) * LProf.List[["ByanysubGER"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["ByanysubGER"]][["LProfunweighted"]]$n)

LProf.List[["ByanysubGER"]][["LProfunweighted"]] <- LProf.List[["ByanysubGER"]][["LProfunweighted"]][!is.na(LProf.List[["ByanysubGER"]][["LProfunweighted"]]$LProf),]


#Affiliates  unweighted LProf


LProf.List[["Affiliates"]][["LProfunweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                              "LProf" = mean(unique(Reduce("rbind", LProf.List[["Byanyown"]][["LProfunweightedEBT"]]))[,13], na.rm = TRUE), 
                                                              "sd" = sd(unique(Reduce("rbind", LProf.List[["Byanyown"]][["LProfunweightedEBT"]]))[,13], na.rm = TRUE),
                                                              "n" = length(unique(Reduce("rbind", LProf.List[["Byanyown"]][["LProfunweightedEBT"]]))[,13][!is.na(unique(Reduce("rbind", LProf.List[["Byanyown"]][["LProfunweightedEBT"]]))[,13])]))




LProf.List[["Affiliates"]][["LProfunweighted"]]$low95 <- LProf.List[["Affiliates"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["Affiliates"]][["LProfunweighted"]]$n-1) * LProf.List[["Affiliates"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Affiliates"]][["LProfunweighted"]]$n)
LProf.List[["Affiliates"]][["LProfunweighted"]]$high95 <- LProf.List[["Affiliates"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["Affiliates"]][["LProfunweighted"]]$n-1) * LProf.List[["Affiliates"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Affiliates"]][["LProfunweighted"]]$n)




#GerGUO unweighted LProf


LProf.List[["GerGUO"]][["LProfunweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                          "LProf" = mean(unique(Reduce("rbind", LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]))[,13], na.rm = TRUE), 
                                                          "sd" = sd(unique(Reduce("rbind", LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]))[,13], na.rm = TRUE),
                                                          "n" = length(unique(Reduce("rbind", LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]))[,13][!is.na(unique(Reduce("rbind", LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]))[,13])]))




LProf.List[["GerGUO"]][["LProfunweighted"]]$low95 <- LProf.List[["GerGUO"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["GerGUO"]][["LProfunweighted"]]$n-1) * LProf.List[["GerGUO"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["GerGUO"]][["LProfunweighted"]]$n)
LProf.List[["GerGUO"]][["LProfunweighted"]]$high95 <- LProf.List[["GerGUO"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["GerGUO"]][["LProfunweighted"]]$n-1) * LProf.List[["GerGUO"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["GerGUO"]][["LProfunweighted"]]$n)

LProf.List[["GerGUO"]][["LProfunweighted"]] <- LProf.List[["GerGUO"]][["LProfunweighted"]][!is.na(LProf.List[["GerGUO"]][["LProfunweighted"]]$LProf),]




#Domestic firms weighted LProf


LProf.List[["DeDom"]][["LProfweighted"]] <- data.frame("ISO" = "DEDOM")
LProf.List[["DeDom"]][["LProfweighted"]]$LProf <- sum(LProf.List[["DeDom"]][["LProfunweightedEBT"]]$sum, na.rm = TRUE)  / sum(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]]$sum, na.rm = TRUE)
LProf.List[["DeDom"]][["LProfweighted"]]$sd <- sqrt(wtd.var(LProf.List[["DeDom"]][["LProfunweightedEBT"]]$LProf, sqrt(LProf.List[["DeDom"]][["LProfunweightedEmployeeCost"]]$sum^2), na.rm = TRUE ))
LProf.List[["DeDom"]][["LProfweighted"]]$n <- length(LProf.List[["DeDom"]][["LProfunweightedEBT"]]$LProf[!is.na(LProf.List[["DeDom"]][["LProfunweightedEBT"]]$LProf)])
LProf.List[["DeDom"]][["LProfweighted"]]$low95 <- LProf.List[["DeDom"]][["LProfweighted"]]$LProf - qt(0.975, df = LProf.List[["DeDom"]][["LProfweighted"]]$n-1) * LProf.List[["DeDom"]][["LProfweighted"]]$sd / sqrt(LProf.List[["DeDom"]][["LProfweighted"]]$n)
LProf.List[["DeDom"]][["LProfweighted"]]$high95 <- LProf.List[["DeDom"]][["LProfweighted"]]$LProf + qt(0.975, df = LProf.List[["DeDom"]][["LProfweighted"]]$n-1) * LProf.List[["DeDom"]][["LProfweighted"]]$sd / sqrt(LProf.List[["DeDom"]][["LProfweighted"]]$n)


#International firms weighted LProf


LProf.List[["DeInt"]][["LProfweighted"]] <- data.frame("ISO" = "DEINT")
LProf.List[["DeInt"]][["LProfweighted"]]$LProf <- sum(LProf.List[["DeInt"]][["LProfunweightedEBT"]]$sum, na.rm = TRUE)  / sum(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]]$sum, na.rm = TRUE)
LProf.List[["DeInt"]][["LProfweighted"]]$sd <- sqrt(wtd.var(LProf.List[["DeInt"]][["LProfunweightedEBT"]]$LProf, sqrt(LProf.List[["DeInt"]][["LProfunweightedEmployeeCost"]]$sum^2), na.rm = TRUE ))
LProf.List[["DeInt"]][["LProfweighted"]]$n <- length(LProf.List[["DeInt"]][["LProfunweightedEBT"]]$LProf[!is.na(LProf.List[["DeInt"]][["LProfunweightedEBT"]]$LProf)])
LProf.List[["DeInt"]][["LProfweighted"]]$low95 <- LProf.List[["DeInt"]][["LProfweighted"]]$LProf - qt(0.975, df = LProf.List[["DeInt"]][["LProfweighted"]]$n-1) * LProf.List[["DeInt"]][["LProfweighted"]]$sd / sqrt(LProf.List[["DeInt"]][["LProfweighted"]]$n)
LProf.List[["DeInt"]][["LProfweighted"]]$high95 <- LProf.List[["DeInt"]][["LProfweighted"]]$LProf + qt(0.975, df = LProf.List[["DeInt"]][["LProfweighted"]]$n-1) * LProf.List[["DeInt"]][["LProfweighted"]]$sd / sqrt(LProf.List[["DeInt"]][["LProfweighted"]]$n)


#CSH firms weighted LProf 

LProf.List[["ByCSH"]][["LProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]]),function(y) sum(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]]),function(y) sqrt(wtd.var(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[y]][,13], sqrt(LProf.List[["ByCSH"]][["LProfunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["ByCSH"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["ByCSH"]][["LProfweighted"]]$low95 <- LProf.List[["ByCSH"]][["LProfweighted"]]$LProf - qt(0.975, df= LProf.List[["ByCSH"]][["LProfweighted"]]$n-1) * LProf.List[["ByCSH"]][["LProfweighted"]]$sd /sqrt(LProf.List[["ByCSH"]][["LProfweighted"]]$n)
LProf.List[["ByCSH"]][["LProfweighted"]]$high95 <- LProf.List[["ByCSH"]][["LProfweighted"]]$LProf + qt(0.975, df= LProf.List[["ByCSH"]][["LProfweighted"]]$n-1) * LProf.List[["ByCSH"]][["LProfweighted"]]$sd /sqrt(LProf.List[["ByCSH"]][["LProfweighted"]]$n)

LProf.List[["ByCSH"]][["LProfweighted"]] <- LProf.List[["ByCSH"]][["LProfweighted"]][!is.na(LProf.List[["ByCSH"]][["LProfweighted"]]$LProf),]


#GUO firms weighted LProf 

LProf.List[["ByGUO"]][["LProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]]),function(y) sum(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]]),function(y) sqrt(wtd.var(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[y]][,13], sqrt(LProf.List[["ByGUO"]][["LProfunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["ByGUO"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["ByGUO"]][["LProfweighted"]]$low95 <- LProf.List[["ByGUO"]][["LProfweighted"]]$LProf - qt(0.975, df= LProf.List[["ByGUO"]][["LProfweighted"]]$n-1) * LProf.List[["ByGUO"]][["LProfweighted"]]$sd /sqrt(LProf.List[["ByGUO"]][["LProfweighted"]]$n)
LProf.List[["ByGUO"]][["LProfweighted"]]$high95 <- LProf.List[["ByGUO"]][["LProfweighted"]]$LProf + qt(0.975, df= LProf.List[["ByGUO"]][["LProfweighted"]]$n-1) * LProf.List[["ByGUO"]][["LProfweighted"]]$sd /sqrt(LProf.List[["ByGUO"]][["LProfweighted"]]$n)

LProf.List[["ByGUO"]][["LProfweighted"]] <- LProf.List[["ByGUO"]][["LProfweighted"]][!is.na(LProf.List[["ByGUO"]][["LProfweighted"]]$LProf),]


#anyown firms weighted LProf 

LProf.List[["Byanyown"]][["LProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]),function(y) sum(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]),function(y) sqrt(wtd.var(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[y]][,13], sqrt(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["Byanyown"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["Byanyown"]][["LProfweighted"]]$low95 <- LProf.List[["Byanyown"]][["LProfweighted"]]$LProf - qt(0.975, df= LProf.List[["Byanyown"]][["LProfweighted"]]$n-1) * LProf.List[["Byanyown"]][["LProfweighted"]]$sd /sqrt(LProf.List[["Byanyown"]][["LProfweighted"]]$n)
LProf.List[["Byanyown"]][["LProfweighted"]]$high95 <- LProf.List[["Byanyown"]][["LProfweighted"]]$LProf + qt(0.975, df= LProf.List[["Byanyown"]][["LProfweighted"]]$n-1) * LProf.List[["Byanyown"]][["LProfweighted"]]$sd /sqrt(LProf.List[["Byanyown"]][["LProfweighted"]]$n)

LProf.List[["Byanyown"]][["LProfweighted"]] <- LProf.List[["Byanyown"]][["LProfweighted"]][!is.na(LProf.List[["Byanyown"]][["LProfweighted"]]$LProf),]


#intermed firms weighted LProf 

LProf.List[["Byintermed"]][["LProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]),function(y) sum(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                            "sd" = c(sapply(1:length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]),function(y) sqrt(wtd.var(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[y]][,13], sqrt(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                            "n" = c(sapply(1:length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["Byintermed"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["Byintermed"]][["LProfweighted"]]$low95 <- LProf.List[["Byintermed"]][["LProfweighted"]]$LProf - qt(0.975, df= LProf.List[["Byintermed"]][["LProfweighted"]]$n-1) * LProf.List[["Byintermed"]][["LProfweighted"]]$sd /sqrt(LProf.List[["Byintermed"]][["LProfweighted"]]$n)
LProf.List[["Byintermed"]][["LProfweighted"]]$high95 <- LProf.List[["Byintermed"]][["LProfweighted"]]$LProf + qt(0.975, df= LProf.List[["Byintermed"]][["LProfweighted"]]$n-1) * LProf.List[["Byintermed"]][["LProfweighted"]]$sd /sqrt(LProf.List[["Byintermed"]][["LProfweighted"]]$n)

LProf.List[["Byintermed"]][["LProfweighted"]] <- LProf.List[["Byintermed"]][["LProfweighted"]][!is.na(LProf.List[["Byintermed"]][["LProfweighted"]]$LProf),]


#Loops firms weighted LProf 

LProf.List[["Loop"]][["LProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["Loop"]][["LProfunweightedEBT"]]),function(y) sum(LProf.List[["Loop"]][["LProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(LProf.List[["Loop"]][["LProfunweightedEBT"]]),function(y) sqrt(wtd.var(LProf.List[["Loop"]][["LProfunweightedEBT"]][[y]][,13], sqrt(LProf.List[["Loop"]][["LProfunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(LProf.List[["Loop"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["Loop"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["Loop"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["Loop"]][["LProfweighted"]]$low95 <- LProf.List[["Loop"]][["LProfweighted"]]$LProf - qt(0.975, df= LProf.List[["Loop"]][["LProfweighted"]]$n-1) * LProf.List[["Loop"]][["LProfweighted"]]$sd /sqrt(LProf.List[["Loop"]][["LProfweighted"]]$n)
LProf.List[["Loop"]][["LProfweighted"]]$high95 <- LProf.List[["Loop"]][["LProfweighted"]]$LProf + qt(0.975, df= LProf.List[["Loop"]][["LProfweighted"]]$n-1) * LProf.List[["Loop"]][["LProfweighted"]]$sd /sqrt(LProf.List[["Loop"]][["LProfweighted"]]$n)

LProf.List[["Loop"]][["LProfweighted"]] <- LProf.List[["Loop"]][["LProfweighted"]][!is.na(LProf.List[["Loop"]][["LProfweighted"]]$LProf),]



#anysub firms weighted LProf 

LProf.List[["Byanysub"]][["LProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]]),function(y) sum(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]]),function(y) sqrt(wtd.var(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[y]][,13], sqrt(LProf.List[["Byanysub"]][["LProfunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["Byanysub"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["Byanysub"]][["LProfweighted"]]$low95 <- LProf.List[["Byanysub"]][["LProfweighted"]]$LProf - qt(0.975, df= LProf.List[["Byanysub"]][["LProfweighted"]]$n-1) * LProf.List[["Byanysub"]][["LProfweighted"]]$sd /sqrt(LProf.List[["Byanysub"]][["LProfweighted"]]$n)
LProf.List[["Byanysub"]][["LProfweighted"]]$high95 <- LProf.List[["Byanysub"]][["LProfweighted"]]$LProf + qt(0.975, df= LProf.List[["Byanysub"]][["LProfweighted"]]$n-1) * LProf.List[["Byanysub"]][["LProfweighted"]]$sd /sqrt(LProf.List[["Byanysub"]][["LProfweighted"]]$n)

LProf.List[["Byanysub"]][["LProfweighted"]] <- LProf.List[["Byanysub"]][["LProfweighted"]][!is.na(LProf.List[["Byanysub"]][["LProfweighted"]]$LProf),]


#anysubGER firms weighted LProf 

LProf.List[["ByanysubGER"]][["LProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProf" = c(sapply(1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]),function(y) sum(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                             "sd" = c(sapply(1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]),function(y) sqrt(wtd.var(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[y]][,13], sqrt(LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                             "n" = c(sapply(1:length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]),function(y) length(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]][[y]][,13]))]))))


LProf.List[["ByanysubGER"]][["LProfweighted"]]$low95 <- LProf.List[["ByanysubGER"]][["LProfweighted"]]$LProf - qt(0.975, df= LProf.List[["ByanysubGER"]][["LProfweighted"]]$n-1) * LProf.List[["ByanysubGER"]][["LProfweighted"]]$sd /sqrt(LProf.List[["ByanysubGER"]][["LProfweighted"]]$n)
LProf.List[["ByanysubGER"]][["LProfweighted"]]$high95 <- LProf.List[["ByanysubGER"]][["LProfweighted"]]$LProf + qt(0.975, df= LProf.List[["ByanysubGER"]][["LProfweighted"]]$n-1) * LProf.List[["ByanysubGER"]][["LProfweighted"]]$sd /sqrt(LProf.List[["ByanysubGER"]][["LProfweighted"]]$n)

LProf.List[["ByanysubGER"]][["LProfweighted"]] <- LProf.List[["ByanysubGER"]][["LProfweighted"]][!is.na(LProf.List[["ByanysubGER"]][["LProfweighted"]]$LProf),]


#Affiliates  weighted LProf


LProf.List[["Affiliates"]][["LProfweighted"]] <- data.frame("ISO" = "Affiliates")
LProf.List[["Affiliates"]][["LProfweighted"]]$LProf <- sum(unique(Reduce("rbind", LProf.List[["Byanyown"]][["LProfunweightedEBT"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]]))[,12], na.rm = TRUE)
LProf.List[["Affiliates"]][["LProfweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", LProf.List[["Byanyown"]][["LProfunweightedEBT"]])[,13]), sqrt(unique(Reduce("rbind", LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]])[,12])^2), na.rm = TRUE ))
LProf.List[["Affiliates"]][["LProfweighted"]]$n <- length(unique(Reduce("rbind", LProf.List[["Byanyown"]][["LProfunweightedEBT"]]))[,13][!is.na(unique(Reduce("rbind", LProf.List[["Byanyown"]][["LProfunweightedEBT"]]))[,13])])
LProf.List[["Affiliates"]][["LProfweighted"]]$low95 <- LProf.List[["Affiliates"]][["LProfweighted"]]$LProf - qt(0.975, df = LProf.List[["Affiliates"]][["LProfweighted"]]$n-1) * LProf.List[["Affiliates"]][["LProfweighted"]]$sd / sqrt(LProf.List[["Affiliates"]][["LProfweighted"]]$n)
LProf.List[["Affiliates"]][["LProfweighted"]]$high95 <- LProf.List[["Affiliates"]][["LProfweighted"]]$LProf + qt(0.975, df = LProf.List[["Affiliates"]][["LProfweighted"]]$n-1) * LProf.List[["Affiliates"]][["LProfweighted"]]$sd / sqrt(LProf.List[["Affiliates"]][["LProfweighted"]]$n)



#GerGUO weighted LProf


LProf.List[["GerGUO"]][["LProfweighted"]] <- data.frame("ISO" = "GerGUO")
LProf.List[["GerGUO"]][["LProfweighted"]]$LProf <- sum(unique(Reduce("rbind", LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]]))[,12], na.rm = TRUE)
LProf.List[["GerGUO"]][["LProfweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]])[,13]), sqrt(unique(Reduce("rbind", LProf.List[["ByanysubGER"]][["LProfunweightedEmployeeCost"]])[,12])^2), na.rm = TRUE ))
LProf.List[["GerGUO"]][["LProfweighted"]]$n <- length(unique(Reduce("rbind", LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]))[,13][!is.na(unique(Reduce("rbind", LProf.List[["ByanysubGER"]][["LProfunweightedEBT"]]))[,13])])
LProf.List[["GerGUO"]][["LProfweighted"]]$low95 <- LProf.List[["GerGUO"]][["LProfweighted"]]$LProf - qt(0.975, df = LProf.List[["GerGUO"]][["LProfweighted"]]$n-1) * LProf.List[["GerGUO"]][["LProfweighted"]]$sd / sqrt(LProf.List[["GerGUO"]][["LProfweighted"]]$n)
LProf.List[["GerGUO"]][["LProfweighted"]]$high95 <- LProf.List[["GerGUO"]][["LProfweighted"]]$LProf + qt(0.975, df = LProf.List[["GerGUO"]][["LProfweighted"]]$n-1) * LProf.List[["GerGUO"]][["LProfweighted"]]$sd / sqrt(LProf.List[["GerGUO"]][["LProfweighted"]]$n)

LProf.List[["GerGUO"]][["LProfweighted"]] <- LProf.List[["GerGUO"]][["LProfweighted"]][!is.na(LProf.List[["GerGUO"]][["LProfweighted"]]$LProf),]





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, EBT Havens and EU EBT havens to anyown

LProf.List[["Byanyown"]][["LProfunweighted"]] <- rbind(LProf.List[["Byanyown"]][["LProfunweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavens"), "LProf" = c(mean(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% Taxhavens])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))

LProf.List[["Byanyown"]][["LProfunweighted"]] <- rbind(LProf.List[["Byanyown"]][["LProfunweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEU"), "LProf" = c(mean(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))



LProf.List[["Byanyown"]][["LProfunweighted"]] <- rbind(LProf.List[["Byanyown"]][["LProfunweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEUProxy"), "LProf" = c(mean(unique(Reduce("rbind",LProf.List[["Byintermed"]][["LProfunweightedEBT"]][names(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",LProf.List[["Byintermed"]][["LProfunweightedEBT"]][names(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",LProf.List[["Byintermed"]][["LProfunweightedEBT"]][names(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",LProf.List[["Byintermed"]][["LProfunweightedEBT"]][names(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))



LProf.List[["Byanyown"]][["LProfunweighted"]] <- rbind(LProf.List[["Byanyown"]][["LProfunweighted"]], 
                                                       LProf.List[["DeInt"]][["LProfunweighted"]],
                                                       LProf.List[["DeDom"]][["LProfunweighted"]],
                                                       LProf.List[["Affiliates"]][["LProfunweighted"]],
                                                       LProf.List[["GerGUO"]][["LProfunweighted"]]
)


LProf.List[["Byanyown"]][["LProfunweighted"]]$low95 <- LProf.List[["Byanyown"]][["LProfunweighted"]]$LProf - qt(0.975, df= LProf.List[["Byanyown"]][["LProfunweighted"]]$n-1) * LProf.List[["Byanyown"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Byanyown"]][["LProfunweighted"]]$n)
LProf.List[["Byanyown"]][["LProfunweighted"]]$high95 <- LProf.List[["Byanyown"]][["LProfunweighted"]]$LProf + qt(0.975, df= LProf.List[["Byanyown"]][["LProfunweighted"]]$n-1) * LProf.List[["Byanyown"]][["LProfunweighted"]]$sd /sqrt(LProf.List[["Byanyown"]][["LProfunweighted"]]$n)





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, EBT Havens and EU EBT havens to anyown


LProf.List[["Byanyown"]][["LProfweighted"]] <- rbind(LProf.List[["Byanyown"]][["LProfweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavens"), "LProf" = c(sum(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% Taxhavens])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][names(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]]) %in% Taxhavens])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% Taxhavens])[,13]), sqrt(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% Taxhavens])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% Taxhavens])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))

LProf.List[["Byanyown"]][["LProfweighted"]] <- rbind(LProf.List[["Byanyown"]][["LProfweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEU"), "LProf" = c(sum(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]][names(LProf.List[["Byanyown"]][["LProfunweightedEmployeeCost"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",LProf.List[["Byanyown"]][["LProfunweightedEBT"]][names(LProf.List[["Byanyown"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



LProf.List[["Byanyown"]][["LProfweighted"]] <- rbind(LProf.List[["Byanyown"]][["LProfweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEUProxy"), "LProf" = c(sum(unique(Reduce("rbind",LProf.List[["Byintermed"]][["LProfunweightedEBT"]][names(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]][names(LProf.List[["Byintermed"]][["LProfunweightedEmployeeCost"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",LProf.List[["Byintermed"]][["LProfunweightedEBT"]][names(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",LProf.List[["Byintermed"]][["LProfunweightedEBT"]][names(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",LProf.List[["Byintermed"]][["LProfunweightedEBT"]][names(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",LProf.List[["Byintermed"]][["LProfunweightedEBT"]][names(LProf.List[["Byintermed"]][["LProfunweightedEBT"]]) %in% TaxhavensEU])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



LProf.List[["Byanyown"]][["LProfweighted"]] <- rbind(LProf.List[["Byanyown"]][["LProfweighted"]], 
                                                     LProf.List[["DeInt"]][["LProfweighted"]],
                                                     LProf.List[["DeDom"]][["LProfweighted"]],
                                                     LProf.List[["Affiliates"]][["LProfweighted"]],
                                                     LProf.List[["GerGUO"]][["LProfweighted"]]
)


LProf.List[["Byanyown"]][["LProfweighted"]]$low95 <- LProf.List[["Byanyown"]][["LProfweighted"]]$LProf - qt(0.975, df= LProf.List[["Byanyown"]][["LProfweighted"]]$n-1) * LProf.List[["Byanyown"]][["LProfweighted"]]$sd /sqrt(LProf.List[["Byanyown"]][["LProfweighted"]]$n)
LProf.List[["Byanyown"]][["LProfweighted"]]$high95 <- LProf.List[["Byanyown"]][["LProfweighted"]]$LProf + qt(0.975, df= LProf.List[["Byanyown"]][["LProfweighted"]]$n-1) * LProf.List[["Byanyown"]][["LProfweighted"]]$sd /sqrt(LProf.List[["Byanyown"]][["LProfweighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, LProf, g, h, i, ISO, j, x, y , z)





