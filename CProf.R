

EBT <- rio::import("ImportEBT.xlsx", which = "Results")
EBT <- cbind(data.frame("CompanyBVDID" = c(EBT$`BvD ID number`)),EBT[,4:13])

Assets <- rio::import("ImportAssets.xlsx", which = "Results")
Assets <- cbind(data.frame("CompanyBVDID" = c(Assets$`BvD ID number`)),Assets[,4:13])

BvDIDNorm <- intersect(EBT$CompanyBVDID, Assets$CompanyBVDID)
BvDIDNorm <- data.frame("CompanyBVDID" = c(BvDIDNorm))

EBT <- left_join(BvDIDNorm, EBT, by = "CompanyBVDID")
Assets <- left_join(BvDIDNorm, Assets, by = "CompanyBVDID")



##equal out samples

for(i in 1:nrow(Assets)) {
  for (j in 2:ncol(Assets)) {
    Assets[i,j] <- ifelse(!is.na(as.numeric(Assets[i,j])) & !is.na(as.numeric(EBT[i,j])),  as.numeric(Assets[i,j]) , NA )
  }
}


for(i in 1:nrow(EBT)) {
  for (j in 2:ncol(EBT)) {
    
    EBT[i,j] <- ifelse(!is.na(as.numeric(EBT[i,j])) & !is.na(as.numeric(Assets[i,j])),  as.numeric(EBT[i,j]) , NA )
    
  }
}





## Drop if <3 obs


Assets <- Assets[apply(Assets,1,function (z) length(z[!is.na(as.numeric(z))]) > 2),]
EBT <- EBT[EBT$CompanyBVDID %in% Assets$CompanyBVDID,]








CProf.List <- vector(mode = "list")
CProf.List[[1]] <- vector(mode = "list")
names(CProf.List) <- "ByCSH"


#Domestic firms unweightet CProf


CProf.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProf.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(CProf.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(CProf.List[["DeDom"]][["CompanyList"]])) {
  CProf.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(CProf.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- CProf.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(CProf.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(CProf.List[["DeDom"]][["CompanyList"]][[i]])))
}

CProf.List[["DeDom"]][["CProfunweightedAssets"]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)

for (i in 1:nrow(CProf.List[["DeDom"]][["CProfunweightedAssets"]])) {
  for (j in 2:ncol(CProf.List[["DeDom"]][["CProfunweightedAssets"]])) {
    CProf.List[["DeDom"]][["CProfunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["DeDom"]][["CProfunweightedAssets"]][i,j])) & CProf.List[["DeDom"]][["CProfunweightedAssets"]][i,1] %in% CProf.List[["DeDom"]][["CompanyList"]][[(j-1)]], CProf.List[["DeDom"]][["CProfunweightedAssets"]][i,j], NA)
  }}



for (i in 1:length(CProf.List[["DeDom"]][["CompanyList"]])) {
  CProf.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(CProf.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- CProf.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(CProf.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(CProf.List[["DeDom"]][["CompanyList"]][[i]])))
}


CProf.List[["DeDom"]][["CProfunweightedEBT"]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(CProf.List[["DeDom"]][["CProfunweightedEBT"]])) {
  for (j in 2:ncol(CProf.List[["DeDom"]][["CProfunweightedEBT"]])) {
    CProf.List[["DeDom"]][["CProfunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["DeDom"]][["CProfunweightedEBT"]][i,j])) & CProf.List[["DeDom"]][["CProfunweightedEBT"]][i,1] %in% CProf.List[["DeDom"]][["CompanyList"]][[(j-1)]], CProf.List[["DeDom"]][["CProfunweightedEBT"]][i,j], NA)
  }}


for(i in 1:nrow(CProf.List[["DeDom"]][["CProfunweightedAssets"]])) {
  for(j in 2:ncol(CProf.List[["DeDom"]][["CProfunweightedAssets"]])) {
    CProf.List[["DeDom"]][["CProfunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["DeDom"]][["CProfunweightedAssets"]][i,j])) & !is.na(as.numeric(CProf.List[["DeDom"]][["CProfunweightedEBT"]][i,j])) ,  as.numeric(CProf.List[["DeDom"]][["CProfunweightedAssets"]][i,j])  , NA  )
  }}


for(i in 1:nrow(CProf.List[["DeDom"]][["CProfunweightedEBT"]])) {
  for(j in 2:ncol(CProf.List[["DeDom"]][["CProfunweightedEBT"]])) {
    CProf.List[["DeDom"]][["CProfunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["DeDom"]][["CProfunweightedAssets"]][i,j])) & !is.na(as.numeric(CProf.List[["DeDom"]][["CProfunweightedEBT"]][i,j])) ,  as.numeric(CProf.List[["DeDom"]][["CProfunweightedEBT"]][i,j])  , NA  )
  }}



CProf.List[["DeDom"]][["CProfunweightedAssets"]]$sum <- sapply(1:nrow(CProf.List[["DeDom"]][["CProfunweightedAssets"]]), function (y) sum(as.numeric(CProf.List[["DeDom"]][["CProfunweightedAssets"]][y,2:11]) , na.rm = TRUE ))
CProf.List[["DeDom"]][["CProfunweightedAssets"]]$sum[CProf.List[["DeDom"]][["CProfunweightedAssets"]]$sum == 0] <- NA
CProf.List[["DeDom"]][["CProfunweightedEBT"]]$sum <- sapply(1:nrow(CProf.List[["DeDom"]][["CProfunweightedEBT"]]), function (y) sum(as.numeric(CProf.List[["DeDom"]][["CProfunweightedEBT"]][y,2:11]), na.rm = TRUE))
CProf.List[["DeDom"]][["CProfunweightedEBT"]]$sum[CProf.List[["DeDom"]][["CProfunweightedEBT"]]$sum == 0] <- NA
CProf.List[["DeDom"]][["CProfunweightedEBT"]]$CProf <- sapply(1:nrow(CProf.List[["DeDom"]][["CProfunweightedEBT"]]), function (y) CProf.List[["DeDom"]][["CProfunweightedEBT"]][[y,12]] / CProf.List[["DeDom"]][["CProfunweightedAssets"]][[y,12]])



CProf.List[["DeDom"]][["CProfunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                         "CProf" = mean(CProf.List[["DeDom"]][["CProfunweightedEBT"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(CProf.List[["DeDom"]][["CProfunweightedEBT"]][,13], na.rm = TRUE),
                                                         "n" = length(CProf.List[["DeDom"]][["CProfunweightedEBT"]][,13][!is.na(as.numeric(CProf.List[["DeDom"]][["CProfunweightedEBT"]][,13]))]))

CProf.List[["DeDom"]][["CProfunweighted"]]$low95 <- CProf.List[["DeDom"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["DeDom"]][["CProfunweighted"]]$n-1) * CProf.List[["DeDom"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["DeDom"]][["CProfunweighted"]]$n)
CProf.List[["DeDom"]][["CProfunweighted"]]$high95 <- CProf.List[["DeDom"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["DeDom"]][["CProfunweighted"]]$n-1) * CProf.List[["DeDom"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["DeDom"]][["CProfunweighted"]]$n)

CProf.List[["DeDom"]][["CProfunweighted"]] <- CProf.List[["DeDom"]][["CProfunweighted"]][!is.na(CProf.List[["DeDom"]][["CProfunweighted"]]$CProf),]


#International firms unweightet CProf


CProf.List[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProf.List[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(CProf.List[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(CProf.List[["DeInt"]][["CompanyList"]])) {
  CProf.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(CProf.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- CProf.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(CProf.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(CProf.List[["DeInt"]][["CompanyList"]][[i]])))
}

CProf.List[["DeInt"]][["CProfunweightedAssets"]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)

for (i in 1:nrow(CProf.List[["DeInt"]][["CProfunweightedAssets"]])) {
  for (j in 2:ncol(CProf.List[["DeInt"]][["CProfunweightedAssets"]])) {
    CProf.List[["DeInt"]][["CProfunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["DeInt"]][["CProfunweightedAssets"]][i,j])) & CProf.List[["DeInt"]][["CProfunweightedAssets"]][i,1] %in% CProf.List[["DeInt"]][["CompanyList"]][[(j-1)]], CProf.List[["DeInt"]][["CProfunweightedAssets"]][i,j], NA)
  }}



for (i in 1:length(CProf.List[["DeInt"]][["CompanyList"]])) {
  CProf.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(CProf.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- CProf.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(CProf.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(CProf.List[["DeInt"]][["CompanyList"]][[i]])))
}


CProf.List[["DeInt"]][["CProfunweightedEBT"]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(CProf.List[["DeInt"]][["CProfunweightedEBT"]])) {
  for (j in 2:ncol(CProf.List[["DeInt"]][["CProfunweightedEBT"]])) {
    CProf.List[["DeInt"]][["CProfunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["DeInt"]][["CProfunweightedEBT"]][i,j])) & CProf.List[["DeInt"]][["CProfunweightedEBT"]][i,1] %in% CProf.List[["DeInt"]][["CompanyList"]][[(j-1)]], CProf.List[["DeInt"]][["CProfunweightedEBT"]][i,j], NA)
  }}


for(i in 1:nrow(CProf.List[["DeInt"]][["CProfunweightedAssets"]])) {
  for(j in 2:ncol(CProf.List[["DeInt"]][["CProfunweightedAssets"]])) {
    CProf.List[["DeInt"]][["CProfunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["DeInt"]][["CProfunweightedAssets"]][i,j])) & !is.na(as.numeric(CProf.List[["DeInt"]][["CProfunweightedEBT"]][i,j])) ,  as.numeric(CProf.List[["DeInt"]][["CProfunweightedAssets"]][i,j])  , NA  )
  }}


for(i in 1:nrow(CProf.List[["DeInt"]][["CProfunweightedEBT"]])) {
  for(j in 2:ncol(CProf.List[["DeInt"]][["CProfunweightedEBT"]])) {
    CProf.List[["DeInt"]][["CProfunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["DeInt"]][["CProfunweightedAssets"]][i,j])) & !is.na(as.numeric(CProf.List[["DeInt"]][["CProfunweightedEBT"]][i,j])) ,  as.numeric(CProf.List[["DeInt"]][["CProfunweightedEBT"]][i,j])  , NA  )
  }}


CProf.List[["DeInt"]][["CProfunweightedAssets"]]$sum <- sapply(1:nrow(CProf.List[["DeInt"]][["CProfunweightedAssets"]]), function (y) sum(as.numeric(CProf.List[["DeInt"]][["CProfunweightedAssets"]][y,2:11]) , na.rm = TRUE ))
CProf.List[["DeInt"]][["CProfunweightedAssets"]]$sum[CProf.List[["DeInt"]][["CProfunweightedAssets"]]$sum == 0] <- NA
CProf.List[["DeInt"]][["CProfunweightedEBT"]]$sum <- sapply(1:nrow(CProf.List[["DeInt"]][["CProfunweightedEBT"]]), function (y) sum(as.numeric(CProf.List[["DeInt"]][["CProfunweightedEBT"]][y,2:11]), na.rm = TRUE))
CProf.List[["DeInt"]][["CProfunweightedEBT"]]$sum[CProf.List[["DeInt"]][["CProfunweightedEBT"]]$sum == 0] <- NA
CProf.List[["DeInt"]][["CProfunweightedEBT"]]$CProf <- sapply(1:nrow(CProf.List[["DeInt"]][["CProfunweightedEBT"]]), function (y) CProf.List[["DeInt"]][["CProfunweightedEBT"]][[y,12]] / CProf.List[["DeInt"]][["CProfunweightedAssets"]][[y,12]])


CProf.List[["DeInt"]][["CProfunweighted"]] <- data.frame("ISO" = "DeInt", 
                                                         "CProf" = mean(CProf.List[["DeInt"]][["CProfunweightedEBT"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(CProf.List[["DeInt"]][["CProfunweightedEBT"]][,13], na.rm = TRUE),
                                                         "n" = length(CProf.List[["DeInt"]][["CProfunweightedEBT"]][,13][!is.na(as.numeric(CProf.List[["DeInt"]][["CProfunweightedEBT"]][,13]))]))

CProf.List[["DeInt"]][["CProfunweighted"]]$low95 <- CProf.List[["DeInt"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["DeInt"]][["CProfunweighted"]]$n-1) * CProf.List[["DeInt"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["DeInt"]][["CProfunweighted"]]$n)
CProf.List[["DeInt"]][["CProfunweighted"]]$high95 <- CProf.List[["DeInt"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["DeInt"]][["CProfunweighted"]]$n-1) * CProf.List[["DeInt"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["DeInt"]][["CProfunweighted"]]$n)

CProf.List[["DeInt"]][["CProfunweighted"]] <- CProf.List[["DeInt"]][["CProfunweighted"]][!is.na(CProf.List[["DeInt"]][["CProfunweighted"]]$CProf),]



#CSH unweighted CProf


CProf.List[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProf.List[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(CProf.List[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProf.List[["ByCSH"]][["CompanyList"]])) {CProf.List[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CProf.List[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(CProf.List[["ByCSH"]][["CompanyList"]][[i]])) {
    CProf.List[["ByCSH"]][["CompanyList"]][[i]][[j]] <- CProf.List[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(CProf.List[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(CProf.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(CProf.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProf.List[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

CProf.List[["ByCSH"]][["CProfunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["ByCSH"]][["CProfunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(CProf.List[["ByCSH"]][["CProfunweightedAssets"]])) {
  Temp1 <- CProf.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[i]]) == 0 ) {CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["ByCSH"]][["CProfunweightedAssets"]])) {
  for (i in 1:nrow(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]][i,j])) & CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]][i,1] %in% CProf.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]][i,j], NA)
    }}}



CProf.List[["ByCSH"]][["CProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["ByCSH"]][["CProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]])) {
  Temp1 <- CProf.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[i]]) == 0 ) {CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]])) {
  for (i in 1:nrow(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]][i,j])) & CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]][i,1] %in% CProf.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProf.List[["ByCSH"]][["CProfunweightedAssets"]])) {
  for(i in 1:nrow(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]])) {
  for(i in 1:nrow(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProf.List[["ByCSH"]][["CProfunweightedAssets"]])) {
  CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]]$sum[CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]]$sum == 0] <- NA
  CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]]$sum[CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]]$sum == 0] <- NA
  CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]]$CProf <- sapply(1:nrow(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]]), function (y) CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[x]][[y,12]] / CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[x]][y,12])
  
  
}



CProf.List[["ByCSH"]][["CProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]]),function(y) mean(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]]),function(y) sd(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["ByCSH"]][["CProfunweighted"]]$low95 <- CProf.List[["ByCSH"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["ByCSH"]][["CProfunweighted"]]$n-1) * CProf.List[["ByCSH"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["ByCSH"]][["CProfunweighted"]]$n)
CProf.List[["ByCSH"]][["CProfunweighted"]]$high95 <- CProf.List[["ByCSH"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["ByCSH"]][["CProfunweighted"]]$n-1) * CProf.List[["ByCSH"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["ByCSH"]][["CProfunweighted"]]$n)

CProf.List[["ByCSH"]][["CProfunweighted"]] <- CProf.List[["ByCSH"]][["CProfunweighted"]][!is.na(CProf.List[["ByCSH"]][["CProfunweighted"]]$CProf),]



#GUO unweighted CProf


CProf.List[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProf.List[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(CProf.List[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProf.List[["ByGUO"]][["CompanyList"]])) {CProf.List[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(CProf.List[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(CProf.List[["ByGUO"]][["CompanyList"]][[i]])) {
    CProf.List[["ByGUO"]][["CompanyList"]][[i]][[j]] <- CProf.List[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(CProf.List[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(CProf.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(CProf.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProf.List[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

CProf.List[["ByGUO"]][["CProfunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["ByGUO"]][["CProfunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["ByGUO"]][["CProfunweightedAssets"]])) {
  Temp1 <- CProf.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[i]]) == 0 ) {CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["ByGUO"]][["CProfunweightedAssets"]])) {
  for (i in 1:nrow(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]][i,j])) & CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]][i,1] %in% CProf.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]][i,j], NA)
    }}}



CProf.List[["ByGUO"]][["CProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["ByGUO"]][["CProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]])) {
  Temp1 <- CProf.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[i]]) == 0 ) {CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]])) {
  for (i in 1:nrow(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]][i,j])) & CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]][i,1] %in% CProf.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProf.List[["ByGUO"]][["CProfunweightedAssets"]])) {
  for(i in 1:nrow(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]])) {
  for(i in 1:nrow(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProf.List[["ByGUO"]][["CProfunweightedAssets"]])) {
  CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]]$sum[CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]]$sum == 0] <- NA
  CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]]$sum[CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]]$sum == 0] <- NA
  CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]]$CProf <- sapply(1:nrow(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]]), function (y) CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[x]][[y,12]] / CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[x]][y,12])
  
  
}


CProf.List[["ByGUO"]][["CProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]]),function(y) mean(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]]),function(y) sd(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["ByGUO"]][["CProfunweighted"]]$low95 <- CProf.List[["ByGUO"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["ByGUO"]][["CProfunweighted"]]$n-1) * CProf.List[["ByGUO"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["ByGUO"]][["CProfunweighted"]]$n)
CProf.List[["ByGUO"]][["CProfunweighted"]]$high95 <- CProf.List[["ByGUO"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["ByGUO"]][["CProfunweighted"]]$n-1) * CProf.List[["ByGUO"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["ByGUO"]][["CProfunweighted"]]$n)

CProf.List[["ByGUO"]][["CProfunweighted"]] <- CProf.List[["ByGUO"]][["CProfunweighted"]][!is.na(CProf.List[["ByGUO"]][["CProfunweighted"]]$CProf),]


#anyown unweighted CProf


CProf.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProf.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(CProf.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProf.List[["Byanyown"]][["CompanyList"]])) {CProf.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CProf.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(CProf.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(CProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {CProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(CProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProf.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- CProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- CProf.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CProf.List[["Byanyown"]][["CProfunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["Byanyown"]][["CProfunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["Byanyown"]][["CProfunweightedAssets"]])) {
  Temp1 <- CProf.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[i]]) == 0 ) {CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["Byanyown"]][["CProfunweightedAssets"]])) {
  for (i in 1:nrow(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]][i,j])) & CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]][i,1] %in% CProf.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]][i,j], NA)
    }}}



CProf.List[["Byanyown"]][["CProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]])) {
  Temp1 <- CProf.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[i]]) == 0 ) {CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]])) {
  for (i in 1:nrow(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]][i,j])) & CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]][i,1] %in% CProf.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProf.List[["Byanyown"]][["CProfunweightedAssets"]])) {
  for(i in 1:nrow(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]])) {
  for(i in 1:nrow(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProf.List[["Byanyown"]][["CProfunweightedAssets"]])) {
  CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]]$sum[CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]]$sum == 0] <- NA
  CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]]$sum[CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]]$sum == 0] <- NA
  CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]]$CProf <- sapply(1:nrow(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]]), function (y) CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[x]][[y,12]] / CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[x]][y,12])
  
}


CProf.List[["Byanyown"]][["CProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]),function(y) mean(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]),function(y) sd(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["Byanyown"]][["CProfunweighted"]]$low95 <- CProf.List[["Byanyown"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["Byanyown"]][["CProfunweighted"]]$n-1) * CProf.List[["Byanyown"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Byanyown"]][["CProfunweighted"]]$n)
CProf.List[["Byanyown"]][["CProfunweighted"]]$high95 <- CProf.List[["Byanyown"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["Byanyown"]][["CProfunweighted"]]$n-1) * CProf.List[["Byanyown"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Byanyown"]][["CProfunweighted"]]$n)

CProf.List[["Byanyown"]][["CProfunweighted"]] <- CProf.List[["Byanyown"]][["CProfunweighted"]][!is.na(CProf.List[["Byanyown"]][["CProfunweighted"]]$CProf),]




#intermed unweighted CProf


CProf.List[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProf.List[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(CProf.List[["Byintermed"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProf.List[["Byintermed"]][["CompanyList"]])) {CProf.List[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CProf.List[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(CProf.List[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(CProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {CProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(CProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProf.List[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- CProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- CProf.List[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CProf.List[["Byintermed"]][["CProfunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["Byintermed"]][["CProfunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["Byintermed"]][["CProfunweightedAssets"]])) {
  Temp1 <- CProf.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[i]]) == 0 ) {CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["Byintermed"]][["CProfunweightedAssets"]])) {
  for (i in 1:nrow(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]][i,j])) & CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]][i,1] %in% CProf.List[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]][i,j], NA)
    }}}



CProf.List[["Byintermed"]][["CProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]])) {
  Temp1 <- CProf.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[i]]) == 0 ) {CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]])) {
  for (i in 1:nrow(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]][i,j])) & CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]][i,1] %in% CProf.List[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProf.List[["Byintermed"]][["CProfunweightedAssets"]])) {
  for(i in 1:nrow(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]])) {
  for(i in 1:nrow(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProf.List[["Byintermed"]][["CProfunweightedAssets"]])) {
  CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]]$sum[CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]]$sum == 0] <- NA
  CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]]$sum[CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]]$sum == 0] <- NA
  CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]]$CProf <- sapply(1:nrow(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]]), function (y) CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[x]][[y,12]] / CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[x]][y,12])
  
  
}


CProf.List[["Byintermed"]][["CProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]),function(y) mean(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                              "sd" = c(sapply(1:length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]),function(y) sd(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                              "n" = c(sapply(1:length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["Byintermed"]][["CProfunweighted"]]$low95 <- CProf.List[["Byintermed"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["Byintermed"]][["CProfunweighted"]]$n-1) * CProf.List[["Byintermed"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Byintermed"]][["CProfunweighted"]]$n)
CProf.List[["Byintermed"]][["CProfunweighted"]]$high95 <- CProf.List[["Byintermed"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["Byintermed"]][["CProfunweighted"]]$n-1) * CProf.List[["Byintermed"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Byintermed"]][["CProfunweighted"]]$n)

CProf.List[["Byintermed"]][["CProfunweighted"]] <- CProf.List[["Byintermed"]][["CProfunweighted"]][!is.na(CProf.List[["Byintermed"]][["CProfunweighted"]]$CProf),]





#CProf unweighted Loops


CProf.List[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProf.List[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(CProf.List[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(CProf.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(CProf.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CProf.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    CProf.List[["Loop"]][["CompanyList"]][[i]][[j]] <- CProf.List[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(CProf.List[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(CProf.List[["Loop"]][["CompanyList"]])) {CProf.List[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CProf.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(CProf.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CProf.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(CProf.List[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CProf.List[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {CProf.List[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(CProf.List[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CProf.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProf.List[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CProf.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- CProf.List[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CProf.List[["Loop"]][["CompanyList"]][[i]][[j]] <- CProf.List[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CProf.List[["Loop"]][["CProfunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["Loop"]][["CProfunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["Loop"]][["CProfunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["Loop"]][["CProfunweightedAssets"]])) {
  Temp1 <- CProf.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["Loop"]][["CProfunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["Loop"]][["CProfunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["Loop"]][["CProfunweightedAssets"]][[i]]) == 0 ) {CProf.List[["Loop"]][["CProfunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["Loop"]][["CProfunweightedAssets"]])) {
  for (i in 1:nrow(CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]][i,j])) & CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]][i,1] %in% CProf.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]][i,j], NA)
    }}}



CProf.List[["Loop"]][["CProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["Loop"]][["CProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["Loop"]][["CProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["Loop"]][["CProfunweightedEBT"]])) {
  Temp1 <- CProf.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["Loop"]][["CProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["Loop"]][["CProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["Loop"]][["CProfunweightedEBT"]][[i]]) == 0 ) {CProf.List[["Loop"]][["CProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["Loop"]][["CProfunweightedEBT"]])) {
  for (i in 1:nrow(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]][i,j])) & CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]][i,1] %in% CProf.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProf.List[["Loop"]][["CProfunweightedAssets"]])) {
  for(i in 1:nrow(CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProf.List[["Loop"]][["CProfunweightedEBT"]])) {
  for(i in 1:nrow(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProf.List[["Loop"]][["CProfunweightedAssets"]])) {
  CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]]$sum[CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]]$sum == 0] <- NA
  CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]]$sum[CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]]$sum == 0] <- NA
  CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]]$CProf <- sapply(1:nrow(CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]]), function (y) CProf.List[["Loop"]][["CProfunweightedEBT"]][[x]][[y,12]] / CProf.List[["Loop"]][["CProfunweightedAssets"]][[x]][y,12])
  
  
}


CProf.List[["Loop"]][["CProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["Loop"]][["CProfunweightedEBT"]]),function(y) mean(CProf.List[["Loop"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(CProf.List[["Loop"]][["CProfunweightedEBT"]]),function(y) sd(CProf.List[["Loop"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(CProf.List[["Loop"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["Loop"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["Loop"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["Loop"]][["CProfunweighted"]]$low95 <- CProf.List[["Loop"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["Loop"]][["CProfunweighted"]]$n-1) * CProf.List[["Loop"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Loop"]][["CProfunweighted"]]$n)
CProf.List[["Loop"]][["CProfunweighted"]]$high95 <- CProf.List[["Loop"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["Loop"]][["CProfunweighted"]]$n-1) * CProf.List[["Loop"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Loop"]][["CProfunweighted"]]$n)

CProf.List[["Loop"]][["CProfunweighted"]] <- CProf.List[["Loop"]][["CProfunweighted"]][!is.na(CProf.List[["Loop"]][["CProfunweighted"]]$CProf),]


#anysub unweighted CProf


CProf.List[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProf.List[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(CProf.List[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProf.List[["Byanysub"]][["CompanyList"]])) {CProf.List[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CProf.List[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(CProf.List[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(CProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {CProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(CProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProf.List[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- CProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- CProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CProf.List[["Byanysub"]][["CProfunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["Byanysub"]][["CProfunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["Byanysub"]][["CProfunweightedAssets"]])) {
  Temp1 <- CProf.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[i]]) == 0 ) {CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["Byanysub"]][["CProfunweightedAssets"]])) {
  for (i in 1:nrow(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]][i,j])) & CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]][i,1] %in% CProf.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]][i,j], NA)
    }}}



CProf.List[["Byanysub"]][["CProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["Byanysub"]][["CProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]])) {
  Temp1 <- CProf.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[i]]) == 0 ) {CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]])) {
  for (i in 1:nrow(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]][i,j])) & CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]][i,1] %in% CProf.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProf.List[["Byanysub"]][["CProfunweightedAssets"]])) {
  for(i in 1:nrow(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]])) {
  for(i in 1:nrow(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProf.List[["Byanysub"]][["CProfunweightedAssets"]])) {
  CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]]$sum[CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]]$sum == 0] <- NA
  CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]]$sum[CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]]$sum == 0] <- NA
  CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]]$CProf <- sapply(1:nrow(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]]), function (y) CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[x]][[y,12]] / CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[x]][y,12])
  
}

CProf.List[["Byanysub"]][["CProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]]),function(y) mean(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]]),function(y) sd(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["Byanysub"]][["CProfunweighted"]]$low95 <- CProf.List[["Byanysub"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["Byanysub"]][["CProfunweighted"]]$n-1) * CProf.List[["Byanysub"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Byanysub"]][["CProfunweighted"]]$n)
CProf.List[["Byanysub"]][["CProfunweighted"]]$high95 <- CProf.List[["Byanysub"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["Byanysub"]][["CProfunweighted"]]$n-1) * CProf.List[["Byanysub"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Byanysub"]][["CProfunweighted"]]$n)

CProf.List[["Byanysub"]][["CProfunweighted"]] <- CProf.List[["Byanysub"]][["CProfunweighted"]][!is.na(CProf.List[["Byanysub"]][["CProfunweighted"]]$CProf),]


#anysubGER unweighted CProf


CProf.List[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProf.List[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(CProf.List[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProf.List[["ByanysubGER"]][["CompanyList"]])) {CProf.List[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(CProf.List[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(CProf.List[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    CProf.List[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- CProf.List[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(CProf.List[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(CProf.List[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]])) {
  Temp1 <- CProf.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[i]]) == 0 ) {CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]])) {
  for (i in 1:nrow(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]][i,j])) & CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]][i,1] %in% CProf.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]][i,j], NA)
    }}}



CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]])) {
  Temp1 <- CProf.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProf.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[i]]) == 0 ) {CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]])) {
  for (i in 1:nrow(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]])) {
    for (j in 2:ncol(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]][i,j])) & CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]][i,1] %in% CProf.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]])) {
  for(i in 1:nrow(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]])) {
      CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]])) {
  for(i in 1:nrow(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]])) {
    for(j in 2:ncol(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]])) {
      CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]][i,j])) ,  as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]])) {
  CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]]$sum[CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]]$sum == 0] <- NA
  CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]]$sum <- sapply(1:nrow(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]]), function (y) sum(as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]][y,2:11]), na.rm = TRUE))
  CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]]$sum[CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]]$sum == 0] <- NA
  CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]]$CProf <- sapply(1:nrow(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]]), function (y) CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[x]][[y,12]] / CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[x]][y,12])
  
}


CProf.List[["ByanysubGER"]][["CProfunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]),function(y) mean(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))), 
                                                               "sd" = c(sapply(1:length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]),function(y) sd(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[y]][,13], na.rm = TRUE))),
                                                               "n" = c(sapply(1:length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["ByanysubGER"]][["CProfunweighted"]]$low95 <- CProf.List[["ByanysubGER"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["ByanysubGER"]][["CProfunweighted"]]$n-1) * CProf.List[["ByanysubGER"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["ByanysubGER"]][["CProfunweighted"]]$n)
CProf.List[["ByanysubGER"]][["CProfunweighted"]]$high95 <- CProf.List[["ByanysubGER"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["ByanysubGER"]][["CProfunweighted"]]$n-1) * CProf.List[["ByanysubGER"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["ByanysubGER"]][["CProfunweighted"]]$n)

CProf.List[["ByanysubGER"]][["CProfunweighted"]] <- CProf.List[["ByanysubGER"]][["CProfunweighted"]][!is.na(CProf.List[["ByanysubGER"]][["CProfunweighted"]]$CProf),]


#Affiliates  unweighted CProf


CProf.List[["Affiliates"]][["CProfunweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                              "CProf" = mean(unique(Reduce("rbind", CProf.List[["Byanyown"]][["CProfunweightedEBT"]]))[,13], na.rm = TRUE), 
                                                              "sd" = sd(unique(Reduce("rbind", CProf.List[["Byanyown"]][["CProfunweightedEBT"]]))[,13], na.rm = TRUE),
                                                              "n" = length(unique(Reduce("rbind", CProf.List[["Byanyown"]][["CProfunweightedEBT"]]))[,13][!is.na(unique(Reduce("rbind", CProf.List[["Byanyown"]][["CProfunweightedEBT"]]))[,13])]))




CProf.List[["Affiliates"]][["CProfunweighted"]]$low95 <- CProf.List[["Affiliates"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["Affiliates"]][["CProfunweighted"]]$n-1) * CProf.List[["Affiliates"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Affiliates"]][["CProfunweighted"]]$n)
CProf.List[["Affiliates"]][["CProfunweighted"]]$high95 <- CProf.List[["Affiliates"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["Affiliates"]][["CProfunweighted"]]$n-1) * CProf.List[["Affiliates"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Affiliates"]][["CProfunweighted"]]$n)




#GerGUO unweighted CProf


CProf.List[["GerGUO"]][["CProfunweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                          "CProf" = mean(unique(Reduce("rbind", CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]))[,13], na.rm = TRUE), 
                                                          "sd" = sd(unique(Reduce("rbind", CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]))[,13], na.rm = TRUE),
                                                          "n" = length(unique(Reduce("rbind", CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]))[,13][!is.na(unique(Reduce("rbind", CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]))[,13])]))




CProf.List[["GerGUO"]][["CProfunweighted"]]$low95 <- CProf.List[["GerGUO"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["GerGUO"]][["CProfunweighted"]]$n-1) * CProf.List[["GerGUO"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["GerGUO"]][["CProfunweighted"]]$n)
CProf.List[["GerGUO"]][["CProfunweighted"]]$high95 <- CProf.List[["GerGUO"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["GerGUO"]][["CProfunweighted"]]$n-1) * CProf.List[["GerGUO"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["GerGUO"]][["CProfunweighted"]]$n)

CProf.List[["GerGUO"]][["CProfunweighted"]] <- CProf.List[["GerGUO"]][["CProfunweighted"]][!is.na(CProf.List[["GerGUO"]][["CProfunweighted"]]$CProf),]




#Domestic firms weighted CProf


CProf.List[["DeDom"]][["CProfweighted"]] <- data.frame("ISO" = "DEDOM")
CProf.List[["DeDom"]][["CProfweighted"]]$CProf <- sum(CProf.List[["DeDom"]][["CProfunweightedEBT"]]$sum, na.rm = TRUE)  / sum(CProf.List[["DeDom"]][["CProfunweightedAssets"]]$sum, na.rm = TRUE)
CProf.List[["DeDom"]][["CProfweighted"]]$sd <- sqrt(wtd.var(CProf.List[["DeDom"]][["CProfunweightedEBT"]]$CProf, sqrt(CProf.List[["DeDom"]][["CProfunweightedAssets"]]$sum^2), na.rm = TRUE ))
CProf.List[["DeDom"]][["CProfweighted"]]$n <- length(CProf.List[["DeDom"]][["CProfunweightedEBT"]]$CProf[!is.na(CProf.List[["DeDom"]][["CProfunweightedEBT"]]$CProf)])
CProf.List[["DeDom"]][["CProfweighted"]]$low95 <- CProf.List[["DeDom"]][["CProfweighted"]]$CProf - qt(0.975, df = CProf.List[["DeDom"]][["CProfweighted"]]$n-1) * CProf.List[["DeDom"]][["CProfweighted"]]$sd / sqrt(CProf.List[["DeDom"]][["CProfweighted"]]$n)
CProf.List[["DeDom"]][["CProfweighted"]]$high95 <- CProf.List[["DeDom"]][["CProfweighted"]]$CProf + qt(0.975, df = CProf.List[["DeDom"]][["CProfweighted"]]$n-1) * CProf.List[["DeDom"]][["CProfweighted"]]$sd / sqrt(CProf.List[["DeDom"]][["CProfweighted"]]$n)


#International firms weighted CProf


CProf.List[["DeInt"]][["CProfweighted"]] <- data.frame("ISO" = "DEINT")
CProf.List[["DeInt"]][["CProfweighted"]]$CProf <- sum(CProf.List[["DeInt"]][["CProfunweightedEBT"]]$sum, na.rm = TRUE)  / sum(CProf.List[["DeInt"]][["CProfunweightedAssets"]]$sum, na.rm = TRUE)
CProf.List[["DeInt"]][["CProfweighted"]]$sd <- sqrt(wtd.var(CProf.List[["DeInt"]][["CProfunweightedEBT"]]$CProf, sqrt(CProf.List[["DeInt"]][["CProfunweightedAssets"]]$sum^2), na.rm = TRUE ))
CProf.List[["DeInt"]][["CProfweighted"]]$n <- length(CProf.List[["DeInt"]][["CProfunweightedEBT"]]$CProf[!is.na(CProf.List[["DeInt"]][["CProfunweightedEBT"]]$CProf)])
CProf.List[["DeInt"]][["CProfweighted"]]$low95 <- CProf.List[["DeInt"]][["CProfweighted"]]$CProf - qt(0.975, df = CProf.List[["DeInt"]][["CProfweighted"]]$n-1) * CProf.List[["DeInt"]][["CProfweighted"]]$sd / sqrt(CProf.List[["DeInt"]][["CProfweighted"]]$n)
CProf.List[["DeInt"]][["CProfweighted"]]$high95 <- CProf.List[["DeInt"]][["CProfweighted"]]$CProf + qt(0.975, df = CProf.List[["DeInt"]][["CProfweighted"]]$n-1) * CProf.List[["DeInt"]][["CProfweighted"]]$sd / sqrt(CProf.List[["DeInt"]][["CProfweighted"]]$n)


#CSH firms weighted CProf 

CProf.List[["ByCSH"]][["CProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]]),function(y) sum(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]]),function(y) sqrt(wtd.var(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[y]][,13], sqrt(CProf.List[["ByCSH"]][["CProfunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["ByCSH"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["ByCSH"]][["CProfweighted"]]$low95 <- CProf.List[["ByCSH"]][["CProfweighted"]]$CProf - qt(0.975, df= CProf.List[["ByCSH"]][["CProfweighted"]]$n-1) * CProf.List[["ByCSH"]][["CProfweighted"]]$sd /sqrt(CProf.List[["ByCSH"]][["CProfweighted"]]$n)
CProf.List[["ByCSH"]][["CProfweighted"]]$high95 <- CProf.List[["ByCSH"]][["CProfweighted"]]$CProf + qt(0.975, df= CProf.List[["ByCSH"]][["CProfweighted"]]$n-1) * CProf.List[["ByCSH"]][["CProfweighted"]]$sd /sqrt(CProf.List[["ByCSH"]][["CProfweighted"]]$n)

CProf.List[["ByCSH"]][["CProfweighted"]] <- CProf.List[["ByCSH"]][["CProfweighted"]][!is.na(CProf.List[["ByCSH"]][["CProfweighted"]]$CProf),]


#GUO firms weighted CProf 

CProf.List[["ByGUO"]][["CProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]]),function(y) sum(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]]),function(y) sqrt(wtd.var(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[y]][,13], sqrt(CProf.List[["ByGUO"]][["CProfunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["ByGUO"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["ByGUO"]][["CProfweighted"]]$low95 <- CProf.List[["ByGUO"]][["CProfweighted"]]$CProf - qt(0.975, df= CProf.List[["ByGUO"]][["CProfweighted"]]$n-1) * CProf.List[["ByGUO"]][["CProfweighted"]]$sd /sqrt(CProf.List[["ByGUO"]][["CProfweighted"]]$n)
CProf.List[["ByGUO"]][["CProfweighted"]]$high95 <- CProf.List[["ByGUO"]][["CProfweighted"]]$CProf + qt(0.975, df= CProf.List[["ByGUO"]][["CProfweighted"]]$n-1) * CProf.List[["ByGUO"]][["CProfweighted"]]$sd /sqrt(CProf.List[["ByGUO"]][["CProfweighted"]]$n)

CProf.List[["ByGUO"]][["CProfweighted"]] <- CProf.List[["ByGUO"]][["CProfweighted"]][!is.na(CProf.List[["ByGUO"]][["CProfweighted"]]$CProf),]


#anyown firms weighted CProf 

CProf.List[["Byanyown"]][["CProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]),function(y) sum(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]),function(y) sqrt(wtd.var(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[y]][,13], sqrt(CProf.List[["Byanyown"]][["CProfunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["Byanyown"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["Byanyown"]][["CProfweighted"]]$low95 <- CProf.List[["Byanyown"]][["CProfweighted"]]$CProf - qt(0.975, df= CProf.List[["Byanyown"]][["CProfweighted"]]$n-1) * CProf.List[["Byanyown"]][["CProfweighted"]]$sd /sqrt(CProf.List[["Byanyown"]][["CProfweighted"]]$n)
CProf.List[["Byanyown"]][["CProfweighted"]]$high95 <- CProf.List[["Byanyown"]][["CProfweighted"]]$CProf + qt(0.975, df= CProf.List[["Byanyown"]][["CProfweighted"]]$n-1) * CProf.List[["Byanyown"]][["CProfweighted"]]$sd /sqrt(CProf.List[["Byanyown"]][["CProfweighted"]]$n)

CProf.List[["Byanyown"]][["CProfweighted"]] <- CProf.List[["Byanyown"]][["CProfweighted"]][!is.na(CProf.List[["Byanyown"]][["CProfweighted"]]$CProf),]


#intermed firms weighted CProf 

CProf.List[["Byintermed"]][["CProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]),function(y) sum(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                            "sd" = c(sapply(1:length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]),function(y) sqrt(wtd.var(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[y]][,13], sqrt(CProf.List[["Byintermed"]][["CProfunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                            "n" = c(sapply(1:length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["Byintermed"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["Byintermed"]][["CProfweighted"]]$low95 <- CProf.List[["Byintermed"]][["CProfweighted"]]$CProf - qt(0.975, df= CProf.List[["Byintermed"]][["CProfweighted"]]$n-1) * CProf.List[["Byintermed"]][["CProfweighted"]]$sd /sqrt(CProf.List[["Byintermed"]][["CProfweighted"]]$n)
CProf.List[["Byintermed"]][["CProfweighted"]]$high95 <- CProf.List[["Byintermed"]][["CProfweighted"]]$CProf + qt(0.975, df= CProf.List[["Byintermed"]][["CProfweighted"]]$n-1) * CProf.List[["Byintermed"]][["CProfweighted"]]$sd /sqrt(CProf.List[["Byintermed"]][["CProfweighted"]]$n)

CProf.List[["Byintermed"]][["CProfweighted"]] <- CProf.List[["Byintermed"]][["CProfweighted"]][!is.na(CProf.List[["Byintermed"]][["CProfweighted"]]$CProf),]


#Loops firms weighted CProf 

CProf.List[["Loop"]][["CProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["Loop"]][["CProfunweightedEBT"]]),function(y) sum(CProf.List[["Loop"]][["CProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CProf.List[["Loop"]][["CProfunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(CProf.List[["Loop"]][["CProfunweightedEBT"]]),function(y) sqrt(wtd.var(CProf.List[["Loop"]][["CProfunweightedEBT"]][[y]][,13], sqrt(CProf.List[["Loop"]][["CProfunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(CProf.List[["Loop"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["Loop"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["Loop"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["Loop"]][["CProfweighted"]]$low95 <- CProf.List[["Loop"]][["CProfweighted"]]$CProf - qt(0.975, df= CProf.List[["Loop"]][["CProfweighted"]]$n-1) * CProf.List[["Loop"]][["CProfweighted"]]$sd /sqrt(CProf.List[["Loop"]][["CProfweighted"]]$n)
CProf.List[["Loop"]][["CProfweighted"]]$high95 <- CProf.List[["Loop"]][["CProfweighted"]]$CProf + qt(0.975, df= CProf.List[["Loop"]][["CProfweighted"]]$n-1) * CProf.List[["Loop"]][["CProfweighted"]]$sd /sqrt(CProf.List[["Loop"]][["CProfweighted"]]$n)

CProf.List[["Loop"]][["CProfweighted"]] <- CProf.List[["Loop"]][["CProfweighted"]][!is.na(CProf.List[["Loop"]][["CProfweighted"]]$CProf),]



#anysub firms weighted CProf 

CProf.List[["Byanysub"]][["CProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]]),function(y) sum(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]]),function(y) sqrt(wtd.var(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[y]][,13], sqrt(CProf.List[["Byanysub"]][["CProfunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["Byanysub"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["Byanysub"]][["CProfweighted"]]$low95 <- CProf.List[["Byanysub"]][["CProfweighted"]]$CProf - qt(0.975, df= CProf.List[["Byanysub"]][["CProfweighted"]]$n-1) * CProf.List[["Byanysub"]][["CProfweighted"]]$sd /sqrt(CProf.List[["Byanysub"]][["CProfweighted"]]$n)
CProf.List[["Byanysub"]][["CProfweighted"]]$high95 <- CProf.List[["Byanysub"]][["CProfweighted"]]$CProf + qt(0.975, df= CProf.List[["Byanysub"]][["CProfweighted"]]$n-1) * CProf.List[["Byanysub"]][["CProfweighted"]]$sd /sqrt(CProf.List[["Byanysub"]][["CProfweighted"]]$n)

CProf.List[["Byanysub"]][["CProfweighted"]] <- CProf.List[["Byanysub"]][["CProfweighted"]][!is.na(CProf.List[["Byanysub"]][["CProfweighted"]]$CProf),]


#anysubGER firms weighted CProf 

CProf.List[["ByanysubGER"]][["CProfweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProf" = c(sapply(1:length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]),function(y) sum(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[y]][,12], na.rm = TRUE) / sum(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                             "sd" = c(sapply(1:length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]),function(y) sqrt(wtd.var(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[y]][,13], sqrt(CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                             "n" = c(sapply(1:length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]),function(y) length(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[y]][,13][!is.na(as.numeric(CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]][[y]][,13]))]))))


CProf.List[["ByanysubGER"]][["CProfweighted"]]$low95 <- CProf.List[["ByanysubGER"]][["CProfweighted"]]$CProf - qt(0.975, df= CProf.List[["ByanysubGER"]][["CProfweighted"]]$n-1) * CProf.List[["ByanysubGER"]][["CProfweighted"]]$sd /sqrt(CProf.List[["ByanysubGER"]][["CProfweighted"]]$n)
CProf.List[["ByanysubGER"]][["CProfweighted"]]$high95 <- CProf.List[["ByanysubGER"]][["CProfweighted"]]$CProf + qt(0.975, df= CProf.List[["ByanysubGER"]][["CProfweighted"]]$n-1) * CProf.List[["ByanysubGER"]][["CProfweighted"]]$sd /sqrt(CProf.List[["ByanysubGER"]][["CProfweighted"]]$n)

CProf.List[["ByanysubGER"]][["CProfweighted"]] <- CProf.List[["ByanysubGER"]][["CProfweighted"]][!is.na(CProf.List[["ByanysubGER"]][["CProfweighted"]]$CProf),]


#Affiliates  weighted CProf


CProf.List[["Affiliates"]][["CProfweighted"]] <- data.frame("ISO" = "Affiliates")
CProf.List[["Affiliates"]][["CProfweighted"]]$CProf <- sum(unique(Reduce("rbind", CProf.List[["Byanyown"]][["CProfunweightedEBT"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", CProf.List[["Byanyown"]][["CProfunweightedAssets"]]))[,12], na.rm = TRUE)
CProf.List[["Affiliates"]][["CProfweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", CProf.List[["Byanyown"]][["CProfunweightedEBT"]])[,13]), sqrt(unique(Reduce("rbind", CProf.List[["Byanyown"]][["CProfunweightedAssets"]])[,12])^2), na.rm = TRUE ))
CProf.List[["Affiliates"]][["CProfweighted"]]$n <- length(unique(Reduce("rbind", CProf.List[["Byanyown"]][["CProfunweightedEBT"]]))[,13][!is.na(unique(Reduce("rbind", CProf.List[["Byanyown"]][["CProfunweightedEBT"]]))[,13])])
CProf.List[["Affiliates"]][["CProfweighted"]]$low95 <- CProf.List[["Affiliates"]][["CProfweighted"]]$CProf - qt(0.975, df = CProf.List[["Affiliates"]][["CProfweighted"]]$n-1) * CProf.List[["Affiliates"]][["CProfweighted"]]$sd / sqrt(CProf.List[["Affiliates"]][["CProfweighted"]]$n)
CProf.List[["Affiliates"]][["CProfweighted"]]$high95 <- CProf.List[["Affiliates"]][["CProfweighted"]]$CProf + qt(0.975, df = CProf.List[["Affiliates"]][["CProfweighted"]]$n-1) * CProf.List[["Affiliates"]][["CProfweighted"]]$sd / sqrt(CProf.List[["Affiliates"]][["CProfweighted"]]$n)



#GerGUO weighted CProf


CProf.List[["GerGUO"]][["CProfweighted"]] <- data.frame("ISO" = "GerGUO")
CProf.List[["GerGUO"]][["CProfweighted"]]$CProf <- sum(unique(Reduce("rbind", CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]]))[,12], na.rm = TRUE)
CProf.List[["GerGUO"]][["CProfweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]])[,13]), sqrt(unique(Reduce("rbind", CProf.List[["ByanysubGER"]][["CProfunweightedAssets"]])[,12])^2), na.rm = TRUE ))
CProf.List[["GerGUO"]][["CProfweighted"]]$n <- length(unique(Reduce("rbind", CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]))[,13][!is.na(unique(Reduce("rbind", CProf.List[["ByanysubGER"]][["CProfunweightedEBT"]]))[,13])])
CProf.List[["GerGUO"]][["CProfweighted"]]$low95 <- CProf.List[["GerGUO"]][["CProfweighted"]]$CProf - qt(0.975, df = CProf.List[["GerGUO"]][["CProfweighted"]]$n-1) * CProf.List[["GerGUO"]][["CProfweighted"]]$sd / sqrt(CProf.List[["GerGUO"]][["CProfweighted"]]$n)
CProf.List[["GerGUO"]][["CProfweighted"]]$high95 <- CProf.List[["GerGUO"]][["CProfweighted"]]$CProf + qt(0.975, df = CProf.List[["GerGUO"]][["CProfweighted"]]$n-1) * CProf.List[["GerGUO"]][["CProfweighted"]]$sd / sqrt(CProf.List[["GerGUO"]][["CProfweighted"]]$n)

CProf.List[["GerGUO"]][["CProfweighted"]] <- CProf.List[["GerGUO"]][["CProfweighted"]][!is.na(CProf.List[["GerGUO"]][["CProfweighted"]]$CProf),]





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, EBT Havens and EU EBT havens to anyown

CProf.List[["Byanyown"]][["CProfunweighted"]] <- rbind(CProf.List[["Byanyown"]][["CProfunweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavens"), "CProf" = c(mean(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% Taxhavens])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))

CProf.List[["Byanyown"]][["CProfunweighted"]] <- rbind(CProf.List[["Byanyown"]][["CProfunweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEU"), "CProf" = c(mean(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))



CProf.List[["Byanyown"]][["CProfunweighted"]] <- rbind(CProf.List[["Byanyown"]][["CProfunweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEUProxy"), "CProf" = c(mean(unique(Reduce("rbind",CProf.List[["Byintermed"]][["CProfunweightedEBT"]][names(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",CProf.List[["Byintermed"]][["CProfunweightedEBT"]][names(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",CProf.List[["Byintermed"]][["CProfunweightedEBT"]][names(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",CProf.List[["Byintermed"]][["CProfunweightedEBT"]][names(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))



CProf.List[["Byanyown"]][["CProfunweighted"]] <- rbind(CProf.List[["Byanyown"]][["CProfunweighted"]], 
                                                       CProf.List[["DeInt"]][["CProfunweighted"]],
                                                       CProf.List[["DeDom"]][["CProfunweighted"]],
                                                       CProf.List[["Affiliates"]][["CProfunweighted"]],
                                                       CProf.List[["GerGUO"]][["CProfunweighted"]]
)


CProf.List[["Byanyown"]][["CProfunweighted"]]$low95 <- CProf.List[["Byanyown"]][["CProfunweighted"]]$CProf - qt(0.975, df= CProf.List[["Byanyown"]][["CProfunweighted"]]$n-1) * CProf.List[["Byanyown"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Byanyown"]][["CProfunweighted"]]$n)
CProf.List[["Byanyown"]][["CProfunweighted"]]$high95 <- CProf.List[["Byanyown"]][["CProfunweighted"]]$CProf + qt(0.975, df= CProf.List[["Byanyown"]][["CProfunweighted"]]$n-1) * CProf.List[["Byanyown"]][["CProfunweighted"]]$sd /sqrt(CProf.List[["Byanyown"]][["CProfunweighted"]]$n)





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, EBT Havens and EU EBT havens to anyown


CProf.List[["Byanyown"]][["CProfweighted"]] <- rbind(CProf.List[["Byanyown"]][["CProfweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavens"), "CProf" = c(sum(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% Taxhavens])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedAssets"]][names(CProf.List[["Byanyown"]][["CProfunweightedAssets"]]) %in% Taxhavens])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% Taxhavens])[,13]), sqrt(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% Taxhavens])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% Taxhavens])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))

CProf.List[["Byanyown"]][["CProfweighted"]] <- rbind(CProf.List[["Byanyown"]][["CProfweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEU"), "CProf" = c(sum(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedAssets"]][names(CProf.List[["Byanyown"]][["CProfunweightedAssets"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",CProf.List[["Byanyown"]][["CProfunweightedEBT"]][names(CProf.List[["Byanyown"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



CProf.List[["Byanyown"]][["CProfweighted"]] <- rbind(CProf.List[["Byanyown"]][["CProfweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEUProxy"), "CProf" = c(sum(unique(Reduce("rbind",CProf.List[["Byintermed"]][["CProfunweightedEBT"]][names(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",CProf.List[["Byintermed"]][["CProfunweightedAssets"]][names(CProf.List[["Byintermed"]][["CProfunweightedAssets"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",CProf.List[["Byintermed"]][["CProfunweightedEBT"]][names(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",CProf.List[["Byintermed"]][["CProfunweightedEBT"]][names(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",CProf.List[["Byintermed"]][["CProfunweightedEBT"]][names(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",CProf.List[["Byintermed"]][["CProfunweightedEBT"]][names(CProf.List[["Byintermed"]][["CProfunweightedEBT"]]) %in% TaxhavensEU])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



CProf.List[["Byanyown"]][["CProfweighted"]] <- rbind(CProf.List[["Byanyown"]][["CProfweighted"]], 
                                                     CProf.List[["DeInt"]][["CProfweighted"]],
                                                     CProf.List[["DeDom"]][["CProfweighted"]],
                                                     CProf.List[["Affiliates"]][["CProfweighted"]],
                                                     CProf.List[["GerGUO"]][["CProfweighted"]]
)


CProf.List[["Byanyown"]][["CProfweighted"]]$low95 <- CProf.List[["Byanyown"]][["CProfweighted"]]$CProf - qt(0.975, df= CProf.List[["Byanyown"]][["CProfweighted"]]$n-1) * CProf.List[["Byanyown"]][["CProfweighted"]]$sd /sqrt(CProf.List[["Byanyown"]][["CProfweighted"]]$n)
CProf.List[["Byanyown"]][["CProfweighted"]]$high95 <- CProf.List[["Byanyown"]][["CProfweighted"]]$CProf + qt(0.975, df= CProf.List[["Byanyown"]][["CProfweighted"]]$n-1) * CProf.List[["Byanyown"]][["CProfweighted"]]$sd /sqrt(CProf.List[["Byanyown"]][["CProfweighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, CProf, g, h, i, ISO, j, x, y , z)



