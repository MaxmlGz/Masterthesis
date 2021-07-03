


Assets <- rio::import("ImportEC.xlsx", which = "Results")
Assets <- cbind(data.frame("CompanyBVDID" = c(Assets$`BvD ID number`)),Assets[,4:13])
Assets <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), Assets, by = "CompanyBVDID")


Turnover <- rio::import("ImportTurnover.xlsx", which = "Results")
Turnover <- cbind(data.frame("CompanyBVDID" = c(Turnover$`BvD ID number`)),Turnover[,4:13])
Turnover <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), Turnover, by = "CompanyBVDID")

CProd.List <- vector(mode = "list")
CProd.List[[1]] <- vector(mode = "list")
names(CProd.List) <- "ByCSH"

#Domestic firms unweightet CProd


CProd.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProd.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(CProd.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(CProd.List[["DeDom"]][["CompanyList"]])) {
  CProd.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(CProd.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- CProd.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(CProd.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(CProd.List[["DeDom"]][["CompanyList"]][[i]])))
}

CProd.List[["DeDom"]][["CProdunweightedAssets"]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)

for (i in 1:nrow(CProd.List[["DeDom"]][["CProdunweightedAssets"]])) {
  for (j in 2:ncol(CProd.List[["DeDom"]][["CProdunweightedAssets"]])) {
    CProd.List[["DeDom"]][["CProdunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["DeDom"]][["CProdunweightedAssets"]][i,j])) & CProd.List[["DeDom"]][["CProdunweightedAssets"]][i,1] %in% CProd.List[["DeDom"]][["CompanyList"]][[(j-1)]], CProd.List[["DeDom"]][["CProdunweightedAssets"]][i,j], NA)
  }}



for (i in 1:length(CProd.List[["DeDom"]][["CompanyList"]])) {
  CProd.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(CProd.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- CProd.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(CProd.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(CProd.List[["DeDom"]][["CompanyList"]][[i]])))
}


CProd.List[["DeDom"]][["CProdunweightedTurnover"]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(CProd.List[["DeDom"]][["CProdunweightedTurnover"]])) {
  for (j in 2:ncol(CProd.List[["DeDom"]][["CProdunweightedTurnover"]])) {
    CProd.List[["DeDom"]][["CProdunweightedTurnover"]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["DeDom"]][["CProdunweightedTurnover"]][i,j])) & CProd.List[["DeDom"]][["CProdunweightedTurnover"]][i,1] %in% CProd.List[["DeDom"]][["CompanyList"]][[(j-1)]], CProd.List[["DeDom"]][["CProdunweightedTurnover"]][i,j], NA)
  }}


for(i in 1:nrow(CProd.List[["DeDom"]][["CProdunweightedAssets"]])) {
  for(j in 2:ncol(CProd.List[["DeDom"]][["CProdunweightedAssets"]])) {
    CProd.List[["DeDom"]][["CProdunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["DeDom"]][["CProdunweightedAssets"]][i,j])) & !is.na(as.numeric(CProd.List[["DeDom"]][["CProdunweightedTurnover"]][i,j])) ,  as.numeric(CProd.List[["DeDom"]][["CProdunweightedAssets"]][i,j])  , NA  )
  }}


for(i in 1:nrow(CProd.List[["DeDom"]][["CProdunweightedTurnover"]])) {
  for(j in 2:ncol(CProd.List[["DeDom"]][["CProdunweightedTurnover"]])) {
    CProd.List[["DeDom"]][["CProdunweightedTurnover"]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["DeDom"]][["CProdunweightedAssets"]][i,j])) & !is.na(as.numeric(CProd.List[["DeDom"]][["CProdunweightedTurnover"]][i,j])) ,  as.numeric(CProd.List[["DeDom"]][["CProdunweightedTurnover"]][i,j])  , NA  )
  }}


CProd.List[["DeDom"]][["CProdunweightedAssets"]]$sum <- sapply(1:nrow(CProd.List[["DeDom"]][["CProdunweightedAssets"]]), function (y) sum(as.numeric(CProd.List[["DeDom"]][["CProdunweightedAssets"]][y,2:11]) , na.rm = TRUE ))
CProd.List[["DeDom"]][["CProdunweightedAssets"]]$sum[CProd.List[["DeDom"]][["CProdunweightedAssets"]]$sum == 0] <- NA

CProd.List[["DeDom"]][["CProdunweightedTurnover"]]$sum <- sapply(1:nrow(CProd.List[["DeDom"]][["CProdunweightedTurnover"]]), function (y) sum(as.numeric(CProd.List[["DeDom"]][["CProdunweightedTurnover"]][y,2:11]), na.rm = TRUE))
CProd.List[["DeDom"]][["CProdunweightedTurnover"]]$sum[CProd.List[["DeDom"]][["CProdunweightedTurnover"]]$sum == 0] <- NA
CProd.List[["DeDom"]][["CProdunweightedTurnover"]]$CProd <- sapply(1:nrow(CProd.List[["DeDom"]][["CProdunweightedTurnover"]]), function (y) CProd.List[["DeDom"]][["CProdunweightedTurnover"]][[y,12]] / CProd.List[["DeDom"]][["CProdunweightedAssets"]][[y,12]])



CProd.List[["DeDom"]][["CProdunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                         "CProd" = mean(CProd.List[["DeDom"]][["CProdunweightedTurnover"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(CProd.List[["DeDom"]][["CProdunweightedTurnover"]][,13], na.rm = TRUE),
                                                         "n" = length(CProd.List[["DeDom"]][["CProdunweightedTurnover"]][,13][!is.na(as.numeric(CProd.List[["DeDom"]][["CProdunweightedTurnover"]][,13]))]))

CProd.List[["DeDom"]][["CProdunweighted"]]$low95 <- CProd.List[["DeDom"]][["CProdunweighted"]]$CProd - qt(0.975, df= CProd.List[["DeDom"]][["CProdunweighted"]]$n-1) * CProd.List[["DeDom"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["DeDom"]][["CProdunweighted"]]$n)
CProd.List[["DeDom"]][["CProdunweighted"]]$high95 <- CProd.List[["DeDom"]][["CProdunweighted"]]$CProd + qt(0.975, df= CProd.List[["DeDom"]][["CProdunweighted"]]$n-1) * CProd.List[["DeDom"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["DeDom"]][["CProdunweighted"]]$n)



#International firms unweightet CProd


CProd.List[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProd.List[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(CProd.List[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(CProd.List[["DeInt"]][["CompanyList"]])) {
  CProd.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(CProd.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- CProd.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(CProd.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(CProd.List[["DeInt"]][["CompanyList"]][[i]])))
}

CProd.List[["DeInt"]][["CProdunweightedAssets"]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)

for (i in 1:nrow(CProd.List[["DeInt"]][["CProdunweightedAssets"]])) {
  for (j in 2:ncol(CProd.List[["DeInt"]][["CProdunweightedAssets"]])) {
    CProd.List[["DeInt"]][["CProdunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["DeInt"]][["CProdunweightedAssets"]][i,j])) & CProd.List[["DeInt"]][["CProdunweightedAssets"]][i,1] %in% CProd.List[["DeInt"]][["CompanyList"]][[(j-1)]], CProd.List[["DeInt"]][["CProdunweightedAssets"]][i,j], NA)
  }}



for (i in 1:length(CProd.List[["DeInt"]][["CompanyList"]])) {
  CProd.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(CProd.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- CProd.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(CProd.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(CProd.List[["DeInt"]][["CompanyList"]][[i]])))
}


CProd.List[["DeInt"]][["CProdunweightedTurnover"]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(CProd.List[["DeInt"]][["CProdunweightedTurnover"]])) {
  for (j in 2:ncol(CProd.List[["DeInt"]][["CProdunweightedTurnover"]])) {
    CProd.List[["DeInt"]][["CProdunweightedTurnover"]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["DeInt"]][["CProdunweightedTurnover"]][i,j])) & CProd.List[["DeInt"]][["CProdunweightedTurnover"]][i,1] %in% CProd.List[["DeInt"]][["CompanyList"]][[(j-1)]], CProd.List[["DeInt"]][["CProdunweightedTurnover"]][i,j], NA)
  }}


for(i in 1:nrow(CProd.List[["DeInt"]][["CProdunweightedAssets"]])) {
  for(j in 2:ncol(CProd.List[["DeInt"]][["CProdunweightedAssets"]])) {
    CProd.List[["DeInt"]][["CProdunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["DeInt"]][["CProdunweightedAssets"]][i,j])) & !is.na(as.numeric(CProd.List[["DeInt"]][["CProdunweightedTurnover"]][i,j])) ,  as.numeric(CProd.List[["DeInt"]][["CProdunweightedAssets"]][i,j])  , NA  )
  }}


for(i in 1:nrow(CProd.List[["DeInt"]][["CProdunweightedTurnover"]])) {
  for(j in 2:ncol(CProd.List[["DeInt"]][["CProdunweightedTurnover"]])) {
    CProd.List[["DeInt"]][["CProdunweightedTurnover"]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["DeInt"]][["CProdunweightedAssets"]][i,j])) & !is.na(as.numeric(CProd.List[["DeInt"]][["CProdunweightedTurnover"]][i,j])) ,  as.numeric(CProd.List[["DeInt"]][["CProdunweightedTurnover"]][i,j])  , NA  )
  }}


CProd.List[["DeInt"]][["CProdunweightedAssets"]]$sum <- sapply(1:nrow(CProd.List[["DeInt"]][["CProdunweightedAssets"]]), function (y) sum(as.numeric(CProd.List[["DeInt"]][["CProdunweightedAssets"]][y,2:11]) , na.rm = TRUE ))
CProd.List[["DeInt"]][["CProdunweightedAssets"]]$sum[CProd.List[["DeInt"]][["CProdunweightedAssets"]]$sum == 0] <- NA

CProd.List[["DeInt"]][["CProdunweightedTurnover"]]$sum <- sapply(1:nrow(CProd.List[["DeInt"]][["CProdunweightedTurnover"]]), function (y) sum(as.numeric(CProd.List[["DeInt"]][["CProdunweightedTurnover"]][y,2:11]), na.rm = TRUE))
CProd.List[["DeInt"]][["CProdunweightedTurnover"]]$sum[CProd.List[["DeInt"]][["CProdunweightedTurnover"]]$sum == 0] <- NA
CProd.List[["DeInt"]][["CProdunweightedTurnover"]]$CProd <- sapply(1:nrow(CProd.List[["DeInt"]][["CProdunweightedTurnover"]]), function (y) CProd.List[["DeInt"]][["CProdunweightedTurnover"]][[y,12]] / CProd.List[["DeInt"]][["CProdunweightedAssets"]][[y,12]])



CProd.List[["DeInt"]][["CProdunweighted"]] <- data.frame("ISO" = "DEINT", 
                                                         "CProd" = mean(CProd.List[["DeInt"]][["CProdunweightedTurnover"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(CProd.List[["DeInt"]][["CProdunweightedTurnover"]][,13], na.rm = TRUE),
                                                         "n" = length(CProd.List[["DeInt"]][["CProdunweightedTurnover"]][,13][!is.na(as.numeric(CProd.List[["DeInt"]][["CProdunweightedTurnover"]][,13]))]))


CProd.List[["DeInt"]][["CProdunweighted"]]$low95 <- CProd.List[["DeInt"]][["CProdunweighted"]]$CProd - qt(0.975, df= CProd.List[["DeInt"]][["CProdunweighted"]]$n-1) * CProd.List[["DeInt"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["DeInt"]][["CProdunweighted"]]$n)
CProd.List[["DeInt"]][["CProdunweighted"]]$high95 <- CProd.List[["DeInt"]][["CProdunweighted"]]$CProd + qt(0.975, df= CProd.List[["DeInt"]][["CProdunweighted"]]$n-1) * CProd.List[["DeInt"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["DeInt"]][["CProdunweighted"]]$n)




#CSH unweighted CProd


CProd.List[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProd.List[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(CProd.List[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProd.List[["ByCSH"]][["CompanyList"]])) {CProd.List[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CProd.List[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(CProd.List[["ByCSH"]][["CompanyList"]][[i]])) {
    CProd.List[["ByCSH"]][["CompanyList"]][[i]][[j]] <- CProd.List[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(CProd.List[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(CProd.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(CProd.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProd.List[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

CProd.List[["ByCSH"]][["CProdunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["ByCSH"]][["CProdunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(CProd.List[["ByCSH"]][["CProdunweightedAssets"]])) {
  Temp1 <- CProd.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[i]]) == 0 ) {CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["ByCSH"]][["CProdunweightedAssets"]])) {
  for (i in 1:nrow(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]][i,j])) & CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]][i,1] %in% CProd.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]][i,j], NA)
    }}}



CProd.List[["ByCSH"]][["CProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]])) {
  Temp1 <- CProd.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[i]]) == 0 ) {CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]])) {
  for (i in 1:nrow(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]][i,j])) & CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]][i,1] %in% CProd.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProd.List[["ByCSH"]][["CProdunweightedAssets"]])) {
  for(i in 1:nrow(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]])) {
  for(i in 1:nrow(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProd.List[["ByCSH"]][["CProdunweightedAssets"]])) {
  CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]]$sum[CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]])) {
  CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]]$sum[CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]]$CProd <- sapply(1:nrow(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]]), function (y) CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[x]][[y,12]] / CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[x]][[y,12]])
}


CProd.List[["ByCSH"]][["CProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]]),function(y) mean(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]]),function(y) sd(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["ByCSH"]][["CProdunweighted"]]$low95 <- CProd.List[["ByCSH"]][["CProdunweighted"]]$CProd - qt(0.975, df= CProd.List[["ByCSH"]][["CProdunweighted"]]$n-1) * CProd.List[["ByCSH"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["ByCSH"]][["CProdunweighted"]]$n)
CProd.List[["ByCSH"]][["CProdunweighted"]]$high95 <- CProd.List[["ByCSH"]][["CProdunweighted"]]$CProd + qt(0.975, df= CProd.List[["ByCSH"]][["CProdunweighted"]]$n-1) * CProd.List[["ByCSH"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["ByCSH"]][["CProdunweighted"]]$n)





#GUO unweighted CProd


CProd.List[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProd.List[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(CProd.List[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProd.List[["ByGUO"]][["CompanyList"]])) {CProd.List[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(CProd.List[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(CProd.List[["ByGUO"]][["CompanyList"]][[i]])) {
    CProd.List[["ByGUO"]][["CompanyList"]][[i]][[j]] <- CProd.List[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(CProd.List[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(CProd.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(CProd.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProd.List[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

CProd.List[["ByGUO"]][["CProdunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["ByGUO"]][["CProdunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["ByGUO"]][["CProdunweightedAssets"]])) {
  Temp1 <- CProd.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[i]]) == 0 ) {CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["ByGUO"]][["CProdunweightedAssets"]])) {
  for (i in 1:nrow(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]][i,j])) & CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]][i,1] %in% CProd.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]][i,j], NA)
    }}}



CProd.List[["ByGUO"]][["CProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]])) {
  Temp1 <- CProd.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[i]]) == 0 ) {CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]])) {
  for (i in 1:nrow(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]][i,j])) & CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]][i,1] %in% CProd.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProd.List[["ByGUO"]][["CProdunweightedAssets"]])) {
  for(i in 1:nrow(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]])) {
  for(i in 1:nrow(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProd.List[["ByGUO"]][["CProdunweightedAssets"]])) {
  CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]]$sum[CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]])) {
  CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]]$sum[CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]]$CProd <- sapply(1:nrow(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]]), function (y) CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[x]][[y,12]] / CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[x]][[y,12]])
}


CProd.List[["ByGUO"]][["CProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]]),function(y) mean(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]]),function(y) sd(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["ByGUO"]][["CProdunweighted"]]$low95 <- CProd.List[["ByGUO"]][["CProdunweighted"]]$CProd - qt(0.975, df= CProd.List[["ByGUO"]][["CProdunweighted"]]$n-1) * CProd.List[["ByGUO"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["ByGUO"]][["CProdunweighted"]]$n)
CProd.List[["ByGUO"]][["CProdunweighted"]]$high95 <- CProd.List[["ByGUO"]][["CProdunweighted"]]$CProd + qt(0.975, df= CProd.List[["ByGUO"]][["CProdunweighted"]]$n-1) * CProd.List[["ByGUO"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["ByGUO"]][["CProdunweighted"]]$n)




#anyown unweighted CProd


CProd.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProd.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(CProd.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProd.List[["Byanyown"]][["CompanyList"]])) {CProd.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CProd.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(CProd.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(CProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {CProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(CProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProd.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- CProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- CProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CProd.List[["Byanyown"]][["CProdunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["Byanyown"]][["CProdunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["Byanyown"]][["CProdunweightedAssets"]])) {
  Temp1 <- CProd.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[i]]) == 0 ) {CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["Byanyown"]][["CProdunweightedAssets"]])) {
  for (i in 1:nrow(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]][i,j])) & CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]][i,1] %in% CProd.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]][i,j], NA)
    }}}



CProd.List[["Byanyown"]][["CProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]])) {
  Temp1 <- CProd.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[i]]) == 0 ) {CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]])) {
  for (i in 1:nrow(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]][i,j])) & CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]][i,1] %in% CProd.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProd.List[["Byanyown"]][["CProdunweightedAssets"]])) {
  for(i in 1:nrow(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]])) {
  for(i in 1:nrow(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProd.List[["Byanyown"]][["CProdunweightedAssets"]])) {
  CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]]$sum[CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]])) {
  CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]]$sum[CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]]$CProd <- sapply(1:nrow(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]]), function (y) CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[x]][[y,12]] / CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[x]][[y,12]])
}


CProd.List[["Byanyown"]][["CProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]),function(y) mean(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]),function(y) sd(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["Byanyown"]][["CProdunweighted"]]$low95 <- CProd.List[["Byanyown"]][["CProdunweighted"]]$CProd - qt(0.975, df= CProd.List[["Byanyown"]][["CProdunweighted"]]$n-1) * CProd.List[["Byanyown"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["Byanyown"]][["CProdunweighted"]]$n)
CProd.List[["Byanyown"]][["CProdunweighted"]]$high95 <- CProd.List[["Byanyown"]][["CProdunweighted"]]$CProd + qt(0.975, df= CProd.List[["Byanyown"]][["CProdunweighted"]]$n-1) * CProd.List[["Byanyown"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["Byanyown"]][["CProdunweighted"]]$n)


#CProd unweighted Loops


CProd.List[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProd.List[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(CProd.List[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(CProd.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(CProd.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CProd.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    CProd.List[["Loop"]][["CompanyList"]][[i]][[j]] <- CProd.List[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(CProd.List[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(CProd.List[["Loop"]][["CompanyList"]])) {CProd.List[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CProd.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(CProd.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(CProd.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(CProd.List[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CProd.List[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {CProd.List[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(CProd.List[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CProd.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProd.List[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CProd.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- CProd.List[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CProd.List[["Loop"]][["CompanyList"]][[i]][[j]] <- CProd.List[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CProd.List[["Loop"]][["CProdunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["Loop"]][["CProdunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["Loop"]][["CProdunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["Loop"]][["CProdunweightedAssets"]])) {
  Temp1 <- CProd.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["Loop"]][["CProdunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["Loop"]][["CProdunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["Loop"]][["CProdunweightedAssets"]][[i]]) == 0 ) {CProd.List[["Loop"]][["CProdunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["Loop"]][["CProdunweightedAssets"]])) {
  for (i in 1:nrow(CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]][i,j])) & CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]][i,1] %in% CProd.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]][i,j], NA)
    }}}



CProd.List[["Loop"]][["CProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["Loop"]][["CProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["Loop"]][["CProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]])) {
  Temp1 <- CProd.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["Loop"]][["CProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[i]]) == 0 ) {CProd.List[["Loop"]][["CProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]])) {
  for (i in 1:nrow(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]][i,j])) & CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]][i,1] %in% CProd.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProd.List[["Loop"]][["CProdunweightedAssets"]])) {
  for(i in 1:nrow(CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]])) {
  for(i in 1:nrow(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProd.List[["Loop"]][["CProdunweightedAssets"]])) {
  CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]]$sum[CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]])) {
  CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]]$sum[CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]]$CProd <- sapply(1:nrow(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]]), function (y) CProd.List[["Loop"]][["CProdunweightedTurnover"]][[x]][[y,12]] / CProd.List[["Loop"]][["CProdunweightedAssets"]][[x]][[y,12]])
}


CProd.List[["Loop"]][["CProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]]),function(y) mean(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]]),function(y) sd(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["Loop"]][["CProdunweighted"]]$low95 <- CProd.List[["Loop"]][["CProdunweighted"]]$CProd - qt(0.975, df= CProd.List[["Loop"]][["CProdunweighted"]]$n-1) * CProd.List[["Loop"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["Loop"]][["CProdunweighted"]]$n)
CProd.List[["Loop"]][["CProdunweighted"]]$high95 <- CProd.List[["Loop"]][["CProdunweighted"]]$CProd + qt(0.975, df= CProd.List[["Loop"]][["CProdunweighted"]]$n-1) * CProd.List[["Loop"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["Loop"]][["CProdunweighted"]]$n)




#anysub unweighted CProd


CProd.List[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProd.List[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(CProd.List[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProd.List[["Byanysub"]][["CompanyList"]])) {CProd.List[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(CProd.List[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(CProd.List[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(CProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(CProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {CProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(CProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(CProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(CProd.List[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(CProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- CProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    CProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- CProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



CProd.List[["Byanysub"]][["CProdunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["Byanysub"]][["CProdunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["Byanysub"]][["CProdunweightedAssets"]])) {
  Temp1 <- CProd.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[i]]) == 0 ) {CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["Byanysub"]][["CProdunweightedAssets"]])) {
  for (i in 1:nrow(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]][i,j])) & CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]][i,1] %in% CProd.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]][i,j], NA)
    }}}



CProd.List[["Byanysub"]][["CProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]])) {
  Temp1 <- CProd.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[i]]) == 0 ) {CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]])) {
  for (i in 1:nrow(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]][i,j])) & CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]][i,1] %in% CProd.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProd.List[["Byanysub"]][["CProdunweightedAssets"]])) {
  for(i in 1:nrow(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]])) {
  for(i in 1:nrow(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProd.List[["Byanysub"]][["CProdunweightedAssets"]])) {
  CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]]$sum[CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]])) {
  CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]]$sum[CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]]$CProd <- sapply(1:nrow(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]]), function (y) CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[x]][[y,12]] / CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[x]][[y,12]])
}


CProd.List[["Byanysub"]][["CProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]]),function(y) mean(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]]),function(y) sd(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["Byanysub"]][["CProdunweighted"]]$low95 <- CProd.List[["Byanysub"]][["CProdunweighted"]]$CProd - qt(0.975, df= CProd.List[["Byanysub"]][["CProdunweighted"]]$n-1) * CProd.List[["Byanysub"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["Byanysub"]][["CProdunweighted"]]$n)
CProd.List[["Byanysub"]][["CProdunweighted"]]$high95 <- CProd.List[["Byanysub"]][["CProdunweighted"]]$CProd + qt(0.975, df= CProd.List[["Byanysub"]][["CProdunweighted"]]$n-1) * CProd.List[["Byanysub"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["Byanysub"]][["CProdunweighted"]]$n)




#anysubGER unweighted CProd


CProd.List[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {CProd.List[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(CProd.List[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(CProd.List[["ByanysubGER"]][["CompanyList"]])) {CProd.List[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(CProd.List[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(CProd.List[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    CProd.List[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- CProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(CProd.List[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(CProd.List[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]])) {
  Temp1 <- CProd.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[i]]) == 0 ) {CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]])) {
  for (i in 1:nrow(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]])) {
    for (j in 2:ncol(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]][i,j])) & CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]][i,1] %in% CProd.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]][i,j], NA)
    }}}



CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]])) {
  Temp1 <- CProd.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,CProd.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[i]]) == 0 ) {CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]])) {
  for (i in 1:nrow(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]][i,j])) & CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]][i,1] %in% CProd.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]])) {
  for(i in 1:nrow(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]])) {
    for(j in 2:ncol(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]])) {
      CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]])) {
  for(i in 1:nrow(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]])) {
      CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]])) {
  CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]]$sum <- sapply(1:nrow(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]]), function (y) sum(as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]]$sum[CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]])) {
  CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]]$sum[CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]]$CProd <- sapply(1:nrow(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]]), function (y) CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[x]][[y,12]] / CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[x]][[y,12]])
}


CProd.List[["ByanysubGER"]][["CProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]),function(y) mean(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                               "sd" = c(sapply(1:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]),function(y) sd(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                               "n" = c(sapply(1:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["ByanysubGER"]][["CProdunweighted"]]$low95 <- CProd.List[["ByanysubGER"]][["CProdunweighted"]]$CProd - qt(0.975, df= CProd.List[["ByanysubGER"]][["CProdunweighted"]]$n-1) * CProd.List[["ByanysubGER"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["ByanysubGER"]][["CProdunweighted"]]$n)
CProd.List[["ByanysubGER"]][["CProdunweighted"]]$high95 <- CProd.List[["ByanysubGER"]][["CProdunweighted"]]$CProd + qt(0.975, df= CProd.List[["ByanysubGER"]][["CProdunweighted"]]$n-1) * CProd.List[["ByanysubGER"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["ByanysubGER"]][["CProdunweighted"]]$n)




#Affiliates  unweighted CProd


CProd.List[["Affiliates"]][["CProdunweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                              "CProd" = mean(unique(Reduce("rbind", CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]))[,13], na.rm = TRUE), 
                                                              "sd" = sd(unique(Reduce("rbind", CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]))[,13], na.rm = TRUE),
                                                              "n" = length(unique(Reduce("rbind", CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]))[,13][!is.na(unique(Reduce("rbind", CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]))[,13])]))




CProd.List[["Affiliates"]][["CProdunweighted"]]$low95 <- CProd.List[["Affiliates"]][["CProdunweighted"]]$CProd - qt(0.975, df= CProd.List[["Affiliates"]][["CProdunweighted"]]$n-1) * CProd.List[["Affiliates"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["Affiliates"]][["CProdunweighted"]]$n)
CProd.List[["Affiliates"]][["CProdunweighted"]]$high95 <- CProd.List[["Affiliates"]][["CProdunweighted"]]$CProd + qt(0.975, df= CProd.List[["Affiliates"]][["CProdunweighted"]]$n-1) * CProd.List[["Affiliates"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["Affiliates"]][["CProdunweighted"]]$n)




#GerGUO unweighted CProd


CProd.List[["GerGUO"]][["CProdunweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                          "CProd" = mean(unique(Reduce("rbind", CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]))[,13], na.rm = TRUE), 
                                                          "sd" = sd(unique(Reduce("rbind", CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]))[,13], na.rm = TRUE),
                                                          "n" = length(unique(Reduce("rbind", CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]))[,13][!is.na(unique(Reduce("rbind", CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]))[,13])]))




CProd.List[["GerGUO"]][["CProdunweighted"]]$low95 <- CProd.List[["GerGUO"]][["CProdunweighted"]]$CProd - qt(0.975, df= CProd.List[["GerGUO"]][["CProdunweighted"]]$n-1) * CProd.List[["GerGUO"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["GerGUO"]][["CProdunweighted"]]$n)
CProd.List[["GerGUO"]][["CProdunweighted"]]$high95 <- CProd.List[["GerGUO"]][["CProdunweighted"]]$CProd + qt(0.975, df= CProd.List[["GerGUO"]][["CProdunweighted"]]$n-1) * CProd.List[["GerGUO"]][["CProdunweighted"]]$sd /sqrt(CProd.List[["GerGUO"]][["CProdunweighted"]]$n)





#Domestic firms weighted CProd


CProd.List[["DeDom"]][["CProdweighted"]] <- data.frame("ISO" = "DEDOM")
CProd.List[["DeDom"]][["CProdweighted"]]$CProd <- sum(CProd.List[["DeDom"]][["CProdunweightedTurnover"]]$sum, na.rm = TRUE)  / sum(CProd.List[["DeDom"]][["CProdunweightedAssets"]]$sum, na.rm = TRUE)
CProd.List[["DeDom"]][["CProdweighted"]]$sd <- sqrt(wtd.var(CProd.List[["DeDom"]][["CProdunweightedTurnover"]]$CProd, sqrt(CProd.List[["DeDom"]][["CProdunweightedAssets"]]$sum^2), na.rm = TRUE ))
CProd.List[["DeDom"]][["CProdweighted"]]$n <- length(CProd.List[["DeDom"]][["CProdunweightedTurnover"]]$CProd[!is.na(CProd.List[["DeDom"]][["CProdunweightedTurnover"]]$CProd)])
CProd.List[["DeDom"]][["CProdweighted"]]$low95 <- CProd.List[["DeDom"]][["CProdweighted"]]$CProd - qt(0.975, df = CProd.List[["DeDom"]][["CProdweighted"]]$n-1) * CProd.List[["DeDom"]][["CProdweighted"]]$sd / sqrt(CProd.List[["DeDom"]][["CProdweighted"]]$n)
CProd.List[["DeDom"]][["CProdweighted"]]$high95 <- CProd.List[["DeDom"]][["CProdweighted"]]$CProd + qt(0.975, df = CProd.List[["DeDom"]][["CProdweighted"]]$n-1) * CProd.List[["DeDom"]][["CProdweighted"]]$sd / sqrt(CProd.List[["DeDom"]][["CProdweighted"]]$n)


#International firms weighted CProd


CProd.List[["DeInt"]][["CProdweighted"]] <- data.frame("ISO" = "DEINT")
CProd.List[["DeInt"]][["CProdweighted"]]$CProd <- sum(CProd.List[["DeInt"]][["CProdunweightedTurnover"]]$sum, na.rm = TRUE)  / sum(CProd.List[["DeInt"]][["CProdunweightedAssets"]]$sum, na.rm = TRUE)
CProd.List[["DeInt"]][["CProdweighted"]]$sd <- sqrt(wtd.var(CProd.List[["DeInt"]][["CProdunweightedTurnover"]]$CProd, sqrt(CProd.List[["DeInt"]][["CProdunweightedAssets"]]$sum^2), na.rm = TRUE ))
CProd.List[["DeInt"]][["CProdweighted"]]$n <- length(CProd.List[["DeInt"]][["CProdunweightedTurnover"]]$CProd[!is.na(CProd.List[["DeInt"]][["CProdunweightedTurnover"]]$CProd)])
CProd.List[["DeInt"]][["CProdweighted"]]$low95 <- CProd.List[["DeInt"]][["CProdweighted"]]$CProd - qt(0.975, df = CProd.List[["DeInt"]][["CProdweighted"]]$n-1) * CProd.List[["DeInt"]][["CProdweighted"]]$sd / sqrt(CProd.List[["DeInt"]][["CProdweighted"]]$n)
CProd.List[["DeInt"]][["CProdweighted"]]$high95 <- CProd.List[["DeInt"]][["CProdweighted"]]$CProd + qt(0.975, df = CProd.List[["DeInt"]][["CProdweighted"]]$n-1) * CProd.List[["DeInt"]][["CProdweighted"]]$sd / sqrt(CProd.List[["DeInt"]][["CProdweighted"]]$n)


#CSH firms weighted CProd 

CProd.List[["ByCSH"]][["CProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]]),function(y) sum(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]]),function(y) sqrt(wtd.var(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[y]][,13], sqrt(CProd.List[["ByCSH"]][["CProdunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["ByCSH"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["ByCSH"]][["CProdweighted"]]$low95 <- CProd.List[["ByCSH"]][["CProdweighted"]]$CProd - qt(0.975, df= CProd.List[["ByCSH"]][["CProdweighted"]]$n-1) * CProd.List[["ByCSH"]][["CProdweighted"]]$sd /sqrt(CProd.List[["ByCSH"]][["CProdweighted"]]$n)
CProd.List[["ByCSH"]][["CProdweighted"]]$high95 <- CProd.List[["ByCSH"]][["CProdweighted"]]$CProd + qt(0.975, df= CProd.List[["ByCSH"]][["CProdweighted"]]$n-1) * CProd.List[["ByCSH"]][["CProdweighted"]]$sd /sqrt(CProd.List[["ByCSH"]][["CProdweighted"]]$n)



#GUO firms weighted CProd 

CProd.List[["ByGUO"]][["CProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]]),function(y) sum(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]]),function(y) sqrt(wtd.var(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[y]][,13], sqrt(CProd.List[["ByGUO"]][["CProdunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["ByGUO"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["ByGUO"]][["CProdweighted"]]$low95 <- CProd.List[["ByGUO"]][["CProdweighted"]]$CProd - qt(0.975, df= CProd.List[["ByGUO"]][["CProdweighted"]]$n-1) * CProd.List[["ByGUO"]][["CProdweighted"]]$sd /sqrt(CProd.List[["ByGUO"]][["CProdweighted"]]$n)
CProd.List[["ByGUO"]][["CProdweighted"]]$high95 <- CProd.List[["ByGUO"]][["CProdweighted"]]$CProd + qt(0.975, df= CProd.List[["ByGUO"]][["CProdweighted"]]$n-1) * CProd.List[["ByGUO"]][["CProdweighted"]]$sd /sqrt(CProd.List[["ByGUO"]][["CProdweighted"]]$n)


#anyown firms weighted CProd 

CProd.List[["Byanyown"]][["CProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]),function(y) sum(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]),function(y) sqrt(wtd.var(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[y]][,13], sqrt(CProd.List[["Byanyown"]][["CProdunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["Byanyown"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["Byanyown"]][["CProdweighted"]]$low95 <- CProd.List[["Byanyown"]][["CProdweighted"]]$CProd - qt(0.975, df= CProd.List[["Byanyown"]][["CProdweighted"]]$n-1) * CProd.List[["Byanyown"]][["CProdweighted"]]$sd /sqrt(CProd.List[["Byanyown"]][["CProdweighted"]]$n)
CProd.List[["Byanyown"]][["CProdweighted"]]$high95 <- CProd.List[["Byanyown"]][["CProdweighted"]]$CProd + qt(0.975, df= CProd.List[["Byanyown"]][["CProdweighted"]]$n-1) * CProd.List[["Byanyown"]][["CProdweighted"]]$sd /sqrt(CProd.List[["Byanyown"]][["CProdweighted"]]$n)


#Loops firms weighted CProd 

CProd.List[["Loop"]][["CProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]]),function(y) sum(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(CProd.List[["Loop"]][["CProdunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]]),function(y) sqrt(wtd.var(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[y]][,13], sqrt(CProd.List[["Loop"]][["CProdunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(CProd.List[["Loop"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["Loop"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["Loop"]][["CProdweighted"]]$low95 <- CProd.List[["Loop"]][["CProdweighted"]]$CProd - qt(0.975, df= CProd.List[["Loop"]][["CProdweighted"]]$n-1) * CProd.List[["Loop"]][["CProdweighted"]]$sd /sqrt(CProd.List[["Loop"]][["CProdweighted"]]$n)
CProd.List[["Loop"]][["CProdweighted"]]$high95 <- CProd.List[["Loop"]][["CProdweighted"]]$CProd + qt(0.975, df= CProd.List[["Loop"]][["CProdweighted"]]$n-1) * CProd.List[["Loop"]][["CProdweighted"]]$sd /sqrt(CProd.List[["Loop"]][["CProdweighted"]]$n)



#anysub firms weighted CProd 

CProd.List[["Byanysub"]][["CProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]]),function(y) sum(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]]),function(y) sqrt(wtd.var(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[y]][,13], sqrt(CProd.List[["Byanysub"]][["CProdunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["Byanysub"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["Byanysub"]][["CProdweighted"]]$low95 <- CProd.List[["Byanysub"]][["CProdweighted"]]$CProd - qt(0.975, df= CProd.List[["Byanysub"]][["CProdweighted"]]$n-1) * CProd.List[["Byanysub"]][["CProdweighted"]]$sd /sqrt(CProd.List[["Byanysub"]][["CProdweighted"]]$n)
CProd.List[["Byanysub"]][["CProdweighted"]]$high95 <- CProd.List[["Byanysub"]][["CProdweighted"]]$CProd + qt(0.975, df= CProd.List[["Byanysub"]][["CProdweighted"]]$n-1) * CProd.List[["Byanysub"]][["CProdweighted"]]$sd /sqrt(CProd.List[["Byanysub"]][["CProdweighted"]]$n)


#anysubGER firms weighted CProd 

CProd.List[["ByanysubGER"]][["CProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "CProd" = c(sapply(1:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]),function(y) sum(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                             "sd" = c(sapply(1:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]),function(y) sqrt(wtd.var(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[y]][,13], sqrt(CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                             "n" = c(sapply(1:length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]),function(y) length(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]][[y]][,13]))]))))


CProd.List[["ByanysubGER"]][["CProdweighted"]]$low95 <- CProd.List[["ByanysubGER"]][["CProdweighted"]]$CProd - qt(0.975, df= CProd.List[["ByanysubGER"]][["CProdweighted"]]$n-1) * CProd.List[["ByanysubGER"]][["CProdweighted"]]$sd /sqrt(CProd.List[["ByanysubGER"]][["CProdweighted"]]$n)
CProd.List[["ByanysubGER"]][["CProdweighted"]]$high95 <- CProd.List[["ByanysubGER"]][["CProdweighted"]]$CProd + qt(0.975, df= CProd.List[["ByanysubGER"]][["CProdweighted"]]$n-1) * CProd.List[["ByanysubGER"]][["CProdweighted"]]$sd /sqrt(CProd.List[["ByanysubGER"]][["CProdweighted"]]$n)



#Affiliates  weighted CProd


CProd.List[["Affiliates"]][["CProdweighted"]] <- data.frame("ISO" = "Affiliates")
CProd.List[["Affiliates"]][["CProdweighted"]]$CProd <- sum(unique(Reduce("rbind", CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", CProd.List[["Byanyown"]][["CProdunweightedAssets"]]))[,12], na.rm = TRUE)
CProd.List[["Affiliates"]][["CProdweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", CProd.List[["Byanyown"]][["CProdunweightedTurnover"]])[,13]), sqrt(unique(Reduce("rbind", CProd.List[["Byanyown"]][["CProdunweightedTurnover"]])[,12])^2), na.rm = TRUE ))
CProd.List[["Affiliates"]][["CProdweighted"]]$n <- length(unique(Reduce("rbind", CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]))[,13][!is.na(unique(Reduce("rbind", CProd.List[["Byanyown"]][["CProdunweightedTurnover"]]))[,13])])
CProd.List[["Affiliates"]][["CProdweighted"]]$low95 <- CProd.List[["Affiliates"]][["CProdweighted"]]$CProd - qt(0.975, df = CProd.List[["Affiliates"]][["CProdweighted"]]$n-1) * CProd.List[["Affiliates"]][["CProdweighted"]]$sd / sqrt(CProd.List[["Affiliates"]][["CProdweighted"]]$n)
CProd.List[["Affiliates"]][["CProdweighted"]]$high95 <- CProd.List[["Affiliates"]][["CProdweighted"]]$CProd + qt(0.975, df = CProd.List[["Affiliates"]][["CProdweighted"]]$n-1) * CProd.List[["Affiliates"]][["CProdweighted"]]$sd / sqrt(CProd.List[["Affiliates"]][["CProdweighted"]]$n)



#GerGUO weighted CProd


CProd.List[["GerGUO"]][["CProdweighted"]] <- data.frame("ISO" = "GerGUO")
CProd.List[["GerGUO"]][["CProdweighted"]]$CProd <- sum(unique(Reduce("rbind", CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", CProd.List[["ByanysubGER"]][["CProdunweightedAssets"]]))[,12], na.rm = TRUE)
CProd.List[["GerGUO"]][["CProdweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]])[,13]), sqrt(unique(Reduce("rbind", CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]])[,12])^2), na.rm = TRUE ))
CProd.List[["GerGUO"]][["CProdweighted"]]$n <- length(unique(Reduce("rbind", CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]))[,13][!is.na(unique(Reduce("rbind", CProd.List[["ByanysubGER"]][["CProdunweightedTurnover"]]))[,13])])
CProd.List[["GerGUO"]][["CProdweighted"]]$low95 <- CProd.List[["GerGUO"]][["CProdweighted"]]$CProd - qt(0.975, df = CProd.List[["GerGUO"]][["CProdweighted"]]$n-1) * CProd.List[["GerGUO"]][["CProdweighted"]]$sd / sqrt(CProd.List[["GerGUO"]][["CProdweighted"]]$n)
CProd.List[["GerGUO"]][["CProdweighted"]]$high95 <- CProd.List[["GerGUO"]][["CProdweighted"]]$CProd + qt(0.975, df = CProd.List[["GerGUO"]][["CProdweighted"]]$n-1) * CProd.List[["GerGUO"]][["CProdweighted"]]$sd / sqrt(CProd.List[["GerGUO"]][["CProdweighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, CProd, g, h, i, ISO, j, x, y , z)


















