



EmployeeCost <- rio::import("ImportEC.xlsx", which = "Results")
EmployeeCost <- cbind(data.frame("CompanyBVDID" = c(EmployeeCost$`BvD ID number`)),EmployeeCost[,4:13])
EmployeeCost <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), EmployeeCost, by = "CompanyBVDID")


Turnover <- rio::import("ImportTurnover.xlsx", which = "Results")
Turnover <- cbind(data.frame("CompanyBVDID" = c(Turnover$`BvD ID number`)),Turnover[,4:13])
Turnover <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), Turnover, by = "CompanyBVDID")

LProd.List <- vector(mode = "list")
LProd.List[[1]] <- vector(mode = "list")
names(LProd.List) <- "ByCSH"

#Domestic firms unweightet LProd


LProd.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProd.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(LProd.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(LProd.List[["DeDom"]][["CompanyList"]])) {
  LProd.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(LProd.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- LProd.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(LProd.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(LProd.List[["DeDom"]][["CompanyList"]][[i]])))
}

LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)

for (i in 1:nrow(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]])) {
  for (j in 2:ncol(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]])) {
    LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]][i,j])) & LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]][i,1] %in% LProd.List[["DeDom"]][["CompanyList"]][[(j-1)]], LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]][i,j], NA)
  }}



for (i in 1:length(LProd.List[["DeDom"]][["CompanyList"]])) {
  LProd.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(LProd.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- LProd.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(LProd.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(LProd.List[["DeDom"]][["CompanyList"]][[i]])))
}


LProd.List[["DeDom"]][["LProdunweightedTurnover"]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(LProd.List[["DeDom"]][["LProdunweightedTurnover"]])) {
  for (j in 2:ncol(LProd.List[["DeDom"]][["LProdunweightedTurnover"]])) {
    LProd.List[["DeDom"]][["LProdunweightedTurnover"]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["DeDom"]][["LProdunweightedTurnover"]][i,j])) & LProd.List[["DeDom"]][["LProdunweightedTurnover"]][i,1] %in% LProd.List[["DeDom"]][["CompanyList"]][[(j-1)]], LProd.List[["DeDom"]][["LProdunweightedTurnover"]][i,j], NA)
  }}


for(i in 1:nrow(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]])) {
  for(j in 2:ncol(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]])) {
    LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]][i,j])) & !is.na(as.numeric(LProd.List[["DeDom"]][["LProdunweightedTurnover"]][i,j])) ,  as.numeric(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]][i,j])  , NA  )
  }}


for(i in 1:nrow(LProd.List[["DeDom"]][["LProdunweightedTurnover"]])) {
  for(j in 2:ncol(LProd.List[["DeDom"]][["LProdunweightedTurnover"]])) {
    LProd.List[["DeDom"]][["LProdunweightedTurnover"]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]][i,j])) & !is.na(as.numeric(LProd.List[["DeDom"]][["LProdunweightedTurnover"]][i,j])) ,  as.numeric(LProd.List[["DeDom"]][["LProdunweightedTurnover"]][i,j])  , NA  )
  }}


LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]]$sum <- sapply(1:nrow(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]]), function (y) sum(as.numeric(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]][y,2:11]) , na.rm = TRUE ))
LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]]$sum[LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]]$sum == 0] <- NA

LProd.List[["DeDom"]][["LProdunweightedTurnover"]]$sum <- sapply(1:nrow(LProd.List[["DeDom"]][["LProdunweightedTurnover"]]), function (y) sum(as.numeric(LProd.List[["DeDom"]][["LProdunweightedTurnover"]][y,2:11]), na.rm = TRUE))
LProd.List[["DeDom"]][["LProdunweightedTurnover"]]$sum[LProd.List[["DeDom"]][["LProdunweightedTurnover"]]$sum == 0] <- NA
LProd.List[["DeDom"]][["LProdunweightedTurnover"]]$LProd <- sapply(1:nrow(LProd.List[["DeDom"]][["LProdunweightedTurnover"]]), function (y) LProd.List[["DeDom"]][["LProdunweightedTurnover"]][[y,12]] / LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]][[y,12]])



LProd.List[["DeDom"]][["LProdunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                     "LProd" = mean(LProd.List[["DeDom"]][["LProdunweightedTurnover"]][,13], na.rm = TRUE), 
                                                     "sd" = sd(LProd.List[["DeDom"]][["LProdunweightedTurnover"]][,13], na.rm = TRUE),
                                                     "n" = length(LProd.List[["DeDom"]][["LProdunweightedTurnover"]][,13][!is.na(as.numeric(LProd.List[["DeDom"]][["LProdunweightedTurnover"]][,13]))]))

LProd.List[["DeDom"]][["LProdunweighted"]]$low95 <- LProd.List[["DeDom"]][["LProdunweighted"]]$LProd - qt(0.975, df= LProd.List[["DeDom"]][["LProdunweighted"]]$n-1) * LProd.List[["DeDom"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["DeDom"]][["LProdunweighted"]]$n)
LProd.List[["DeDom"]][["LProdunweighted"]]$high95 <- LProd.List[["DeDom"]][["LProdunweighted"]]$LProd + qt(0.975, df= LProd.List[["DeDom"]][["LProdunweighted"]]$n-1) * LProd.List[["DeDom"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["DeDom"]][["LProdunweighted"]]$n)



#International firms unweightet LProd


LProd.List[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProd.List[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(LProd.List[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(LProd.List[["DeInt"]][["CompanyList"]])) {
  LProd.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(LProd.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- LProd.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(LProd.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(LProd.List[["DeInt"]][["CompanyList"]][[i]])))
}

LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)

for (i in 1:nrow(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]])) {
  for (j in 2:ncol(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]])) {
    LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]][i,j])) & LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]][i,1] %in% LProd.List[["DeInt"]][["CompanyList"]][[(j-1)]], LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]][i,j], NA)
  }}



for (i in 1:length(LProd.List[["DeInt"]][["CompanyList"]])) {
  LProd.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(LProd.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- LProd.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(LProd.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(LProd.List[["DeInt"]][["CompanyList"]][[i]])))
}


LProd.List[["DeInt"]][["LProdunweightedTurnover"]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(LProd.List[["DeInt"]][["LProdunweightedTurnover"]])) {
  for (j in 2:ncol(LProd.List[["DeInt"]][["LProdunweightedTurnover"]])) {
    LProd.List[["DeInt"]][["LProdunweightedTurnover"]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["DeInt"]][["LProdunweightedTurnover"]][i,j])) & LProd.List[["DeInt"]][["LProdunweightedTurnover"]][i,1] %in% LProd.List[["DeInt"]][["CompanyList"]][[(j-1)]], LProd.List[["DeInt"]][["LProdunweightedTurnover"]][i,j], NA)
  }}


for(i in 1:nrow(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]])) {
  for(j in 2:ncol(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]])) {
    LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]][i,j])) & !is.na(as.numeric(LProd.List[["DeInt"]][["LProdunweightedTurnover"]][i,j])) ,  as.numeric(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]][i,j])  , NA  )
  }}


for(i in 1:nrow(LProd.List[["DeInt"]][["LProdunweightedTurnover"]])) {
  for(j in 2:ncol(LProd.List[["DeInt"]][["LProdunweightedTurnover"]])) {
    LProd.List[["DeInt"]][["LProdunweightedTurnover"]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]][i,j])) & !is.na(as.numeric(LProd.List[["DeInt"]][["LProdunweightedTurnover"]][i,j])) ,  as.numeric(LProd.List[["DeInt"]][["LProdunweightedTurnover"]][i,j])  , NA  )
  }}


LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]]$sum <- sapply(1:nrow(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]]), function (y) sum(as.numeric(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]][y,2:11]) , na.rm = TRUE ))
LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]]$sum[LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]]$sum == 0] <- NA

LProd.List[["DeInt"]][["LProdunweightedTurnover"]]$sum <- sapply(1:nrow(LProd.List[["DeInt"]][["LProdunweightedTurnover"]]), function (y) sum(as.numeric(LProd.List[["DeInt"]][["LProdunweightedTurnover"]][y,2:11]), na.rm = TRUE))
LProd.List[["DeInt"]][["LProdunweightedTurnover"]]$sum[LProd.List[["DeInt"]][["LProdunweightedTurnover"]]$sum == 0] <- NA
LProd.List[["DeInt"]][["LProdunweightedTurnover"]]$LProd <- sapply(1:nrow(LProd.List[["DeInt"]][["LProdunweightedTurnover"]]), function (y) LProd.List[["DeInt"]][["LProdunweightedTurnover"]][[y,12]] / LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]][[y,12]])



LProd.List[["DeInt"]][["LProdunweighted"]] <- data.frame("ISO" = "DEINT", 
                                                     "LProd" = mean(LProd.List[["DeInt"]][["LProdunweightedTurnover"]][,13], na.rm = TRUE), 
                                                     "sd" = sd(LProd.List[["DeInt"]][["LProdunweightedTurnover"]][,13], na.rm = TRUE),
                                                     "n" = length(LProd.List[["DeInt"]][["LProdunweightedTurnover"]][,13][!is.na(as.numeric(LProd.List[["DeInt"]][["LProdunweightedTurnover"]][,13]))]))


LProd.List[["DeInt"]][["LProdunweighted"]]$low95 <- LProd.List[["DeInt"]][["LProdunweighted"]]$LProd - qt(0.975, df= LProd.List[["DeInt"]][["LProdunweighted"]]$n-1) * LProd.List[["DeInt"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["DeInt"]][["LProdunweighted"]]$n)
LProd.List[["DeInt"]][["LProdunweighted"]]$high95 <- LProd.List[["DeInt"]][["LProdunweighted"]]$LProd + qt(0.975, df= LProd.List[["DeInt"]][["LProdunweighted"]]$n-1) * LProd.List[["DeInt"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["DeInt"]][["LProdunweighted"]]$n)




#CSH unweighted LProd


LProd.List[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProd.List[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(LProd.List[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProd.List[["ByCSH"]][["CompanyList"]])) {LProd.List[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(LProd.List[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(LProd.List[["ByCSH"]][["CompanyList"]][[i]])) {
    LProd.List[["ByCSH"]][["CompanyList"]][[i]][[j]] <- LProd.List[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(LProd.List[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(LProd.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(LProd.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProd.List[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]])) {
  Temp1 <- LProd.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[i]]) == 0 ) {LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]][i,1] %in% LProd.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProd.List[["ByCSH"]][["LProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]])) {
  Temp1 <- LProd.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[i]]) == 0 ) {LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]])) {
  for (i in 1:nrow(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]][i,j])) & LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]][i,1] %in% LProd.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]])) {
  for(i in 1:nrow(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]])) {
  LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]]$sum[LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]])) {
  LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]]$sum[LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]]$LProd <- sapply(1:nrow(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]]), function (y) LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[x]][[y,12]] / LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[x]][[y,12]])
}


LProd.List[["ByCSH"]][["LProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]]),function(y) mean(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                     "sd" = c(sapply(1:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]]),function(y) sd(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                     "n" = c(sapply(1:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["ByCSH"]][["LProdunweighted"]]$low95 <- LProd.List[["ByCSH"]][["LProdunweighted"]]$LProd - qt(0.975, df= LProd.List[["ByCSH"]][["LProdunweighted"]]$n-1) * LProd.List[["ByCSH"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["ByCSH"]][["LProdunweighted"]]$n)
LProd.List[["ByCSH"]][["LProdunweighted"]]$high95 <- LProd.List[["ByCSH"]][["LProdunweighted"]]$LProd + qt(0.975, df= LProd.List[["ByCSH"]][["LProdunweighted"]]$n-1) * LProd.List[["ByCSH"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["ByCSH"]][["LProdunweighted"]]$n)





#GUO unweighted LProd


LProd.List[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProd.List[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(LProd.List[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProd.List[["ByGUO"]][["CompanyList"]])) {LProd.List[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(LProd.List[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(LProd.List[["ByGUO"]][["CompanyList"]][[i]])) {
    LProd.List[["ByGUO"]][["CompanyList"]][[i]][[j]] <- LProd.List[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(LProd.List[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(LProd.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(LProd.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProd.List[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]])) {
  Temp1 <- LProd.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[i]]) == 0 ) {LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]][i,1] %in% LProd.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProd.List[["ByGUO"]][["LProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]])) {
  Temp1 <- LProd.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[i]]) == 0 ) {LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]])) {
  for (i in 1:nrow(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]][i,j])) & LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]][i,1] %in% LProd.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]])) {
  for(i in 1:nrow(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]])) {
  LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]]$sum[LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]])) {
  LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]]$sum[LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]]$LProd <- sapply(1:nrow(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]]), function (y) LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[x]][[y,12]] / LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[x]][[y,12]])
}


LProd.List[["ByGUO"]][["LProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]]),function(y) mean(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                     "sd" = c(sapply(1:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]]),function(y) sd(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                     "n" = c(sapply(1:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["ByGUO"]][["LProdunweighted"]]$low95 <- LProd.List[["ByGUO"]][["LProdunweighted"]]$LProd - qt(0.975, df= LProd.List[["ByGUO"]][["LProdunweighted"]]$n-1) * LProd.List[["ByGUO"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["ByGUO"]][["LProdunweighted"]]$n)
LProd.List[["ByGUO"]][["LProdunweighted"]]$high95 <- LProd.List[["ByGUO"]][["LProdunweighted"]]$LProd + qt(0.975, df= LProd.List[["ByGUO"]][["LProdunweighted"]]$n-1) * LProd.List[["ByGUO"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["ByGUO"]][["LProdunweighted"]]$n)




#anyown unweighted LProd


LProd.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProd.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(LProd.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProd.List[["Byanyown"]][["CompanyList"]])) {LProd.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(LProd.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(LProd.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(LProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(LProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(LProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {LProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(LProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(LProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProd.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(LProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- LProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    LProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- LProd.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]])) {
  Temp1 <- LProd.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[i]]) == 0 ) {LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]][i,1] %in% LProd.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProd.List[["Byanyown"]][["LProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]])) {
  Temp1 <- LProd.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[i]]) == 0 ) {LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]])) {
  for (i in 1:nrow(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]][i,j])) & LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]][i,1] %in% LProd.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]])) {
  for(i in 1:nrow(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]])) {
  LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]]$sum[LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]])) {
  LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]]$sum[LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]]$LProd <- sapply(1:nrow(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]]), function (y) LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[x]][[y,12]] / LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[x]][[y,12]])
}


LProd.List[["Byanyown"]][["LProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]),function(y) mean(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]),function(y) sd(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["Byanyown"]][["LProdunweighted"]]$low95 <- LProd.List[["Byanyown"]][["LProdunweighted"]]$LProd - qt(0.975, df= LProd.List[["Byanyown"]][["LProdunweighted"]]$n-1) * LProd.List[["Byanyown"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["Byanyown"]][["LProdunweighted"]]$n)
LProd.List[["Byanyown"]][["LProdunweighted"]]$high95 <- LProd.List[["Byanyown"]][["LProdunweighted"]]$LProd + qt(0.975, df= LProd.List[["Byanyown"]][["LProdunweighted"]]$n-1) * LProd.List[["Byanyown"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["Byanyown"]][["LProdunweighted"]]$n)


#LProd unweighted Loops


LProd.List[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProd.List[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(LProd.List[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(LProd.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(LProd.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(LProd.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    LProd.List[["Loop"]][["CompanyList"]][[i]][[j]] <- LProd.List[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(LProd.List[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(LProd.List[["Loop"]][["CompanyList"]])) {LProd.List[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(LProd.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(LProd.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(LProd.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(LProd.List[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(LProd.List[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {LProd.List[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(LProd.List[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(LProd.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProd.List[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(LProd.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- LProd.List[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    LProd.List[["Loop"]][["CompanyList"]][[i]][[j]] <- LProd.List[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]])) {
  Temp1 <- LProd.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[i]]) == 0 ) {LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]][i,1] %in% LProd.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProd.List[["Loop"]][["LProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["Loop"]][["LProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["Loop"]][["LProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]])) {
  Temp1 <- LProd.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["Loop"]][["LProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[i]]) == 0 ) {LProd.List[["Loop"]][["LProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]])) {
  for (i in 1:nrow(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]][i,j])) & LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]][i,1] %in% LProd.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]])) {
  for(i in 1:nrow(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]])) {
  LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]]$sum[LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]])) {
  LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]]$sum[LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]]$LProd <- sapply(1:nrow(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]]), function (y) LProd.List[["Loop"]][["LProdunweightedTurnover"]][[x]][[y,12]] / LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[x]][[y,12]])
}


LProd.List[["Loop"]][["LProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]]),function(y) mean(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                    "sd" = c(sapply(1:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]]),function(y) sd(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                    "n" = c(sapply(1:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["Loop"]][["LProdunweighted"]]$low95 <- LProd.List[["Loop"]][["LProdunweighted"]]$LProd - qt(0.975, df= LProd.List[["Loop"]][["LProdunweighted"]]$n-1) * LProd.List[["Loop"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["Loop"]][["LProdunweighted"]]$n)
LProd.List[["Loop"]][["LProdunweighted"]]$high95 <- LProd.List[["Loop"]][["LProdunweighted"]]$LProd + qt(0.975, df= LProd.List[["Loop"]][["LProdunweighted"]]$n-1) * LProd.List[["Loop"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["Loop"]][["LProdunweighted"]]$n)




#anysub unweighted LProd


LProd.List[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProd.List[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(LProd.List[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProd.List[["Byanysub"]][["CompanyList"]])) {LProd.List[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(LProd.List[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(LProd.List[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(LProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(LProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {LProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(LProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(LProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(LProd.List[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(LProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- LProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    LProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- LProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]])) {
  Temp1 <- LProd.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[i]]) == 0 ) {LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]][i,1] %in% LProd.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProd.List[["Byanysub"]][["LProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]])) {
  Temp1 <- LProd.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[i]]) == 0 ) {LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]])) {
  for (i in 1:nrow(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]][i,j])) & LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]][i,1] %in% LProd.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]])) {
  for(i in 1:nrow(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]])) {
  LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]]$sum[LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]])) {
  LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]]$sum[LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]]$LProd <- sapply(1:nrow(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]]), function (y) LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[x]][[y,12]] / LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[x]][[y,12]])
}


LProd.List[["Byanysub"]][["LProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]]),function(y) mean(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]]),function(y) sd(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["Byanysub"]][["LProdunweighted"]]$low95 <- LProd.List[["Byanysub"]][["LProdunweighted"]]$LProd - qt(0.975, df= LProd.List[["Byanysub"]][["LProdunweighted"]]$n-1) * LProd.List[["Byanysub"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["Byanysub"]][["LProdunweighted"]]$n)
LProd.List[["Byanysub"]][["LProdunweighted"]]$high95 <- LProd.List[["Byanysub"]][["LProdunweighted"]]$LProd + qt(0.975, df= LProd.List[["Byanysub"]][["LProdunweighted"]]$n-1) * LProd.List[["Byanysub"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["Byanysub"]][["LProdunweighted"]]$n)




#anysubGER unweighted LProd


LProd.List[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {LProd.List[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(LProd.List[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(LProd.List[["ByanysubGER"]][["CompanyList"]])) {LProd.List[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(LProd.List[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(LProd.List[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    LProd.List[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- LProd.List[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(LProd.List[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(LProd.List[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]])) {
  Temp1 <- LProd.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[i]] <- subset(EmployeeCost, EmployeeCost$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[i]]) == 0 ) {LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]])) {
  for (i in 1:nrow(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for (j in 2:ncol(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]][i,1] %in% LProd.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]][i,j], NA)
    }}}



LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]])) {
  Temp1 <- LProd.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[i]])) {
    Temp1 <- unique(c(Temp1,LProd.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[i]] <- subset(Turnover, Turnover$CompanyBVDID %in% Temp1)
  if (nrow(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[i]]) == 0 ) {LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[i]][1,] <- NA}
}

for (x in 1:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]])) {
  for (i in 1:nrow(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]])) {
    for (j in 2:ncol(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]][i,j])) & LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]][i,1] %in% LProd.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]][i,j], NA)
    }}}


for(x in 1:length(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]])) {
  for(i in 1:nrow(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]])) {
    for(j in 2:ncol(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]])) {
      LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]])) {
  for(i in 1:nrow(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]])) {
    for(j in 2:ncol(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]])) {
      LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]][i,j] <- ifelse(!is.na(as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]][i,j])) & !is.na(as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]][i,j])) ,  as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]])) {
  LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]]$sum <- sapply(1:nrow(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]]), function (y) sum(as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]][y,2:11]) , na.rm = TRUE ))
  LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]]$sum[LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]])) {
  LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]]$sum <- sapply(1:nrow(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]]), function (y) sum(as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]][y,2:11]), na.rm = TRUE))
  LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]]$sum[LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]]$sum == 0] <- NA
  LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]]$LProd <- sapply(1:nrow(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]]), function (y) LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[x]][[y,12]] / LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[x]][[y,12]])
}


LProd.List[["ByanysubGER"]][["LProdunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]),function(y) mean(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))), 
                                                           "sd" = c(sapply(1:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]),function(y) sd(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[y]][,13], na.rm = TRUE))),
                                                           "n" = c(sapply(1:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["ByanysubGER"]][["LProdunweighted"]]$low95 <- LProd.List[["ByanysubGER"]][["LProdunweighted"]]$LProd - qt(0.975, df= LProd.List[["ByanysubGER"]][["LProdunweighted"]]$n-1) * LProd.List[["ByanysubGER"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["ByanysubGER"]][["LProdunweighted"]]$n)
LProd.List[["ByanysubGER"]][["LProdunweighted"]]$high95 <- LProd.List[["ByanysubGER"]][["LProdunweighted"]]$LProd + qt(0.975, df= LProd.List[["ByanysubGER"]][["LProdunweighted"]]$n-1) * LProd.List[["ByanysubGER"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["ByanysubGER"]][["LProdunweighted"]]$n)




#Affiliates  unweighted LProd


LProd.List[["Affiliates"]][["LProdunweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                          "LProd" = mean(unique(Reduce("rbind", LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]))[,13], na.rm = TRUE), 
                                                          "sd" = sd(unique(Reduce("rbind", LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]))[,13], na.rm = TRUE),
                                                          "n" = length(unique(Reduce("rbind", LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]))[,13][!is.na(unique(Reduce("rbind", LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]))[,13])]))




LProd.List[["Affiliates"]][["LProdunweighted"]]$low95 <- LProd.List[["Affiliates"]][["LProdunweighted"]]$LProd - qt(0.975, df= LProd.List[["Affiliates"]][["LProdunweighted"]]$n-1) * LProd.List[["Affiliates"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["Affiliates"]][["LProdunweighted"]]$n)
LProd.List[["Affiliates"]][["LProdunweighted"]]$high95 <- LProd.List[["Affiliates"]][["LProdunweighted"]]$LProd + qt(0.975, df= LProd.List[["Affiliates"]][["LProdunweighted"]]$n-1) * LProd.List[["Affiliates"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["Affiliates"]][["LProdunweighted"]]$n)




#GerGUO unweighted LProd


LProd.List[["GerGUO"]][["LProdunweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                      "LProd" = mean(unique(Reduce("rbind", LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]))[,13], na.rm = TRUE), 
                                                      "sd" = sd(unique(Reduce("rbind", LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]))[,13], na.rm = TRUE),
                                                      "n" = length(unique(Reduce("rbind", LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]))[,13][!is.na(unique(Reduce("rbind", LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]))[,13])]))




LProd.List[["GerGUO"]][["LProdunweighted"]]$low95 <- LProd.List[["GerGUO"]][["LProdunweighted"]]$LProd - qt(0.975, df= LProd.List[["GerGUO"]][["LProdunweighted"]]$n-1) * LProd.List[["GerGUO"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["GerGUO"]][["LProdunweighted"]]$n)
LProd.List[["GerGUO"]][["LProdunweighted"]]$high95 <- LProd.List[["GerGUO"]][["LProdunweighted"]]$LProd + qt(0.975, df= LProd.List[["GerGUO"]][["LProdunweighted"]]$n-1) * LProd.List[["GerGUO"]][["LProdunweighted"]]$sd /sqrt(LProd.List[["GerGUO"]][["LProdunweighted"]]$n)





#Domestic firms weighted LProd


LProd.List[["DeDom"]][["LProdweighted"]] <- data.frame("ISO" = "DEDOM")
LProd.List[["DeDom"]][["LProdweighted"]]$LProd <- sum(LProd.List[["DeDom"]][["LProdunweightedTurnover"]]$sum, na.rm = TRUE)  / sum(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]]$sum, na.rm = TRUE)
LProd.List[["DeDom"]][["LProdweighted"]]$sd <- sqrt(wtd.var(LProd.List[["DeDom"]][["LProdunweightedTurnover"]]$LProd, sqrt(LProd.List[["DeDom"]][["LProdunweightedEmployeeCost"]]$sum^2), na.rm = TRUE ))
LProd.List[["DeDom"]][["LProdweighted"]]$n <- length(LProd.List[["DeDom"]][["LProdunweightedTurnover"]]$LProd[!is.na(LProd.List[["DeDom"]][["LProdunweightedTurnover"]]$LProd)])
LProd.List[["DeDom"]][["LProdweighted"]]$low95 <- LProd.List[["DeDom"]][["LProdweighted"]]$LProd - qt(0.975, df = LProd.List[["DeDom"]][["LProdweighted"]]$n-1) * LProd.List[["DeDom"]][["LProdweighted"]]$sd / sqrt(LProd.List[["DeDom"]][["LProdweighted"]]$n)
LProd.List[["DeDom"]][["LProdweighted"]]$high95 <- LProd.List[["DeDom"]][["LProdweighted"]]$LProd + qt(0.975, df = LProd.List[["DeDom"]][["LProdweighted"]]$n-1) * LProd.List[["DeDom"]][["LProdweighted"]]$sd / sqrt(LProd.List[["DeDom"]][["LProdweighted"]]$n)


#International firms weighted LProd


LProd.List[["DeInt"]][["LProdweighted"]] <- data.frame("ISO" = "DEINT")
LProd.List[["DeInt"]][["LProdweighted"]]$LProd <- sum(LProd.List[["DeInt"]][["LProdunweightedTurnover"]]$sum, na.rm = TRUE)  / sum(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]]$sum, na.rm = TRUE)
LProd.List[["DeInt"]][["LProdweighted"]]$sd <- sqrt(wtd.var(LProd.List[["DeInt"]][["LProdunweightedTurnover"]]$LProd, sqrt(LProd.List[["DeInt"]][["LProdunweightedEmployeeCost"]]$sum^2), na.rm = TRUE ))
LProd.List[["DeInt"]][["LProdweighted"]]$n <- length(LProd.List[["DeInt"]][["LProdunweightedTurnover"]]$LProd[!is.na(LProd.List[["DeInt"]][["LProdunweightedTurnover"]]$LProd)])
LProd.List[["DeInt"]][["LProdweighted"]]$low95 <- LProd.List[["DeInt"]][["LProdweighted"]]$LProd - qt(0.975, df = LProd.List[["DeInt"]][["LProdweighted"]]$n-1) * LProd.List[["DeInt"]][["LProdweighted"]]$sd / sqrt(LProd.List[["DeInt"]][["LProdweighted"]]$n)
LProd.List[["DeInt"]][["LProdweighted"]]$high95 <- LProd.List[["DeInt"]][["LProdweighted"]]$LProd + qt(0.975, df = LProd.List[["DeInt"]][["LProdweighted"]]$n-1) * LProd.List[["DeInt"]][["LProdweighted"]]$sd / sqrt(LProd.List[["DeInt"]][["LProdweighted"]]$n)


#CSH firms weighted LProd 

LProd.List[["ByCSH"]][["LProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]]),function(y) sum(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                   "sd" = c(sapply(1:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]]),function(y) sqrt(wtd.var(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[y]][,13], sqrt(LProd.List[["ByCSH"]][["LProdunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                   "n" = c(sapply(1:length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["ByCSH"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["ByCSH"]][["LProdweighted"]]$low95 <- LProd.List[["ByCSH"]][["LProdweighted"]]$LProd - qt(0.975, df= LProd.List[["ByCSH"]][["LProdweighted"]]$n-1) * LProd.List[["ByCSH"]][["LProdweighted"]]$sd /sqrt(LProd.List[["ByCSH"]][["LProdweighted"]]$n)
LProd.List[["ByCSH"]][["LProdweighted"]]$high95 <- LProd.List[["ByCSH"]][["LProdweighted"]]$LProd + qt(0.975, df= LProd.List[["ByCSH"]][["LProdweighted"]]$n-1) * LProd.List[["ByCSH"]][["LProdweighted"]]$sd /sqrt(LProd.List[["ByCSH"]][["LProdweighted"]]$n)



#GUO firms weighted LProd 

LProd.List[["ByGUO"]][["LProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]]),function(y) sum(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                   "sd" = c(sapply(1:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]]),function(y) sqrt(wtd.var(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[y]][,13], sqrt(LProd.List[["ByGUO"]][["LProdunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                   "n" = c(sapply(1:length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["ByGUO"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["ByGUO"]][["LProdweighted"]]$low95 <- LProd.List[["ByGUO"]][["LProdweighted"]]$LProd - qt(0.975, df= LProd.List[["ByGUO"]][["LProdweighted"]]$n-1) * LProd.List[["ByGUO"]][["LProdweighted"]]$sd /sqrt(LProd.List[["ByGUO"]][["LProdweighted"]]$n)
LProd.List[["ByGUO"]][["LProdweighted"]]$high95 <- LProd.List[["ByGUO"]][["LProdweighted"]]$LProd + qt(0.975, df= LProd.List[["ByGUO"]][["LProdweighted"]]$n-1) * LProd.List[["ByGUO"]][["LProdweighted"]]$sd /sqrt(LProd.List[["ByGUO"]][["LProdweighted"]]$n)


#anyown firms weighted LProd 

LProd.List[["Byanyown"]][["LProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]),function(y) sum(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]),function(y) sqrt(wtd.var(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[y]][,13], sqrt(LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["Byanyown"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["Byanyown"]][["LProdweighted"]]$low95 <- LProd.List[["Byanyown"]][["LProdweighted"]]$LProd - qt(0.975, df= LProd.List[["Byanyown"]][["LProdweighted"]]$n-1) * LProd.List[["Byanyown"]][["LProdweighted"]]$sd /sqrt(LProd.List[["Byanyown"]][["LProdweighted"]]$n)
LProd.List[["Byanyown"]][["LProdweighted"]]$high95 <- LProd.List[["Byanyown"]][["LProdweighted"]]$LProd + qt(0.975, df= LProd.List[["Byanyown"]][["LProdweighted"]]$n-1) * LProd.List[["Byanyown"]][["LProdweighted"]]$sd /sqrt(LProd.List[["Byanyown"]][["LProdweighted"]]$n)


#Loops firms weighted LProd 

LProd.List[["Loop"]][["LProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]]),function(y) sum(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                  "sd" = c(sapply(1:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]]),function(y) sqrt(wtd.var(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[y]][,13], sqrt(LProd.List[["Loop"]][["LProdunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                  "n" = c(sapply(1:length(LProd.List[["Loop"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["Loop"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["Loop"]][["LProdweighted"]]$low95 <- LProd.List[["Loop"]][["LProdweighted"]]$LProd - qt(0.975, df= LProd.List[["Loop"]][["LProdweighted"]]$n-1) * LProd.List[["Loop"]][["LProdweighted"]]$sd /sqrt(LProd.List[["Loop"]][["LProdweighted"]]$n)
LProd.List[["Loop"]][["LProdweighted"]]$high95 <- LProd.List[["Loop"]][["LProdweighted"]]$LProd + qt(0.975, df= LProd.List[["Loop"]][["LProdweighted"]]$n-1) * LProd.List[["Loop"]][["LProdweighted"]]$sd /sqrt(LProd.List[["Loop"]][["LProdweighted"]]$n)



#anysub firms weighted LProd 

LProd.List[["Byanysub"]][["LProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]]),function(y) sum(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]]),function(y) sqrt(wtd.var(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[y]][,13], sqrt(LProd.List[["Byanysub"]][["LProdunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["Byanysub"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["Byanysub"]][["LProdweighted"]]$low95 <- LProd.List[["Byanysub"]][["LProdweighted"]]$LProd - qt(0.975, df= LProd.List[["Byanysub"]][["LProdweighted"]]$n-1) * LProd.List[["Byanysub"]][["LProdweighted"]]$sd /sqrt(LProd.List[["Byanysub"]][["LProdweighted"]]$n)
LProd.List[["Byanysub"]][["LProdweighted"]]$high95 <- LProd.List[["Byanysub"]][["LProdweighted"]]$LProd + qt(0.975, df= LProd.List[["Byanysub"]][["LProdweighted"]]$n-1) * LProd.List[["Byanysub"]][["LProdweighted"]]$sd /sqrt(LProd.List[["Byanysub"]][["LProdweighted"]]$n)


#anysubGER firms weighted LProd 

LProd.List[["ByanysubGER"]][["LProdweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "LProd" = c(sapply(1:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]),function(y) sum(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[y]][,12], na.rm = TRUE) / sum(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[y]][,12], na.rm = TRUE ))), 
                                                         "sd" = c(sapply(1:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]),function(y) sqrt(wtd.var(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[y]][,13], sqrt(LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                         "n" = c(sapply(1:length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]),function(y) length(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[y]][,13][!is.na(as.numeric(LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]][[y]][,13]))]))))


LProd.List[["ByanysubGER"]][["LProdweighted"]]$low95 <- LProd.List[["ByanysubGER"]][["LProdweighted"]]$LProd - qt(0.975, df= LProd.List[["ByanysubGER"]][["LProdweighted"]]$n-1) * LProd.List[["ByanysubGER"]][["LProdweighted"]]$sd /sqrt(LProd.List[["ByanysubGER"]][["LProdweighted"]]$n)
LProd.List[["ByanysubGER"]][["LProdweighted"]]$high95 <- LProd.List[["ByanysubGER"]][["LProdweighted"]]$LProd + qt(0.975, df= LProd.List[["ByanysubGER"]][["LProdweighted"]]$n-1) * LProd.List[["ByanysubGER"]][["LProdweighted"]]$sd /sqrt(LProd.List[["ByanysubGER"]][["LProdweighted"]]$n)



#Affiliates  weighted LProd


LProd.List[["Affiliates"]][["LProdweighted"]] <- data.frame("ISO" = "Affiliates")
LProd.List[["Affiliates"]][["LProdweighted"]]$LProd <- sum(unique(Reduce("rbind", LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", LProd.List[["Byanyown"]][["LProdunweightedEmployeeCost"]]))[,12], na.rm = TRUE)
LProd.List[["Affiliates"]][["LProdweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", LProd.List[["Byanyown"]][["LProdunweightedTurnover"]])[,13]), sqrt(unique(Reduce("rbind", LProd.List[["Byanyown"]][["LProdunweightedTurnover"]])[,12])^2), na.rm = TRUE ))
LProd.List[["Affiliates"]][["LProdweighted"]]$n <- length(unique(Reduce("rbind", LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]))[,13][!is.na(unique(Reduce("rbind", LProd.List[["Byanyown"]][["LProdunweightedTurnover"]]))[,13])])
LProd.List[["Affiliates"]][["LProdweighted"]]$low95 <- LProd.List[["Affiliates"]][["LProdweighted"]]$LProd - qt(0.975, df = LProd.List[["Affiliates"]][["LProdweighted"]]$n-1) * LProd.List[["Affiliates"]][["LProdweighted"]]$sd / sqrt(LProd.List[["Affiliates"]][["LProdweighted"]]$n)
LProd.List[["Affiliates"]][["LProdweighted"]]$high95 <- LProd.List[["Affiliates"]][["LProdweighted"]]$LProd + qt(0.975, df = LProd.List[["Affiliates"]][["LProdweighted"]]$n-1) * LProd.List[["Affiliates"]][["LProdweighted"]]$sd / sqrt(LProd.List[["Affiliates"]][["LProdweighted"]]$n)



#GerGUO weighted LProd


LProd.List[["GerGUO"]][["LProdweighted"]] <- data.frame("ISO" = "GerGUO")
LProd.List[["GerGUO"]][["LProdweighted"]]$LProd <- sum(unique(Reduce("rbind", LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", LProd.List[["ByanysubGER"]][["LProdunweightedEmployeeCost"]]))[,12], na.rm = TRUE)
LProd.List[["GerGUO"]][["LProdweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]])[,13]), sqrt(unique(Reduce("rbind", LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]])[,12])^2), na.rm = TRUE ))
LProd.List[["GerGUO"]][["LProdweighted"]]$n <- length(unique(Reduce("rbind", LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]))[,13][!is.na(unique(Reduce("rbind", LProd.List[["ByanysubGER"]][["LProdunweightedTurnover"]]))[,13])])
LProd.List[["GerGUO"]][["LProdweighted"]]$low95 <- LProd.List[["GerGUO"]][["LProdweighted"]]$LProd - qt(0.975, df = LProd.List[["GerGUO"]][["LProdweighted"]]$n-1) * LProd.List[["GerGUO"]][["LProdweighted"]]$sd / sqrt(LProd.List[["GerGUO"]][["LProdweighted"]]$n)
LProd.List[["GerGUO"]][["LProdweighted"]]$high95 <- LProd.List[["GerGUO"]][["LProdweighted"]]$LProd + qt(0.975, df = LProd.List[["GerGUO"]][["LProdweighted"]]$n-1) * LProd.List[["GerGUO"]][["LProdweighted"]]$sd / sqrt(LProd.List[["GerGUO"]][["LProdweighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, LProd, g, h, i, ISO, j, x, y , z)









