


Debt <- rio::import("ImportDebt.xlsx", which = "Results")
Debt <- cbind(data.frame("CompanyBVDID" = c(Debt$`BvD ID number`)),Debt[,4:12])
Debt <- full_join(data.frame("CompanyBVDID" = Debt$CompanyBVDID), Debt, by = "CompanyBVDID")
Debt$DUMMY <- NA


Assets <- rio::import("ImportAssets.xlsx", which = "Results")
Assets <- cbind(data.frame("CompanyBVDID" = c(Assets$`BvD ID number`)),Assets[,4:13])
Assets <- full_join(data.frame("CompanyBVDID" = Debt$CompanyBVDID), Assets, by = "CompanyBVDID")




DebtQuota.List <- vector(mode = "list")
DebtQuota.List[[1]] <- vector(mode = "list")
names(DebtQuota.List) <- "ByCSH"



#Domestic firms unweightet DebtQuota


DebtQuota.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {DebtQuota.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(DebtQuota.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(DebtQuota.List[["DeDom"]][["CompanyList"]])) {
  DebtQuota.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(DebtQuota.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- DebtQuota.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(DebtQuota.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(DebtQuota.List[["DeDom"]][["CompanyList"]][[i]])))
}

DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)

for (i in 1:nrow(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]])) {
  for (j in 2:ncol(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]])) {
    DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][i,j])) & DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][i,1] %in% DebtQuota.List[["DeDom"]][["CompanyList"]][[(j-1)]], DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][i,j], NA)
  }}



for (i in 1:length(DebtQuota.List[["DeDom"]][["CompanyList"]])) {
  DebtQuota.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(DebtQuota.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- DebtQuota.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(DebtQuota.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(DebtQuota.List[["DeDom"]][["CompanyList"]][[i]])))
}


DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]] <- subset(Debt, Debt$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]])) {
  for (j in 2:ncol(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]])) {
    DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][i,j])) & DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][i,1] %in% DebtQuota.List[["DeDom"]][["CompanyList"]][[(j-1)]], DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][i,j], NA)
  }}


for(i in 1:nrow(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]])) {
  for(j in 2:ncol(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]])) {
    DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][i,j])) & !is.na(as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][i,j])) ,  as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][i,j])  , NA  )
  }}


for(i in 1:nrow(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]])) {
  for(j in 2:ncol(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]])) {
    DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][i,j])) & !is.na(as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][i,j])) ,  as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][i,j])  , NA  )
  }}



DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]]$sum <- sapply(1:nrow(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]]), function (y) sum(as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][y,2:11]) , na.rm = TRUE ))
DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]]$sum[DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]]$sum == 0] <- NA
DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]$sum <- sapply(1:nrow(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]), function (y) sum(as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][y,2:11]), na.rm = TRUE))
DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]$sum[DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]$sum == 0] <- NA
DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]$DebtQuota <- sapply(1:nrow(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]), function (y) DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][[y,12]] / DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][[y,12]])

DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]] <- DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][!DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]$DebtQuota > 1,]
DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]] <- DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][!DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]$DebtQuota > 1,]
DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]] <- DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][!DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]]$sum < 0,]
DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]] <- DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]][!DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]]$sum < 0,]



DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                         "DebtQuota" = mean(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][,13], na.rm = TRUE),
                                                         "n" = length(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][,13][!is.na(as.numeric(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]][,13]))]))

DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$n)

DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]] <- DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]][!is.na(DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]]$DebtQuota),]


#International firms unweightet DebtQuota


DebtQuota.List[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {DebtQuota.List[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(DebtQuota.List[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(DebtQuota.List[["DeInt"]][["CompanyList"]])) {
  DebtQuota.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(DebtQuota.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- DebtQuota.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(DebtQuota.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(DebtQuota.List[["DeInt"]][["CompanyList"]][[i]])))
}

DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)

for (i in 1:nrow(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]])) {
  for (j in 2:ncol(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]])) {
    DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][i,j])) & DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][i,1] %in% DebtQuota.List[["DeInt"]][["CompanyList"]][[(j-1)]], DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][i,j], NA)
  }}



for (i in 1:length(DebtQuota.List[["DeInt"]][["CompanyList"]])) {
  DebtQuota.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(DebtQuota.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- DebtQuota.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(DebtQuota.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(DebtQuota.List[["DeInt"]][["CompanyList"]][[i]])))
}


DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]] <- subset(Debt, Debt$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]])) {
  for (j in 2:ncol(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]])) {
    DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][i,j])) & DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][i,1] %in% DebtQuota.List[["DeInt"]][["CompanyList"]][[(j-1)]], DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][i,j], NA)
  }}


for(i in 1:nrow(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]])) {
  for(j in 2:ncol(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]])) {
    DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][i,j])) & !is.na(as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][i,j])) ,  as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][i,j])  , NA  )
  }}


for(i in 1:nrow(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]])) {
  for(j in 2:ncol(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]])) {
    DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][i,j])) & !is.na(as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][i,j])) ,  as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][i,j])  , NA  )
  }}


DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]]$sum <- sapply(1:nrow(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]]), function (y) sum(as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][y,2:11]) , na.rm = TRUE ))
DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]]$sum[DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]]$sum == 0] <- NA
DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]$sum <- sapply(1:nrow(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]), function (y) sum(as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][y,2:11]), na.rm = TRUE))
DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]$sum[DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]$sum == 0] <- NA
DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]$DebtQuota <- sapply(1:nrow(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]), function (y) DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][[y,12]] / DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][[y,12]])

DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]] <- DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][!DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]$DebtQuota > 1,]
DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]] <- DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][!DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]$DebtQuota > 1,]
DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]] <- DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][!DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]]$sum < 0,]
DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]] <- DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]][!DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]]$sum < 0,]


DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]] <- data.frame("ISO" = "DeInt", 
                                                         "DebtQuota" = mean(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][,13], na.rm = TRUE),
                                                         "n" = length(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][,13][!is.na(as.numeric(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]][,13]))]))

DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$n)

DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]] <- DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]][!is.na(DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]]$DebtQuota),]



#CSH unweighted DebtQuota


DebtQuota.List[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {DebtQuota.List[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(DebtQuota.List[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(DebtQuota.List[["ByCSH"]][["CompanyList"]])) {DebtQuota.List[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(DebtQuota.List[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(DebtQuota.List[["ByCSH"]][["CompanyList"]][[i]])) {
    DebtQuota.List[["ByCSH"]][["CompanyList"]][[i]][[j]] <- DebtQuota.List[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(DebtQuota.List[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(DebtQuota.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(DebtQuota.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(DebtQuota.List[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]])) {
  Temp1 <- DebtQuota.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[i]]) == 0 ) {DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]])) {
  for (i in 1:nrow(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][i,1] %in% DebtQuota.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][i,j], NA)
    }}}



DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]])) {
  Temp1 <- DebtQuota.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[i]] <- subset(Debt, Debt$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[i]]) == 0 ) {DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]])) {
  for (i in 1:nrow(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) & DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][i,1] %in% DebtQuota.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][i,j], NA)
    }}}


for(x in 1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]])) {
  for(i in 1:nrow(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]])) {
  for(i in 1:nrow(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]])) {
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]]$sum[DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][y,2:11]), na.rm = TRUE))
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]]$sum[DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota <- sapply(1:nrow(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][[y,12]] / DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][y,12])
  
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  
}



DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]]),function(y) mean(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]]),function(y) sd(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$n)

DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]] <- DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]][!is.na(DebtQuota.List[["ByCSH"]][["DebtQuotaunweighted"]]$DebtQuota),]



#GUO unweighted DebtQuota


DebtQuota.List[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {DebtQuota.List[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(DebtQuota.List[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(DebtQuota.List[["ByGUO"]][["CompanyList"]])) {DebtQuota.List[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(DebtQuota.List[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(DebtQuota.List[["ByGUO"]][["CompanyList"]][[i]])) {
    DebtQuota.List[["ByGUO"]][["CompanyList"]][[i]][[j]] <- DebtQuota.List[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(DebtQuota.List[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(DebtQuota.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(DebtQuota.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(DebtQuota.List[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]])) {
  Temp1 <- DebtQuota.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[i]]) == 0 ) {DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]])) {
  for (i in 1:nrow(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][i,1] %in% DebtQuota.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][i,j], NA)
    }}}



DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]])) {
  Temp1 <- DebtQuota.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[i]] <- subset(Debt, Debt$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[i]]) == 0 ) {DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]])) {
  for (i in 1:nrow(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) & DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][i,1] %in% DebtQuota.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][i,j], NA)
    }}}


for(x in 1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]])) {
  for(i in 1:nrow(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]])) {
  for(i in 1:nrow(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]])) {
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]]$sum[DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][y,2:11]), na.rm = TRUE))
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]]$sum[DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota <- sapply(1:nrow(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][[y,12]] / DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][y,12])
  
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  
}


DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]]),function(y) mean(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]]),function(y) sd(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$n)

DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]] <- DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]][!is.na(DebtQuota.List[["ByGUO"]][["DebtQuotaunweighted"]]$DebtQuota),]


#anyown unweighted DebtQuota


DebtQuota.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {DebtQuota.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(DebtQuota.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(DebtQuota.List[["Byanyown"]][["CompanyList"]])) {DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(DebtQuota.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- DebtQuota.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]])) {
  Temp1 <- DebtQuota.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[i]]) == 0 ) {DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]])) {
  for (i in 1:nrow(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][i,1] %in% DebtQuota.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][i,j], NA)
    }}}



DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]])) {
  Temp1 <- DebtQuota.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[i]] <- subset(Debt, Debt$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[i]]) == 0 ) {DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]])) {
  for (i in 1:nrow(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) & DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][i,1] %in% DebtQuota.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][i,j], NA)
    }}}


for(x in 1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]])) {
  for(i in 1:nrow(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]])) {
  for(i in 1:nrow(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]])) {
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]]$sum[DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][y,2:11]), na.rm = TRUE))
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]]$sum[DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota <- sapply(1:nrow(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][[y,12]] / DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][y,12])
  
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  
}


DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]),function(y) mean(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]),function(y) sd(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$n)

DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]] <- DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]][!is.na(DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$DebtQuota),]




#DebtQuota unweighted Loops


DebtQuota.List[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {DebtQuota.List[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(DebtQuota.List[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(DebtQuota.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(DebtQuota.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]] <- DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(DebtQuota.List[["Loop"]][["CompanyList"]])) {DebtQuota.List[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(DebtQuota.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(DebtQuota.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(DebtQuota.List[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]] <- DebtQuota.List[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]])) {
  Temp1 <- DebtQuota.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[i]]) == 0 ) {DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]])) {
  for (i in 1:nrow(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][i,1] %in% DebtQuota.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][i,j], NA)
    }}}



DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]])) {
  Temp1 <- DebtQuota.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[i]] <- subset(Debt, Debt$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[i]]) == 0 ) {DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]])) {
  for (i in 1:nrow(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) & DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][i,1] %in% DebtQuota.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][i,j], NA)
    }}}


for(x in 1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]])) {
  for(i in 1:nrow(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]])) {
  for(i in 1:nrow(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]])) {
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]]$sum[DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][y,2:11]), na.rm = TRUE))
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]]$sum[DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota <- sapply(1:nrow(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][[y,12]] / DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][y,12])
  
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  
}


DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]]),function(y) mean(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]]),function(y) sd(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$n)

DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]] <- DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]][!is.na(DebtQuota.List[["Loop"]][["DebtQuotaunweighted"]]$DebtQuota),]


#anysub unweighted DebtQuota


DebtQuota.List[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {DebtQuota.List[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(DebtQuota.List[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(DebtQuota.List[["Byanysub"]][["CompanyList"]])) {DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(DebtQuota.List[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]])) {
  Temp1 <- DebtQuota.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[i]]) == 0 ) {DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]])) {
  for (i in 1:nrow(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][i,1] %in% DebtQuota.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][i,j], NA)
    }}}



DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]])) {
  Temp1 <- DebtQuota.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[i]] <- subset(Debt, Debt$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[i]]) == 0 ) {DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]])) {
  for (i in 1:nrow(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) & DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][i,1] %in% DebtQuota.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][i,j], NA)
    }}}


for(x in 1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]])) {
  for(i in 1:nrow(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]])) {
  for(i in 1:nrow(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]])) {
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]]$sum[DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][y,2:11]), na.rm = TRUE))
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]]$sum[DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota <- sapply(1:nrow(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][[y,12]] / DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][y,12])
  
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  
}

DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]]),function(y) mean(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]]),function(y) sd(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$n)

DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]] <- DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]][!is.na(DebtQuota.List[["Byanysub"]][["DebtQuotaunweighted"]]$DebtQuota),]


#anysubGER unweighted DebtQuota


DebtQuota.List[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(DebtQuota.List[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(DebtQuota.List[["ByanysubGER"]][["CompanyList"]])) {DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(DebtQuota.List[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(DebtQuota.List[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]])) {
  Temp1 <- DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[i]] <- subset(Assets, Assets$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[i]]) == 0 ) {DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]])) {
  for (i in 1:nrow(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][i,1] %in% DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][i,j], NA)
    }}}



DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]])) {
  Temp1 <- DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[i]])) {
    Temp1 <- unique(c(Temp1,DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[i]] <- subset(Debt, Debt$CompanyBVDID %in% Temp1)
  if (nrow(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[i]]) == 0 ) {DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[i]][1,] <- NA}
}

for (x in 1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]])) {
  for (i in 1:nrow(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for (j in 2:ncol(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) & DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][i,1] %in% DebtQuota.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][i,j], NA)
    }}}


for(x in 1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]])) {
  for(i in 1:nrow(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]])) {
      DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]])) {
  for(i in 1:nrow(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]])) {
    for(j in 2:ncol(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]])) {
      DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][i,j] <- ifelse(!is.na(as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][i,j])) & !is.na(as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][i,j])) ,  as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]])) {
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][y,2:11]) , na.rm = TRUE ))
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]]$sum[DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]]$sum <- sapply(1:nrow(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) sum(as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][y,2:11]), na.rm = TRUE))
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]]$sum[DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]]$sum == 0] <- NA
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota <- sapply(1:nrow(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]]), function (y) DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][[y,12]] / DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][y,12])
  
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]]$DebtQuota > 1,]
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]] <- DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[x]][!DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]] <- DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]][!DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[x]]$sum < 0,]
  
}


DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]),function(y) mean(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))), 
                                                               "sd" = c(sapply(1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]),function(y) sd(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[y]][,13], na.rm = TRUE))),
                                                               "n" = c(sapply(1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$n)

DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]] <- DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]][!is.na(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweighted"]]$DebtQuota),]


#Affiliates  unweighted DebtQuota


DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                              "DebtQuota" = mean(unique(Reduce("rbind", DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]))[,13], na.rm = TRUE), 
                                                              "sd" = sd(unique(Reduce("rbind", DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]))[,13], na.rm = TRUE),
                                                              "n" = length(unique(Reduce("rbind", DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]))[,13][!is.na(unique(Reduce("rbind", DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]))[,13])]))




DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]]$n)




#GerGUO unweighted DebtQuota


DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                          "DebtQuota" = mean(unique(Reduce("rbind", DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]))[,13], na.rm = TRUE), 
                                                          "sd" = sd(unique(Reduce("rbind", DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]))[,13], na.rm = TRUE),
                                                          "n" = length(unique(Reduce("rbind", DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]))[,13][!is.na(unique(Reduce("rbind", DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]))[,13])]))




DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$n)

DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]] <- DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]][!is.na(DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]$DebtQuota),]




#Domestic firms weighted DebtQuota


DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]] <- data.frame("ISO" = "DEDOM")
DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$DebtQuota <- sum(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]$sum, na.rm = TRUE)  / sum(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]]$sum, na.rm = TRUE)
DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$sd <- sqrt(wtd.var(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]$DebtQuota, sqrt(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedAssets"]]$sum^2), na.rm = TRUE ))
DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$n <- length(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]$DebtQuota[!is.na(DebtQuota.List[["DeDom"]][["DebtQuotaunweightedDebt"]]$DebtQuota)])
DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df = DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$sd / sqrt(DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df = DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$sd / sqrt(DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]]$n)


#International firms weighted DebtQuota


DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]] <- data.frame("ISO" = "DEINT")
DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$DebtQuota <- sum(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]$sum, na.rm = TRUE)  / sum(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]]$sum, na.rm = TRUE)
DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$sd <- sqrt(wtd.var(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]$DebtQuota, sqrt(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedAssets"]]$sum^2), na.rm = TRUE ))
DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$n <- length(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]$DebtQuota[!is.na(DebtQuota.List[["DeInt"]][["DebtQuotaunweightedDebt"]]$DebtQuota)])
DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df = DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$sd / sqrt(DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df = DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$sd / sqrt(DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]]$n)


#CSH firms weighted DebtQuota 

DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]]),function(y) sum(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[y]][,12], na.rm = TRUE) / sum(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]]),function(y) sqrt(wtd.var(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[y]][,13], sqrt(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["ByCSH"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$n)

DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]] <- DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]][!is.na(DebtQuota.List[["ByCSH"]][["DebtQuotaweighted"]]$DebtQuota),]


#GUO firms weighted DebtQuota 

DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]]),function(y) sum(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[y]][,12], na.rm = TRUE) / sum(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]]),function(y) sqrt(wtd.var(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[y]][,13], sqrt(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["ByGUO"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$n)

DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]] <- DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]][!is.na(DebtQuota.List[["ByGUO"]][["DebtQuotaweighted"]]$DebtQuota),]


#anyown firms weighted DebtQuota 

DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]),function(y) sum(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[y]][,12], na.rm = TRUE) / sum(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]),function(y) sqrt(wtd.var(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[y]][,13], sqrt(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$n)

DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]] <- DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]][!is.na(DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$DebtQuota),]



#Loops firms weighted DebtQuota 

DebtQuota.List[["Loop"]][["DebtQuotaweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]]),function(y) sum(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[y]][,12], na.rm = TRUE) / sum(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]]),function(y) sqrt(wtd.var(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[y]][,13], sqrt(DebtQuota.List[["Loop"]][["DebtQuotaunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["Loop"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$n)

DebtQuota.List[["Loop"]][["DebtQuotaweighted"]] <- DebtQuota.List[["Loop"]][["DebtQuotaweighted"]][!is.na(DebtQuota.List[["Loop"]][["DebtQuotaweighted"]]$DebtQuota),]



#anysub firms weighted DebtQuota 

DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]]),function(y) sum(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[y]][,12], na.rm = TRUE) / sum(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]]),function(y) sqrt(wtd.var(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[y]][,13], sqrt(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["Byanysub"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$n)

DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]] <- DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]][!is.na(DebtQuota.List[["Byanysub"]][["DebtQuotaweighted"]]$DebtQuota),]


#anysubGER firms weighted DebtQuota 

DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "DebtQuota" = c(sapply(1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]),function(y) sum(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[y]][,12], na.rm = TRUE) / sum(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[y]][,12], na.rm = TRUE ))), 
                                                             "sd" = c(sapply(1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]),function(y) sqrt(wtd.var(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[y]][,13], sqrt(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                             "n" = c(sapply(1:length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]),function(y) length(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[y]][,13][!is.na(as.numeric(DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]][[y]][,13]))]))))


DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$n)

DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]] <- DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]][!is.na(DebtQuota.List[["ByanysubGER"]][["DebtQuotaweighted"]]$DebtQuota),]


#Affiliates  weighted DebtQuota


DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]] <- data.frame("ISO" = "Affiliates")
DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$DebtQuota <- sum(unique(Reduce("rbind", DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]]))[,12], na.rm = TRUE)
DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]])[,13]), sqrt(unique(Reduce("rbind", DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]])[,12])^2), na.rm = TRUE ))
DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$n <- length(unique(Reduce("rbind", DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]))[,13][!is.na(unique(Reduce("rbind", DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]))[,13])])
DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df = DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$sd / sqrt(DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df = DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$sd / sqrt(DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]]$n)



#GerGUO weighted DebtQuota


DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]] <- data.frame("ISO" = "GerGUO")
DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$DebtQuota <- sum(unique(Reduce("rbind", DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]]))[,12], na.rm = TRUE)
DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]])[,13]), sqrt(unique(Reduce("rbind", DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedAssets"]])[,12])^2), na.rm = TRUE ))
DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$n <- length(unique(Reduce("rbind", DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]))[,13][!is.na(unique(Reduce("rbind", DebtQuota.List[["ByanysubGER"]][["DebtQuotaunweightedDebt"]]))[,13])])
DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df = DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$sd / sqrt(DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df = DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$sd / sqrt(DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$n)

DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]] <- DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]][!is.na(DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]$DebtQuota),]





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Debt Havens and EU Debt havens to anyown

DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]] <- rbind(DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavens"), "DebtQuota" = c(mean(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% Taxhavens])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))

DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]] <- rbind(DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEU"), "DebtQuota" = c(mean(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))

DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]] <- rbind(DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]], 
                                                       DebtQuota.List[["DeInt"]][["DebtQuotaunweighted"]],
                                                       DebtQuota.List[["DeDom"]][["DebtQuotaunweighted"]],
                                                       DebtQuota.List[["Affiliates"]][["DebtQuotaunweighted"]],
                                                       DebtQuota.List[["GerGUO"]][["DebtQuotaunweighted"]]
)


DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$low95 <- DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$n)
DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$high95 <- DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$n-1) * DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$sd /sqrt(DebtQuota.List[["Byanyown"]][["DebtQuotaunweighted"]]$n)





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Debt Havens and EU Debt havens to anyown


DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]] <- rbind(DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavens"), "DebtQuota" = c(sum(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% Taxhavens])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]]) %in% Taxhavens])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% Taxhavens])[,13]), sqrt(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% Taxhavens])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% Taxhavens])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))

DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]] <- rbind(DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEU"), "DebtQuota" = c(sum(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedAssets"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]][names(DebtQuota.List[["Byanyown"]][["DebtQuotaunweightedDebt"]]) %in% TaxhavensEU])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))

DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]] <- rbind(DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]], 
                                                     DebtQuota.List[["DeInt"]][["DebtQuotaweighted"]],
                                                     DebtQuota.List[["DeDom"]][["DebtQuotaweighted"]],
                                                     DebtQuota.List[["Affiliates"]][["DebtQuotaweighted"]],
                                                     DebtQuota.List[["GerGUO"]][["DebtQuotaweighted"]]
)


DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$low95 <- DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$DebtQuota - qt(0.975, df= DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$n)
DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$high95 <- DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$DebtQuota + qt(0.975, df= DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$n-1) * DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$sd /sqrt(DebtQuota.List[["Byanyown"]][["DebtQuotaweighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, DebtQuota, g, h, i, ISO, j, x, y , z)





