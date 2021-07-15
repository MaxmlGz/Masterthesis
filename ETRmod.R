



EBT <- rio::import("ImportEBT.xlsx", which = "Results")
EBT <- cbind(data.frame("CompanyBVDID" = c(EBT$`BvD ID number`)),EBT[,4:13])
EBT <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), EBT, by = "CompanyBVDID")


Tax <- rio::import("ImportTax.xlsx", which = "Results")
Tax <- cbind(data.frame("CompanyBVDID" = c(Tax$`BvD ID number`)),Tax[,4:13])
Tax <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), Tax, by = "CompanyBVDID")



##equal out samples

for(i in 1:nrow(EBT)) {
  for (j in 2:ncol(EBT)) {
    EBT[i,j] <- ifelse(!is.na(as.numeric(EBT[i,j])) & !is.na(as.numeric(Tax[i,j])),  as.numeric(EBT[i,j]) , NA )
  }
}


for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(EBT[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}



## Drop company if losses in 2010

EBT <- EBT[EBT[,11] > 0 | is.na(EBT[,11]),]
EBT <- EBT[!is.na(EBT$CompanyBVDID),]
Tax <- Tax[Tax$CompanyBVDID %in% EBT$CompanyBVDID,]


## Keep 2010 - 2019

EBT[,11] <- NA
Tax[,11] <- NA


EBT[,2] <- NA
Tax[,2] <- NA


## Drop last year if negative profits


EBT[,3][EBT[,3] < 0] <- NA



for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(EBT[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}


## Drop if <3 obs


EBT <- EBT[apply(EBT,1,function (z) length(z[!is.na(as.numeric(z))]) > 2),]
Tax <- Tax[Tax$CompanyBVDID %in% EBT$CompanyBVDID,]








ETR.List.Pos <- vector(mode = "list")
ETR.List.Pos[[1]] <- vector(mode = "list")
names(ETR.List.Pos) <- "ByCSH"


#Domestic firms unweightet ETR


ETR.List.Pos[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List.Pos[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(ETR.List.Pos[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(ETR.List.Pos[["DeDom"]][["CompanyList"]])) {
  ETR.List.Pos[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR.List.Pos[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR.List.Pos[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR.List.Pos[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR.List.Pos[["DeDom"]][["CompanyList"]][[i]])))
}

ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]])) {
  for (j in 2:ncol(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]])) {
    ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][i,j])) & ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][i,1] %in% ETR.List.Pos[["DeDom"]][["CompanyList"]][[(j-1)]], ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][i,j], NA)
  }}



for (i in 1:length(ETR.List.Pos[["DeDom"]][["CompanyList"]])) {
  ETR.List.Pos[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR.List.Pos[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR.List.Pos[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR.List.Pos[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR.List.Pos[["DeDom"]][["CompanyList"]][[i]])))
}


ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]])) {
  for (j in 2:ncol(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]])) {
    ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][i,j])) & ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][i,1] %in% ETR.List.Pos[["DeDom"]][["CompanyList"]][[(j-1)]], ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]])) {
  for(j in 2:ncol(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]])) {
    ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][i,j])) ,  as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]])) {
  for(j in 2:ncol(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]])) {
    ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][i,j])) ,  as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][i,j])  , NA  )
  }}



ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]]$sum <- sapply(1:nrow(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]]), function (y) sum(as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][y,2:11]) , na.rm = TRUE ))
ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]]$sum[ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]]$sum == 0] <- NA
ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]$sum <- sapply(1:nrow(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]), function (y) sum(as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][y,2:11]), na.rm = TRUE))
ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]$sum[ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]$sum == 0] <- NA
ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]$ETR <- sapply(1:nrow(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]), function (y) ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][[y,12]] / ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][[y,12]])

ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]] <- ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][!ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]$ETR > 1,]
ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]] <- ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][!ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]$ETR > 1,]
ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]] <- ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][!ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]]$sum < 0,]
ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]] <- ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]][!ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]]$sum < 0,]



ETR.List.Pos[["DeDom"]][["ETRunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                     "ETR" = mean(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][,13], na.rm = TRUE), 
                                                     "sd" = sd(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][,13], na.rm = TRUE),
                                                     "n" = length(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][,13][!is.na(as.numeric(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]][,13]))]))

ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$n)
ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$n)

ETR.List.Pos[["DeDom"]][["ETRunweighted"]] <- ETR.List.Pos[["DeDom"]][["ETRunweighted"]][!is.na(ETR.List.Pos[["DeDom"]][["ETRunweighted"]]$ETR),]


#International firms unweightet ETR


ETR.List.Pos[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List.Pos[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(ETR.List.Pos[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(ETR.List.Pos[["DeInt"]][["CompanyList"]])) {
  ETR.List.Pos[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR.List.Pos[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR.List.Pos[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR.List.Pos[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR.List.Pos[["DeInt"]][["CompanyList"]][[i]])))
}

ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]])) {
  for (j in 2:ncol(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]])) {
    ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][i,j])) & ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][i,1] %in% ETR.List.Pos[["DeInt"]][["CompanyList"]][[(j-1)]], ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][i,j], NA)
  }}



for (i in 1:length(ETR.List.Pos[["DeInt"]][["CompanyList"]])) {
  ETR.List.Pos[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR.List.Pos[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR.List.Pos[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR.List.Pos[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR.List.Pos[["DeInt"]][["CompanyList"]][[i]])))
}


ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]])) {
  for (j in 2:ncol(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]])) {
    ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][i,j])) & ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][i,1] %in% ETR.List.Pos[["DeInt"]][["CompanyList"]][[(j-1)]], ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]])) {
  for(j in 2:ncol(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]])) {
    ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][i,j])) ,  as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]])) {
  for(j in 2:ncol(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]])) {
    ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][i,j])) ,  as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][i,j])  , NA  )
  }}


ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]]$sum <- sapply(1:nrow(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]]), function (y) sum(as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][y,2:11]) , na.rm = TRUE ))
ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]]$sum[ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]]$sum == 0] <- NA
ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]$sum <- sapply(1:nrow(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]), function (y) sum(as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][y,2:11]), na.rm = TRUE))
ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]$sum[ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]$sum == 0] <- NA
ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]$ETR <- sapply(1:nrow(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]), function (y) ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][[y,12]] / ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][[y,12]])

ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]] <- ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][!ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]$ETR > 1,]
ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]] <- ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][!ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]$ETR > 1,]
ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]] <- ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][!ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]]$sum < 0,]
ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]] <- ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]][!ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]]$sum < 0,]


ETR.List.Pos[["DeInt"]][["ETRunweighted"]] <- data.frame("ISO" = "DeInt", 
                                                         "ETR" = mean(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][,13], na.rm = TRUE),
                                                         "n" = length(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][,13][!is.na(as.numeric(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]][,13]))]))

ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$n)
ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$n)

ETR.List.Pos[["DeInt"]][["ETRunweighted"]] <- ETR.List.Pos[["DeInt"]][["ETRunweighted"]][!is.na(ETR.List.Pos[["DeInt"]][["ETRunweighted"]]$ETR),]



#CSH unweighted ETR


ETR.List.Pos[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List.Pos[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(ETR.List.Pos[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List.Pos[["ByCSH"]][["CompanyList"]])) {ETR.List.Pos[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR.List.Pos[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List.Pos[["ByCSH"]][["CompanyList"]][[i]])) {
    ETR.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]] <- ETR.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List.Pos[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List.Pos[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List.Pos[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List.Pos[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List.Pos[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]])) {
  ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]]$sum[ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][y,12])
  
  ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[x]]$sum < 0,]

}



ETR.List.Pos[["ByCSH"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]]),function(y) mean(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                     "sd" = c(sapply(1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]]),function(y) sd(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                     "n" = c(sapply(1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$n)
ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$n)

ETR.List.Pos[["ByCSH"]][["ETRunweighted"]] <- ETR.List.Pos[["ByCSH"]][["ETRunweighted"]][!is.na(ETR.List.Pos[["ByCSH"]][["ETRunweighted"]]$ETR),]



#GUO unweighted ETR


ETR.List.Pos[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List.Pos[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(ETR.List.Pos[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List.Pos[["ByGUO"]][["CompanyList"]])) {ETR.List.Pos[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(ETR.List.Pos[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List.Pos[["ByGUO"]][["CompanyList"]][[i]])) {
    ETR.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]] <- ETR.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List.Pos[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List.Pos[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List.Pos[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List.Pos[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List.Pos[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]])) {
  ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]]$sum[ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][y,12])
  
  ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  
}


ETR.List.Pos[["ByGUO"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]]),function(y) mean(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]]),function(y) sd(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$n)
ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$n)

ETR.List.Pos[["ByGUO"]][["ETRunweighted"]] <- ETR.List.Pos[["ByGUO"]][["ETRunweighted"]][!is.na(ETR.List.Pos[["ByGUO"]][["ETRunweighted"]]$ETR),]


#anyown unweighted ETR


ETR.List.Pos[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List.Pos[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR.List.Pos[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List.Pos[["Byanyown"]][["CompanyList"]])) {ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR.List.Pos[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]] <- ETR.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List.Pos[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List.Pos[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List.Pos[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List.Pos[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]])) {
  ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]]$sum[ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][y,12])
  
  ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  
}


ETR.List.Pos[["Byanyown"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]),function(y) mean(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]),function(y) sd(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$n)
ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$n)

ETR.List.Pos[["Byanyown"]][["ETRunweighted"]] <- ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][!is.na(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ETR),]




#intermed unweighted ETR


ETR.List.Pos[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List.Pos[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(ETR.List.Pos[["Byintermed"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List.Pos[["Byintermed"]][["CompanyList"]])) {ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR.List.Pos[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]] <- ETR.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List.Pos[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List.Pos[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List.Pos[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List.Pos[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]])) {
  ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]]$sum[ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][y,12])
  
  ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  
}


ETR.List.Pos[["Byintermed"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]),function(y) mean(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]),function(y) sd(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$n)
ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$n)

ETR.List.Pos[["Byintermed"]][["ETRunweighted"]] <- ETR.List.Pos[["Byintermed"]][["ETRunweighted"]][!is.na(ETR.List.Pos[["Byintermed"]][["ETRunweighted"]]$ETR),]





#ETR unweighted Loops


ETR.List.Pos[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List.Pos[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR.List.Pos[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(ETR.List.Pos[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(ETR.List.Pos[["Loop"]][["CompanyList"]])) {ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR.List.Pos[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List.Pos[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List.Pos[["Loop"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List.Pos[["Loop"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List.Pos[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List.Pos[["Loop"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]])) {
  ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]]$sum[ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][y,12])
  
  ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  
}


ETR.List.Pos[["Loop"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]]),function(y) mean(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]]),function(y) sd(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["Loop"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["Loop"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Loop"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Loop"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Loop"]][["ETRunweighted"]]$n)
ETR.List.Pos[["Loop"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["Loop"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Loop"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Loop"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Loop"]][["ETRunweighted"]]$n)

ETR.List.Pos[["Loop"]][["ETRunweighted"]] <- ETR.List.Pos[["Loop"]][["ETRunweighted"]][!is.na(ETR.List.Pos[["Loop"]][["ETRunweighted"]]$ETR),]


#anysub unweighted ETR


ETR.List.Pos[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List.Pos[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR.List.Pos[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List.Pos[["Byanysub"]][["CompanyList"]])) {ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR.List.Pos[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] <- ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List.Pos[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List.Pos[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List.Pos[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List.Pos[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]])) {
  ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]]$sum[ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][y,12])
  
  ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  
}

ETR.List.Pos[["Byanysub"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]]),function(y) mean(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]]),function(y) sd(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$n)
ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$n)

ETR.List.Pos[["Byanysub"]][["ETRunweighted"]] <- ETR.List.Pos[["Byanysub"]][["ETRunweighted"]][!is.na(ETR.List.Pos[["Byanysub"]][["ETRunweighted"]]$ETR),]


#anysubGER unweighted ETR


ETR.List.Pos[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR.List.Pos[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List.Pos[["ByanysubGER"]][["CompanyList"]])) {ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(ETR.List.Pos[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(ETR.List.Pos[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List.Pos[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]])) {
      ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]])) {
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]]$sum[ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][y,12])
  
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]]$ETR > 1,]
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]] <- ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[x]][!ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]] <- ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][!ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[x]]$sum < 0,]
  
}


ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) mean(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) sd(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$n)
ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$n)

ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]] <- ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]][!is.na(ETR.List.Pos[["ByanysubGER"]][["ETRunweighted"]]$ETR),]


#Affiliates  unweighted ETR


ETR.List.Pos[["Affiliates"]][["ETRunweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                          "ETR" = mean(unique(Reduce("rbind", ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]))[,13], na.rm = TRUE), 
                                                          "sd" = sd(unique(Reduce("rbind", ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]))[,13], na.rm = TRUE),
                                                          "n" = length(unique(Reduce("rbind", ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]))[,13])]))




ETR.List.Pos[["Affiliates"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["Affiliates"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Affiliates"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Affiliates"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Affiliates"]][["ETRunweighted"]]$n)
ETR.List.Pos[["Affiliates"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["Affiliates"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Affiliates"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Affiliates"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Affiliates"]][["ETRunweighted"]]$n)




#GerGUO unweighted ETR


ETR.List.Pos[["GerGUO"]][["ETRunweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                      "ETR" = mean(unique(Reduce("rbind", ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]))[,13], na.rm = TRUE), 
                                                      "sd" = sd(unique(Reduce("rbind", ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]))[,13], na.rm = TRUE),
                                                      "n" = length(unique(Reduce("rbind", ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]))[,13])]))




ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$n)
ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$n)

ETR.List.Pos[["GerGUO"]][["ETRunweighted"]] <- ETR.List.Pos[["GerGUO"]][["ETRunweighted"]][!is.na(ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]$ETR),]




#Domestic firms weighted ETR


ETR.List.Pos[["DeDom"]][["ETRweighted"]] <- data.frame("ISO" = "DEDOM")
ETR.List.Pos[["DeDom"]][["ETRweighted"]]$ETR <- sum(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]]$sum, na.rm = TRUE)
ETR.List.Pos[["DeDom"]][["ETRweighted"]]$sd <- sqrt(wtd.var(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]$ETR, sqrt(ETR.List.Pos[["DeDom"]][["ETRunweightedEBT"]]$sum^2), na.rm = TRUE ))
ETR.List.Pos[["DeDom"]][["ETRweighted"]]$n <- length(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]$ETR[!is.na(ETR.List.Pos[["DeDom"]][["ETRunweightedTax"]]$ETR)])
ETR.List.Pos[["DeDom"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["DeDom"]][["ETRweighted"]]$ETR - qt(0.975, df = ETR.List.Pos[["DeDom"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["DeDom"]][["ETRweighted"]]$sd / sqrt(ETR.List.Pos[["DeDom"]][["ETRweighted"]]$n)
ETR.List.Pos[["DeDom"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["DeDom"]][["ETRweighted"]]$ETR + qt(0.975, df = ETR.List.Pos[["DeDom"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["DeDom"]][["ETRweighted"]]$sd / sqrt(ETR.List.Pos[["DeDom"]][["ETRweighted"]]$n)


#International firms weighted ETR


ETR.List.Pos[["DeInt"]][["ETRweighted"]] <- data.frame("ISO" = "DEINT")
ETR.List.Pos[["DeInt"]][["ETRweighted"]]$ETR <- sum(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]]$sum, na.rm = TRUE)
ETR.List.Pos[["DeInt"]][["ETRweighted"]]$sd <- sqrt(wtd.var(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]$ETR, sqrt(ETR.List.Pos[["DeInt"]][["ETRunweightedEBT"]]$sum^2), na.rm = TRUE ))
ETR.List.Pos[["DeInt"]][["ETRweighted"]]$n <- length(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]$ETR[!is.na(ETR.List.Pos[["DeInt"]][["ETRunweightedTax"]]$ETR)])
ETR.List.Pos[["DeInt"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["DeInt"]][["ETRweighted"]]$ETR - qt(0.975, df = ETR.List.Pos[["DeInt"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["DeInt"]][["ETRweighted"]]$sd / sqrt(ETR.List.Pos[["DeInt"]][["ETRweighted"]]$n)
ETR.List.Pos[["DeInt"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["DeInt"]][["ETRweighted"]]$ETR + qt(0.975, df = ETR.List.Pos[["DeInt"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["DeInt"]][["ETRweighted"]]$sd / sqrt(ETR.List.Pos[["DeInt"]][["ETRweighted"]]$n)


#CSH firms weighted ETR 

ETR.List.Pos[["ByCSH"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]]),function(y) sum(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                   "sd" = c(sapply(1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List.Pos[["ByCSH"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                   "n" = c(sapply(1:length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["ByCSH"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$n)
ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$n)

ETR.List.Pos[["ByCSH"]][["ETRweighted"]] <- ETR.List.Pos[["ByCSH"]][["ETRweighted"]][!is.na(ETR.List.Pos[["ByCSH"]][["ETRweighted"]]$ETR),]


#GUO firms weighted ETR 

ETR.List.Pos[["ByGUO"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]]),function(y) sum(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                   "sd" = c(sapply(1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List.Pos[["ByGUO"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                   "n" = c(sapply(1:length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["ByGUO"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$n)
ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$n)

ETR.List.Pos[["ByGUO"]][["ETRweighted"]] <- ETR.List.Pos[["ByGUO"]][["ETRweighted"]][!is.na(ETR.List.Pos[["ByGUO"]][["ETRweighted"]]$ETR),]


#anyown firms weighted ETR 

ETR.List.Pos[["Byanyown"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]),function(y) sum(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$n)
ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$n)

ETR.List.Pos[["Byanyown"]][["ETRweighted"]] <- ETR.List.Pos[["Byanyown"]][["ETRweighted"]][!is.na(ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ETR),]


#intermed firms weighted ETR 

ETR.List.Pos[["Byintermed"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]),function(y) sum(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$n)
ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$n)

ETR.List.Pos[["Byintermed"]][["ETRweighted"]] <- ETR.List.Pos[["Byintermed"]][["ETRweighted"]][!is.na(ETR.List.Pos[["Byintermed"]][["ETRweighted"]]$ETR),]


#Loops firms weighted ETR 

ETR.List.Pos[["Loop"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]]),function(y) sum(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                  "sd" = c(sapply(1:length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List.Pos[["Loop"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                  "n" = c(sapply(1:length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["Loop"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["Loop"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["Loop"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Loop"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Loop"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["Loop"]][["ETRweighted"]]$n)
ETR.List.Pos[["Loop"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["Loop"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Loop"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Loop"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["Loop"]][["ETRweighted"]]$n)

ETR.List.Pos[["Loop"]][["ETRweighted"]] <- ETR.List.Pos[["Loop"]][["ETRweighted"]][!is.na(ETR.List.Pos[["Loop"]][["ETRweighted"]]$ETR),]



#anysub firms weighted ETR 

ETR.List.Pos[["Byanysub"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]]),function(y) sum(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List.Pos[["Byanysub"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["Byanysub"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$n)
ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$n)

ETR.List.Pos[["Byanysub"]][["ETRweighted"]] <- ETR.List.Pos[["Byanysub"]][["ETRweighted"]][!is.na(ETR.List.Pos[["Byanysub"]][["ETRweighted"]]$ETR),]


#anysubGER firms weighted ETR 

ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) sum(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                         "sd" = c(sapply(1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                         "n" = c(sapply(1:length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) length(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$n)
ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$n)

ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]] <- ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]][!is.na(ETR.List.Pos[["ByanysubGER"]][["ETRweighted"]]$ETR),]


#Affiliates  weighted ETR


ETR.List.Pos[["Affiliates"]][["ETRweighted"]] <- data.frame("ISO" = "Affiliates")
ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$ETR <- sum(unique(Reduce("rbind", ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]]))[,12], na.rm = TRUE)
ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]])[,12])^2), na.rm = TRUE ))
ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$n <- length(unique(Reduce("rbind", ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]))[,13])])
ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$ETR - qt(0.975, df = ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$sd / sqrt(ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$n)
ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$ETR + qt(0.975, df = ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$sd / sqrt(ETR.List.Pos[["Affiliates"]][["ETRweighted"]]$n)



#GerGUO weighted ETR


ETR.List.Pos[["GerGUO"]][["ETRweighted"]] <- data.frame("ISO" = "GerGUO")
ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$ETR <- sum(unique(Reduce("rbind", ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]]))[,12], na.rm = TRUE)
ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR.List.Pos[["ByanysubGER"]][["ETRunweightedEBT"]])[,12])^2), na.rm = TRUE ))
ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$n <- length(unique(Reduce("rbind", ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR.List.Pos[["ByanysubGER"]][["ETRunweightedTax"]]))[,13])])
ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$ETR - qt(0.975, df = ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$sd / sqrt(ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$n)
ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$ETR + qt(0.975, df = ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$sd / sqrt(ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$n)

ETR.List.Pos[["GerGUO"]][["ETRweighted"]] <- ETR.List.Pos[["GerGUO"]][["ETRweighted"]][!is.na(ETR.List.Pos[["GerGUO"]][["ETRweighted"]]$ETR),]





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown

ETR.List.Pos[["Byanyown"]][["ETRunweighted"]] <- rbind(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavens"), "ETR" = c(mean(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                              "sd" = c(sd(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                              "n" = c(length(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))

ETR.List.Pos[["Byanyown"]][["ETRunweighted"]] <- rbind(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavensEU"), "ETR" = c(mean(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                              "sd" = c(sd(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                              "n" = c(length(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))



ETR.List.Pos[["Byanyown"]][["ETRunweighted"]] <- rbind(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEUProxy"), "ETR" = c(mean(unique(Reduce("rbind",ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))



ETR.List.Pos[["Byanyown"]][["ETRunweighted"]] <- rbind(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]], 
                                                   ETR.List.Pos[["DeInt"]][["ETRunweighted"]],
                                                   ETR.List.Pos[["DeDom"]][["ETRunweighted"]],
                                                   ETR.List.Pos[["Affiliates"]][["ETRunweighted"]],
                                                   ETR.List.Pos[["GerGUO"]][["ETRunweighted"]]
)


ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$low95 <- ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$n)
ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$high95 <- ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$n-1) * ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$sd /sqrt(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$n)





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown


ETR.List.Pos[["Byanyown"]][["ETRweighted"]] <- rbind(ETR.List.Pos[["Byanyown"]][["ETRweighted"]], 
                                                 
                                                 data.frame("ISO" = c("TaxHavens"), "ETR" = c(sum(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]]) %in% Taxhavens])[,12]), na.rm = TRUE)),
                                                            "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13]), sqrt(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,12])^2),  na.rm = TRUE))),
                                                            "n" = c(length(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                            "low95" = c(NA),
                                                            "high95" = c(NA)
                                                 ))

ETR.List.Pos[["Byanyown"]][["ETRweighted"]] <- rbind(ETR.List.Pos[["Byanyown"]][["ETRweighted"]], 
                                                 
                                                 data.frame("ISO" = c("TaxHavensEU"), "ETR" = c(sum(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedEBT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                            "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                            "n" = c(length(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                            "low95" = c(NA),
                                                            "high95" = c(NA)
                                                 ))



ETR.List.Pos[["Byanyown"]][["ETRweighted"]] <- rbind(ETR.List.Pos[["Byanyown"]][["ETRweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEUProxy"), "ETR" = c(sum(unique(Reduce("rbind",ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]][names(ETR.List.Pos[["Byintermed"]][["ETRunweightedEBT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]][names(ETR.List.Pos[["Byintermed"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



ETR.List.Pos[["Byanyown"]][["ETRweighted"]] <- rbind(ETR.List.Pos[["Byanyown"]][["ETRweighted"]], 
                                                 ETR.List.Pos[["DeInt"]][["ETRweighted"]],
                                                 ETR.List.Pos[["DeDom"]][["ETRweighted"]],
                                                 ETR.List.Pos[["Affiliates"]][["ETRweighted"]],
                                                 ETR.List.Pos[["GerGUO"]][["ETRweighted"]]
)


ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$low95 <- ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$n)
ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$high95 <- ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$n-1) * ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$sd /sqrt(ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, ETR, g, h, i, ISO, j, x, y , z)





