


EBT <- rio::import("ImportEBT.xlsx", which = "Results")
EBT <- cbind(data.frame("CompanyBVDID" = c(EBT$`BvD ID number`)),EBT[,4:13])
EBT <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), EBT, by = "CompanyBVDID")


Tax <- rio::import("ImportTax.xlsx", which = "Results")
Tax <- cbind(data.frame("CompanyBVDID" = c(Tax$`BvD ID number`)),Tax[,4:13])
Tax <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), Tax, by = "CompanyBVDID")

InterestPaid <- rio::import("ImportIP.xlsx", which = "Results")
InterestPaid <- cbind(data.frame("CompanyBVDID" = c(InterestPaid$`BvD ID number`)),InterestPaid[,5:13])
InterestPaid$DUMMY <- NA
InterestPaid <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), InterestPaid, by = "CompanyBVDID")


EBIPT <- EBT

for (i in 1:nrow(EBIPT)) {
  for (j in 2:ncol(EBIPT)) {
    
  EBIPT[i,j] <- ifelse(!is.na(as.numeric(EBT[i,j])) & !is.na(as.numeric(InterestPaid[i,j])), as.numeric(EBT[i,j]) + as.numeric(InterestPaid[i,j]), NA  )  
    
  }
}





##equal out samples

for(i in 1:nrow(EBIPT)) {
  for (j in 2:ncol(EBIPT)) {
    EBIPT[i,j] <- ifelse(!is.na(as.numeric(EBIPT[i,j])) & !is.na(as.numeric(Tax[i,j])),  as.numeric(EBIPT[i,j]) , NA )
  }
}


for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(EBIPT[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}



## Drop company if losses in 2010

EBIPT <- EBIPT[EBIPT[,11] > 0 | is.na(EBIPT[,11]),]
EBIPT <- EBIPT[!is.na(EBIPT$CompanyBVDID),]
Tax <- Tax[Tax$CompanyBVDID %in% EBIPT$CompanyBVDID,]


## Keep 2010 - 2019

EBIPT[,11] <- NA
Tax[,11] <- NA


EBIPT[,2] <- NA
Tax[,2] <- NA


## Drop last year if negative profits


EBIPT[,3][EBIPT[,3] < 0] <- NA



for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(EBIPT[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}


## Drop if <3 obs


EBIPT <- EBIPT[apply(EBIPT,1,function (z) length(z[!is.na(as.numeric(z))]) > 2),]
Tax <- Tax[Tax$CompanyBVDID %in% EBIPT$CompanyBVDID,]








ETR2.List.Pos <- vector(mode = "list")
ETR2.List.Pos[[1]] <- vector(mode = "list")
names(ETR2.List.Pos) <- "ByCSH"


#Domestic firms unweightet ETR2


ETR2.List.Pos[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR2.List.Pos[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(ETR2.List.Pos[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(ETR2.List.Pos[["DeDom"]][["CompanyList"]])) {
  ETR2.List.Pos[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR2.List.Pos[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR2.List.Pos[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR2.List.Pos[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR2.List.Pos[["DeDom"]][["CompanyList"]][[i]])))
}

ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]] <- subset(EBIPT, EBIPT$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]])) {
  for (j in 2:ncol(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]])) {
    ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][i,j])) & ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][i,1] %in% ETR2.List.Pos[["DeDom"]][["CompanyList"]][[(j-1)]], ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][i,j], NA)
  }}



for (i in 1:length(ETR2.List.Pos[["DeDom"]][["CompanyList"]])) {
  ETR2.List.Pos[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR2.List.Pos[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR2.List.Pos[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR2.List.Pos[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR2.List.Pos[["DeDom"]][["CompanyList"]][[i]])))
}


ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]])) {
  for (j in 2:ncol(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]])) {
    ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][i,j])) & ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][i,1] %in% ETR2.List.Pos[["DeDom"]][["CompanyList"]][[(j-1)]], ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]])) {
  for(j in 2:ncol(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]])) {
    ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][i,j])) ,  as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]])) {
  for(j in 2:ncol(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]])) {
    ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][i,j])) ,  as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][i,j])  , NA  )
  }}



ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]]$sum <- sapply(1:nrow(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]]), function (y) sum(as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][y,2:11]) , na.rm = TRUE ))
ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]]$sum[ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]]$sum == 0] <- NA
ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]]$sum <- sapply(1:nrow(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]]), function (y) sum(as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][y,2:11]), na.rm = TRUE))
ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]]$sum[ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]]$sum == 0] <- NA
ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]]$ETR2 <- sapply(1:nrow(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]]), function (y) ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][[y,12]] / ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][[y,12]])

ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]] <- ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][!ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]]$sum < 0,]
ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]] <- ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]][!ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]]$sum < 0,]



ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                         "ETR2" = mean(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][,13], na.rm = TRUE),
                                                         "n" = length(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][,13][!is.na(as.numeric(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]][,13]))]))

ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$n)

ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]] <- ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]][!is.na(ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]]$ETR2),]


#International firms unweightet ETR2


ETR2.List.Pos[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR2.List.Pos[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(ETR2.List.Pos[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(ETR2.List.Pos[["DeInt"]][["CompanyList"]])) {
  ETR2.List.Pos[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR2.List.Pos[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR2.List.Pos[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR2.List.Pos[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR2.List.Pos[["DeInt"]][["CompanyList"]][[i]])))
}

ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]] <- subset(EBIPT, EBIPT$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]])) {
  for (j in 2:ncol(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]])) {
    ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][i,j])) & ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][i,1] %in% ETR2.List.Pos[["DeInt"]][["CompanyList"]][[(j-1)]], ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][i,j], NA)
  }}



for (i in 1:length(ETR2.List.Pos[["DeInt"]][["CompanyList"]])) {
  ETR2.List.Pos[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR2.List.Pos[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR2.List.Pos[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR2.List.Pos[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR2.List.Pos[["DeInt"]][["CompanyList"]][[i]])))
}


ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]])) {
  for (j in 2:ncol(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]])) {
    ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][i,j])) & ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][i,1] %in% ETR2.List.Pos[["DeInt"]][["CompanyList"]][[(j-1)]], ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]])) {
  for(j in 2:ncol(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]])) {
    ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][i,j])) ,  as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]])) {
  for(j in 2:ncol(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]])) {
    ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][i,j])) ,  as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][i,j])  , NA  )
  }}


ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]]$sum <- sapply(1:nrow(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]]), function (y) sum(as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][y,2:11]) , na.rm = TRUE ))
ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]]$sum[ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]]$sum == 0] <- NA
ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]]$sum <- sapply(1:nrow(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]]), function (y) sum(as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][y,2:11]), na.rm = TRUE))
ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]]$sum[ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]]$sum == 0] <- NA
ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]]$ETR2 <- sapply(1:nrow(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]]), function (y) ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][[y,12]] / ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][[y,12]])

ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]] <- ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][!ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]]$sum < 0,]
ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]] <- ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]][!ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]]$sum < 0,]


ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]] <- data.frame("ISO" = "DeInt", 
                                                         "ETR2" = mean(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][,13], na.rm = TRUE),
                                                         "n" = length(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][,13][!is.na(as.numeric(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]][,13]))]))

ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$n)

ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]] <- ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]][!is.na(ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]]$ETR2),]



#CSH unweighted ETR2


ETR2.List.Pos[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(ETR2.List.Pos[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR2.List.Pos[["ByCSH"]][["CompanyList"]])) {ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR2.List.Pos[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[i]])) {
    ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]] <- ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]])) {
  Temp1 <- ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[i]] <- subset(EBIPT, EBIPT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[i]]) == 0 ) {ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][i,1] %in% ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][i,j], NA)
    }}}



ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List.Pos[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]])) {
  ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]]$sum[ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][y,12])
  
  ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]] <- ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[x]][!ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]] <- ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]][!ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  
}



ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$n)

ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]] <- ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]][!is.na(ETR2.List.Pos[["ByCSH"]][["ETR2unweighted"]]$ETR2),]



#GUO unweighted ETR2


ETR2.List.Pos[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(ETR2.List.Pos[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR2.List.Pos[["ByGUO"]][["CompanyList"]])) {ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(ETR2.List.Pos[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[i]])) {
    ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]] <- ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]])) {
  Temp1 <- ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[i]] <- subset(EBIPT, EBIPT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[i]]) == 0 ) {ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][i,1] %in% ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][i,j], NA)
    }}}



ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List.Pos[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]])) {
  ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]]$sum[ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][y,12])
  
  ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]] <- ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[x]][!ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]] <- ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]][!ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  
}


ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$n)

ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]] <- ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]][!is.na(ETR2.List.Pos[["ByGUO"]][["ETR2unweighted"]]$ETR2),]


#anyown unweighted ETR2


ETR2.List.Pos[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR2.List.Pos[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR2.List.Pos[["Byanyown"]][["CompanyList"]])) {ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR2.List.Pos[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]] <- ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]])) {
  Temp1 <- ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[i]] <- subset(EBIPT, EBIPT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[i]]) == 0 ) {ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][i,1] %in% ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][i,j], NA)
    }}}



ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List.Pos[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]])) {
  ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]]$sum[ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][y,12])
  
  ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]] <- ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[x]][!ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]] <- ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]][!ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  
}


ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$n)

ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]] <- ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]][!is.na(ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$ETR2),]




#intermed unweighted ETR


ETR2.List.Pos[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(ETR2.List.Pos[["Byintermed"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR2.List.Pos[["Byintermed"]][["CompanyList"]])) {ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR2.List.Pos[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]] <- ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]])) {
  Temp1 <- ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[i]] <- subset(EBIPT, EBIPT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[i]]) == 0 ) {ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][i,1] %in% ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][i,j], NA)
    }}}



ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List.Pos[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]])) {
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]]$sum[ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][y,12])
  
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]] <- ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][!ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]] <- ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][!ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]]$ETR2 > 1,]
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]] <- ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[x]][!ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]] <- ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]][!ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  
}


ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                              "sd" = c(sapply(1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                              "n" = c(sapply(1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$n)

ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]] <- ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]][!is.na(ETR2.List.Pos[["Byintermed"]][["ETR2unweighted"]]$ETR2),]



#ETR2 unweighted Loops


ETR2.List.Pos[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR2.List.Pos[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR2.List.Pos[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(ETR2.List.Pos[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(ETR2.List.Pos[["Loop"]][["CompanyList"]])) {ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR2.List.Pos[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR2.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]])) {
  Temp1 <- ETR2.List.Pos[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[i]] <- subset(EBIPT, EBIPT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[i]]) == 0 ) {ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][i,1] %in% ETR2.List.Pos[["Loop"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][i,j], NA)
    }}}



ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List.Pos[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List.Pos[["Loop"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]])) {
  ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]]$sum[ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][y,12])
  
  ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]] <- ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[x]][!ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]] <- ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]][!ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  
}


ETR2.List.Pos[["Loop"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$n)

ETR2.List.Pos[["Loop"]][["ETR2unweighted"]] <- ETR2.List.Pos[["Loop"]][["ETR2unweighted"]][!is.na(ETR2.List.Pos[["Loop"]][["ETR2unweighted"]]$ETR2),]


#anysub unweighted ETR2


ETR2.List.Pos[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR2.List.Pos[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR2.List.Pos[["Byanysub"]][["CompanyList"]])) {ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR2.List.Pos[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] <- ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]])) {
  Temp1 <- ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[i]] <- subset(EBIPT, EBIPT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[i]]) == 0 ) {ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][i,1] %in% ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][i,j], NA)
    }}}



ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]])) {
  ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]]$sum[ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][y,12])
  
  ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]] <- ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[x]][!ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]] <- ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]][!ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  
}

ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$n)

ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]] <- ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]][!is.na(ETR2.List.Pos[["Byanysub"]][["ETR2unweighted"]]$ETR2),]


#anysubGER unweighted ETR2


ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]])) {ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(ETR2.List.Pos[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]])) {
  Temp1 <- ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[i]] <- subset(EBIPT, EBIPT$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[i]]) == 0 ) {ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][i,1] %in% ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][i,j], NA)
    }}}



ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]])) {
  Temp1 <- ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[i]]) == 0 ) {ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]])) {
  for (i in 1:nrow(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j])) & ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,1] %in% ETR2.List.Pos[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]])) {
      ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]])) {
  for(i in 1:nrow(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]])) {
      ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][i,j])) & !is.na(as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]])) {
  ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]]$sum[ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]$sum[ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]$sum == 0] <- NA
  ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]$ETR2 <- sapply(1:nrow(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]]), function (y) ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][[y,12]] / ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][y,12])

  ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]] <- ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[x]][!ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]] <- ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]][!ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[x]]$sum < 0,]
  
}


ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) mean(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                               "sd" = c(sapply(1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) sd(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                               "n" = c(sapply(1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$n)

ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]] <- ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]][!is.na(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweighted"]]$ETR2),]


#Affiliates  unweighted ETR2


ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                              "ETR2" = mean(unique(Reduce("rbind", ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]))[,13], na.rm = TRUE), 
                                                              "sd" = sd(unique(Reduce("rbind", ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]))[,13], na.rm = TRUE),
                                                              "n" = length(unique(Reduce("rbind", ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]))[,13])]))




ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]]$n)




#GerGUO unweighted ETR2


ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                          "ETR2" = mean(unique(Reduce("rbind", ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13], na.rm = TRUE), 
                                                          "sd" = sd(unique(Reduce("rbind", ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13], na.rm = TRUE),
                                                          "n" = length(unique(Reduce("rbind", ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13])]))




ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$n)

ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]] <- ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]][!is.na(ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]$ETR2),]




#Domestic firms weighted ETR2


ETR2.List.Pos[["DeDom"]][["ETR2weighted"]] <- data.frame("ISO" = "DEDOM")
ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$ETR2 <- sum(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]]$sum, na.rm = TRUE)
ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$sd <- sqrt(wtd.var(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]]$ETR2, sqrt(ETR2.List.Pos[["DeDom"]][["ETR2unweightedEBIPT"]]$sum^2), na.rm = TRUE ))
ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$n <- length(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]]$ETR2[!is.na(ETR2.List.Pos[["DeDom"]][["ETR2unweightedTax"]]$ETR2)])
ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$ETR2 - qt(0.975, df = ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$sd / sqrt(ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$ETR2 + qt(0.975, df = ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$sd / sqrt(ETR2.List.Pos[["DeDom"]][["ETR2weighted"]]$n)


#International firms weighted ETR2


ETR2.List.Pos[["DeInt"]][["ETR2weighted"]] <- data.frame("ISO" = "DEINT")
ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$ETR2 <- sum(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]]$sum, na.rm = TRUE)
ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$sd <- sqrt(wtd.var(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]]$ETR2, sqrt(ETR2.List.Pos[["DeInt"]][["ETR2unweightedEBIPT"]]$sum^2), na.rm = TRUE ))
ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$n <- length(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]]$ETR2[!is.na(ETR2.List.Pos[["DeInt"]][["ETR2unweightedTax"]]$ETR2)])
ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$ETR2 - qt(0.975, df = ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$sd / sqrt(ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$ETR2 + qt(0.975, df = ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$sd / sqrt(ETR2.List.Pos[["DeInt"]][["ETR2weighted"]]$n)


#CSH firms weighted ETR2 

ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedEBIPT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["ByCSH"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$n)

ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]] <- ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]][!is.na(ETR2.List.Pos[["ByCSH"]][["ETR2weighted"]]$ETR2),]


#GUO firms weighted ETR2 

ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedEBIPT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["ByGUO"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$n)

ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]] <- ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]][!is.na(ETR2.List.Pos[["ByGUO"]][["ETR2weighted"]]$ETR2),]


#anyown firms weighted ETR2 

ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$n)

ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]] <- ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]][!is.na(ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$ETR2),]



#intermed firms weighted ETR2 

ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[y]][,12], na.rm = TRUE ))), 
                                                            "sd" = c(sapply(1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                            "n" = c(sapply(1:length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$n)

ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]] <- ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]][!is.na(ETR2.List.Pos[["Byintermed"]][["ETR2weighted"]]$ETR2),]





#Loops firms weighted ETR2 

ETR2.List.Pos[["Loop"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List.Pos[["Loop"]][["ETR2unweightedEBIPT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["Loop"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$n)

ETR2.List.Pos[["Loop"]][["ETR2weighted"]] <- ETR2.List.Pos[["Loop"]][["ETR2weighted"]][!is.na(ETR2.List.Pos[["Loop"]][["ETR2weighted"]]$ETR2),]



#anysub firms weighted ETR2 

ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedEBIPT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["Byanysub"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$n)

ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]] <- ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]][!is.na(ETR2.List.Pos[["Byanysub"]][["ETR2weighted"]]$ETR2),]


#anysubGER firms weighted ETR2 

ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR2" = c(sapply(1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) sum(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[y]][,12], na.rm = TRUE ))), 
                                                             "sd" = c(sapply(1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) sqrt(wtd.var(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13], sqrt(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                             "n" = c(sapply(1:length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]),function(y) length(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]][[y]][,13]))]))))


ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$n)

ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]] <- ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]][!is.na(ETR2.List.Pos[["ByanysubGER"]][["ETR2weighted"]]$ETR2),]


#Affiliates  weighted ETR2


ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]] <- data.frame("ISO" = "Affiliates")
ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$ETR2 <- sum(unique(Reduce("rbind", ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]]))[,12], na.rm = TRUE)
ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]])[,12])^2), na.rm = TRUE ))
ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$n <- length(unique(Reduce("rbind", ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]))[,13])])
ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$ETR2 - qt(0.975, df = ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$sd / sqrt(ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$ETR2 + qt(0.975, df = ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$sd / sqrt(ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]]$n)



#GerGUO weighted ETR2


ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]] <- data.frame("ISO" = "GerGUO")
ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$ETR2 <- sum(unique(Reduce("rbind", ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]]))[,12], na.rm = TRUE)
ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedEBIPT"]])[,12])^2), na.rm = TRUE ))
ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$n <- length(unique(Reduce("rbind", ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR2.List.Pos[["ByanysubGER"]][["ETR2unweightedTax"]]))[,13])])
ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$ETR2 - qt(0.975, df = ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$sd / sqrt(ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$ETR2 + qt(0.975, df = ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$sd / sqrt(ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$n)

ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]] <- ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]][!is.na(ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]$ETR2),]





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown

ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]] <- rbind(ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavens"), "ETR2" = c(mean(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))

ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]] <- rbind(ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEU"), "ETR2" = c(mean(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))

ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]] <- rbind(ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]], 
                                                       ETR2.List.Pos[["DeInt"]][["ETR2unweighted"]],
                                                       ETR2.List.Pos[["DeDom"]][["ETR2unweighted"]],
                                                       ETR2.List.Pos[["Affiliates"]][["ETR2unweighted"]],
                                                       ETR2.List.Pos[["GerGUO"]][["ETR2unweighted"]]
)

ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]] <- rbind(ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEUProxy"), "ETR2" = c(mean(unique(Reduce("rbind",ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))


ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$low95 <- ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$n)
ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$high95 <- ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$n-1) * ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$sd /sqrt(ETR2.List.Pos[["Byanyown"]][["ETR2unweighted"]]$n)





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown


ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]] <- rbind(ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavens"), "ETR2" = c(sum(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]]) %in% Taxhavens])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13]), sqrt(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))

ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]] <- rbind(ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEU"), "ETR2" = c(sum(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedEBIPT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byanyown"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))

ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]] <- rbind(ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]], 
                                                     ETR2.List.Pos[["DeInt"]][["ETR2weighted"]],
                                                     ETR2.List.Pos[["DeDom"]][["ETR2weighted"]],
                                                     ETR2.List.Pos[["Affiliates"]][["ETR2weighted"]],
                                                     ETR2.List.Pos[["GerGUO"]][["ETR2weighted"]]
)


ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]] <- rbind(ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEUProxy"), "ETR2" = c(sum(unique(Reduce("rbind",ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]][names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedEBIPT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]][names(ETR2.List.Pos[["Byintermed"]][["ETR2unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))


ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$low95 <- ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$ETR2 - qt(0.975, df= ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$n)
ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$high95 <- ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$ETR2 + qt(0.975, df= ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$n-1) * ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$sd /sqrt(ETR2.List.Pos[["Byanyown"]][["ETR2weighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, ETR2, g, h, i, ISO, j, x, y , z)





