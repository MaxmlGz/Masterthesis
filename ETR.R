


EBT <- rio::import("ImportEBT.xlsx", which = "Results")
EBT <- cbind(data.frame("CompanyBVDID" = c(EBT$`BvD ID number`)),EBT[,4:13])
EBT <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), EBT, by = "CompanyBVDID")


Tax <- rio::import("ImportTax.xlsx", which = "Results")
Tax <- cbind(data.frame("CompanyBVDID" = c(Tax$`BvD ID number`)),Tax[,4:13])
Tax <- full_join(data.frame("CompanyBVDID" = EBT$CompanyBVDID), Tax, by = "CompanyBVDID")



ETR.List <- vector(mode = "list")
ETR.List[[1]] <- vector(mode = "list")
names(ETR.List) <- "ByCSH"


#Domestic firms unweightet ETR


ETR.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(ETR.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(ETR.List[["DeDom"]][["CompanyList"]])) {
    ETR.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR.List[["DeDom"]][["CompanyList"]][[i]])))
}

ETR.List[["DeDom"]][["ETRunweightedEBT"]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR.List[["DeDom"]][["ETRunweightedEBT"]])) {
  for (j in 2:ncol(ETR.List[["DeDom"]][["ETRunweightedEBT"]])) {
    ETR.List[["DeDom"]][["ETRunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["DeDom"]][["ETRunweightedEBT"]][i,j])) & ETR.List[["DeDom"]][["ETRunweightedEBT"]][i,1] %in% ETR.List[["DeDom"]][["CompanyList"]][[(j-1)]], ETR.List[["DeDom"]][["ETRunweightedEBT"]][i,j], NA)
  }}



for (i in 1:length(ETR.List[["DeDom"]][["CompanyList"]])) {
  ETR.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR.List[["DeDom"]][["CompanyList"]][[i]])))
}


ETR.List[["DeDom"]][["ETRunweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  
  
for (i in 1:nrow(ETR.List[["DeDom"]][["ETRunweightedTax"]])) {
  for (j in 2:ncol(ETR.List[["DeDom"]][["ETRunweightedTax"]])) {
    ETR.List[["DeDom"]][["ETRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["DeDom"]][["ETRunweightedTax"]][i,j])) & ETR.List[["DeDom"]][["ETRunweightedTax"]][i,1] %in% ETR.List[["DeDom"]][["CompanyList"]][[(j-1)]], ETR.List[["DeDom"]][["ETRunweightedTax"]][i,j], NA)
  }}

  
  for(i in 1:nrow(ETR.List[["DeDom"]][["ETRunweightedEBT"]])) {
    for(j in 2:ncol(ETR.List[["DeDom"]][["ETRunweightedEBT"]])) {
      ETR.List[["DeDom"]][["ETRunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["DeDom"]][["ETRunweightedEBT"]][i,j])) & !is.na(as.numeric(ETR.List[["DeDom"]][["ETRunweightedTax"]][i,j])) ,  as.numeric(ETR.List[["DeDom"]][["ETRunweightedEBT"]][i,j])  , NA  )
    }}


  for(i in 1:nrow(ETR.List[["DeDom"]][["ETRunweightedTax"]])) {
    for(j in 2:ncol(ETR.List[["DeDom"]][["ETRunweightedTax"]])) {
      ETR.List[["DeDom"]][["ETRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["DeDom"]][["ETRunweightedEBT"]][i,j])) & !is.na(as.numeric(ETR.List[["DeDom"]][["ETRunweightedTax"]][i,j])) ,  as.numeric(ETR.List[["DeDom"]][["ETRunweightedTax"]][i,j])  , NA  )
    }}


  ETR.List[["DeDom"]][["ETRunweightedEBT"]]$sum <- sapply(1:nrow(ETR.List[["DeDom"]][["ETRunweightedEBT"]]), function (y) sum(as.numeric(ETR.List[["DeDom"]][["ETRunweightedEBT"]][y,2:11]) , na.rm = TRUE ))
  ETR.List[["DeDom"]][["ETRunweightedEBT"]]$sum[ETR.List[["DeDom"]][["ETRunweightedEBT"]]$sum == 0] <- NA

  ETR.List[["DeDom"]][["ETRunweightedTax"]]$sum <- sapply(1:nrow(ETR.List[["DeDom"]][["ETRunweightedTax"]]), function (y) sum(as.numeric(ETR.List[["DeDom"]][["ETRunweightedTax"]][y,2:11]), na.rm = TRUE))
  ETR.List[["DeDom"]][["ETRunweightedTax"]]$sum[ETR.List[["DeDom"]][["ETRunweightedTax"]]$sum == 0] <- NA
  ETR.List[["DeDom"]][["ETRunweightedTax"]]$ETR <- sapply(1:nrow(ETR.List[["DeDom"]][["ETRunweightedTax"]]), function (y) ETR.List[["DeDom"]][["ETRunweightedTax"]][[y,12]] / ETR.List[["DeDom"]][["ETRunweightedEBT"]][[y,12]])



ETR.List[["DeDom"]][["ETRunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                     "ETR" = mean(ETR.List[["DeDom"]][["ETRunweightedTax"]][,13], na.rm = TRUE), 
                                                     "sd" = sd(ETR.List[["DeDom"]][["ETRunweightedTax"]][,13], na.rm = TRUE),
                                                     "n" = length(ETR.List[["DeDom"]][["ETRunweightedTax"]][,13][!is.na(as.numeric(ETR.List[["DeDom"]][["ETRunweightedTax"]][,13]))]))

ETR.List[["DeDom"]][["ETRunweighted"]]$low95 <- ETR.List[["DeDom"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["DeDom"]][["ETRunweighted"]]$n-1) * ETR.List[["DeDom"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["DeDom"]][["ETRunweighted"]]$n)
ETR.List[["DeDom"]][["ETRunweighted"]]$high95 <- ETR.List[["DeDom"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["DeDom"]][["ETRunweighted"]]$n-1) * ETR.List[["DeDom"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["DeDom"]][["ETRunweighted"]]$n)

ETR.List[["DeDom"]][["ETRunweighted"]] <- ETR.List[["DeDom"]][["ETRunweighted"]][!is.na(ETR.List[["DeDom"]][["ETRunweighted"]]$ETR),]

#International firms unweightet ETR


ETR.List[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(ETR.List[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(ETR.List[["DeInt"]][["CompanyList"]])) {
  ETR.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR.List[["DeInt"]][["CompanyList"]][[i]])))
}

ETR.List[["DeInt"]][["ETRunweightedEBT"]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR.List[["DeInt"]][["ETRunweightedEBT"]])) {
  for (j in 2:ncol(ETR.List[["DeInt"]][["ETRunweightedEBT"]])) {
    ETR.List[["DeInt"]][["ETRunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["DeInt"]][["ETRunweightedEBT"]][i,j])) & ETR.List[["DeInt"]][["ETRunweightedEBT"]][i,1] %in% ETR.List[["DeInt"]][["CompanyList"]][[(j-1)]], ETR.List[["DeInt"]][["ETRunweightedEBT"]][i,j], NA)
  }}



for (i in 1:length(ETR.List[["DeInt"]][["CompanyList"]])) {
  ETR.List[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR.List[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR.List[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR.List[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR.List[["DeInt"]][["CompanyList"]][[i]])))
}


ETR.List[["DeInt"]][["ETRunweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR.List[["DeInt"]][["ETRunweightedTax"]])) {
  for (j in 2:ncol(ETR.List[["DeInt"]][["ETRunweightedTax"]])) {
    ETR.List[["DeInt"]][["ETRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["DeInt"]][["ETRunweightedTax"]][i,j])) & ETR.List[["DeInt"]][["ETRunweightedTax"]][i,1] %in% ETR.List[["DeInt"]][["CompanyList"]][[(j-1)]], ETR.List[["DeInt"]][["ETRunweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR.List[["DeInt"]][["ETRunweightedEBT"]])) {
  for(j in 2:ncol(ETR.List[["DeInt"]][["ETRunweightedEBT"]])) {
    ETR.List[["DeInt"]][["ETRunweightedEBT"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["DeInt"]][["ETRunweightedEBT"]][i,j])) & !is.na(as.numeric(ETR.List[["DeInt"]][["ETRunweightedTax"]][i,j])) ,  as.numeric(ETR.List[["DeInt"]][["ETRunweightedEBT"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR.List[["DeInt"]][["ETRunweightedTax"]])) {
  for(j in 2:ncol(ETR.List[["DeInt"]][["ETRunweightedTax"]])) {
    ETR.List[["DeInt"]][["ETRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["DeInt"]][["ETRunweightedEBT"]][i,j])) & !is.na(as.numeric(ETR.List[["DeInt"]][["ETRunweightedTax"]][i,j])) ,  as.numeric(ETR.List[["DeInt"]][["ETRunweightedTax"]][i,j])  , NA  )
  }}


ETR.List[["DeInt"]][["ETRunweightedEBT"]]$sum <- sapply(1:nrow(ETR.List[["DeInt"]][["ETRunweightedEBT"]]), function (y) sum(as.numeric(ETR.List[["DeInt"]][["ETRunweightedEBT"]][y,2:11]) , na.rm = TRUE ))
ETR.List[["DeInt"]][["ETRunweightedEBT"]]$sum[ETR.List[["DeInt"]][["ETRunweightedEBT"]]$sum == 0] <- NA

ETR.List[["DeInt"]][["ETRunweightedTax"]]$sum <- sapply(1:nrow(ETR.List[["DeInt"]][["ETRunweightedTax"]]), function (y) sum(as.numeric(ETR.List[["DeInt"]][["ETRunweightedTax"]][y,2:11]), na.rm = TRUE))
ETR.List[["DeInt"]][["ETRunweightedTax"]]$sum[ETR.List[["DeInt"]][["ETRunweightedTax"]]$sum == 0] <- NA
ETR.List[["DeInt"]][["ETRunweightedTax"]]$ETR <- sapply(1:nrow(ETR.List[["DeInt"]][["ETRunweightedTax"]]), function (y) ETR.List[["DeInt"]][["ETRunweightedTax"]][[y,12]] / ETR.List[["DeInt"]][["ETRunweightedEBT"]][[y,12]])



ETR.List[["DeInt"]][["ETRunweighted"]] <- data.frame("ISO" = "DEINT", 
                                                     "ETR" = mean(ETR.List[["DeInt"]][["ETRunweightedTax"]][,13], na.rm = TRUE), 
                                                     "sd" = sd(ETR.List[["DeInt"]][["ETRunweightedTax"]][,13], na.rm = TRUE),
                                                     "n" = length(ETR.List[["DeInt"]][["ETRunweightedTax"]][,13][!is.na(as.numeric(ETR.List[["DeInt"]][["ETRunweightedTax"]][,13]))]))


ETR.List[["DeInt"]][["ETRunweighted"]]$low95 <- ETR.List[["DeInt"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["DeInt"]][["ETRunweighted"]]$n-1) * ETR.List[["DeInt"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["DeInt"]][["ETRunweighted"]]$n)
ETR.List[["DeInt"]][["ETRunweighted"]]$high95 <- ETR.List[["DeInt"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["DeInt"]][["ETRunweighted"]]$n-1) * ETR.List[["DeInt"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["DeInt"]][["ETRunweighted"]]$n)

ETR.List[["DeInt"]][["ETRunweighted"]] <- ETR.List[["DeInt"]][["ETRunweighted"]][!is.na(ETR.List[["DeInt"]][["ETRunweighted"]]$ETR),]


#CSH unweighted ETR


ETR.List[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(ETR.List[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List[["ByCSH"]][["CompanyList"]])) {ETR.List[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR.List[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List[["ByCSH"]][["CompanyList"]][[i]])) {
    ETR.List[["ByCSH"]][["CompanyList"]][[i]][[j]] <- ETR.List[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR.List[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR.List[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

ETR.List[["ByCSH"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["ByCSH"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(ETR.List[["ByCSH"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[i]])) {
  Temp1 <- unique(c(Temp1,ETR.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["ByCSH"]][["ETRunweightedEBT"]])) {
        for (i in 1:nrow(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]])) {
          for (j in 2:ncol(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]])) {
            ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j], NA)
}}}



ETR.List[["ByCSH"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["ByCSH"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["ByCSH"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["ByCSH"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List[["ByCSH"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List[["ByCSH"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
      }}}


for(x in 1:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List[["ByCSH"]][["ETRunweightedEBT"]])) {
  ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]])) {
        ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
        ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]]$sum[ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
        ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List[["ByCSH"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[x]][[y,12]])
  }
    

ETR.List[["ByCSH"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]]),function(y) mean(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                      "sd" = c(sapply(1:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]]),function(y) sd(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                      "n" = c(sapply(1:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["ByCSH"]][["ETRunweighted"]]$low95 <- ETR.List[["ByCSH"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["ByCSH"]][["ETRunweighted"]]$n-1) * ETR.List[["ByCSH"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["ByCSH"]][["ETRunweighted"]]$n)
ETR.List[["ByCSH"]][["ETRunweighted"]]$high95 <- ETR.List[["ByCSH"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["ByCSH"]][["ETRunweighted"]]$n-1) * ETR.List[["ByCSH"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["ByCSH"]][["ETRunweighted"]]$n)

ETR.List[["ByCSH"]][["ETRunweighted"]] <- ETR.List[["ByCSH"]][["ETRunweighted"]][!is.na(ETR.List[["ByCSH"]][["ETRunweighted"]]$ETR),]



#GUO unweighted ETR


ETR.List[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(ETR.List[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List[["ByGUO"]][["CompanyList"]])) {ETR.List[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(ETR.List[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List[["ByGUO"]][["CompanyList"]][[i]])) {
    ETR.List[["ByGUO"]][["CompanyList"]][[i]][[j]] <- ETR.List[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR.List[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR.List[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

ETR.List[["ByGUO"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["ByGUO"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["ByGUO"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["ByGUO"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List[["ByGUO"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["ByGUO"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["ByGUO"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["ByGUO"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List[["ByGUO"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List[["ByGUO"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List[["ByGUO"]][["ETRunweightedEBT"]])) {
  ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]])) {
  ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]]$sum[ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List[["ByGUO"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[x]][[y,12]])
}


ETR.List[["ByGUO"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]]),function(y) mean(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                     "sd" = c(sapply(1:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]]),function(y) sd(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                     "n" = c(sapply(1:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["ByGUO"]][["ETRunweighted"]]$low95 <- ETR.List[["ByGUO"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["ByGUO"]][["ETRunweighted"]]$n-1) * ETR.List[["ByGUO"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["ByGUO"]][["ETRunweighted"]]$n)
ETR.List[["ByGUO"]][["ETRunweighted"]]$high95 <- ETR.List[["ByGUO"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["ByGUO"]][["ETRunweighted"]]$n-1) * ETR.List[["ByGUO"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["ByGUO"]][["ETRunweighted"]]$n)

ETR.List[["ByGUO"]][["ETRunweighted"]] <- ETR.List[["ByGUO"]][["ETRunweighted"]][!is.na(ETR.List[["ByGUO"]][["ETRunweighted"]]$ETR),]


#anyown unweighted ETR


ETR.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List[["Byanyown"]][["CompanyList"]])) {ETR.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {ETR.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(ETR.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}

Temp1 <- apply(ETR.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
Temp2 <- apply(ETR.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))

Temp4 <- ETR.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}

for (g in 1:nrow(Temp4)) {
  for(h in 1:ncol(Temp4)) {
      Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
    }
}

Temp4 <- sapply(Temp4, function (y) y == "TRUE")

ETR.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- ETR.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
}}



ETR.List[["Byanyown"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["Byanyown"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["Byanyown"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["Byanyown"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List[["Byanyown"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["Byanyown"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["Byanyown"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List[["Byanyown"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List[["Byanyown"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List[["Byanyown"]][["ETRunweightedEBT"]])) {
  ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]])) {
  ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]]$sum[ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List[["Byanyown"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[x]][[y,12]])
}


ETR.List[["Byanyown"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]]),function(y) mean(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                     "sd" = c(sapply(1:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]]),function(y) sd(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                     "n" = c(sapply(1:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["Byanyown"]][["ETRunweighted"]]$low95 <- ETR.List[["Byanyown"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["Byanyown"]][["ETRunweighted"]]$n-1) * ETR.List[["Byanyown"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["Byanyown"]][["ETRunweighted"]]$n)
ETR.List[["Byanyown"]][["ETRunweighted"]]$high95 <- ETR.List[["Byanyown"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["Byanyown"]][["ETRunweighted"]]$n-1) * ETR.List[["Byanyown"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["Byanyown"]][["ETRunweighted"]]$n)

ETR.List[["Byanyown"]][["ETRunweighted"]] <- ETR.List[["Byanyown"]][["ETRunweighted"]][!is.na(ETR.List[["Byanyown"]][["ETRunweighted"]]$ETR),]




#ETR unweighted Loops


ETR.List[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR.List[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(ETR.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    ETR.List[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR.List[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(ETR.List[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(ETR.List[["Loop"]][["CompanyList"]])) {ETR.List[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR.List[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR.List[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR.List[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR.List[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {ETR.List[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(ETR.List[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR.List[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR.List[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR.List[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR.List[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR.List[["Loop"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["Loop"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["Loop"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["Loop"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["Loop"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["Loop"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["Loop"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List[["Loop"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["Loop"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List[["Loop"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["Loop"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["Loop"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["Loop"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["Loop"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["Loop"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["Loop"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List[["Loop"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["Loop"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["Loop"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List[["Loop"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List[["Loop"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["Loop"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List[["Loop"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List[["Loop"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["Loop"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List[["Loop"]][["ETRunweightedEBT"]])) {
  ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(ETR.List[["Loop"]][["ETRunweightedTax"]])) {
  ETR.List[["Loop"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List[["Loop"]][["ETRunweightedTax"]][[x]]$sum[ETR.List[["Loop"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List[["Loop"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List[["Loop"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List[["Loop"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List[["Loop"]][["ETRunweightedEBT"]][[x]][[y,12]])
}


ETR.List[["Loop"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["Loop"]][["ETRunweightedTax"]]),function(y) mean(ETR.List[["Loop"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                    "sd" = c(sapply(1:length(ETR.List[["Loop"]][["ETRunweightedTax"]]),function(y) sd(ETR.List[["Loop"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                    "n" = c(sapply(1:length(ETR.List[["Loop"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["Loop"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["Loop"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["Loop"]][["ETRunweighted"]]$low95 <- ETR.List[["Loop"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["Loop"]][["ETRunweighted"]]$n-1) * ETR.List[["Loop"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["Loop"]][["ETRunweighted"]]$n)
ETR.List[["Loop"]][["ETRunweighted"]]$high95 <- ETR.List[["Loop"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["Loop"]][["ETRunweighted"]]$n-1) * ETR.List[["Loop"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["Loop"]][["ETRunweighted"]]$n)

ETR.List[["Loop"]][["ETRunweighted"]] <- ETR.List[["Loop"]][["ETRunweighted"]][!is.na(ETR.List[["Loop"]][["ETRunweighted"]]$ETR),]


#anysub unweighted ETR


ETR.List[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR.List[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List[["Byanysub"]][["CompanyList"]])) {ETR.List[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR.List[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(ETR.List[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR.List[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {ETR.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(ETR.List[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR.List[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR.List[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- ETR.List[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR.List[["Byanysub"]][["CompanyList"]][[i]][[j]] <- ETR.List[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR.List[["Byanysub"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["Byanysub"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["Byanysub"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["Byanysub"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List[["Byanysub"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["Byanysub"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["Byanysub"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["Byanysub"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List[["Byanysub"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List[["Byanysub"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List[["Byanysub"]][["ETRunweightedEBT"]])) {
  ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]])) {
  ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]]$sum[ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List[["Byanysub"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[x]][[y,12]])
}


ETR.List[["Byanysub"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]]),function(y) mean(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]]),function(y) sd(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["Byanysub"]][["ETRunweighted"]]$low95 <- ETR.List[["Byanysub"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["Byanysub"]][["ETRunweighted"]]$n-1) * ETR.List[["Byanysub"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["Byanysub"]][["ETRunweighted"]]$n)
ETR.List[["Byanysub"]][["ETRunweighted"]]$high95 <- ETR.List[["Byanysub"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["Byanysub"]][["ETRunweighted"]]$n-1) * ETR.List[["Byanysub"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["Byanysub"]][["ETRunweighted"]]$n)

ETR.List[["Byanysub"]][["ETRunweighted"]] <- ETR.List[["Byanysub"]][["ETRunweighted"]][!is.na(ETR.List[["Byanysub"]][["ETRunweighted"]]$ETR),]


#anysubGER unweighted ETR


ETR.List[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR.List[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR.List[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR.List[["ByanysubGER"]][["CompanyList"]])) {ETR.List[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(ETR.List[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(ETR.List[["ByanysubGER"]][["CompanyList"]][[i]])) {

    
    ETR.List[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- ETR.List[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(ETR.List[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(ETR.List[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
  
  }
  }


ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]])) {
  Temp1 <- ETR.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[i]] <- subset(EBT, EBT$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[i]]) == 0 ) {ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]])) {
  for (i in 1:nrow(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]])) {
    for (j in 2:ncol(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j])) & ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,1] %in% ETR.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j], NA)
    }}}



ETR.List[["ByanysubGER"]][["ETRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]])) {
  Temp1 <- ETR.List[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR.List[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[i]]) == 0 ) {ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]])) {
  for (i in 1:nrow(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j])) & ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,1] %in% ETR.List[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]])) {
  for(i in 1:nrow(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]])) {
    for(j in 2:ncol(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]])) {
      ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]])) {
  for(i in 1:nrow(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]])) {
      ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][i,j])) & !is.na(as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j])) ,  as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]])) {
  ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]]$sum <- sapply(1:nrow(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]]), function (y) sum(as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]]$sum[ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]]$sum == 0] <- NA
}


for (x in 1:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]])) {
  ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]]), function (y) sum(as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]]$sum[ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]]$sum == 0] <- NA
  ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]]$ETR <- sapply(1:nrow(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]]), function (y) ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[x]][[y,12]] / ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[x]][[y,12]])
}


ETR.List[["ByanysubGER"]][["ETRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) mean(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) sd(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["ByanysubGER"]][["ETRunweighted"]]$low95 <- ETR.List[["ByanysubGER"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["ByanysubGER"]][["ETRunweighted"]]$n-1) * ETR.List[["ByanysubGER"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["ByanysubGER"]][["ETRunweighted"]]$n)
ETR.List[["ByanysubGER"]][["ETRunweighted"]]$high95 <- ETR.List[["ByanysubGER"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["ByanysubGER"]][["ETRunweighted"]]$n-1) * ETR.List[["ByanysubGER"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["ByanysubGER"]][["ETRunweighted"]]$n)

ETR.List[["ByanysubGER"]][["ETRunweighted"]] <- ETR.List[["ByanysubGER"]][["ETRunweighted"]][!is.na(ETR.List[["ByanysubGER"]][["ETRunweighted"]]$ETR),]


#Affiliates  unweighted ETR


ETR.List[["Affiliates"]][["ETRunweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                          "ETR" = mean(unique(Reduce("rbind", ETR.List[["Byanyown"]][["ETRunweightedTax"]]))[,13], na.rm = TRUE), 
                                                          "sd" = sd(unique(Reduce("rbind", ETR.List[["Byanyown"]][["ETRunweightedTax"]]))[,13], na.rm = TRUE),
                                                          "n" = length(unique(Reduce("rbind", ETR.List[["Byanyown"]][["ETRunweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR.List[["Byanyown"]][["ETRunweightedTax"]]))[,13])]))




ETR.List[["Affiliates"]][["ETRunweighted"]]$low95 <- ETR.List[["Affiliates"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["Affiliates"]][["ETRunweighted"]]$n-1) * ETR.List[["Affiliates"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["Affiliates"]][["ETRunweighted"]]$n)
ETR.List[["Affiliates"]][["ETRunweighted"]]$high95 <- ETR.List[["Affiliates"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["Affiliates"]][["ETRunweighted"]]$n-1) * ETR.List[["Affiliates"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["Affiliates"]][["ETRunweighted"]]$n)




#GerGUO unweighted ETR


ETR.List[["GerGUO"]][["ETRunweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                      "ETR" = mean(unique(Reduce("rbind", ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]))[,13], na.rm = TRUE), 
                                                      "sd" = sd(unique(Reduce("rbind", ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]))[,13], na.rm = TRUE),
                                                      "n" = length(unique(Reduce("rbind", ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]))[,13])]))




ETR.List[["GerGUO"]][["ETRunweighted"]]$low95 <- ETR.List[["GerGUO"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["GerGUO"]][["ETRunweighted"]]$n-1) * ETR.List[["GerGUO"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["GerGUO"]][["ETRunweighted"]]$n)
ETR.List[["GerGUO"]][["ETRunweighted"]]$high95 <- ETR.List[["GerGUO"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["GerGUO"]][["ETRunweighted"]]$n-1) * ETR.List[["GerGUO"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["GerGUO"]][["ETRunweighted"]]$n)

ETR.List[["GerGUO"]][["ETRunweighted"]] <- ETR.List[["GerGUO"]][["ETRunweighted"]][!is.na(ETR.List[["GerGUO"]][["ETRunweighted"]]$ETR),]




#Domestic firms weighted ETR


ETR.List[["DeDom"]][["ETRweighted"]] <- data.frame("ISO" = "DEDOM")
ETR.List[["DeDom"]][["ETRweighted"]]$ETR <- sum(ETR.List[["DeDom"]][["ETRunweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR.List[["DeDom"]][["ETRunweightedEBT"]]$sum, na.rm = TRUE)
ETR.List[["DeDom"]][["ETRweighted"]]$sd <- sqrt(wtd.var(ETR.List[["DeDom"]][["ETRunweightedTax"]]$ETR, sqrt(ETR.List[["DeDom"]][["ETRunweightedEBT"]]$sum^2), na.rm = TRUE ))
ETR.List[["DeDom"]][["ETRweighted"]]$n <- length(ETR.List[["DeDom"]][["ETRunweightedTax"]]$ETR[!is.na(ETR.List[["DeDom"]][["ETRunweightedTax"]]$ETR)])
ETR.List[["DeDom"]][["ETRweighted"]]$low95 <- ETR.List[["DeDom"]][["ETRweighted"]]$ETR - qt(0.975, df = ETR.List[["DeDom"]][["ETRweighted"]]$n-1) * ETR.List[["DeDom"]][["ETRweighted"]]$sd / sqrt(ETR.List[["DeDom"]][["ETRweighted"]]$n)
ETR.List[["DeDom"]][["ETRweighted"]]$high95 <- ETR.List[["DeDom"]][["ETRweighted"]]$ETR + qt(0.975, df = ETR.List[["DeDom"]][["ETRweighted"]]$n-1) * ETR.List[["DeDom"]][["ETRweighted"]]$sd / sqrt(ETR.List[["DeDom"]][["ETRweighted"]]$n)
  

#International firms weighted ETR


ETR.List[["DeInt"]][["ETRweighted"]] <- data.frame("ISO" = "DEINT")
ETR.List[["DeInt"]][["ETRweighted"]]$ETR <- sum(ETR.List[["DeInt"]][["ETRunweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR.List[["DeInt"]][["ETRunweightedEBT"]]$sum, na.rm = TRUE)
ETR.List[["DeInt"]][["ETRweighted"]]$sd <- sqrt(wtd.var(ETR.List[["DeInt"]][["ETRunweightedTax"]]$ETR, sqrt(ETR.List[["DeInt"]][["ETRunweightedEBT"]]$sum^2), na.rm = TRUE ))
ETR.List[["DeInt"]][["ETRweighted"]]$n <- length(ETR.List[["DeInt"]][["ETRunweightedTax"]]$ETR[!is.na(ETR.List[["DeInt"]][["ETRunweightedTax"]]$ETR)])
ETR.List[["DeInt"]][["ETRweighted"]]$low95 <- ETR.List[["DeInt"]][["ETRweighted"]]$ETR - qt(0.975, df = ETR.List[["DeInt"]][["ETRweighted"]]$n-1) * ETR.List[["DeInt"]][["ETRweighted"]]$sd / sqrt(ETR.List[["DeInt"]][["ETRweighted"]]$n)
ETR.List[["DeInt"]][["ETRweighted"]]$high95 <- ETR.List[["DeInt"]][["ETRweighted"]]$ETR + qt(0.975, df = ETR.List[["DeInt"]][["ETRweighted"]]$n-1) * ETR.List[["DeInt"]][["ETRweighted"]]$sd / sqrt(ETR.List[["DeInt"]][["ETRweighted"]]$n)


#CSH firms weighted ETR 

ETR.List[["ByCSH"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]]),function(y) sum(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                     "sd" = c(sapply(1:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List[["ByCSH"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                     "n" = c(sapply(1:length(ETR.List[["ByCSH"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["ByCSH"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["ByCSH"]][["ETRweighted"]]$low95 <- ETR.List[["ByCSH"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List[["ByCSH"]][["ETRweighted"]]$n-1) * ETR.List[["ByCSH"]][["ETRweighted"]]$sd /sqrt(ETR.List[["ByCSH"]][["ETRweighted"]]$n)
ETR.List[["ByCSH"]][["ETRweighted"]]$high95 <- ETR.List[["ByCSH"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List[["ByCSH"]][["ETRweighted"]]$n-1) * ETR.List[["ByCSH"]][["ETRweighted"]]$sd /sqrt(ETR.List[["ByCSH"]][["ETRweighted"]]$n)

ETR.List[["ByCSH"]][["ETRweighted"]] <- ETR.List[["ByCSH"]][["ETRweighted"]][!is.na(ETR.List[["ByCSH"]][["ETRweighted"]]$ETR),]


#GUO firms weighted ETR 

ETR.List[["ByGUO"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]]),function(y) sum(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                   "sd" = c(sapply(1:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List[["ByGUO"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                   "n" = c(sapply(1:length(ETR.List[["ByGUO"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["ByGUO"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["ByGUO"]][["ETRweighted"]]$low95 <- ETR.List[["ByGUO"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List[["ByGUO"]][["ETRweighted"]]$n-1) * ETR.List[["ByGUO"]][["ETRweighted"]]$sd /sqrt(ETR.List[["ByGUO"]][["ETRweighted"]]$n)
ETR.List[["ByGUO"]][["ETRweighted"]]$high95 <- ETR.List[["ByGUO"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List[["ByGUO"]][["ETRweighted"]]$n-1) * ETR.List[["ByGUO"]][["ETRweighted"]]$sd /sqrt(ETR.List[["ByGUO"]][["ETRweighted"]]$n)

ETR.List[["ByGUO"]][["ETRweighted"]] <- ETR.List[["ByGUO"]][["ETRweighted"]][!is.na(ETR.List[["ByGUO"]][["ETRweighted"]]$ETR),]


#anyown firms weighted ETR 

ETR.List[["Byanyown"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]]),function(y) sum(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                   "sd" = c(sapply(1:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List[["Byanyown"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                   "n" = c(sapply(1:length(ETR.List[["Byanyown"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["Byanyown"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["Byanyown"]][["ETRweighted"]]$low95 <- ETR.List[["Byanyown"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List[["Byanyown"]][["ETRweighted"]]$n-1) * ETR.List[["Byanyown"]][["ETRweighted"]]$sd /sqrt(ETR.List[["Byanyown"]][["ETRweighted"]]$n)
ETR.List[["Byanyown"]][["ETRweighted"]]$high95 <- ETR.List[["Byanyown"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List[["Byanyown"]][["ETRweighted"]]$n-1) * ETR.List[["Byanyown"]][["ETRweighted"]]$sd /sqrt(ETR.List[["Byanyown"]][["ETRweighted"]]$n)

ETR.List[["Byanyown"]][["ETRweighted"]] <- ETR.List[["Byanyown"]][["ETRweighted"]][!is.na(ETR.List[["Byanyown"]][["ETRweighted"]]$ETR),]



#Loops firms weighted ETR 

ETR.List[["Loop"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["Loop"]][["ETRunweightedTax"]]),function(y) sum(ETR.List[["Loop"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List[["Loop"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(ETR.List[["Loop"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List[["Loop"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List[["Loop"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(ETR.List[["Loop"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["Loop"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["Loop"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["Loop"]][["ETRweighted"]]$low95 <- ETR.List[["Loop"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List[["Loop"]][["ETRweighted"]]$n-1) * ETR.List[["Loop"]][["ETRweighted"]]$sd /sqrt(ETR.List[["Loop"]][["ETRweighted"]]$n)
ETR.List[["Loop"]][["ETRweighted"]]$high95 <- ETR.List[["Loop"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List[["Loop"]][["ETRweighted"]]$n-1) * ETR.List[["Loop"]][["ETRweighted"]]$sd /sqrt(ETR.List[["Loop"]][["ETRweighted"]]$n)

ETR.List[["Loop"]][["ETRweighted"]] <- ETR.List[["Loop"]][["ETRweighted"]][!is.na(ETR.List[["Loop"]][["ETRweighted"]]$ETR),]



#anysub firms weighted ETR 

ETR.List[["Byanysub"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]]),function(y) sum(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                   "sd" = c(sapply(1:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List[["Byanysub"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                   "n" = c(sapply(1:length(ETR.List[["Byanysub"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["Byanysub"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["Byanysub"]][["ETRweighted"]]$low95 <- ETR.List[["Byanysub"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List[["Byanysub"]][["ETRweighted"]]$n-1) * ETR.List[["Byanysub"]][["ETRweighted"]]$sd /sqrt(ETR.List[["Byanysub"]][["ETRweighted"]]$n)
ETR.List[["Byanysub"]][["ETRweighted"]]$high95 <- ETR.List[["Byanysub"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List[["Byanysub"]][["ETRweighted"]]$n-1) * ETR.List[["Byanysub"]][["ETRweighted"]]$sd /sqrt(ETR.List[["Byanysub"]][["ETRweighted"]]$n)

ETR.List[["Byanysub"]][["ETRweighted"]] <- ETR.List[["Byanysub"]][["ETRweighted"]][!is.na(ETR.List[["Byanysub"]][["ETRweighted"]]$ETR),]


#anysubGER firms weighted ETR 

ETR.List[["ByanysubGER"]][["ETRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR" = c(sapply(1:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) sum(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) sqrt(wtd.var(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13], sqrt(ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]),function(y) length(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13][!is.na(as.numeric(ETR.List[["ByanysubGER"]][["ETRunweightedTax"]][[y]][,13]))]))))


ETR.List[["ByanysubGER"]][["ETRweighted"]]$low95 <- ETR.List[["ByanysubGER"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List[["ByanysubGER"]][["ETRweighted"]]$n-1) * ETR.List[["ByanysubGER"]][["ETRweighted"]]$sd /sqrt(ETR.List[["ByanysubGER"]][["ETRweighted"]]$n)
ETR.List[["ByanysubGER"]][["ETRweighted"]]$high95 <- ETR.List[["ByanysubGER"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List[["ByanysubGER"]][["ETRweighted"]]$n-1) * ETR.List[["ByanysubGER"]][["ETRweighted"]]$sd /sqrt(ETR.List[["ByanysubGER"]][["ETRweighted"]]$n)

ETR.List[["ByanysubGER"]][["ETRweighted"]] <- ETR.List[["ByanysubGER"]][["ETRweighted"]][!is.na(ETR.List[["ByanysubGER"]][["ETRweighted"]]$ETR),]

#Affiliates  weighted ETR


ETR.List[["Affiliates"]][["ETRweighted"]] <- data.frame("ISO" = "Affiliates")
ETR.List[["Affiliates"]][["ETRweighted"]]$ETR <- sum(unique(Reduce("rbind", ETR.List[["Byanyown"]][["ETRunweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR.List[["Byanyown"]][["ETRunweightedEBT"]]))[,12], na.rm = TRUE)
ETR.List[["Affiliates"]][["ETRweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR.List[["Byanyown"]][["ETRunweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR.List[["Byanyown"]][["ETRunweightedEBT"]])[,12])^2), na.rm = TRUE ))
ETR.List[["Affiliates"]][["ETRweighted"]]$n <- length(unique(Reduce("rbind", ETR.List[["Byanyown"]][["ETRunweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR.List[["Byanyown"]][["ETRunweightedTax"]]))[,13])])
ETR.List[["Affiliates"]][["ETRweighted"]]$low95 <- ETR.List[["Affiliates"]][["ETRweighted"]]$ETR - qt(0.975, df = ETR.List[["Affiliates"]][["ETRweighted"]]$n-1) * ETR.List[["Affiliates"]][["ETRweighted"]]$sd / sqrt(ETR.List[["Affiliates"]][["ETRweighted"]]$n)
ETR.List[["Affiliates"]][["ETRweighted"]]$high95 <- ETR.List[["Affiliates"]][["ETRweighted"]]$ETR + qt(0.975, df = ETR.List[["Affiliates"]][["ETRweighted"]]$n-1) * ETR.List[["Affiliates"]][["ETRweighted"]]$sd / sqrt(ETR.List[["Affiliates"]][["ETRweighted"]]$n)



#GerGUO weighted ETR


ETR.List[["GerGUO"]][["ETRweighted"]] <- data.frame("ISO" = "GerGUO")
ETR.List[["GerGUO"]][["ETRweighted"]]$ETR <- sum(unique(Reduce("rbind", ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]]))[,12], na.rm = TRUE)
ETR.List[["GerGUO"]][["ETRweighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR.List[["ByanysubGER"]][["ETRunweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR.List[["ByanysubGER"]][["ETRunweightedEBT"]])[,12])^2), na.rm = TRUE ))
ETR.List[["GerGUO"]][["ETRweighted"]]$n <- length(unique(Reduce("rbind", ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR.List[["ByanysubGER"]][["ETRunweightedTax"]]))[,13])])
ETR.List[["GerGUO"]][["ETRweighted"]]$low95 <- ETR.List[["GerGUO"]][["ETRweighted"]]$ETR - qt(0.975, df = ETR.List[["GerGUO"]][["ETRweighted"]]$n-1) * ETR.List[["GerGUO"]][["ETRweighted"]]$sd / sqrt(ETR.List[["GerGUO"]][["ETRweighted"]]$n)
ETR.List[["GerGUO"]][["ETRweighted"]]$high95 <- ETR.List[["GerGUO"]][["ETRweighted"]]$ETR + qt(0.975, df = ETR.List[["GerGUO"]][["ETRweighted"]]$n-1) * ETR.List[["GerGUO"]][["ETRweighted"]]$sd / sqrt(ETR.List[["GerGUO"]][["ETRweighted"]]$n)

ETR.List[["GerGUO"]][["ETRweighted"]] <- ETR.List[["GerGUO"]][["ETRweighted"]][!is.na(ETR.List[["GerGUO"]][["ETRweighted"]]$ETR),]





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown

ETR.List[["Byanyown"]][["ETRunweighted"]] <- rbind(ETR.List[["Byanyown"]][["ETRunweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavens"), "ETR" = c(mean(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                              "sd" = c(sd(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                              "n" = c(length(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))

ETR.List[["Byanyown"]][["ETRunweighted"]] <- rbind(ETR.List[["Byanyown"]][["ETRunweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavensEU"), "ETR" = c(mean(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                              "sd" = c(sd(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                              "n" = c(length(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))


ETR.List[["Byanyown"]][["ETRunweighted"]] <- rbind(ETR.List[["Byanyown"]][["ETRunweighted"]], 
                                                   ETR.List[["DeInt"]][["ETRunweighted"]],
                                                   ETR.List[["DeDom"]][["ETRunweighted"]],
                                                   ETR.List[["Affiliates"]][["ETRunweighted"]],
                                                   ETR.List[["GerGUO"]][["ETRunweighted"]]
)


ETR.List[["Byanyown"]][["ETRunweighted"]]$low95 <- ETR.List[["Byanyown"]][["ETRunweighted"]]$ETR - qt(0.975, df= ETR.List[["Byanyown"]][["ETRunweighted"]]$n-1) * ETR.List[["Byanyown"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["Byanyown"]][["ETRunweighted"]]$n)
ETR.List[["Byanyown"]][["ETRunweighted"]]$high95 <- ETR.List[["Byanyown"]][["ETRunweighted"]]$ETR + qt(0.975, df= ETR.List[["Byanyown"]][["ETRunweighted"]]$n-1) * ETR.List[["Byanyown"]][["ETRunweighted"]]$sd /sqrt(ETR.List[["Byanyown"]][["ETRunweighted"]]$n)





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown


ETR.List[["Byanyown"]][["ETRweighted"]] <- rbind(ETR.List[["Byanyown"]][["ETRweighted"]], 
                                                 
                                                 data.frame("ISO" = c("TaxHavens"), "ETR" = c(sum(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedEBT"]][names(ETR.List[["Byanyown"]][["ETRunweightedEBT"]]) %in% Taxhavens])[,12]), na.rm = TRUE)),
                                                            "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13]), sqrt(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,12])^2),  na.rm = TRUE))),
                                                            "n" = c(length(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                            "low95" = c(NA),
                                                            "high95" = c(NA)
                                                 ))

ETR.List[["Byanyown"]][["ETRweighted"]] <- rbind(ETR.List[["Byanyown"]][["ETRweighted"]], 
                                                 
                                                 data.frame("ISO" = c("TaxHavensEU"), "ETR" = c(sum(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedEBT"]][names(ETR.List[["Byanyown"]][["ETRunweightedEBT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                            "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                            "n" = c(length(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR.List[["Byanyown"]][["ETRunweightedTax"]][names(ETR.List[["Byanyown"]][["ETRunweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                            "low95" = c(NA),
                                                            "high95" = c(NA)
                                                 ))

ETR.List[["Byanyown"]][["ETRweighted"]] <- rbind(ETR.List[["Byanyown"]][["ETRweighted"]], 
                                                 ETR.List[["DeInt"]][["ETRweighted"]],
                                                 ETR.List[["DeDom"]][["ETRweighted"]],
                                                 ETR.List[["Affiliates"]][["ETRweighted"]],
                                                 ETR.List[["GerGUO"]][["ETRweighted"]]
)


ETR.List[["Byanyown"]][["ETRweighted"]]$low95 <- ETR.List[["Byanyown"]][["ETRweighted"]]$ETR - qt(0.975, df= ETR.List[["Byanyown"]][["ETRweighted"]]$n-1) * ETR.List[["Byanyown"]][["ETRweighted"]]$sd /sqrt(ETR.List[["Byanyown"]][["ETRweighted"]]$n)
ETR.List[["Byanyown"]][["ETRweighted"]]$high95 <- ETR.List[["Byanyown"]][["ETRweighted"]]$ETR + qt(0.975, df= ETR.List[["Byanyown"]][["ETRweighted"]]$n-1) * ETR.List[["Byanyown"]][["ETRweighted"]]$sd /sqrt(ETR.List[["Byanyown"]][["ETRweighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, ETR, g, h, i, ISO, j, x, y , z)





