




EBIT <- rio::import("ImportEBIT.xlsx", which = "Results")
EBIT <- cbind(data.frame("CompanyBVDID" = c(EBIT$`BvD ID number`)),EBIT[,4:13])
EBIT <- full_join(data.frame("CompanyBVDID" = EBIT$CompanyBVDID), EBIT, by = "CompanyBVDID")


Tax <- rio::import("ImportTax.xlsx", which = "Results")
Tax <- cbind(data.frame("CompanyBVDID" = c(Tax$`BvD ID number`)),Tax[,4:13])
Tax <- full_join(data.frame("CompanyBVDID" = EBIT$CompanyBVDID), Tax, by = "CompanyBVDID")

InterestPaid <- rio::import("ImportIP.xlsx", which = "Results")
InterestPaid <- cbind(data.frame("CompanyBVDID" = c(InterestPaid$`BvD ID number`)),InterestPaid[,5:13])
InterestPaid$DUMMY <- NA
InterestPaid <- full_join(data.frame("CompanyBVDID" = EBIT$CompanyBVDID), InterestPaid, by = "CompanyBVDID")




EBITAIP <- EBIT

for (i in 1:nrow(EBITAIP)) {
  for (j in 2:ncol(EBITAIP)) {
    
    EBITAIP[i,j] <- ifelse(!is.na(as.numeric(EBIT[i,j])) & !is.na(as.numeric(InterestPaid[i,j])), as.numeric(EBIT[i,j]) - as.numeric(InterestPaid[i,j]), NA  )  
    
  }
}





##equal out samples

for(i in 1:nrow(EBITAIP)) {
  for (j in 2:ncol(EBITAIP)) {
    EBITAIP[i,j] <- ifelse(!is.na(as.numeric(EBITAIP[i,j])) & !is.na(as.numeric(Tax[i,j])),  as.numeric(EBITAIP[i,j]) , NA )
  }
}


for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(EBITAIP[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}



## Drop company if losses in 2010

EBITAIP <- EBITAIP[EBITAIP[,11] > 0 | is.na(EBITAIP[,11]),]
EBITAIP <- EBITAIP[!is.na(EBITAIP$CompanyBVDID),]
Tax <- Tax[Tax$CompanyBVDID %in% EBITAIP$CompanyBVDID,]


## Keep 2010 - 2019

EBITAIP[,11] <- NA
Tax[,11] <- NA


EBITAIP[,2] <- NA
Tax[,2] <- NA


## Drop last year if negative profits


EBITAIP[,3][EBITAIP[,3] < 0] <- NA



for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(EBITAIP[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}


## Drop if <3 obs


EBITAIP <- EBITAIP[apply(EBITAIP,1,function (z) length(z[!is.na(as.numeric(z))]) > 2),]
Tax <- Tax[Tax$CompanyBVDID %in% EBITAIP$CompanyBVDID,]








ETR4.List.Pos <- vector(mode = "list")
ETR4.List.Pos[[1]] <- vector(mode = "list")
names(ETR4.List.Pos) <- "ByCSH"


#Domestic firms unweightet ETR4


ETR4.List.Pos[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR4.List.Pos[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(ETR4.List.Pos[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(ETR4.List.Pos[["DeDom"]][["CompanyList"]])) {
  ETR4.List.Pos[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR4.List.Pos[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR4.List.Pos[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR4.List.Pos[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR4.List.Pos[["DeDom"]][["CompanyList"]][[i]])))
}

ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]] <- subset(EBITAIP, EBITAIP$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]])) {
  for (j in 2:ncol(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]])) {
    ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][i,j])) & ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][i,1] %in% ETR4.List.Pos[["DeDom"]][["CompanyList"]][[(j-1)]], ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][i,j], NA)
  }}



for (i in 1:length(ETR4.List.Pos[["DeDom"]][["CompanyList"]])) {
  ETR4.List.Pos[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR4.List.Pos[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR4.List.Pos[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR4.List.Pos[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR4.List.Pos[["DeDom"]][["CompanyList"]][[i]])))
}


ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]])) {
  for (j in 2:ncol(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]])) {
    ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][i,j])) & ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][i,1] %in% ETR4.List.Pos[["DeDom"]][["CompanyList"]][[(j-1)]], ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]])) {
  for(j in 2:ncol(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]])) {
    ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][i,j])) ,  as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]])) {
  for(j in 2:ncol(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]])) {
    ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][i,j])) ,  as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][i,j])  , NA  )
  }}



ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]]$sum <- sapply(1:nrow(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]]), function (y) sum(as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][y,2:11]) , na.rm = TRUE ))
ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]]$sum[ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]]$sum == 0] <- NA
ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]]$sum <- sapply(1:nrow(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]]), function (y) sum(as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][y,2:11]), na.rm = TRUE))
ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]]$sum[ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]]$sum == 0] <- NA
ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]]$ETR4 <- sapply(1:nrow(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]]), function (y) ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][[y,12]] / ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][[y,12]])

ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]] <- ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][!ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]]$sum < 0,]
ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]] <- ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]][!ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]]$sum < 0,]



ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                           "ETR4" = mean(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][,13], na.rm = TRUE), 
                                                           "sd" = sd(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][,13], na.rm = TRUE),
                                                           "n" = length(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][,13][!is.na(as.numeric(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]][,13]))]))

ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$n)

ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]] <- ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]][!is.na(ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]]$ETR4),]


#International firms unweightet ETR4


ETR4.List.Pos[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR4.List.Pos[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(ETR4.List.Pos[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(ETR4.List.Pos[["DeInt"]][["CompanyList"]])) {
  ETR4.List.Pos[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR4.List.Pos[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR4.List.Pos[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR4.List.Pos[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR4.List.Pos[["DeInt"]][["CompanyList"]][[i]])))
}

ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]] <- subset(EBITAIP, EBITAIP$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]])) {
  for (j in 2:ncol(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]])) {
    ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][i,j])) & ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][i,1] %in% ETR4.List.Pos[["DeInt"]][["CompanyList"]][[(j-1)]], ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][i,j], NA)
  }}



for (i in 1:length(ETR4.List.Pos[["DeInt"]][["CompanyList"]])) {
  ETR4.List.Pos[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR4.List.Pos[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR4.List.Pos[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR4.List.Pos[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR4.List.Pos[["DeInt"]][["CompanyList"]][[i]])))
}


ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]])) {
  for (j in 2:ncol(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]])) {
    ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][i,j])) & ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][i,1] %in% ETR4.List.Pos[["DeInt"]][["CompanyList"]][[(j-1)]], ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]])) {
  for(j in 2:ncol(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]])) {
    ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][i,j])) ,  as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]])) {
  for(j in 2:ncol(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]])) {
    ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][i,j])) ,  as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][i,j])  , NA  )
  }}


ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]]$sum <- sapply(1:nrow(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]]), function (y) sum(as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][y,2:11]) , na.rm = TRUE ))
ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]]$sum[ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]]$sum == 0] <- NA
ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]]$sum <- sapply(1:nrow(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]]), function (y) sum(as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][y,2:11]), na.rm = TRUE))
ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]]$sum[ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]]$sum == 0] <- NA
ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]]$ETR4 <- sapply(1:nrow(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]]), function (y) ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][[y,12]] / ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][[y,12]])

ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]] <- ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][!ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]]$sum < 0,]
ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]] <- ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]][!ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]]$sum < 0,]


ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]] <- data.frame("ISO" = "DeInt", 
                                                           "ETR4" = mean(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][,13], na.rm = TRUE), 
                                                           "sd" = sd(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][,13], na.rm = TRUE),
                                                           "n" = length(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][,13][!is.na(as.numeric(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]][,13]))]))

ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$n)

ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]] <- ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]][!is.na(ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]]$ETR4),]



#CSH unweighted ETR4


ETR4.List.Pos[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(ETR4.List.Pos[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR4.List.Pos[["ByCSH"]][["CompanyList"]])) {ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR4.List.Pos[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[i]])) {
    ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]] <- ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]])) {
  Temp1 <- ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[i]] <- subset(EBITAIP, EBITAIP$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[i]]) == 0 ) {ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][i,1] %in% ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][i,j], NA)
    }}}



ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]])) {
  Temp1 <- ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[i]]) == 0 ) {ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][i,j])) & ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][i,1] %in% ETR4.List.Pos[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]])) {
  ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]]$sum[ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]]$sum[ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]]$ETR4 <- sapply(1:nrow(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]]), function (y) ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][[y,12]] / ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][y,12])
  
  ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]] <- ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[x]][!ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]] <- ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]][!ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  
}



ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]]),function(y) mean(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                           "sd" = c(sapply(1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]]),function(y) sd(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                           "n" = c(sapply(1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$n)

ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]] <- ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]][!is.na(ETR4.List.Pos[["ByCSH"]][["ETR4unweighted"]]$ETR4),]



#GUO unweighted ETR4


ETR4.List.Pos[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(ETR4.List.Pos[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR4.List.Pos[["ByGUO"]][["CompanyList"]])) {ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(ETR4.List.Pos[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[i]])) {
    ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]] <- ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]])) {
  Temp1 <- ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[i]] <- subset(EBITAIP, EBITAIP$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[i]]) == 0 ) {ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][i,1] %in% ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][i,j], NA)
    }}}



ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]])) {
  Temp1 <- ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[i]]) == 0 ) {ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][i,j])) & ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][i,1] %in% ETR4.List.Pos[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]])) {
  ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]]$sum[ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]]$sum[ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]]$ETR4 <- sapply(1:nrow(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]]), function (y) ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][[y,12]] / ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][y,12])
  
  ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]] <- ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[x]][!ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]] <- ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]][!ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  
}


ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]]),function(y) mean(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                           "sd" = c(sapply(1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]]),function(y) sd(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                           "n" = c(sapply(1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$n)

ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]] <- ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]][!is.na(ETR4.List.Pos[["ByGUO"]][["ETR4unweighted"]]$ETR4),]


#anyown unweighted ETR4


ETR4.List.Pos[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR4.List.Pos[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR4.List.Pos[["Byanyown"]][["CompanyList"]])) {ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR4.List.Pos[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]] <- ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]])) {
  Temp1 <- ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[i]] <- subset(EBITAIP, EBITAIP$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[i]]) == 0 ) {ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][i,1] %in% ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][i,j], NA)
    }}}



ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]])) {
  Temp1 <- ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[i]]) == 0 ) {ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][i,j])) & ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][i,1] %in% ETR4.List.Pos[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]])) {
  ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]]$sum[ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]]$sum[ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]]$ETR4 <- sapply(1:nrow(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]]), function (y) ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][[y,12]] / ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][y,12])
  
  ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]] <- ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[x]][!ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]] <- ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]][!ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  
}


ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]),function(y) mean(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                              "sd" = c(sapply(1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]),function(y) sd(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                              "n" = c(sapply(1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$n)

ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]] <- ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]][!is.na(ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$ETR4),]




#intermed unweighted ETR


ETR4.List.Pos[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(ETR4.List.Pos[["Byintermed"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR4.List.Pos[["Byintermed"]][["CompanyList"]])) {ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR4.List.Pos[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]] <- ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]])) {
  Temp1 <- ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[i]] <- subset(EBITAIP, EBITAIP$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[i]]) == 0 ) {ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][i,1] %in% ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][i,j], NA)
    }}}



ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]])) {
  Temp1 <- ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[i]]) == 0 ) {ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][i,j])) & ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][i,1] %in% ETR4.List.Pos[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]])) {
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]]$sum[ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]]$sum[ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]]$ETR4 <- sapply(1:nrow(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]]), function (y) ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][[y,12]] / ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][y,12])
  
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]] <- ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][!ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]]$ETR4 > 1,]
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]] <- ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][!ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]]$ETR4 > 1,]
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]] <- ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[x]][!ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]] <- ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]][!ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  
}


ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]),function(y) mean(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                                "sd" = c(sapply(1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]),function(y) sd(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                                "n" = c(sapply(1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$n)

ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]] <- ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]][!is.na(ETR4.List.Pos[["Byintermed"]][["ETR4unweighted"]]$ETR4),]



#ETR4 unweighted Loops


ETR4.List.Pos[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR4.List.Pos[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR4.List.Pos[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(ETR4.List.Pos[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(ETR4.List.Pos[["Loop"]][["CompanyList"]])) {ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR4.List.Pos[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR4.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]])) {
  Temp1 <- ETR4.List.Pos[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[i]] <- subset(EBITAIP, EBITAIP$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[i]]) == 0 ) {ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][i,1] %in% ETR4.List.Pos[["Loop"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][i,j], NA)
    }}}



ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]])) {
  Temp1 <- ETR4.List.Pos[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[i]]) == 0 ) {ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][i,j])) & ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][i,1] %in% ETR4.List.Pos[["Loop"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]])) {
  ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]]$sum[ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]]$sum[ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]]$ETR4 <- sapply(1:nrow(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]]), function (y) ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][[y,12]] / ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][y,12])
  
  ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]] <- ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[x]][!ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]] <- ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]][!ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  
}


ETR4.List.Pos[["Loop"]][["ETR4unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]]),function(y) mean(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                          "sd" = c(sapply(1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]]),function(y) sd(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                          "n" = c(sapply(1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$n)

ETR4.List.Pos[["Loop"]][["ETR4unweighted"]] <- ETR4.List.Pos[["Loop"]][["ETR4unweighted"]][!is.na(ETR4.List.Pos[["Loop"]][["ETR4unweighted"]]$ETR4),]


#anysub unweighted ETR4


ETR4.List.Pos[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR4.List.Pos[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR4.List.Pos[["Byanysub"]][["CompanyList"]])) {ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR4.List.Pos[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] <- ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]])) {
  Temp1 <- ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[i]] <- subset(EBITAIP, EBITAIP$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[i]]) == 0 ) {ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][i,1] %in% ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][i,j], NA)
    }}}



ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]])) {
  Temp1 <- ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[i]]) == 0 ) {ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][i,j])) & ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][i,1] %in% ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]])) {
  ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]]$sum[ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]]$sum[ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]]$ETR4 <- sapply(1:nrow(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]]), function (y) ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][[y,12]] / ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][y,12])
  
  ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]] <- ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[x]][!ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]] <- ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]][!ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  
}

ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]]),function(y) mean(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                              "sd" = c(sapply(1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]]),function(y) sd(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                              "n" = c(sapply(1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$n)

ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]] <- ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]][!is.na(ETR4.List.Pos[["Byanysub"]][["ETR4unweighted"]]$ETR4),]


#anysubGER unweighted ETR4


ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]])) {ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(ETR4.List.Pos[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]])) {
  Temp1 <- ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[i]] <- subset(EBITAIP, EBITAIP$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[i]]) == 0 ) {ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][i,1] %in% ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][i,j], NA)
    }}}



ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]])) {
  Temp1 <- ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[i]]) == 0 ) {ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]])) {
  for (i in 1:nrow(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][i,j])) & ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][i,1] %in% ETR4.List.Pos[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]])) {
      ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]])) {
  for(i in 1:nrow(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]])) {
      ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][i,j])) & !is.na(as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]])) {
  ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]]$sum[ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]]$sum[ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]]$sum == 0] <- NA
  ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]]$ETR4 <- sapply(1:nrow(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]]), function (y) ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][[y,12]] / ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][y,12])
  
  ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]] <- ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[x]][!ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]] <- ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]][!ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[x]]$sum < 0,]
  
}


ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]),function(y) mean(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                                 "sd" = c(sapply(1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]),function(y) sd(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                                 "n" = c(sapply(1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$n)

ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]] <- ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]][!is.na(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweighted"]]$ETR4),]


#Affiliates  unweighted ETR4


ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                                "ETR4" = mean(unique(Reduce("rbind", ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]))[,13], na.rm = TRUE), 
                                                                "sd" = sd(unique(Reduce("rbind", ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]))[,13], na.rm = TRUE),
                                                                "n" = length(unique(Reduce("rbind", ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]))[,13])]))




ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]]$n)




#GerGUO unweighted ETR4


ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                            "ETR4" = mean(unique(Reduce("rbind", ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]))[,13], na.rm = TRUE), 
                                                            "sd" = sd(unique(Reduce("rbind", ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]))[,13], na.rm = TRUE),
                                                            "n" = length(unique(Reduce("rbind", ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]))[,13])]))




ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$n)

ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]] <- ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]][!is.na(ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]$ETR4),]




#Domestic firms weighted ETR4


ETR4.List.Pos[["DeDom"]][["ETR4weighted"]] <- data.frame("ISO" = "DEDOM")
ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$ETR4 <- sum(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]]$sum, na.rm = TRUE)
ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$sd <- sqrt(wtd.var(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]]$ETR4, sqrt(ETR4.List.Pos[["DeDom"]][["ETR4unweightedEBITAIP"]]$sum^2), na.rm = TRUE ))
ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$n <- length(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]]$ETR4[!is.na(ETR4.List.Pos[["DeDom"]][["ETR4unweightedTax"]]$ETR4)])
ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$ETR4 - qt(0.975, df = ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$sd / sqrt(ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$ETR4 + qt(0.975, df = ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$sd / sqrt(ETR4.List.Pos[["DeDom"]][["ETR4weighted"]]$n)


#International firms weighted ETR4


ETR4.List.Pos[["DeInt"]][["ETR4weighted"]] <- data.frame("ISO" = "DEINT")
ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$ETR4 <- sum(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]]$sum, na.rm = TRUE)
ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$sd <- sqrt(wtd.var(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]]$ETR4, sqrt(ETR4.List.Pos[["DeInt"]][["ETR4unweightedEBITAIP"]]$sum^2), na.rm = TRUE ))
ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$n <- length(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]]$ETR4[!is.na(ETR4.List.Pos[["DeInt"]][["ETR4unweightedTax"]]$ETR4)])
ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$ETR4 - qt(0.975, df = ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$sd / sqrt(ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$ETR4 + qt(0.975, df = ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$sd / sqrt(ETR4.List.Pos[["DeInt"]][["ETR4weighted"]]$n)


#CSH firms weighted ETR4 

ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]]),function(y) sum(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[y]][,12], na.rm = TRUE ))), 
                                                         "sd" = c(sapply(1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]]),function(y) sqrt(wtd.var(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[y]][,13], sqrt(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedEBITAIP"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                         "n" = c(sapply(1:length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["ByCSH"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$n)

ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]] <- ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]][!is.na(ETR4.List.Pos[["ByCSH"]][["ETR4weighted"]]$ETR4),]


#GUO firms weighted ETR4 

ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]]),function(y) sum(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[y]][,12], na.rm = TRUE ))), 
                                                         "sd" = c(sapply(1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]]),function(y) sqrt(wtd.var(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[y]][,13], sqrt(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedEBITAIP"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                         "n" = c(sapply(1:length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["ByGUO"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$n)

ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]] <- ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]][!is.na(ETR4.List.Pos[["ByGUO"]][["ETR4weighted"]]$ETR4),]


#anyown firms weighted ETR4 

ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]),function(y) sum(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[y]][,12], na.rm = TRUE ))), 
                                                            "sd" = c(sapply(1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]),function(y) sqrt(wtd.var(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[y]][,13], sqrt(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                            "n" = c(sapply(1:length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$n)

ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]] <- ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]][!is.na(ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$ETR4),]



#intermed firms weighted ETR4 

ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]),function(y) sum(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[y]][,12], na.rm = TRUE ))), 
                                                              "sd" = c(sapply(1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]),function(y) sqrt(wtd.var(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[y]][,13], sqrt(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                              "n" = c(sapply(1:length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$n)

ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]] <- ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]][!is.na(ETR4.List.Pos[["Byintermed"]][["ETR4weighted"]]$ETR4),]





#Loops firms weighted ETR4 

ETR4.List.Pos[["Loop"]][["ETR4weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]]),function(y) sum(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[y]][,12], na.rm = TRUE ))), 
                                                        "sd" = c(sapply(1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]]),function(y) sqrt(wtd.var(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[y]][,13], sqrt(ETR4.List.Pos[["Loop"]][["ETR4unweightedEBITAIP"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                        "n" = c(sapply(1:length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["Loop"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$n)

ETR4.List.Pos[["Loop"]][["ETR4weighted"]] <- ETR4.List.Pos[["Loop"]][["ETR4weighted"]][!is.na(ETR4.List.Pos[["Loop"]][["ETR4weighted"]]$ETR4),]



#anysub firms weighted ETR4 

ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]]),function(y) sum(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[y]][,12], na.rm = TRUE ))), 
                                                            "sd" = c(sapply(1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]]),function(y) sqrt(wtd.var(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[y]][,13], sqrt(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedEBITAIP"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                            "n" = c(sapply(1:length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["Byanysub"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$n)

ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]] <- ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]][!is.na(ETR4.List.Pos[["Byanysub"]][["ETR4weighted"]]$ETR4),]


#anysubGER firms weighted ETR4 

ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR4" = c(sapply(1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]),function(y) sum(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[y]][,12], na.rm = TRUE ))), 
                                                               "sd" = c(sapply(1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]),function(y) sqrt(wtd.var(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[y]][,13], sqrt(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                               "n" = c(sapply(1:length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]),function(y) length(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]][[y]][,13]))]))))


ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$n)

ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]] <- ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]][!is.na(ETR4.List.Pos[["ByanysubGER"]][["ETR4weighted"]]$ETR4),]


#Affiliates  weighted ETR4


ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]] <- data.frame("ISO" = "Affiliates")
ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$ETR4 <- sum(unique(Reduce("rbind", ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]]))[,12], na.rm = TRUE)
ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]])[,12])^2), na.rm = TRUE ))
ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$n <- length(unique(Reduce("rbind", ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]))[,13])])
ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$ETR4 - qt(0.975, df = ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$sd / sqrt(ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$ETR4 + qt(0.975, df = ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$sd / sqrt(ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]]$n)



#GerGUO weighted ETR4


ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]] <- data.frame("ISO" = "GerGUO")
ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$ETR4 <- sum(unique(Reduce("rbind", ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]]))[,12], na.rm = TRUE)
ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedEBITAIP"]])[,12])^2), na.rm = TRUE ))
ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$n <- length(unique(Reduce("rbind", ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR4.List.Pos[["ByanysubGER"]][["ETR4unweightedTax"]]))[,13])])
ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$ETR4 - qt(0.975, df = ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$sd / sqrt(ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$ETR4 + qt(0.975, df = ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$sd / sqrt(ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$n)

ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]] <- ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]][!is.na(ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]$ETR4),]





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown

ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]] <- rbind(ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]], 
                                                         
                                                         data.frame("ISO" = c("TaxHavens"), "ETR4" = c(mean(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                    "sd" = c(sd(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                    "n" = c(length(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                                    "low95" = c(NA),
                                                                    "high95" = c(NA)
                                                         ))

ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]] <- rbind(ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]], 
                                                         
                                                         data.frame("ISO" = c("TaxHavensEU"), "ETR4" = c(mean(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                    "sd" = c(sd(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                    "n" = c(length(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                    "low95" = c(NA),
                                                                    "high95" = c(NA)
                                                         ))

ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]] <- rbind(ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]], 
                                                         ETR4.List.Pos[["DeInt"]][["ETR4unweighted"]],
                                                         ETR4.List.Pos[["DeDom"]][["ETR4unweighted"]],
                                                         ETR4.List.Pos[["Affiliates"]][["ETR4unweighted"]],
                                                         ETR4.List.Pos[["GerGUO"]][["ETR4unweighted"]]
)

ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]] <- rbind(ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]], 
                                                         
                                                         data.frame("ISO" = c("TaxHavensEUProxy"), "ETR4" = c(mean(unique(Reduce("rbind",ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                    "sd" = c(sd(unique(Reduce("rbind",ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                    "n" = c(length(unique(Reduce("rbind",ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                    "low95" = c(NA),
                                                                    "high95" = c(NA)
                                                         ))


ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$low95 <- ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$n)
ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$high95 <- ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$n-1) * ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$sd /sqrt(ETR4.List.Pos[["Byanyown"]][["ETR4unweighted"]]$n)





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown


ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]] <- rbind(ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavens"), "ETR4" = c(sum(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% Taxhavens])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]]) %in% Taxhavens])[,12]), na.rm = TRUE)),
                                                                  "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% Taxhavens])[,13]), sqrt(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% Taxhavens])[,12])^2),  na.rm = TRUE))),
                                                                  "n" = c(length(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))

ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]] <- rbind(ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEU"), "ETR4" = c(sum(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedEBITAIP"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                  "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                  "n" = c(length(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byanyown"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))

ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]] <- rbind(ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]], 
                                                       ETR4.List.Pos[["DeInt"]][["ETR4weighted"]],
                                                       ETR4.List.Pos[["DeDom"]][["ETR4weighted"]],
                                                       ETR4.List.Pos[["Affiliates"]][["ETR4weighted"]],
                                                       ETR4.List.Pos[["GerGUO"]][["ETR4weighted"]]
)


ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]] <- rbind(ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEUProxy"), "ETR4" = c(sum(unique(Reduce("rbind",ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]][names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedEBITAIP"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                  "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                  "n" = c(length(unique(Reduce("rbind",ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]][names(ETR4.List.Pos[["Byintermed"]][["ETR4unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))


ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$low95 <- ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$ETR4 - qt(0.975, df= ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$n)
ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$high95 <- ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$ETR4 + qt(0.975, df= ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$n-1) * ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$sd /sqrt(ETR4.List.Pos[["Byanyown"]][["ETR4weighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, ETR4, g, h, i, ISO, j, x, y , z)






