



EBIT <- rio::import("ImportEBIT.xlsx", which = "Results")
EBIT <- cbind(data.frame("CompanyBVDID" = c(EBIT$`BvD ID number`)),EBIT[,4:13])
EBIT <- full_join(data.frame("CompanyBVDID" = EBIT$CompanyBVDID), EBIT, by = "CompanyBVDID")


Tax <- rio::import("ImportTax.xlsx", which = "Results")
Tax <- cbind(data.frame("CompanyBVDID" = c(Tax$`BvD ID number`)),Tax[,4:13])
Tax <- full_join(data.frame("CompanyBVDID" = EBIT$CompanyBVDID), Tax, by = "CompanyBVDID")



##equal out samples

for(i in 1:nrow(EBIT)) {
  for (j in 2:ncol(EBIT)) {
    EBIT[i,j] <- ifelse(!is.na(as.numeric(EBIT[i,j])) & !is.na(as.numeric(Tax[i,j])),  as.numeric(EBIT[i,j]) , NA )
  }
}


for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(EBIT[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}



## Drop company if losses in 2010

EBIT <- EBIT[EBIT[,11] > 0 | is.na(EBIT[,11]),]
EBIT <- EBIT[!is.na(EBIT$CompanyBVDID),]
Tax <- Tax[Tax$CompanyBVDID %in% EBIT$CompanyBVDID,]


## Keep 2010 - 2019

EBIT[,11] <- NA
Tax[,11] <- NA


EBIT[,2] <- NA
Tax[,2] <- NA


## Drop last year if negative profits


EBIT[,3][EBIT[,3] < 0] <- NA



for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(EBIT[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}


## Drop if <3 obs


EBIT <- EBIT[apply(EBIT,1,function (z) length(z[!is.na(as.numeric(z))]) > 2),]
Tax <- Tax[Tax$CompanyBVDID %in% EBIT$CompanyBVDID,]








ETR3.List.Pos <- vector(mode = "list")
ETR3.List.Pos[[1]] <- vector(mode = "list")
names(ETR3.List.Pos) <- "ByCSH"


#Domestic firms unweightet ETR3


ETR3.List.Pos[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR3.List.Pos[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(ETR3.List.Pos[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(ETR3.List.Pos[["DeDom"]][["CompanyList"]])) {
  ETR3.List.Pos[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR3.List.Pos[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR3.List.Pos[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR3.List.Pos[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR3.List.Pos[["DeDom"]][["CompanyList"]][[i]])))
}

ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]])) {
  for (j in 2:ncol(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]])) {
    ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][i,j])) & ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][i,1] %in% ETR3.List.Pos[["DeDom"]][["CompanyList"]][[(j-1)]], ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][i,j], NA)
  }}



for (i in 1:length(ETR3.List.Pos[["DeDom"]][["CompanyList"]])) {
  ETR3.List.Pos[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR3.List.Pos[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR3.List.Pos[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(ETR3.List.Pos[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR3.List.Pos[["DeDom"]][["CompanyList"]][[i]])))
}


ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]])) {
  for (j in 2:ncol(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]])) {
    ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][i,j])) & ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][i,1] %in% ETR3.List.Pos[["DeDom"]][["CompanyList"]][[(j-1)]], ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]])) {
  for(j in 2:ncol(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]])) {
    ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][i,j])) ,  as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]])) {
  for(j in 2:ncol(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]])) {
    ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][i,j])) ,  as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][i,j])  , NA  )
  }}



ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]]$sum <- sapply(1:nrow(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]]), function (y) sum(as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][y,2:11]) , na.rm = TRUE ))
ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]]$sum[ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]]$sum == 0] <- NA
ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]$sum <- sapply(1:nrow(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]), function (y) sum(as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][y,2:11]), na.rm = TRUE))
ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]$sum[ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]$sum == 0] <- NA
ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]$ETR3 <- sapply(1:nrow(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]), function (y) ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][[y,12]] / ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][[y,12]])

ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]] <- ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][!ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]$ETR3 > 1,]
ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]] <- ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][!ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]$ETR3 > 1,]
ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]] <- ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][!ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]]$sum < 0,]
ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]] <- ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]][!ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]]$sum < 0,]



ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                         "ETR3" = mean(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][,13], na.rm = TRUE),
                                                         "n" = length(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][,13][!is.na(as.numeric(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]][,13]))]))

ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$n)

ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]] <- ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]][!is.na(ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]]$ETR3),]


#International firms unweightet ETR3


ETR3.List.Pos[["DeInt"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR3.List.Pos[["DeInt"]][["CompanyList"]][[(i-1)]] <- EdgelistInt[[i]][sapply(EdgelistInt[[i]], function (y) Nodelist.List[[(i+1)]]$CompanyISO[match(y, Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE")]}
names(ETR3.List.Pos[["DeInt"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(ETR3.List.Pos[["DeInt"]][["CompanyList"]])) {
  ETR3.List.Pos[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR3.List.Pos[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR3.List.Pos[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR3.List.Pos[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR3.List.Pos[["DeInt"]][["CompanyList"]][[i]])))
}

ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)

for (i in 1:nrow(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]])) {
  for (j in 2:ncol(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]])) {
    ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][i,j])) & ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][i,1] %in% ETR3.List.Pos[["DeInt"]][["CompanyList"]][[(j-1)]], ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][i,j], NA)
  }}



for (i in 1:length(ETR3.List.Pos[["DeInt"]][["CompanyList"]])) {
  ETR3.List.Pos[["DeInt"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(ETR3.List.Pos[["DeInt"]][["CompanyList"]][[i]]))))
}

Temp1 <- ETR3.List.Pos[["DeInt"]][["CompanyList"]][[1]]
for(i in 2:length(ETR3.List.Pos[["DeInt"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(ETR3.List.Pos[["DeInt"]][["CompanyList"]][[i]])))
}


ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)  

for (i in 1:nrow(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]])) {
  for (j in 2:ncol(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]])) {
    ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][i,j])) & ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][i,1] %in% ETR3.List.Pos[["DeInt"]][["CompanyList"]][[(j-1)]], ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][i,j], NA)
  }}


for(i in 1:nrow(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]])) {
  for(j in 2:ncol(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]])) {
    ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][i,j])) ,  as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][i,j])  , NA  )
  }}


for(i in 1:nrow(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]])) {
  for(j in 2:ncol(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]])) {
    ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][i,j])) ,  as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][i,j])  , NA  )
  }}


ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]]$sum <- sapply(1:nrow(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]]), function (y) sum(as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][y,2:11]) , na.rm = TRUE ))
ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]]$sum[ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]]$sum == 0] <- NA
ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]$sum <- sapply(1:nrow(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]), function (y) sum(as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][y,2:11]), na.rm = TRUE))
ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]$sum[ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]$sum == 0] <- NA
ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]$ETR3 <- sapply(1:nrow(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]), function (y) ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][[y,12]] / ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][[y,12]])

ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]] <- ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][!ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]$ETR3 > 1,]
ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]] <- ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][!ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]$ETR3 > 1,]
ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]] <- ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][!ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]]$sum < 0,]
ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]] <- ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]][!ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]]$sum < 0,]


ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]] <- data.frame("ISO" = "DeInt", 
                                                         "ETR3" = mean(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][,13], na.rm = TRUE), 
                                                         "sd" = sd(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][,13], na.rm = TRUE),
                                                         "n" = length(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][,13][!is.na(as.numeric(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]][,13]))]))

ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$n)

ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]] <- ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]][!is.na(ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]]$ETR3),]



#CSH unweighted ETR3


ETR3.List.Pos[["ByCSH"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[(i-1)]] <- EdgelistByCSH[[i]]}
names(ETR3.List.Pos[["ByCSH"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR3.List.Pos[["ByCSH"]][["CompanyList"]])) {ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR3.List.Pos[["ByCSH"]][["CompanyList"]])) {
  for (j in 1:length(ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[i]])) {
    ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]] <- ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[i]][[j]][,(y-1)], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[i]][j])))]
  }}

ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"][na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"] != "DE"]

for (i in 1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]])) {
  Temp1 <- ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[i]]) == 0 ) {ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][i,j])) & ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][i,1] %in% ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]])) {
  Temp1 <- ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[i]]) == 0 ) {ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][i,j])) & ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][i,1] %in% ETR3.List.Pos[["ByCSH"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]])) {
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]]$sum[ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]]$sum[ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]]$ETR3 <- sapply(1:nrow(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]]), function (y) ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][[y,12]] / ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][y,12])
  
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  
}



ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]]),function(y) mean(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]]),function(y) sd(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$n)

ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]] <- ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]][!is.na(ETR3.List.Pos[["ByCSH"]][["ETR3unweighted"]]$ETR3),]



#GUO unweighted ETR3


ETR3.List.Pos[["ByGUO"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[(i-1)]] <- EdgelistByGUO[[i]]}
names(ETR3.List.Pos[["ByGUO"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR3.List.Pos[["ByGUO"]][["CompanyList"]])) {ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[i]][["DE"]] <- NULL}



for (i in 1:length(ETR3.List.Pos[["ByGUO"]][["CompanyList"]])) {
  for (j in 1:length(ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[i]])) {
    ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]] <- ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][cbind(c(FALSE), sapply(2:ncol(ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]]), function (y) Nodelist.List[[i+1]]$CompanyISO[match(ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][,y], Nodelist.List[[i+1]]$CompanyBvDID)] == "DE" & Nodelist.List[[i+1]]$CompanyISO[match(ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[i]][j])))]
  }}

ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]])) {
  Temp1 <- ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[i]]) == 0 ) {ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][i,j])) & ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][i,1] %in% ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]])) {
  Temp1 <- ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[i]]) == 0 ) {ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][i,j])) & ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][i,1] %in% ETR3.List.Pos[["ByGUO"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]])) {
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]]$sum[ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]]$sum[ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]]$ETR3 <- sapply(1:nrow(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]]), function (y) ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][[y,12]] / ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][y,12])
  
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  
}


ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]]),function(y) mean(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                         "sd" = c(sapply(1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]]),function(y) sd(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                         "n" = c(sapply(1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$n)

ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]] <- ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]][!is.na(ETR3.List.Pos[["ByGUO"]][["ETR3unweighted"]]$ETR3),]


#anyown unweighted ETR3


ETR3.List.Pos[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR3.List.Pos[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR3.List.Pos[["Byanyown"]][["CompanyList"]])) {ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR3.List.Pos[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]] <- ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]])) {
  Temp1 <- ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[i]]) == 0 ) {ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][i,j])) & ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][i,1] %in% ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]])) {
  Temp1 <- ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[i]]) == 0 ) {ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][i,j])) & ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][i,1] %in% ETR3.List.Pos[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]])) {
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]]$sum[ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]]$sum[ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]]$ETR3 <- sapply(1:nrow(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]]), function (y) ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][[y,12]] / ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][y,12])
  
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  
}


ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]),function(y) mean(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]),function(y) sd(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$n)

ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]] <- ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][!is.na(ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ETR3),]




#intermed unweighted ETR3


ETR3.List.Pos[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(ETR3.List.Pos[["Byintermed"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR3.List.Pos[["Byintermed"]][["CompanyList"]])) {ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR3.List.Pos[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]] <- ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]])) {
  Temp1 <- ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[i]]) == 0 ) {ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][i,j])) & ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][i,1] %in% ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]])) {
  Temp1 <- ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[i]]) == 0 ) {ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][i,j])) & ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][i,1] %in% ETR3.List.Pos[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]])) {
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]]$sum[ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]]$sum[ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]]$ETR3 <- sapply(1:nrow(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]]), function (y) ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][[y,12]] / ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][y,12])
  
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  
}


ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]),function(y) mean(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                              "sd" = c(sapply(1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]),function(y) sd(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                              "n" = c(sapply(1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$n)

ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]] <- ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]][!is.na(ETR3.List.Pos[["Byintermed"]][["ETR3unweighted"]]$ETR3),]





#ETR3 unweighted Loops


ETR3.List.Pos[["Loop"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR3.List.Pos[["Loop"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(ETR3.List.Pos[["Loop"]][["CompanyList"]]) <- paste(2020:2010)


for(i in 1:length(ETR3.List.Pos[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    
    ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][ Nodelist.List[[(i+1)]]$CompanyISO[match(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][,1], Nodelist.List[[(i+1)]]$CompanyBvDID)] == "DE",]
    
  }
}


for(i in 1:length(ETR3.List.Pos[["Loop"]][["CompanyList"]])) {ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR3.List.Pos[["Loop"]][["CompanyList"]])) {
  for (j in 1:length(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]]) < 20) {ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- rbind(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]] <- ETR3.List.Pos[["Loop"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]])) {
  Temp1 <- ETR3.List.Pos[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[i]]) == 0 ) {ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][i,j])) & ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][i,1] %in% ETR3.List.Pos[["Loop"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]])) {
  Temp1 <- ETR3.List.Pos[["Loop"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["Loop"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[i]]) == 0 ) {ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][i,j])) & ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][i,1] %in% ETR3.List.Pos[["Loop"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]])) {
  ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]]$sum[ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]]$sum[ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]]$ETR3 <- sapply(1:nrow(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]]), function (y) ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][[y,12]] / ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][y,12])
  
  ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  
}


ETR3.List.Pos[["Loop"]][["ETR3unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]]),function(y) mean(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                        "sd" = c(sapply(1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]]),function(y) sd(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                        "n" = c(sapply(1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$n)

ETR3.List.Pos[["Loop"]][["ETR3unweighted"]] <- ETR3.List.Pos[["Loop"]][["ETR3unweighted"]][!is.na(ETR3.List.Pos[["Loop"]][["ETR3unweighted"]]$ETR3),]


#anysub unweighted ETR3


ETR3.List.Pos[["Byanysub"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR3.List.Pos[["Byanysub"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR3.List.Pos[["Byanysub"]][["CompanyList"]])) {ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(ETR3.List.Pos[["Byanysub"]][["CompanyList"]])) {
  for (j in 1:length(ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]])) {
    
    if (all(is.na(ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]]) < 20) {ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] <- rbind(ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] < last(Temp1[[z]]))
    
    Temp4 <- ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] <- ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]][Temp4]
  }}



ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]])) {
  Temp1 <- ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[i]]) == 0 ) {ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][i,j])) & ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][i,1] %in% ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]])) {
  Temp1 <- ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[i]]) == 0 ) {ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][i,j])) & ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][i,1] %in% ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]])) {
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]]$sum[ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]]$sum[ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]]$ETR3 <- sapply(1:nrow(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]]), function (y) ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][[y,12]] / ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][y,12])
  
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  
}

ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]]),function(y) mean(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                            "sd" = c(sapply(1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]]),function(y) sd(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                            "n" = c(sapply(1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$n)

ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]] <- ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]][!is.na(ETR3.List.Pos[["Byanysub"]][["ETR3unweighted"]]$ETR3),]


#anysubGER unweighted ETR3


ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[(i-1)]] <- EdgelistByanysub[[i]]}
names(ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]])) {ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][["DE"]] <- NULL}




for (i in 1:length(ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]])) {
  for (j in 1:length(ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]])) {
    
    
    ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][[j]] <- ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]][[j]] |> subset(ETR3.List.Pos[["Byanysub"]][["CompanyList"]][[i]] %in% EdgelistByanysub[[(i+1)]][[names(ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[i]][j])]][,1])
    
  }
}


ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]])) {
  Temp1 <- ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[i]] <- subset(EBIT, EBIT$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[i]]) == 0 ) {ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][i,j])) & ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][i,1] %in% ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][i,j], NA)
    }}}



ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]])) {
  Temp1 <- ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[j]][[i]]))
  }
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[i]]) == 0 ) {ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]])) {
  for (i in 1:nrow(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]])) {
    for (j in 2:ncol(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][i,j])) & ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][i,1] %in% ETR3.List.Pos[["ByanysubGER"]][["CompanyList"]][[(j-1)]][[x]], ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][i,j], NA)
    }}}


for(x in 1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]])) {
      ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][i,j])  , NA  )
    }}}


for(x in 1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]])) {
  for(i in 1:nrow(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]])) {
    for(j in 2:ncol(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]])) {
      ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][i,j])) & !is.na(as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][i,j])) ,  as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][i,j])  , NA  )
    }}}



for (x in 1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]])) {
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][y,2:11]) , na.rm = TRUE ))
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]]$sum[ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]]$sum <- sapply(1:nrow(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]]), function (y) sum(as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][y,2:11]), na.rm = TRUE))
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]]$sum[ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]]$sum == 0] <- NA
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]]$ETR3 <- sapply(1:nrow(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]]), function (y) ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][[y,12]] / ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][y,12])
  
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]]$ETR3 > 1,]
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]] <- ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[x]][!ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]] <- ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]][!ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[x]]$sum < 0,]
  
}


ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]),function(y) mean(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))), 
                                                               "sd" = c(sapply(1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]),function(y) sd(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[y]][,13], na.rm = TRUE))),
                                                               "n" = c(sapply(1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$n)

ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]] <- ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]][!is.na(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweighted"]]$ETR3),]


#Affiliates  unweighted ETR3


ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]] <- data.frame("ISO" = "Affiliates", 
                                                              "ETR3" = mean(unique(Reduce("rbind", ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]))[,13], na.rm = TRUE), 
                                                              "sd" = sd(unique(Reduce("rbind", ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]))[,13], na.rm = TRUE),
                                                              "n" = length(unique(Reduce("rbind", ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]))[,13])]))




ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]]$n)




#GerGUO unweighted ETR3


ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]] <- data.frame("ISO" = "GerGUO", 
                                                          "ETR3" = mean(unique(Reduce("rbind", ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]))[,13], na.rm = TRUE), 
                                                          "sd" = sd(unique(Reduce("rbind", ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]))[,13], na.rm = TRUE),
                                                          "n" = length(unique(Reduce("rbind", ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]))[,13])]))




ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$n)

ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]] <- ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]][!is.na(ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]$ETR3),]




#Domestic firms weighted ETR3


ETR3.List.Pos[["DeDom"]][["ETR3weighted"]] <- data.frame("ISO" = "DEDOM")
ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$ETR3 <- sum(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]]$sum, na.rm = TRUE)
ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$sd <- sqrt(wtd.var(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]$ETR3, sqrt(ETR3.List.Pos[["DeDom"]][["ETR3unweightedEBIT"]]$sum^2), na.rm = TRUE ))
ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$n <- length(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]$ETR3[!is.na(ETR3.List.Pos[["DeDom"]][["ETR3unweightedTax"]]$ETR3)])
ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$ETR3 - qt(0.975, df = ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$sd / sqrt(ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$ETR3 + qt(0.975, df = ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$sd / sqrt(ETR3.List.Pos[["DeDom"]][["ETR3weighted"]]$n)


#International firms weighted ETR3


ETR3.List.Pos[["DeInt"]][["ETR3weighted"]] <- data.frame("ISO" = "DEINT")
ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$ETR3 <- sum(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]$sum, na.rm = TRUE)  / sum(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]]$sum, na.rm = TRUE)
ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$sd <- sqrt(wtd.var(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]$ETR3, sqrt(ETR3.List.Pos[["DeInt"]][["ETR3unweightedEBIT"]]$sum^2), na.rm = TRUE ))
ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$n <- length(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]$ETR3[!is.na(ETR3.List.Pos[["DeInt"]][["ETR3unweightedTax"]]$ETR3)])
ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$ETR3 - qt(0.975, df = ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$sd / sqrt(ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$ETR3 + qt(0.975, df = ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$sd / sqrt(ETR3.List.Pos[["DeInt"]][["ETR3weighted"]]$n)


#CSH firms weighted ETR3 

ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]]),function(y) sum(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]]),function(y) sqrt(wtd.var(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[y]][,13], sqrt(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["ByCSH"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$n)

ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]] <- ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]][!is.na(ETR3.List.Pos[["ByCSH"]][["ETR3weighted"]]$ETR3),]


#GUO firms weighted ETR3 

ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]]),function(y) sum(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                       "sd" = c(sapply(1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]]),function(y) sqrt(wtd.var(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[y]][,13], sqrt(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                       "n" = c(sapply(1:length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["ByGUO"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$n)

ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]] <- ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]][!is.na(ETR3.List.Pos[["ByGUO"]][["ETR3weighted"]]$ETR3),]


#anyown firms weighted ETR3 

ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]),function(y) sum(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]),function(y) sqrt(wtd.var(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[y]][,13], sqrt(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$n)

ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]] <- ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][!is.na(ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ETR3),]


#anyown firms weighted ETR3 

ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]),function(y) sum(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                            "sd" = c(sapply(1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]),function(y) sqrt(wtd.var(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[y]][,13], sqrt(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                            "n" = c(sapply(1:length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$n)

ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]] <- ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]][!is.na(ETR3.List.Pos[["Byintermed"]][["ETR3weighted"]]$ETR3),]


#Loops firms weighted ETR3 

ETR3.List.Pos[["Loop"]][["ETR3weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]]),function(y) sum(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                      "sd" = c(sapply(1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]]),function(y) sqrt(wtd.var(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[y]][,13], sqrt(ETR3.List.Pos[["Loop"]][["ETR3unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                      "n" = c(sapply(1:length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["Loop"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$n)

ETR3.List.Pos[["Loop"]][["ETR3weighted"]] <- ETR3.List.Pos[["Loop"]][["ETR3weighted"]][!is.na(ETR3.List.Pos[["Loop"]][["ETR3weighted"]]$ETR3),]



#anysub firms weighted ETR3 

ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]]),function(y) sum(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                          "sd" = c(sapply(1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]]),function(y) sqrt(wtd.var(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[y]][,13], sqrt(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                          "n" = c(sapply(1:length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["Byanysub"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$n)

ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]] <- ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]][!is.na(ETR3.List.Pos[["Byanysub"]][["ETR3weighted"]]$ETR3),]


#anysubGER firms weighted ETR3 

ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), "ETR3" = c(sapply(1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]),function(y) sum(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[y]][,12], na.rm = TRUE) / sum(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[y]][,12], na.rm = TRUE ))), 
                                                             "sd" = c(sapply(1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]),function(y) sqrt(wtd.var(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[y]][,13], sqrt(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]][[y]][,12]^2),  na.rm = TRUE)))),
                                                             "n" = c(sapply(1:length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]),function(y) length(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[y]][,13][!is.na(as.numeric(ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]][[y]][,13]))]))))


ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$n)

ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]] <- ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]][!is.na(ETR3.List.Pos[["ByanysubGER"]][["ETR3weighted"]]$ETR3),]


#Affiliates  weighted ETR3


ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]] <- data.frame("ISO" = "Affiliates")
ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$ETR3 <- sum(unique(Reduce("rbind", ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]]))[,12], na.rm = TRUE)
ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]])[,12])^2), na.rm = TRUE ))
ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$n <- length(unique(Reduce("rbind", ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]))[,13])])
ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$ETR3 - qt(0.975, df = ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$sd / sqrt(ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$ETR3 + qt(0.975, df = ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$sd / sqrt(ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]]$n)



#GerGUO weighted ETR3


ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]] <- data.frame("ISO" = "GerGUO")
ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$ETR3 <- sum(unique(Reduce("rbind", ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]))[,12], na.rm = TRUE)  / sum(unique(Reduce("rbind", ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]]))[,12], na.rm = TRUE)
ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$sd <- sqrt(wtd.var(unique(Reduce("rbind", ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]])[,13]), sqrt(unique(Reduce("rbind", ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedEBIT"]])[,12])^2), na.rm = TRUE ))
ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$n <- length(unique(Reduce("rbind", ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]))[,13][!is.na(unique(Reduce("rbind", ETR3.List.Pos[["ByanysubGER"]][["ETR3unweightedTax"]]))[,13])])
ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$ETR3 - qt(0.975, df = ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$sd / sqrt(ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$ETR3 + qt(0.975, df = ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$sd / sqrt(ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$n)

ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]] <- ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]][!is.na(ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]$ETR3),]





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown

ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]] <- rbind(ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavens"), "ETR3" = c(mean(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% Taxhavens])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))

ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]] <- rbind(ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEU"), "ETR3" = c(mean(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))



ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]] <- rbind(ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], 
                                                       
                                                       data.frame("ISO" = c("TaxHavensEUProxy"), "ETR3" = c(mean(unique(Reduce("rbind",ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "sd" = c(sd(unique(Reduce("rbind",ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13]), na.rm = TRUE)),
                                                                  "n" = c(length(unique(Reduce("rbind",ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                  "low95" = c(NA),
                                                                  "high95" = c(NA)
                                                       ))



ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]] <- rbind(ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], 
                                                       ETR3.List.Pos[["DeInt"]][["ETR3unweighted"]],
                                                       ETR3.List.Pos[["DeDom"]][["ETR3unweighted"]],
                                                       ETR3.List.Pos[["Affiliates"]][["ETR3unweighted"]],
                                                       ETR3.List.Pos[["GerGUO"]][["ETR3unweighted"]]
)


ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$low95 <- ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$n)
ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$high95 <- ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$n-1) * ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$sd /sqrt(ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$n)





## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown


ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]] <- rbind(ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavens"), "ETR3" = c(sum(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% Taxhavens])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]]) %in% Taxhavens])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% Taxhavens])[,13]), sqrt(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% Taxhavens])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% Taxhavens])[,13])[!is.na(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% Taxhavens])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))

ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]] <- rbind(ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEU"), "ETR3" = c(sum(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedEBIT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byanyown"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]] <- rbind(ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEUProxy"), "ETR3" = c(sum(unique(Reduce("rbind",ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE) / sum(unique(Reduce("rbind",ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]][names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedEBIT"]]) %in% TaxhavensEU])[,12]), na.rm = TRUE)),
                                                                "sd" = c(sqrt(wtd.var(unique(Reduce("rbind",ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13]), sqrt(unique(Reduce("rbind",ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,12])^2),  na.rm = TRUE))),
                                                                "n" = c(length(unique(Reduce("rbind",ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13])[!is.na(unique(Reduce("rbind",ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]][names(ETR3.List.Pos[["Byintermed"]][["ETR3unweightedTax"]]) %in% TaxhavensEU])[,13]))])),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]] <- rbind(ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], 
                                                     ETR3.List.Pos[["DeInt"]][["ETR3weighted"]],
                                                     ETR3.List.Pos[["DeDom"]][["ETR3weighted"]],
                                                     ETR3.List.Pos[["Affiliates"]][["ETR3weighted"]],
                                                     ETR3.List.Pos[["GerGUO"]][["ETR3weighted"]]
)


ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$low95 <- ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ETR3 - qt(0.975, df= ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$n)
ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$high95 <- ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ETR3 + qt(0.975, df= ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$n-1) * ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$sd /sqrt(ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$n)




for(i in 1:4){rm(list=paste0("Temp",i))}

rm(CompanyISO, ETR3, g, h, i, ISO, j, x, y , z)





