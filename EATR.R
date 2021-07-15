

NI <- rio::import("ImportNI.xlsx", which = "Results")
NI <- cbind(data.frame("CompanyBVDID" = c(NI$`BvD ID number`)),NI[,5:13])
NI$DUMMY <- NA

Depr <- rio::import("ImportDepr.xlsx", which = "Results")
Depr <- cbind(data.frame("CompanyBVDID" = c(Depr$`BvD ID number`)),Depr[,4:13])

Tax <- rio::import("ImportTax.xlsx", which = "Results")
Tax <- cbind(data.frame("CompanyBVDID" = c(Tax$`BvD ID number`)),Tax[,4:13])

WC <- rio::import("ImportWC.xlsx", which = "Results")
WC <- cbind(data.frame("CompanyBVDID" = c(WC$`BvD ID number`)),WC[,5:13])
WC$DUMMY <- NA


BvDIDNorm <- intersect(NI$CompanyBVDID, Depr$CompanyBVDID)
BvDIDNorm2 <- intersect(Tax$CompanyBVDID, WC$CompanyBVDID)
BvDIDNorm <- intersect(BvDIDNorm, BvDIDNorm2)
rm(BvDIDNorm2)

BvDIDNorm <- as.data.frame(BvDIDNorm)
names(BvDIDNorm) <- "CompanyBVDID"


NI <- left_join(BvDIDNorm, NI, by = "CompanyBVDID")
Depr <- left_join(BvDIDNorm, Depr, by = "CompanyBVDID")
Tax <- left_join(BvDIDNorm, Tax, by = "CompanyBVDID")
WC <- left_join(BvDIDNorm, WC, by = "CompanyBVDID")




CWC <- sapply(2:ncol(WC), function(y) as.numeric(WC[,(y-1)]) - as.numeric(WC[,y]) )
CWC <- cbind(c(NA), CWC)
CWC <- as.data.frame(CWC)



CF <- NI
for(i in 1:nrow(CF)) {
  for (j in 2:ncol(CF)) {
  CF[i,j] <- ifelse(!is.na(as.numeric(NI[i,j])) & !is.na(as.numeric(Depr[i,j])) & !is.na(as.numeric(CWC[i,j])), as.numeric(NI[i,j]) + as.numeric(Depr[i,j]) + as.numeric(CWC[i,j]), NA)  
  }}




##equal out samples

for(i in 1:nrow(CF)) {
  for (j in 2:ncol(CF)) {
    CF[i,j] <- ifelse(!is.na(as.numeric(CF[i,j])) & !is.na(as.numeric(Tax[i,j])),  as.numeric(CF[i,j]) , NA )
  }
}


for(i in 1:nrow(Tax)) {
  for (j in 2:ncol(Tax)) {
    
    Tax[i,j] <- ifelse(!is.na(as.numeric(Tax[i,j])) & !is.na(as.numeric(CF[i,j])),  as.numeric(Tax[i,j]) , NA )
    
  }
}


## Drop if <3 obs


CF <- CF[apply(CF,1,function (z) length(z[!is.na(as.numeric(z))]) > 2),]
Tax <- Tax[Tax$CompanyBVDID %in% CF$CompanyBVDID,]



#####

EATR.List <- vector(mode = "list")
EATR.List[[1]] <- vector(mode = "list")
names(EATR.List) <- "DeDom"

EATR.List[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {EATR.List[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(EATR.List[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(EATR.List[["DeDom"]][["CompanyList"]])) {
  EATR.List[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(EATR.List[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- EATR.List[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(EATR.List[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(EATR.List[["DeDom"]][["CompanyList"]][[i]])))
}




###


EATR.List[["DeDom"]][["EATRunweightedCF"]] <- subset(CF, CF$CompanyBVDID %in% Temp1)

for (i in 1:nrow(EATR.List[["DeDom"]][["EATRunweightedCF"]])) {
  for (j in 2:ncol(EATR.List[["DeDom"]][["EATRunweightedCF"]])) {
    EATR.List[["DeDom"]][["EATRunweightedCF"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["DeDom"]][["EATRunweightedCF"]][i,j])) & EATR.List[["DeDom"]][["EATRunweightedCF"]][i,1] %in% EATR.List[["DeDom"]][["CompanyList"]][[(j-1)]], EATR.List[["DeDom"]][["EATRunweightedCF"]][i,j], NA)
  }}


EATR.List[["DeDom"]][["EATRunweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)

for (i in 1:nrow(EATR.List[["DeDom"]][["EATRunweightedTax"]])) {
  for (j in 2:ncol(EATR.List[["DeDom"]][["EATRunweightedTax"]])) {
    EATR.List[["DeDom"]][["EATRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["DeDom"]][["EATRunweightedTax"]][i,j])) & EATR.List[["DeDom"]][["EATRunweightedTax"]][i,1] %in% EATR.List[["DeDom"]][["CompanyList"]][[(j-1)]], EATR.List[["DeDom"]][["EATRunweightedTax"]][i,j], NA)
  }}




HelpDisc <- EATR.List[["DeDom"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List[["DeDom"]][["EATRunweightedCF"]])) {
  HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))] <- 1.05^seq(length(HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))]),1)
}


EATR.List[["DeDom"]][["EATRunweightedNPVAT"]] <- EATR.List[["DeDom"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]])) {
  for(j in 2:ncol(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]])) {
  
  
    EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][i,j])),  (as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
  }}


EATR.List[["DeDom"]][["EATRunweightedNPVBT"]] <- EATR.List[["DeDom"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]])) {
  for(j in 2:ncol(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]])) {
    EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][i,j])),  (as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][i,j]) + as.numeric(EATR.List[["DeDom"]][["EATRunweightedTax"]][i,j])  ) / as.numeric(HelpDisc[i,j]) , NA)
  }}






EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum <- sapply(1:nrow(EATR.List[["DeDom"]][["EATRunweightedCF"]]), function (y) sum(as.numeric(EATR.List[["DeDom"]][["EATRunweightedCF"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum[EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum == 0] <- NA

EATR.List[["DeDom"]][["EATRunweightedTax"]]$sum <- sapply(1:nrow(EATR.List[["DeDom"]][["EATRunweightedTax"]]), function (y) sum(as.numeric(EATR.List[["DeDom"]][["EATRunweightedTax"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRunweightedTax"]]$sum[EATR.List[["DeDom"]][["EATRunweightedTax"]]$sum == 0] <- NA

EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum <- sapply(1:nrow(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]), function (y) sum(as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum[EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum == 0] <- NA

EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum <- sapply(1:nrow(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]), function (y) sum(as.numeric(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][y,2:11]) , na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum[EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum == 0] <- NA





EATR.List[["DeDom"]][["EATRunweightedNPVBT"]] <- EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][!EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List[["DeDom"]][["EATRunweightedNPVAT"]] <- EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][!EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List[["DeDom"]][["EATRunweightedTax"]] <- EATR.List[["DeDom"]][["EATRunweightedTax"]][!EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List[["DeDom"]][["EATRunweightedCF"]] <- EATR.List[["DeDom"]][["EATRunweightedCF"]][!EATR.List[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]


EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR  <-  1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum


EATR.List[["DeDom"]][["EATRunweightedNPVBT"]] <- EATR.List[["DeDom"]][["EATRunweightedNPVBT"]][!EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < -10 & !EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 10,]
EATR.List[["DeDom"]][["EATRunweightedNPVAT"]] <- EATR.List[["DeDom"]][["EATRunweightedNPVAT"]][!EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < -10 & !EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 10,]
EATR.List[["DeDom"]][["EATRunweightedCF"]] <- EATR.List[["DeDom"]][["EATRunweightedCF"]][!EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < -10 & !EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 10,]
EATR.List[["DeDom"]][["EATRunweightedTax"]] <- EATR.List[["DeDom"]][["EATRunweightedTax"]][!EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < -10 & !EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 10,]




EATR.List[["DeDom"]][["EATRunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                         "EATR" = mean(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR, na.rm = TRUE), 
                                                         "sd" = sd(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR, na.rm = TRUE), 
                                                         "n" = length(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR[!is.na(as.numeric(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR))]),
                                                         "EATRNeg" = mean(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR, na.rm = TRUE), 
                                                         "sdNeg" = sd(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR, na.rm = TRUE), 
                                                         "nNeg" = length(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR[!is.na(as.numeric(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR))]))

EATR.List[["DeDom"]][["EATRunweighted"]]$low95 <- EATR.List[["DeDom"]][["EATRunweighted"]]$EATR - qt(0.975, df= EATR.List[["DeDom"]][["EATRunweighted"]]$n-1) * EATR.List[["DeDom"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["DeDom"]][["EATRunweighted"]]$n)
EATR.List[["DeDom"]][["EATRunweighted"]]$high95 <- EATR.List[["DeDom"]][["EATRunweighted"]]$EATR + qt(0.975, df= EATR.List[["DeDom"]][["EATRunweighted"]]$n-1) * EATR.List[["DeDom"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["DeDom"]][["EATRunweighted"]]$n)

EATR.List[["DeDom"]][["EATRunweighted"]] <- EATR.List[["DeDom"]][["EATRunweighted"]][!is.na(EATR.List[["DeDom"]][["EATRunweighted"]]$EATR),]




#Domestic firms weighted EATR


EATR.List[["DeDom"]][["EATRweighted"]] <- data.frame("ISO" = "DEDOM")
EATR.List[["DeDom"]][["EATRweighted"]]$EATR <- 1 - sum(subset(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]], 1 -  EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum >0)$sum / sum(subset(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]], 1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum >0 )$sum))
EATR.List[["DeDom"]][["EATRweighted"]]$sd <- sqrt(wtd.var(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR, sqrt(subset(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]], 1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum >0)$sum^2), na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRweighted"]]$n <- length(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$sum > 0)$EATR[!is.na(as.numeric(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$sum > 0)$EATR))])

EATR.List[["DeDom"]][["EATRweighted"]]$EATRNeg <- 1 - sum(subset(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]], 1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum <0)$sum / sum(subset(EATR.List[["DeDom"]][["EATRunweightedNPVBT"]], 1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum <0 )$sum))
EATR.List[["DeDom"]][["EATRweighted"]]$sdNeg <- sqrt(wtd.var(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR, sqrt(subset(EATR.List[["DeDom"]][["EATRunweightedNPVAT"]], 1 - EATR.List[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List[["DeDom"]][["EATRunweightedNPVBT"]]$sum <0)$sum^2), na.rm = TRUE ))
EATR.List[["DeDom"]][["EATRweighted"]]$nNeg <- length(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$sum < 0)$EATR[!is.na(as.numeric(subset(EATR.List[["DeDom"]][["EATRunweightedTax"]], EATR.List[["DeDom"]][["EATRunweightedTax"]]$sum < 0)$EATR))])

EATR.List[["DeDom"]][["EATRweighted"]]$low95 <- EATR.List[["DeDom"]][["EATRweighted"]]$EATR - qt(0.975, df = EATR.List[["DeDom"]][["EATRweighted"]]$n-1) * EATR.List[["DeDom"]][["EATRweighted"]]$sd / sqrt(EATR.List[["DeDom"]][["EATRweighted"]]$n)
EATR.List[["DeDom"]][["EATRweighted"]]$high95 <- EATR.List[["DeDom"]][["EATRweighted"]]$EATR + qt(0.975, df = EATR.List[["DeDom"]][["EATRweighted"]]$n-1) * EATR.List[["DeDom"]][["EATRweighted"]]$sd / sqrt(EATR.List[["DeDom"]][["EATRweighted"]]$n)










### reinvesting model


EATR.List.Reinv <- vector(mode = "list")
EATR.List.Reinv[[1]] <- vector(mode = "list")
names(EATR.List.Reinv) <- "DeDom"

EATR.List.Reinv[["DeDom"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {EATR.List.Reinv[["DeDom"]][["CompanyList"]][[(i-1)]] <- EdgelistDeDom[[i]]}
names(EATR.List.Reinv[["DeDom"]][["CompanyList"]]) <- paste(2020:2010)


for (i in 1:length(EATR.List.Reinv[["DeDom"]][["CompanyList"]])) {
  EATR.List.Reinv[["DeDom"]][["CompanyList"]][[i]] <- na.omit(unique(as.character(as.matrix(EATR.List.Reinv[["DeDom"]][["CompanyList"]][[i]]))))
}

Temp1 <- EATR.List.Reinv[["DeDom"]][["CompanyList"]][[1]]
for(i in 2:length(EATR.List.Reinv[["DeDom"]][["CompanyList"]])) {
  Temp1 <- unique(c(Temp1,(EATR.List.Reinv[["DeDom"]][["CompanyList"]][[i]])))
}



EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]] <- subset(CF, CF$CompanyBVDID %in% Temp1)

for (i in 1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]])) {
  for (j in 2:ncol(EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]])) {
    EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]][i,j])) & EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]][i,1] %in% EATR.List.Reinv[["DeDom"]][["CompanyList"]][[(j-1)]], EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]][i,j], NA)
  }}

EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)

for (i in 1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]])) {
  for (j in 2:ncol(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]])) {
    EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]][i,j])) & EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]][i,1] %in% EATR.List.Reinv[["DeDom"]][["CompanyList"]][[(j-1)]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]][i,j], NA)
  }}



NPVBThelp <- EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]


for(i in 1:nrow(NPVBThelp)) {
  for(j in 2:ncol(NPVBThelp)) {
    NPVBThelp[i,j] <- ifelse(!is.na(as.numeric(NPVBThelp[i,j])),  (as.numeric(NPVBThelp[i,j]) + as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]][i,j])  ) , NA)
  }}




GRTemp <- t(apply(NPVBThelp ,1,function(x) {c(x[!is.na(x)],x[is.na(x)])}))

GRTemp <- sapply(2:ncol(GRTemp), function(y) as.numeric(GRTemp[,(y-1)]) / as.numeric(GRTemp[,y]))

GRTemp <- lapply(1:nrow(GRTemp), function(x)   as.numeric(GRTemp[x,][!is.na(as.numeric(GRTemp[x,]))]))



TaxTemp <- lapply(1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]), function(x) na.omit(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]][x,])))

NPVBTTemp <- lapply(1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]), function(x)  as.numeric(last(EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]][x,][!is.na(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]][x,]))])))


NPVBTTemp <- lapply(1:length(NPVBTTemp), function(x)  NPVBTTemp[[x]]+last(TaxTemp[[x]]))


for (x in 1:length(TaxTemp)) {
  
  
  if(!isTruthy(TaxTemp[[x]])) {next}
  
  TaxTemp[[x]] <- TaxTemp[[x]][1:(length(TaxTemp[[x]])-1)]
  
}



for(x in 1:length(NPVBTTemp)) {

if (!isTruthy(NPVBTTemp[[x]])) {next}  
if (!isTruthy(TaxTemp[[x]])) {next}    
if (!isTruthy(GRTemp[[x]])) {next}    

for (i in length(GRTemp[[x]]):1) {
  
  
NPVBTTemp[[x]] <-   c(first(NPVBTTemp[[x]])*GRTemp[[x]][i]+TaxTemp[[x]][i],NPVBTTemp[[x]])  

}
}




HelpDisc <- EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]])) {
  
  
  HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))] <- 1.05^seq(length(HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))]),1)
  
}





EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]

for(i in 1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]])) {
  for(j in 2:ncol(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]])) {
    
    
    EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]][i,j])),  (as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
    
    
  }
}




EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]

EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum <- NULL

for(i in 1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]])) {
  
  
  EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]][i,][!is.na(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]][i,]))] <- NPVBTTemp[[i]]

}



for(i in 1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]])) {
  for(j in 2:ncol(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]])) {
    
    
    EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]][i,j])),  (as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
    
    
  }
}






EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]$sum <- sapply(1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]), function (y) sum(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]][y,2:11]) , na.rm = TRUE ))
EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]$sum[EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]$sum == 0] <- NA

EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$sum <- sapply(1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]), function (y) sum(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]][y,2:11]) , na.rm = TRUE ))
EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$sum[EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$sum == 0] <- NA

EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum <- sapply(1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]), function (y) sum(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]][y,2:11]) , na.rm = TRUE ))
EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum[EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum == 0] <- NA

EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]$sum <- sapply(1:nrow(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]), function (y) sum(as.numeric(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]][y,2:11]) , na.rm = TRUE ))
EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]$sum[EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]$sum == 0] <- NA






EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]][!EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]][!EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]][!EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]
EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]][!EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]$sum < 0,]


EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR  <-  1 - EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum


EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]][!EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR < -10 & !EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR > 10,]
EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]][!EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR < -10 & !EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR > 10,]
EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]][!EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR < -10 & !EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR > 10,]
EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]][!EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR < -10 & !EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR > 10,]


EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]][!EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$CompanyBVDID %notin% EATR.List.Reinv[["DeDom"]][["EATRunweightedCF"]]$CompanyBVDID,]











EATR.List.Reinv[["DeDom"]][["EATRunweighted"]] <- data.frame("ISO" = "DEDOM", 
                                                       "EATR" = mean(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR, na.rm = TRUE), 
                                                       "sd" = sd(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR, na.rm = TRUE), 
                                                       "n" = length(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR))]),
                                                       "EATRNeg" = mean(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR, na.rm = TRUE), 
                                                       "sdNeg" = sd(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR, na.rm = TRUE), 
                                                       "nNeg" = length(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR))]))

EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$low95 <- EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$EATR - qt(0.975, df= EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$n-1) * EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$sd /sqrt(EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$n)
EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$high95 <- EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$EATR + qt(0.975, df= EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$n-1) * EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$sd /sqrt(EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$n)

EATR.List.Reinv[["DeDom"]][["EATRunweighted"]] <- EATR.List.Reinv[["DeDom"]][["EATRunweighted"]][!is.na(EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]$EATR),]




#Domestic firms weighted EATR


EATR.List.Reinv[["DeDom"]][["EATRweighted"]] <- data.frame("ISO" = "DEDOM")
EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$EATR <- 1 - sum(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]], 1 -  EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum >0)$sum / sum(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]], 1 - EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum >0 )$sum))
EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$sd <- sqrt(wtd.var(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR > 0)$EATR, sqrt(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]], 1 - EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum >0)$sum^2), na.rm = TRUE ))
EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$n <- length(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$sum > 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$sum > 0)$EATR))])

EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$EATRNeg <- 1 - sum(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]], 1 - EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum <0)$sum / sum(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]], 1 - EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum <0 )$sum))
EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$sdNeg <- sqrt(wtd.var(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$EATR < 0)$EATR, sqrt(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]], 1 - EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVAT"]]$sum / EATR.List.Reinv[["DeDom"]][["EATRunweightedNPVBT"]]$sum <0)$sum^2), na.rm = TRUE ))
EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$nNeg <- length(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$sum < 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]], EATR.List.Reinv[["DeDom"]][["EATRunweightedTax"]]$sum < 0)$EATR))])

EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$low95 <- EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$EATR - qt(0.975, df = EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$n-1) * EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$sd / sqrt(EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$n)
EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$high95 <- EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$EATR + qt(0.975, df = EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$n-1) * EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$sd / sqrt(EATR.List.Reinv[["DeDom"]][["EATRweighted"]]$n)









## anyown





EATR.List[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {EATR.List[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(EATR.List[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(EATR.List[["Byanyown"]][["CompanyList"]])) {EATR.List[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(EATR.List[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(EATR.List[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(EATR.List[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]] <- EATR.List[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}




EATR.List[["Byanyown"]][["EATRunweightedCF"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List[["Byanyown"]][["EATRunweightedCF"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List[["Byanyown"]][["EATRunweightedCF"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List[["Byanyown"]][["EATRunweightedCF"]])) {
  Temp1 <- EATR.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[i]])) {
    Temp1 <- unique(c(Temp1,EATR.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List[["Byanyown"]][["EATRunweightedCF"]][[i]] <- subset(CF, CF$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[i]]) == 0 ) {EATR.List[["Byanyown"]][["EATRunweightedCF"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List[["Byanyown"]][["EATRunweightedCF"]])) {
  for (i in 1:nrow(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]])) {
    for (j in 2:ncol(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]])) {
      EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][i,j])) & EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][i,1] %in% EATR.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][i,j], NA)
    }}}



EATR.List[["Byanyown"]][["EATRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List[["Byanyown"]][["EATRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]])) {
  Temp1 <- EATR.List[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,EATR.List[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List[["Byanyown"]][["EATRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[i]]) == 0 ) {EATR.List[["Byanyown"]][["EATRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]])) {
  for (i in 1:nrow(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]])) {
    for (j in 2:ncol(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]])) {
      EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j])) & EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][i,1] %in% EATR.List[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j], NA)
    }}}





for(x in 1:length(EATR.List[["Byanyown"]][["EATRunweightedCF"]])) {


HelpDisc <- EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]
for(i in 1:nrow(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]])) {
  HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))] <- 1.05^seq(length(HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))]),1)
}


EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]

for(i in 1:nrow(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]])) {
  for(j in 2:ncol(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]])) {
    EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][i,j])),  (as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
  }}



EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]

for(i in 1:nrow(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]])) {
  for(j in 2:ncol(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]])) {
    EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][i,j])),  (as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][i,j]) + as.numeric(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j])) / as.numeric(HelpDisc[i,j]) , NA)
  }}


print(paste0(x,"/",length(EATR.List[["Byanyown"]][["EATRunweightedCF"]])))

}

names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) <- names(EATR.List[["Byanyown"]][["EATRunweightedCF"]])
names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) <- names(EATR.List[["Byanyown"]][["EATRunweightedCF"]])



for(x in 1:length(EATR.List[["Byanyown"]][["EATRunweightedCF"]])) {



EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum[EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum == 0] <- NA

EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$sum[EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$sum == 0] <- NA

EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum[EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum == 0] <- NA

EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][y,2:11]) , na.rm = TRUE ))
EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum[EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum == 0] <- NA





EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]
EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]
EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]
EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]



EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR  <- 1 - EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum



EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedCF"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]] <- EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]][!EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR > 10,]



}







EATR.List[["Byanyown"]][["EATRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                          "EATR" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                          "sd" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                          "n" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR[!is.na(as.numeric(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR))]))),
                                                          "EATRNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                          "sdNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                          "nNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR[!is.na(as.numeric(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR))]))))

EATR.List[["Byanyown"]][["EATRunweighted"]]$low95 <- EATR.List[["Byanyown"]][["EATRunweighted"]]$EATR - qt(0.975, df= EATR.List[["Byanyown"]][["EATRunweighted"]]$n-1) * EATR.List[["Byanyown"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["Byanyown"]][["EATRunweighted"]]$n)
EATR.List[["Byanyown"]][["EATRunweighted"]]$high95 <- EATR.List[["Byanyown"]][["EATRunweighted"]]$EATR + qt(0.975, df= EATR.List[["Byanyown"]][["EATRunweighted"]]$n-1) * EATR.List[["Byanyown"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["Byanyown"]][["EATRunweighted"]]$n)

EATR.List[["Byanyown"]][["EATRunweighted"]] <- EATR.List[["Byanyown"]][["EATRunweighted"]][!is.na(EATR.List[["Byanyown"]][["EATRunweighted"]]$EATR) | !is.na(EATR.List[["Byanyown"]][["EATRunweighted"]]$EATRNeg),]








## weighted


EATR.List[["Byanyown"]][["EATRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                        "EATR" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum / sum(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum >0 )$sum)))), 
                                                        "sd" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, sqrt(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum^2), na.rm = TRUE )))),
                                                        "n" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR[!is.na(as.numeric(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR))]))),
                                                        "EATRNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum / sum(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum <0 )$sum)))), 
                                                         "sdNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, sqrt(subset(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum^2), na.rm = TRUE )))),
                                                         "nNeg" = c(sapply(1:length(EATR.List[["Byanyown"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR[!is.na(as.numeric(subset(EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR))])))
)




EATR.List[["Byanyown"]][["EATRweighted"]]$low95 <- EATR.List[["Byanyown"]][["EATRweighted"]]$EATR - qt(0.975, df= EATR.List[["Byanyown"]][["EATRweighted"]]$n-1) * EATR.List[["Byanyown"]][["EATRweighted"]]$sd /sqrt(EATR.List[["Byanyown"]][["EATRweighted"]]$n)
EATR.List[["Byanyown"]][["EATRweighted"]]$high95 <- EATR.List[["Byanyown"]][["EATRweighted"]]$EATR + qt(0.975, df= EATR.List[["Byanyown"]][["EATRweighted"]]$n-1) * EATR.List[["Byanyown"]][["EATRweighted"]]$sd /sqrt(EATR.List[["Byanyown"]][["EATRweighted"]]$n)


EATR.List[["Byanyown"]][["EATRweighted"]]$EATR[EATR.List[["Byanyown"]][["EATRweighted"]]$EATR == 1] <- NA
EATR.List[["Byanyown"]][["EATRweighted"]]$EATRNeg[EATR.List[["Byanyown"]][["EATRweighted"]]$EATRNeg == 1] <- NA


EATR.List[["Byanyown"]][["EATRweighted"]] <- EATR.List[["Byanyown"]][["EATRweighted"]][!is.na(EATR.List[["Byanyown"]][["EATRweighted"]]$EATR) | !is.na(EATR.List[["Byanyown"]][["EATRweighted"]]$EATRNeg),]







## intermed


EATR.List[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {EATR.List[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(EATR.List[["Byintermed"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(EATR.List[["Byintermed"]][["CompanyList"]])) {EATR.List[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(EATR.List[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(EATR.List[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(EATR.List[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]] <- EATR.List[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}




EATR.List[["Byintermed"]][["EATRunweightedCF"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List[["Byintermed"]][["EATRunweightedCF"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List[["Byintermed"]][["EATRunweightedCF"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List[["Byintermed"]][["EATRunweightedCF"]])) {
  Temp1 <- EATR.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[i]])) {
    Temp1 <- unique(c(Temp1,EATR.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List[["Byintermed"]][["EATRunweightedCF"]][[i]] <- subset(CF, CF$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[i]]) == 0 ) {EATR.List[["Byintermed"]][["EATRunweightedCF"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List[["Byintermed"]][["EATRunweightedCF"]])) {
  for (i in 1:nrow(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]])) {
    for (j in 2:ncol(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]])) {
      EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][i,j])) & EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][i,1] %in% EATR.List[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][i,j], NA)
    }}}



EATR.List[["Byintermed"]][["EATRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List[["Byintermed"]][["EATRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]])) {
  Temp1 <- EATR.List[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,EATR.List[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[i]]) == 0 ) {EATR.List[["Byintermed"]][["EATRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]])) {
  for (i in 1:nrow(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]])) {
    for (j in 2:ncol(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]])) {
      EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j])) & EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][i,1] %in% EATR.List[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j], NA)
    }}}





for(x in 1:length(EATR.List[["Byintermed"]][["EATRunweightedCF"]])) {
  
  
  HelpDisc <- EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]
  for(i in 1:nrow(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]])) {
    HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))] <- 1.05^seq(length(HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))]),1)
  }
  
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]
  
  for(i in 1:nrow(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]])) {
    for(j in 2:ncol(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]])) {
      EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][i,j])),  (as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
    }}
  
  
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]
  
  for(i in 1:nrow(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]])) {
    for(j in 2:ncol(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]])) {
      EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][i,j])),  (as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][i,j]) + as.numeric(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j])) / as.numeric(HelpDisc[i,j]) , NA)
    }}
  
  
  print(paste0(x,"/",length(EATR.List[["Byintermed"]][["EATRunweightedCF"]])))
  
}

names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) <- names(EATR.List[["Byintermed"]][["EATRunweightedCF"]])
names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) <- names(EATR.List[["Byintermed"]][["EATRunweightedCF"]])



for(x in 1:length(EATR.List[["Byintermed"]][["EATRunweightedCF"]])) {
  
  
  
  EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum[EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum == 0] <- NA
  
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$sum[EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$sum == 0] <- NA
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum[EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum == 0] <- NA
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum <- sapply(1:nrow(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]), function (y) sum(as.numeric(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum[EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum == 0] <- NA
  
  
  
  
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  
  
  
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR  <- 1 - EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum
  
  
  
  EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedCF"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]] <- EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]][!EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  
  
  
}







EATR.List[["Byintermed"]][["EATRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                          "EATR" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                          "sd" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                          "n" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR[!is.na(as.numeric(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR))]))),
                                                          "EATRNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                          "sdNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                          "nNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR[!is.na(as.numeric(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR))]))))

EATR.List[["Byintermed"]][["EATRunweighted"]]$low95 <- EATR.List[["Byintermed"]][["EATRunweighted"]]$EATR - qt(0.975, df= EATR.List[["Byintermed"]][["EATRunweighted"]]$n-1) * EATR.List[["Byintermed"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["Byintermed"]][["EATRunweighted"]]$n)
EATR.List[["Byintermed"]][["EATRunweighted"]]$high95 <- EATR.List[["Byintermed"]][["EATRunweighted"]]$EATR + qt(0.975, df= EATR.List[["Byintermed"]][["EATRunweighted"]]$n-1) * EATR.List[["Byintermed"]][["EATRunweighted"]]$sd /sqrt(EATR.List[["Byintermed"]][["EATRunweighted"]]$n)

EATR.List[["Byintermed"]][["EATRunweighted"]] <- EATR.List[["Byintermed"]][["EATRunweighted"]][!is.na(EATR.List[["Byintermed"]][["EATRunweighted"]]$EATR) | !is.na(EATR.List[["Byintermed"]][["EATRunweighted"]]$EATRNeg),]








## weighted


EATR.List[["Byintermed"]][["EATRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                        "EATR" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum / sum(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum >0 )$sum)))), 
                                                        "sd" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, sqrt(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum^2), na.rm = TRUE )))),
                                                        "n" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR[!is.na(as.numeric(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR))]))),
                                                        "EATRNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum / sum(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum <0 )$sum)))), 
                                                        "sdNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, sqrt(subset(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum^2), na.rm = TRUE )))),
                                                        "nNeg" = c(sapply(1:length(EATR.List[["Byintermed"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR[!is.na(as.numeric(subset(EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR))])))
)




EATR.List[["Byintermed"]][["EATRweighted"]]$low95 <- EATR.List[["Byintermed"]][["EATRweighted"]]$EATR - qt(0.975, df= EATR.List[["Byintermed"]][["EATRweighted"]]$n-1) * EATR.List[["Byintermed"]][["EATRweighted"]]$sd /sqrt(EATR.List[["Byintermed"]][["EATRweighted"]]$n)
EATR.List[["Byintermed"]][["EATRweighted"]]$high95 <- EATR.List[["Byintermed"]][["EATRweighted"]]$EATR + qt(0.975, df= EATR.List[["Byintermed"]][["EATRweighted"]]$n-1) * EATR.List[["Byintermed"]][["EATRweighted"]]$sd /sqrt(EATR.List[["Byintermed"]][["EATRweighted"]]$n)


EATR.List[["Byintermed"]][["EATRweighted"]]$EATR[EATR.List[["Byintermed"]][["EATRweighted"]]$EATR == 1] <- NA
EATR.List[["Byintermed"]][["EATRweighted"]]$EATRNeg[EATR.List[["Byintermed"]][["EATRweighted"]]$EATRNeg == 1] <- NA


EATR.List[["Byintermed"]][["EATRweighted"]] <- EATR.List[["Byintermed"]][["EATRweighted"]][!is.na(EATR.List[["Byintermed"]][["EATRweighted"]]$EATR) | !is.na(EATR.List[["Byintermed"]][["EATRweighted"]]$EATRNeg),]








## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown


EATR.List[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRunweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavens"), 
                                                                "EATR" = mean(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "sd" = sd(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "n" = length(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR))]),
                                                                "EATRNeg" = mean(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "sdNeg" = sd(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "nNeg" = length(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR))]),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



EATR.List[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRunweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavensEU"), 
                                                              "EATR" = mean(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                              "sd" = sd(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                              "n" = length(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR))]),
                                                              "EATRNeg" = mean(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                              "sdNeg" = sd(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                              "nNeg" = length(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR))]),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))



EATR.List[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRunweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavensEUProxy"), 
                                                              "EATR" = mean(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                              "sd" = sd(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                              "n" = length(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR))]),
                                                              "EATRNeg" = mean(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                              "sdNeg" = sd(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                              "nNeg" = length(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR))]),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))



EATR.List[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRunweighted"]],
                                                    EATR.List[["DeDom"]][["EATRunweighted"]]
)






## weighted


EATR.List[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavens"), 
                                                              "EATR" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum >0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum >0 )$sum)),
                                                              "sd" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum >0)$sum^2), na.rm = TRUE )),
                                                              "n" = length(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$sum > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$sum > 0)$EATR))]),
                                                              "EATRNeg" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum <0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum <0 )$sum)),
                                                              "sdNeg" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum <0)$sum^2), na.rm = TRUE )),
                                                              "nNeg" = length(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$sum < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$sum < 0)$EATR))]),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))







EATR.List[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEU"), 
                                                                "EATR" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0 )$sum)),
                                                                "sd" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum^2), na.rm = TRUE )),
                                                                "n" = length(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum > 0)$EATR))]),
                                                                "EATRNeg" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0 )$sum)),
                                                                "sdNeg" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum^2), na.rm = TRUE )),
                                                                "nNeg" = length(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum < 0)$EATR))]),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))





EATR.List[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavensEUProxy"), 
                                                              "EATR" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0 )$sum)),
                                                              "sd" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum^2), na.rm = TRUE )),
                                                              "n" = length(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum > 0)$EATR))]),
                                                              "EATRNeg" = 1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0 )$sum)),
                                                              "sdNeg" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum^2), na.rm = TRUE )),
                                                              "nNeg" = length(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum < 0)$EATR))]),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))




EATR.List[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List[["Byanyown"]][["EATRweighted"]],
                                                     EATR.List[["DeDom"]][["EATRweighted"]]
)










## reinvesting model


EATR.List.Reinv[["Byanyown"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[(i-1)]] <- EdgelistByanyown[[i]]}
names(EATR.List.Reinv[["Byanyown"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(EATR.List.Reinv[["Byanyown"]][["CompanyList"]])) {EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(EATR.List.Reinv[["Byanyown"]][["CompanyList"]])) {
  for (j in 1:length(EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][[j]]) < 20) {EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][[j]] <- rbind(EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][[j]] <- EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[i]][[j]][Temp4]
  }}




EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]])) {
  Temp1 <- EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[i]])) {
    Temp1 <- unique(c(Temp1,EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[i]] <- subset(CF, CF$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[i]]) == 0 ) {EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]])) {
  for (i in 1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]])) {
    for (j in 2:ncol(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]])) {
      EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]][i,j])) & EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]][i,1] %in% EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]][i,j], NA)
    }}}



EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]])) {
  Temp1 <- EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[i]]) == 0 ) {EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]])) {
  for (i in 1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]])) {
    for (j in 2:ncol(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]])) {
      EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j])) & EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]][i,1] %in% EATR.List.Reinv[["Byanyown"]][["CompanyList"]][[(j-1)]][[x]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]][i,j], NA)
    }}}









for (z in 1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]])) {

  

NPVBThelp <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[z]]


for(i in 1:nrow(NPVBThelp)) {
  for(j in 2:ncol(NPVBThelp)) {
    NPVBThelp[i,j] <- ifelse(!is.na(as.numeric(NPVBThelp[i,j])),  (as.numeric(NPVBThelp[i,j]) + as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[z]][i,j])  ) , NA)
  }}


GRTemp <- t(apply(NPVBThelp ,1,function(x) {c(x[!is.na(x)],x[is.na(x)])}))

GRTemp <- as.matrix(sapply(2:ncol(GRTemp), function(y) as.numeric(GRTemp[,(y-1)]) / as.numeric(GRTemp[,y])))

GRTemp <- lapply(1:nrow(GRTemp), function(x)   as.numeric(GRTemp[x,][!is.na(as.numeric(GRTemp[x,]))]))



TaxTemp <- lapply(1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[z]]), function(x) na.omit(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[z]][x,])))

NPVBTTemp <- lapply(1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[z]]), function(x)  as.numeric(last(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[z]][x,][!is.na(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[z]][x,]))])))

NPVBTTemp <- lapply(1:length(NPVBTTemp), function(x)  NPVBTTemp[[x]]+last(TaxTemp[[x]]))


for (x in 1:length(TaxTemp)) {
  
  
  if(!isTruthy(TaxTemp[[x]])) {next}
  
  TaxTemp[[x]] <- TaxTemp[[x]][1:(length(TaxTemp[[x]])-1)]
  
}



for(x in 1:length(NPVBTTemp)) {
  
  if (!isTruthy(NPVBTTemp[[x]])) {next}  
  if (!isTruthy(TaxTemp[[x]])) {next}    
  if (!isTruthy(GRTemp[[x]])) {next}    
  
  for (i in length(GRTemp[[x]]):1) {
    
    
    NPVBTTemp[[x]] <-   c(first(NPVBTTemp[[x]])*GRTemp[[x]][i]+TaxTemp[[x]][i],NPVBTTemp[[x]])  
    
  }
}




HelpDisc <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[z]]

for(i in 1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[z]])) {
  
  
  HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))] <- 1.05^seq(length(HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))]),1)
  
}





EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[z]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[z]]

for(i in 1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[z]])) {
  for(j in 2:ncol(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[z]])) {
    
    
    EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[z]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[z]][i,j])),  (as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[z]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
    
    
  }
}




EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[z]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[z]]

EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[z]]$sum <- NULL

for(i in 1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[z]])) {
  
  
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[z]][i,][!is.na(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[z]][i,]))] <- NPVBTTemp[[i]]
  
}



for(i in 1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[z]])) {
  for(j in 2:ncol(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[z]])) {
    
    
    EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[z]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[z]][i,j])),  (as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[z]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
    
    
  }
}
}


names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) <- names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]])
names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) <- names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]])







for(x in 1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]])) {
  
  
  
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum <- sapply(1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]]), function (y) sum(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum[EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum == 0] <- NA
  
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$sum <- sapply(1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]), function (y) sum(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$sum[EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$sum == 0] <- NA
  
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum <- sapply(1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]), function (y) sum(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum[EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum == 0] <- NA
  
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum <- sapply(1:nrow(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]), function (y) sum(as.numeric(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum[EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum == 0] <- NA
  
  
  
  
  
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][!EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][!EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]][!EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]][!EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  
  
  
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR  <- 1 - EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[x]]$sum / EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[x]]$sum
  
  
  
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[x]][!EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[x]][!EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedCF"]][[x]][!EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]][!EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  
  
  
}







EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                          "EATR" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                          "sd" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                          "n" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR))]))),
                                                          "EATRNeg" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                          "sdNeg" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                          "nNeg" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR))]))))

EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$low95 <- EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$EATR - qt(0.975, df= EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$n-1) * EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$sd /sqrt(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$n)
EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$high95 <- EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$EATR + qt(0.975, df= EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$n-1) * EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$sd /sqrt(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$n)

EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]] <- EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][!is.na(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$EATR) | !is.na(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$EATRNeg),]








## weighted


EATR.List.Reinv[["Byanyown"]][["EATRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                        "EATR" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum / sum(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum >0 )$sum)))), 
                                                        "sd" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, sqrt(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum^2), na.rm = TRUE )))),
                                                        "n" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR))]))),
                                                        "EATRNeg" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum / sum(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum <0 )$sum)))), 
                                                        "sdNeg" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, sqrt(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum^2), na.rm = TRUE )))),
                                                        "nNeg" = c(sapply(1:length(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR))])))
)




EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$low95 <- EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$EATR - qt(0.975, df= EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$n-1) * EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$sd /sqrt(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$n)
EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$high95 <- EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$EATR + qt(0.975, df= EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$n-1) * EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$sd /sqrt(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$n)


EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$EATR[EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$EATR == 1] <- NA
EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$EATRNeg[EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$EATRNeg == 1] <- NA


EATR.List.Reinv[["Byanyown"]][["EATRweighted"]] <- EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][!is.na(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$EATR) | !is.na(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$EATRNeg),]





## intermed 



EATR.List.Reinv[["Byintermed"]][["CompanyList"]] <- vector(mode = "list")

for(i in 2:12) {EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[(i-1)]] <- EdgelistByintermed[[i]]}
names(EATR.List.Reinv[["Byintermed"]][["CompanyList"]]) <- paste(2020:2010)

for(i in 1:length(EATR.List.Reinv[["Byintermed"]][["CompanyList"]])) {EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][["DE"]] <- NULL}


for (i in 1:length(EATR.List.Reinv[["Byintermed"]][["CompanyList"]])) {
  for (j in 1:length(EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]])) {
    
    if (!isTruthy(EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][[j]])) {next}
    if (all(is.na(EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][[j]][,1]))) {next}
    if (nrow(EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][[j]]) < 20) {EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][[j]] <- rbind(EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][[j]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == names(EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][j]))))
    Temp2 <- apply(EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][[j]],1, function (y) as.numeric(which(Nodelist.List[[i+1]]$CompanyISO[match(y, Nodelist.List[[i+1]]$CompanyBvDID)] == "DE")))
    Temp3 <- sapply(1:length(Temp2), function (z) Temp2[[z]] > first(Temp1[[z]]))
    
    Temp4 <- EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][[j]]
    for (g in 1:ncol(Temp4)) {Temp4[,g] <- g}
    
    for (g in 1:nrow(Temp4)) {
      for(h in 1:ncol(Temp4)) {
        Temp4[g,h] <- ifelse(Temp4[g,h] %in% Temp2[[g]], as.character(Temp3[[g]][Temp2[[g]] == Temp4[g,h]]) , "FALSE")
      }
    }
    
    Temp4 <- sapply(Temp4, function (y) y == "TRUE")
    
    EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][[j]] <- EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[i]][[j]][Temp4]
  }}




EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]])) {
  Temp1 <- EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[i]])) {
    Temp1 <- unique(c(Temp1,EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[i]] <- subset(CF, CF$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[i]]) == 0 ) {EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]])) {
  for (i in 1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]])) {
    for (j in 2:ncol(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]])) {
      EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]][i,j])) & EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]][i,1] %in% EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]][i,j], NA)
    }}}



EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]] <- vector(mode = "list")
for (i in 1:length(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"])) {EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[i]] <- data.frame("CompanyBVDID" = c(NA))}
names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) <- na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]

for (i in 1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]])) {
  Temp1 <- EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[1]][[i]]
  for (j in 2:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[i]])) {
    Temp1 <- unique(c(Temp1,EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[j]][[i]]))
  }
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[i]] <- subset(Tax, Tax$CompanyBVDID %in% Temp1)
  if (nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[i]]) == 0 ) {EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[i]][1,] <- NA}
}

for (x in 1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]])) {
  for (i in 1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]])) {
    for (j in 2:ncol(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]])) {
      EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j])) & EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]][i,1] %in% EATR.List.Reinv[["Byintermed"]][["CompanyList"]][[(j-1)]][[x]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]][i,j], NA)
    }}}









for (z in 1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]])) {
  
  
  
  NPVBThelp <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[z]]
  
  
  for(i in 1:nrow(NPVBThelp)) {
    for(j in 2:ncol(NPVBThelp)) {
      NPVBThelp[i,j] <- ifelse(!is.na(as.numeric(NPVBThelp[i,j])),  (as.numeric(NPVBThelp[i,j]) + as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[z]][i,j])  ) , NA)
    }}
  
  
  GRTemp <- t(apply(NPVBThelp ,1,function(x) {c(x[!is.na(x)],x[is.na(x)])}))
  
  GRTemp <- as.matrix(sapply(2:ncol(GRTemp), function(y) as.numeric(GRTemp[,(y-1)]) / as.numeric(GRTemp[,y])))
  
  GRTemp <- lapply(1:nrow(GRTemp), function(x)   as.numeric(GRTemp[x,][!is.na(as.numeric(GRTemp[x,]))]))
  
  
  
  TaxTemp <- lapply(1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[z]]), function(x) na.omit(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[z]][x,])))
  
  NPVBTTemp <- lapply(1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[z]]), function(x)  as.numeric(last(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[z]][x,][!is.na(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[z]][x,]))])))
  
  NPVBTTemp <- lapply(1:length(NPVBTTemp), function(x)  NPVBTTemp[[x]]+last(TaxTemp[[x]]))
  
  
  for (x in 1:length(TaxTemp)) {
    
    
    if(!isTruthy(TaxTemp[[x]])) {next}
    
    TaxTemp[[x]] <- TaxTemp[[x]][1:(length(TaxTemp[[x]])-1)]
    
  }
  
  
  
  for(x in 1:length(NPVBTTemp)) {
    
    if (!isTruthy(NPVBTTemp[[x]])) {next}  
    if (!isTruthy(TaxTemp[[x]])) {next}    
    if (!isTruthy(GRTemp[[x]])) {next}    
    
    for (i in length(GRTemp[[x]]):1) {
      
      
      NPVBTTemp[[x]] <-   c(first(NPVBTTemp[[x]])*GRTemp[[x]][i]+TaxTemp[[x]][i],NPVBTTemp[[x]])  
      
    }
  }
  
  
  
  
  HelpDisc <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[z]]
  
  for(i in 1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[z]])) {
    
    
    HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))] <- 1.05^seq(length(HelpDisc[i,][!is.na(as.numeric(HelpDisc[i,]))]),1)
    
  }
  
  
  
  
  
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[z]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[z]]
  
  for(i in 1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[z]])) {
    for(j in 2:ncol(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[z]])) {
      
      
      EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[z]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[z]][i,j])),  (as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[z]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
      
      
    }
  }
  
  
  
  
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[z]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[z]]
  
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[z]]$sum <- NULL
  
  for(i in 1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[z]])) {
    
    
    EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[z]][i,][!is.na(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[z]][i,]))] <- NPVBTTemp[[i]]
    
  }
  
  
  
  for(i in 1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[z]])) {
    for(j in 2:ncol(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[z]])) {
      
      
      EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[z]][i,j] <- ifelse(!is.na(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[z]][i,j])),  (as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[z]][i,j]) / as.numeric(HelpDisc[i,j])) , NA)
      
      
    }
  }
}


names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) <- names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]])
names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]]) <- names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]])







for(x in 1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]])) {
  
  
  
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum <- sapply(1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]]), function (y) sum(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum[EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum == 0] <- NA
  
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$sum <- sapply(1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]), function (y) sum(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$sum[EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$sum == 0] <- NA
  
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum <- sapply(1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]), function (y) sum(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum[EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum == 0] <- NA
  
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum <- sapply(1:nrow(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]), function (y) sum(as.numeric(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][y,2:11]) , na.rm = TRUE ))
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum[EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum == 0] <- NA
  
  
  
  
  
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][!EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][!EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]][!EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]][!EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]]$sum < 0,]
  
  
  
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR  <- 1 - EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[x]]$sum / EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[x]]$sum
  
  
  
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[x]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[x]][!EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[x]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[x]][!EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedCF"]][[x]][!EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]][!EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR < -10 & !EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[x]]$EATR > 10,]
  
  
  
}







EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                                "EATR" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                                "sd" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, na.rm = TRUE))), 
                                                                "n" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR))]))),
                                                                "EATRNeg" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) mean(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                                "sdNeg" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) sd(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, na.rm = TRUE))), 
                                                                "nNeg" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR))]))))

EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$low95 <- EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$EATR - qt(0.975, df= EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$n-1) * EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$sd /sqrt(EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$n)
EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$high95 <- EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$EATR + qt(0.975, df= EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$n-1) * EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$sd /sqrt(EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$n)

EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]] <- EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]][!is.na(EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$EATR) | !is.na(EATR.List.Reinv[["Byintermed"]][["EATRunweighted"]]$EATRNeg),]








## weighted


EATR.List.Reinv[["Byintermed"]][["EATRweighted"]] <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))[na.omit(unique(NodelistALL$CompanyISO)) != "DE"]), 
                                                              "EATR" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum / sum(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum >0 )$sum)))), 
                                                              "sd" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR, sqrt(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum >0)$sum^2), na.rm = TRUE )))),
                                                              "n" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR > 0)$EATR))]))),
                                                              "EATRNeg" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) 1 - sum(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 -  EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum / sum(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[y]], 1 - EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum <0 )$sum)))), 
                                                              "sdNeg" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) sqrt(wtd.var(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR, sqrt(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[y]], 1 - EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][[y]]$sum / EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][[y]]$sum <0)$sum^2), na.rm = TRUE )))),
                                                              "nNeg" = c(sapply(1:length(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]),function(y) length(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR[!is.na(as.numeric(subset(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]], EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][[y]]$EATR < 0)$EATR))])))
)




EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$low95 <- EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$EATR - qt(0.975, df= EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$n-1) * EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$sd /sqrt(EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$n)
EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$high95 <- EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$EATR + qt(0.975, df= EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$n-1) * EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$sd /sqrt(EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$n)


EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$EATR[EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$EATR == 1] <- NA
EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$EATRNeg[EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$EATRNeg == 1] <- NA


EATR.List.Reinv[["Byintermed"]][["EATRweighted"]] <- EATR.List.Reinv[["Byintermed"]][["EATRweighted"]][!is.na(EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$EATR) | !is.na(EATR.List.Reinv[["Byintermed"]][["EATRweighted"]]$EATRNeg),]







## Append rows for DeInt, DeDom, Affiliates, GermanGUO, Tax Havens and EU Tax havens to anyown


EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavens"), 
                                                                "EATR" = mean(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "sd" = sd(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "n" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR))]),
                                                                "EATRNeg" = mean(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "sdNeg" = sd(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "nNeg" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR))]),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEU"), 
                                                                "EATR" = mean(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "sd" = sd(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "n" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR))]),
                                                                "EATRNeg" = mean(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "sdNeg" = sd(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "nNeg" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR))]),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]], 
                                                     
                                                     data.frame("ISO" = c("TaxHavensEUProxy"), 
                                                                "EATR" = mean(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "sd" = sd(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, na.rm = TRUE), 
                                                                "n" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR))]),
                                                                "EATRNeg" = mean(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "sdNeg" = sd(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, na.rm = TRUE), 
                                                                "nNeg" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR))]),
                                                                "low95" = c(NA),
                                                                "high95" = c(NA)
                                                     ))



EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]] <- rbind(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],
                                                     EATR.List.Reinv[["DeDom"]][["EATRunweighted"]]
)






## weighted


EATR.List.Reinv[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavens"), 
                                                              "EATR" = 1 - sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 -  (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum >0)$sum / sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum >0 )$sum)),
                                                              "sd" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR > 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum >0)$sum^2), na.rm = TRUE )),
                                                              "n" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$sum > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$sum > 0)$EATR))]),
                                                              "EATRNeg" = 1 - sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 -  (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum <0)$sum / sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum <0 )$sum)),
                                                              "sdNeg" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$EATR < 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% Taxhavens])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% Taxhavens])))$sum <0)$sum^2), na.rm = TRUE )),
                                                              "nNeg" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$sum < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% Taxhavens])))$sum < 0)$EATR))]),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))







EATR.List.Reinv[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavensEU"), 
                                                              "EATR" = 1 - sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum / sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0 )$sum)),
                                                              "sd" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum^2), na.rm = TRUE )),
                                                              "n" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum > 0)$EATR))]),
                                                              "EATRNeg" = 1 - sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum / sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0 )$sum)),
                                                              "sdNeg" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum^2), na.rm = TRUE )),
                                                              "nNeg" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum < 0)$EATR))]),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))





EATR.List.Reinv[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]], 
                                                   
                                                   data.frame("ISO" = c("TaxHavensEUProxy"), 
                                                              "EATR" = 1 - sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum / sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0 )$sum)),
                                                              "sd" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum^2), na.rm = TRUE )),
                                                              "n" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum > 0)$EATR))]),
                                                              "EATRNeg" = 1 - sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum / sum(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0 )$sum)),
                                                              "sdNeg" = sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR < 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum <0)$sum^2), na.rm = TRUE )),
                                                              "nNeg" = length(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum < 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List.Reinv[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum < 0)$EATR))]),
                                                              "low95" = c(NA),
                                                              "high95" = c(NA)
                                                   ))




EATR.List.Reinv[["Byanyown"]][["EATRweighted"]] <- rbind(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],
                                                   EATR.List.Reinv[["DeDom"]][["EATRweighted"]]
)









































1 - sum(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 -  (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum / sum(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0 )$sum))

sqrt(wtd.var(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$EATR > 0)$EATR, sqrt(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU]))), 1 - (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))$sum / (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))$sum >0)$sum^2), na.rm = TRUE ))

length(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum > 0)$EATR[!is.na(as.numeric(subset((unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU]))), (unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))$sum > 0)$EATR))])





EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVAT"]]
EATR.List.Reinv[["Byanyown"]][["EATRunweightedNPVBT"]]
EATR.List.Reinv[["Byanyown"]][["EATRunweightedTax"]]


(unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVAT"]]) %in% TaxhavensEU])))
(unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]][names(EATR.List[["Byintermed"]][["EATRunweightedNPVBT"]]) %in% TaxhavensEU])))
(unique(Reduce("rbind",EATR.List[["Byintermed"]][["EATRunweightedTax"]][names(EATR.List[["Byintermed"]][["EATRunweightedTax"]]) %in% TaxhavensEU])))
















