## Load libraries

library(plyr)
library(dplyr)
library(data.table)
library(datasets)
library(igraph)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(graphlayouts)
library(RColorBrewer)
library(cluster)
library(rio)
library(stringr)
library(stringi)
library(qdap)
library(sqldf)
library(lubridate)
library(rlist)
library(purrr)
library(taRifx)
library(devtools)
library(splitstackshape)
library(pbapply)
library(maps)
library(geosphere)
library(ggplot2)
library(shiny)
library(magick)
library(reshape2)
library(ggallin)
library(Hmisc)
library(BSDA)
library(marima)
library(lmtest)
library(dynlm)
library(tseries)
library(XML)
library(xml2)
library(foreach)
library(doParallel)
library(snow)
library(doSNOW)
library(fuzzyjoin)
library(shadowtext)



## calculate share of tax havens



Share.DF <- full_join(full_join(
    data.frame("Year" = c(names(Edgelist.List.Filtered[1:length(NBFI.List[["TaxHavenCompaniesOwner"]])])), "ShareTaxHaven" = c(sapply(1:length(NBFI.List[["TaxHavenCompaniesOwner"]]), function(x) nrow(NBFI.List[["TaxHavenCompaniesOwner"]][[x]]) / nrow(Edgelist.GerSub[[x]])))),
    data.frame("Year" = c(names(Edgelist.List.Filtered[1:length(NBFI.List[["TaxHavenEUCompaniesOwner"]])])), "ShareTaxHavenEU" = c(sapply(1:length(NBFI.List[["TaxHavenEUCompaniesOwner"]]), function(x) nrow(NBFI.List[["TaxHavenEUCompaniesOwner"]][[x]]) / nrow(Edgelist.GerSub[[x]])))),
    by = "Year"),
  data.frame("Year" = c(names(Edgelist.List.Filtered[1:length(NBFI.List[["Affiliates.List"]])])), "ShareAffiliates" = c(sapply(1:length(NBFI.List[["Affiliates.List"]]), function(x) nrow(NBFI.List[["Affiliates.List"]][[x]]) / nrow(Edgelist.GerSub[[x]])))),
  by = "Year")

Share.DF <- Share.DF[apply(Share.DF[,2:ncol(Share.DF)],1, function(y) isTruthy(any(y > 0 ))),]
Share.DF[is.na(Share.DF)] <- 0

Share.DF <- full_join(data.frame("Year" = as.character(c(seq(max(Share.DF$Year),min(Share.DF$Year))))), Share.DF, by= "Year")
Share.DF <- Share.DF |>
                        fill(ShareTaxHaven, .direction = "up") |>
                        fill(ShareTaxHavenEU, .direction = "up") |>
                        fill(ShareAffiliates, .direction = "up")

Share.DF$Year <- as.integer(Share.DF$Year)




BNADir <- dir("GesamtdatenexportBNA")
DataBNA <- vector(mode = "list")



## set up multicore and import BNA data

ncores <- parallel::detectCores() -2
MC.Cluster <- parallel:::makeCluster(ncores, type = "PSOCK")
doSNOW::registerDoSNOW(MC.Cluster)

for (i in seq(1,92,4)) {
pb <- txtProgressBar(max = 4, style = 3)
progress <- function(z) setTxtProgressBar(pb, z)
opts <- list(progress = progress)
Temp <-  foreach (h = i:(i+3), .options.snow = opts) %dopar% {XML::xmlToDataFrame(XML::xmlParse(paste0("GesamtdatenexportBNA/",BNADir[h])))}
DataBNA[c(i:(i+3))] <- Temp

save.image(file = ".RData")
print(paste0((i+3),"/", length(BNADir)))
}


close(pb)
stopCluster(MC.Cluster)
rm(pb)
rm(progress)
rm(opts)

names(DataBNA) <- BNADir


## refine data and match VAT


AnlagenEegSolar <- purrr::reduce(DataBNA[grep("^AnlagenEegSolar", names(DataBNA))], rbind)
AnlagenEegSpeicher <- purrr::reduce(DataBNA[grep("^AnlagenEegSpeicher", names(DataBNA))], rbind)
AnlagenStromSpeicher <- purrr::reduce(DataBNA[grep("^AnlagenStromSpeicher", names(DataBNA))], rbind)
EinheitenSolar <- purrr::reduce(DataBNA[grep("^EinheitenSolar", names(DataBNA))], rbind.fill)
EinheitenStromSpeicher <- purrr::reduce(DataBNA[grep("^EinheitenStromSpeicher", names(DataBNA))], rbind.fill)
Marktakteure <- purrr::reduce(DataBNA[grep("^Marktakteure", names(DataBNA))], rbind.fill)

DataBNA[grep("^AnlagenEegSolar", names(DataBNA))] <- NULL
DataBNA[grep("^AnlagenEegSpeicher", names(DataBNA))] <- NULL
DataBNA[grep("^AnlagenStromSpeicher", names(DataBNA))] <- NULL
DataBNA[grep("^EinheitenSolar", names(DataBNA))] <- NULL
DataBNA[grep("^EinheitenStromSpeicher", names(DataBNA))] <- NULL
DataBNA[grep("^Marktakteure", names(DataBNA))] <- NULL

DataBNA[["AnlagenEegSolar"]] <- AnlagenEegSolar
DataBNA[["AnlagenEegSpeicher"]] <- AnlagenEegSpeicher
DataBNA[["AnlagenStromSpeicher"]] <- AnlagenStromSpeicher
DataBNA[["EinheitenSolar"]] <- EinheitenSolar
DataBNA[["EinheitenStromSpeicher"]] <- EinheitenStromSpeicher
DataBNA[["Marktakteure"]] <- Marktakteure

rm(AnlagenEegSolar, AnlagenEegSpeicher, AnlagenStromSpeicher, EinheitenSolar, EinheitenStromSpeicher, Marktakteure)

RenEnergCompaniesBNA <- subset(DataBNA[["Marktakteure"]], DataBNA[["Marktakteure"]]$MastrNummer %in% DataBNA[["EinheitenWind.xml"]]$AnlagenbetreiberMastrNummer | DataBNA[["Marktakteure"]]$MastrNummer %in% DataBNA[["EinheitenBiomasse.xml"]]$AnlagenbetreiberMastrNummer | DataBNA[["Marktakteure"]]$MastrNummer %in% DataBNA[["EinheitenGeoSolarthermieGrubenKlaerschlammDruckentspannung.xml"]]$AnlagenbetreiberMastrNummer | DataBNA[["Marktakteure"]]$MastrNummer %in% DataBNA[["EinheitenWasser.xml"]]$AnlagenbetreiberMastrNummer | DataBNA[["Marktakteure"]]$MastrNummer %in% DataBNA[["EinheitenSolar"]]$AnlagenbetreiberMastrNummer) 
RenEnergCompaniesBNA$Taetigkeitsbeginn <- as.numeric(str_sub(RenEnergCompaniesBNA$Taetigkeitsbeginn,1,4))
RenEnergCompaniesBNA$Taetigkeitsende <- as.numeric(str_sub(RenEnergCompaniesBNA$Taetigkeitsende,1,4))
RenEnergCompaniesBNA$Taetigkeitsbeginn[RenEnergCompaniesBNA$Taetigkeitsbeginn < 1700] <- NA 
RenEnergCompaniesBNA$Taetigkeitsende[RenEnergCompaniesBNA$Taetigkeitsende < 1700] <- NA 
RenEnergCompaniesBNA$Taetigkeitsbeginn[RenEnergCompaniesBNA$Taetigkeitsbeginn > 2021] <- NA 
RenEnergCompaniesBNA$Taetigkeitsende[RenEnergCompaniesBNA$Taetigkeitsende > 2021] <- NA 

ExportVAT <- unique(RenEnergCompaniesBNA$Umsatzsteueridentifikationsnummer)
rio::export(as.data.frame(ExportVAT), "ExportVAT.xlsx")

MatchVATBVD <- rio::import("MatchVATBVD.xlsx", which = "Results")




Share.DF.Change <- Share.DF

Share.DF.Change$ShareTaxHaven[1:(length(Share.DF.Change$ShareTaxHaven)-1)] <-   sapply(2:length(Share.DF.Change$ShareTaxHaven), function(y) (Share.DF.Change$ShareTaxHaven[(y-1)] - Share.DF.Change$ShareTaxHaven[y])  / Share.DF.Change$ShareTaxHaven[y])
Share.DF.Change$ShareTaxHaven[length(Share.DF.Change$ShareTaxHaven)] <- NA

Share.DF.Change$ShareTaxHavenEU[1:(length(Share.DF.Change$ShareTaxHavenEU)-1)] <-   sapply(2:length(Share.DF.Change$ShareTaxHavenEU), function(y) (Share.DF.Change$ShareTaxHavenEU[(y-1)] - Share.DF.Change$ShareTaxHavenEU[y])  / Share.DF.Change$ShareTaxHavenEU[y])
Share.DF.Change$ShareTaxHavenEU[length(Share.DF.Change$ShareTaxHavenEU)] <- NA

Share.DF.Change$ShareAffiliates[1:(length(Share.DF.Change$ShareAffiliates)-1)] <-   sapply(2:length(Share.DF.Change$ShareAffiliates), function(y) (Share.DF.Change$ShareAffiliates[(y-1)] - Share.DF.Change$ShareAffiliates[y])  / Share.DF.Change$ShareAffiliates[y])
Share.DF.Change$ShareAffiliates[length(Share.DF.Change$ShareAffiliates)] <- NA

Share.DF.Change[Share.DF.Change == "Inf"] <- NA
Share.DF.Change[Share.DF.Change == "NaN"] <- NA

Share.DF.Change$Year <- as.integer(Share.DF.Change$Year)






StringMatchRenewables <- c("WIND","SOLAR","BIO","HYDRO","WASSER","RENEW","ERNEUER","GEOTHERM","PHOTO","WATER","NATURGAS","NATURE","NATURAL","SUSTAIN")

RenewablesALL <- subset(NodelistALL, str_detect(gsub("[[:punct:]]", "", NodelistALL$CompanyName), paste(StringMatchRenewables, collapse = "|")) | NodelistALL$CompanyBvDID %in% MatchVATBVD$`BvD ID number` )



ShareRenewables <- full_join(
  full_join(
  full_join(
    full_join(
      data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(Edgelist.GerSub)]), "ShareRenALL" = pbsapply(1:length(Edgelist.GerSub), function(y) nrow(Edgelist.GerSub[[y]][apply(Edgelist.GerSub[[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(Edgelist.GerSub[[y]]))),
      data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["Affiliates.List"]])]), "ShareRenAffiliates" = pbsapply(1:length(NBFI.List[["Affiliates.List"]]), function(y) nrow(NBFI.List[["Affiliates.List"]][[y]][apply(NBFI.List[["Affiliates.List"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["Affiliates.List"]][[y]]))),
      by = "Year"),
    data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["TaxHavenCompaniesOwner"]])]), "ShareRenTaxHaven" = pbsapply(1:length(NBFI.List[["TaxHavenCompaniesOwner"]]), function(y) nrow(NBFI.List[["TaxHavenCompaniesOwner"]][[y]][apply(NBFI.List[["TaxHavenCompaniesOwner"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["TaxHavenCompaniesOwner"]][[y]]))),
    by= "Year"),
  data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["TaxHavenEUCompaniesOwner"]])]), "ShareRenTaxHavenEU" = pbsapply(1:length(NBFI.List[["TaxHavenEUCompaniesOwner"]]), function(y) nrow(NBFI.List[["TaxHavenEUCompaniesOwner"]][[y]][apply(NBFI.List[["TaxHavenEUCompaniesOwner"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["TaxHavenEUCompaniesOwner"]][[y]]))),
  by="Year"),
  data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(EdgelistDeDom)]), "ShareRenDom" = pbsapply(1:length(EdgelistDeDom), function(y) nrow(EdgelistDeDom[[y]][apply(EdgelistDeDom[[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(EdgelistDeDom[[y]]))),
  by = "Year"
  )





ShareRenewablesNBFI <- full_join(
  full_join(
  full_join(
    full_join(
      data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["NBFIOwner"]])]), "NBFIShareRenALL" = pbsapply(1:length(NBFI.List[["NBFIOwner"]]), function(y) nrow(NBFI.List[["NBFIOwner"]][[y]][apply(NBFI.List[["NBFIOwner"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["NBFIOwner"]][[y]]))),
      data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["NBFIAffiliatesOwner"]])]), "NBFIShareRenAffiliates" = pbsapply(1:length(NBFI.List[["NBFIAffiliatesOwner"]]), function(y) nrow(NBFI.List[["NBFIAffiliatesOwner"]][[y]][apply(NBFI.List[["NBFIAffiliatesOwner"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["NBFIAffiliatesOwner"]][[y]]))),
      by = "Year"),
    data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["NBFITaxhavenOwner"]])]), "NBFIShareRenTaxhaven" = pbsapply(1:length(NBFI.List[["NBFITaxhavenOwner"]]), function(y) nrow(NBFI.List[["NBFITaxhavenOwner"]][[y]][apply(NBFI.List[["NBFITaxhavenOwner"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["NBFITaxhavenOwner"]][[y]]))),
    by= "Year"),
  data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["NBFITaxhavenEUOwner"]])]), "NBFIShareRenTaxhavenEU" = pbsapply(1:length(NBFI.List[["NBFITaxhavenEUOwner"]]), function(y) nrow(NBFI.List[["NBFITaxhavenEUOwner"]][[y]][apply(NBFI.List[["NBFITaxhavenEUOwner"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["NBFITaxhavenEUOwner"]][[y]]))),
  by = "Year"),
  data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["NBFIOwnerDom"]])]), "NBFIShareRenDom" = pbsapply(1:length(NBFI.List[["NBFIOwnerDom"]]), function(y) nrow(NBFI.List[["NBFIOwnerDom"]][[y]][apply(NBFI.List[["NBFIOwnerDom"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["NBFIOwnerDom"]][[y]]))),
  by= "Year"
)


ShareRenewablesNBFInoholding <- full_join(
  full_join(
  full_join(
    full_join(
      data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["NBFInoholdingOwner"]])]), "NBFNoholdingIShareRenALL" = pbsapply(1:length(NBFI.List[["NBFInoholdingOwner"]]), function(y) nrow(NBFI.List[["NBFInoholdingOwner"]][[y]][apply(NBFI.List[["NBFInoholdingOwner"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["NBFInoholdingOwner"]][[y]]))),
      data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["NBFInoholdingAffiliatesOwner"]])]), "NBFNoholdingIShareRenAffiliates" = pbsapply(1:length(NBFI.List[["NBFInoholdingAffiliatesOwner"]]), function(y) nrow(NBFI.List[["NBFInoholdingAffiliatesOwner"]][[y]][apply(NBFI.List[["NBFInoholdingAffiliatesOwner"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["NBFInoholdingAffiliatesOwner"]][[y]]))),
      by = "Year"),
    data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["NBFInoholdingTaxhavenOwner"]])]), "NBFNoholdingIShareRenTaxhaven" = pbsapply(1:length(NBFI.List[["NBFInoholdingTaxhavenOwner"]]), function(y) nrow(NBFI.List[["NBFInoholdingTaxhavenOwner"]][[y]][apply(NBFI.List[["NBFInoholdingTaxhavenOwner"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["NBFInoholdingTaxhavenOwner"]][[y]]))),
    by= "Year"),
  data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["NBFInoholdingTaxhavenEUOwner"]])]), "NBFNoholdingIShareRenTaxhavenEU" = pbsapply(1:length(NBFI.List[["NBFInoholdingTaxhavenEUOwner"]]), function(y) nrow(NBFI.List[["NBFInoholdingTaxhavenEUOwner"]][[y]][apply(NBFI.List[["NBFInoholdingTaxhavenEUOwner"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["NBFInoholdingTaxhavenEUOwner"]][[y]]))),
  by = "Year"),
  data.frame("Year" = as.integer(names(Edgelist.List.Filtered)[1:length(NBFI.List[["NBFInoholdingOwnerDom"]])]), "NBFInoholdingShareRenDom" = pbsapply(1:length(NBFI.List[["NBFInoholdingOwnerDom"]]), function(y) nrow(NBFI.List[["NBFInoholdingOwnerDom"]][[y]][apply(NBFI.List[["NBFInoholdingOwnerDom"]][[y]],1,function(z) last(z[!is.na(z)]) %in% RenewablesALL$CompanyBvDID),]) / nrow(NBFI.List[["NBFInoholdingOwnerDom"]][[y]]))),
  by = "Year"
)














melt(
    full_join(
    full_join(ShareRenewables, ShareRenewablesNBFI, by = "Year"),
    ShareRenewablesNBFInoholding, by = "Year")
    ,id = "Year")   |>
  
  subset(Year > 1995) |>
  subset(!is.na(value)) |>
  
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, linetype=variable, group=variable), size = 1.5) +
  scale_color_manual(values = c("gray1","blue1","red1","orange1", "green1", "gray2","blue2","red2","orange2","green2","gray3","blue3","red3","orange3","green3"), 
                     labels = c("Sample Average", "Affiliates","Sinks","Conduits","Domestic","NBFI_Average","NBFI_Affiliates","NBFI_Sinks","NBFI_Conduits","NBFI_Domestic","NBFI_noHolding_Average","NBFI_noHolding_Affiliates","NBFI_noHolding_Sinks","NBFI_noHolding_Conduits","NBFI_noHolding_Domestc")) +
  scale_linetype_manual(values= c("solid","solid","solid","solid","solid","dashed","dashed","dashed","dashed","dashed","dotted","dotted","dotted","dotted","dotted"),
                        labels = c("Sample Average", "Affiliates","Sinks","Conduits","Domestic","NBFI_Average","NBFI_Affiliates","NBFI_Sinks","NBFI_Conduits","NBFI_Domestic","NBFI_noHolding_Average","NBFI_noHolding_Affiliates","NBFI_noHolding_Sinks","NBFI_noHolding_Conduits","NBFI_noHolding_Domestc")) +
  labs( x = "Year" , y = "Share") + 
  theme(legend.position = "bottom",
        text = element_text(size=30),
        legend.title = element_blank())+
  scale_y_continuous(labels = scales::percent)










rio::export(
  full_join(
    full_join(ShareRenewables, ShareRenewablesNBFI, by = "Year"),
    ShareRenewablesNBFInoholding, by = "Year") |>
    
    subset(Year > 1989) ,
  
  "ShareRen.xlsx"
  
  
)













DataBNA[["Marktakteure"]]$Taetigkeitsbeginn <- as.numeric(str_sub(DataBNA[["Marktakteure"]]$Taetigkeitsbeginn,1,4))
DataBNA[["Marktakteure"]]$Taetigkeitsende <- as.numeric(str_sub(DataBNA[["Marktakteure"]]$Taetigkeitsende,1,4))
DataBNA[["Marktakteure"]]$Taetigkeitsbeginn[DataBNA[["Marktakteure"]]$Taetigkeitsbeginn < 1700] <- NA 
DataBNA[["Marktakteure"]]$Taetigkeitsende[DataBNA[["Marktakteure"]]$Taetigkeitsende < 1700] <- NA 
DataBNA[["Marktakteure"]]$Taetigkeitsbeginn[DataBNA[["Marktakteure"]]$Taetigkeitsbeginn > 2021] <- NA 
DataBNA[["Marktakteure"]]$Taetigkeitsende[DataBNA[["Marktakteure"]]$Taetigkeitsende > 2021] <- NA 



EnCompByYear <- vector(mode = "list")
for(i in max(DataBNA[["Marktakteure"]]$Taetigkeitsende, na.rm = TRUE):min(DataBNA[["Marktakteure"]]$Taetigkeitsbeginn, na.rm = TRUE)) {
  EnCompByYear[[((max(DataBNA[["Marktakteure"]]$Taetigkeitsende, na.rm = TRUE)+1)-i)]] <- subset(DataBNA[["Marktakteure"]], DataBNA[["Marktakteure"]]$Taetigkeitsbeginn < (i-1) & (DataBNA[["Marktakteure"]]$Taetigkeitsende > (i+1) | is.na(DataBNA[["Marktakteure"]]$Taetigkeitsende))) 
  names(EnCompByYear)[(max(DataBNA[["Marktakteure"]]$Taetigkeitsende, na.rm = TRUE)+1)-i] <- as.character(i)
}







  Count.DF.BNA <- data.frame(
    "Year" = names(EnCompByYear),
    "Firms" = pbsapply(1:length(EnCompByYear), function(x) nrow(EnCompByYear[[x]])))
  
  

Count.DF.Compare <-  full_join(Count.DF, Count.DF.BNA, by = "Year")
Count.DF.Compare$Chains <- NULL
Count.DF.Compare$Firms.x <- NULL
colnames(Count.DF.Compare) <- c("Year","Firms_Orbis", "Firms_BNA")


    
    
    melt(Count.DF.Compare, id = "Year") |>
    subset(Year > 1989) |>
    
    ggplot() +
    geom_line(aes(x=Year, y=value, color=variable, linetype=variable, group=variable), size = 1.5) +
    labs(x = "Year", y = "Count") +
      theme(legend.position = "bottom",
            text = element_text(size=30),
            axis.text.x = element_text(angle=90, hjust=1, size = 15),
            legend.title = element_blank()) 
    
  
  
  

rio::export(Count.DF.BNA, "FirmCountBNA.xlsx")













