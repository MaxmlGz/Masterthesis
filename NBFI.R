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






##overall

Edgelist.GerSub <- pblapply(1:length(Edgelist.List.Filtered), function(x) Edgelist.List.Filtered[[x]][apply(Edgelist.List.Filtered[[x]],1,function(y) Nodelist.List[[x]]$CompanyISO[match(last(y[!is.na(y)]),Nodelist.List[[x]]$CompanyBvDID)] == "DE"),])

NBFI <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^64|^66") & Nodelist.List[[x]]$CompanyType != "Bank"))
GermanEnergy.List <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^35") & Nodelist.List[[x]]$CompanyISO == "DE"))

NBFIPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% NBFI[[x]]$CompanyBvDID)))
GermanEnergyPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% GermanEnergy.List[[x]]$CompanyBvDID)))

NBFIFilter <- pblapply(1:113, function(x) lapply(1:length(NBFIPosition[[x]]), function (y) first(NBFIPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))

NBFIOwner <- pblapply(1:length(NBFIFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(NBFIFilter[[x]]),], !is.na(X1)))

NBFIShare <- sapply(1:length(NBFIOwner), function(x) nrow(NBFIOwner[[x]]) / nrow(Edgelist.GerSub[[x]]))

NBFIShareDF <- data.frame("Year" = c(names(Edgelist.List.Filtered[1:113])), "NBFIShare" = c(unlist(NBFIShare)))






##affiliates

Edgelist.GerSub <- pblapply(1:length(Edgelist.List.Filtered), function(x) Edgelist.List.Filtered[[x]][apply(Edgelist.List.Filtered[[x]],1,function(y) Nodelist.List[[x]]$CompanyISO[match(last(y[!is.na(y)]),Nodelist.List[[x]]$CompanyBvDID)] == "DE"),])

NBFI <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^64|^66") & Nodelist.List[[x]]$CompanyType != "Bank"))
GermanEnergy.List <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^35") & Nodelist.List[[x]]$CompanyISO == "DE"))
List.Internationals <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], Nodelist.List[[x]]$CompanyISO != "DE"))

InternationalsPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% List.Internationals[[x]]$CompanyBvDID)))
GermanEnergyPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% GermanEnergy.List[[x]]$CompanyBvDID)))

AffiliatesFilter <- pblapply(1:154, function(x) lapply(1:length(InternationalsPosition[[x]]), function (y) first(InternationalsPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))
Affiliates.List <- pblapply(1:length(AffiliatesFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(AffiliatesFilter[[x]]),], !is.na(X1)))

NBFIPositionAffiliates <- pblapply(1:length(Affiliates.List), function(x) apply(Affiliates.List[[x]],1,function(y) which(y %in% NBFI[[x]]$CompanyBvDID)))

NBFIAffiliatesFilter <- pblapply(1:113, function(x) lapply(1:length(NBFIPositionAffiliates[[x]]), function (y) first(NBFIPositionAffiliates[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))

NBFIAffiliatesOwner <- pblapply(1:length(NBFIAffiliatesFilter), function(x) subset(Affiliates.List[[x]][unlist(NBFIAffiliatesFilter[[x]]),], !is.na(X1)))

NBFIAffiliatesShare <- sapply(1:length(NBFIAffiliatesOwner), function(x) nrow(NBFIAffiliatesOwner[[x]]) / nrow(Affiliates.List[[x]]))

NBFIAffiliatesShareDF <- data.frame("Year" = c(names(Edgelist.List.Filtered[1:113])), "NBFIShare" = c(unlist(NBFIAffiliatesShare)))



##TaxHaven

NBFITaxhaven <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^64|^66") & Nodelist.List[[x]]$CompanyType != "Bank" & Nodelist.List[[x]]$CompanyISO %in% Taxhavens))
TaxHavenCompanies <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], Nodelist.List[[x]]$CompanyISO %in% Taxhavens))

NBFITaxhavenPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% NBFITaxhaven[[x]]$CompanyBvDID)))
TaxHavenCompaniesPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% TaxHavenCompanies[[x]]$CompanyBvDID)))

NBFITaxhavenFilter <- pblapply(1:21, function(x) lapply(1:length(NBFITaxhavenPosition[[x]]), function (y) first(NBFITaxhavenPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))
TaxHavenCompaniesFilter <- pblapply(1:37, function(x) lapply(1:length(TaxHavenCompaniesPosition[[x]]), function (y) first(TaxHavenCompaniesPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))

NBFITaxhavenOwner <- pblapply(1:length(NBFITaxhavenFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(NBFITaxhavenFilter[[x]]),], !is.na(X1)))
TaxHavenCompaniesOwner <- pblapply(1:length(TaxHavenCompaniesFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(TaxHavenCompaniesFilter[[x]]),], !is.na(X1)))

TaxHavenNBFIShare <- sapply(1:length(NBFITaxhavenOwner), function(x) nrow(NBFITaxhavenOwner[[x]]) / nrow(TaxHavenCompaniesOwner[[x]]))

TaxHavenNBFIShareDF <- data.frame("Year" = c(names(Edgelist.List.Filtered[1:21])), "NBFIShare" = c(unlist(TaxHavenNBFIShare)))


##TaxHavenEU

NBFITaxhavenEU <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^64|^66") & Nodelist.List[[x]]$CompanyType != "Bank" & Nodelist.List[[x]]$CompanyISO %in% TaxhavensEU))
TaxHavenEUCompanies <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], Nodelist.List[[x]]$CompanyISO %in% TaxhavensEU))

NBFITaxhavenEUPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% NBFITaxhavenEU[[x]]$CompanyBvDID)))
TaxHavenEUCompaniesPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% TaxHavenEUCompanies[[x]]$CompanyBvDID)))

NBFITaxhavenEUFilter <- pblapply(1:113, function(x) lapply(1:length(NBFITaxhavenEUPosition[[x]]), function (y) first(NBFITaxhavenEUPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))
TaxHavenEUCompaniesFilter <- pblapply(1:113, function(x) lapply(1:length(TaxHavenEUCompaniesPosition[[x]]), function (y) first(TaxHavenEUCompaniesPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))

NBFITaxhavenEUOwner <- pblapply(1:length(NBFITaxhavenEUFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(NBFITaxhavenEUFilter[[x]]),], !is.na(X1)))
TaxHavenEUCompaniesOwner <- pblapply(1:length(TaxHavenEUCompaniesFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(TaxHavenEUCompaniesFilter[[x]]),], !is.na(X1)))

TaxHavenEUNBFIShare <- sapply(1:length(NBFITaxhavenEUOwner), function(x) nrow(NBFITaxhavenEUOwner[[x]]) / nrow(TaxHavenEUCompaniesOwner[[x]]))

TaxHavenEUNBFIShareDF <- data.frame("Year" = c(names(Edgelist.List.Filtered[1:113])), "NBFIShare" = c(unlist(TaxHavenEUNBFIShare)))


## domestic 2 level

DomControl <- lapply(1:length(EdgelistDeDom), function (x) EdgelistDeDom[[x]][!is.na(EdgelistDeDom[[x]]$X2),])

NBFI <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^64|^66") & Nodelist.List[[x]]$CompanyType != "Bank"))
GermanEnergy.List <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^35") & Nodelist.List[[x]]$CompanyISO == "DE"))

NBFIPosition <- pblapply(1:length(DomControl), function(x) apply(DomControl[[x]],1,function(y) which(y %in% NBFI[[x]]$CompanyBvDID)))
GermanEnergyPosition <- pblapply(1:length(DomControl), function(x) apply(DomControl[[x]],1,function(y) which(y %in% GermanEnergy.List[[x]]$CompanyBvDID)))

NBFIFilterDom <- pblapply(1:90, function(x) lapply(1:length(NBFIPosition[[x]]), function (y) first(NBFIPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))

NBFIOwnerDom <- pblapply(1:length(NBFIFilterDom), function(x) subset(DomControl[[x]][unlist(NBFIFilterDom[[x]]),], !is.na(X1)))

NBFIShareDom <- sapply(1:length(NBFIOwnerDom), function(x) nrow(NBFIOwnerDom[[x]]) / nrow(DomControl[[x]]))

NBFIShareDomDF <- data.frame("Year" = c(names(Edgelist.List.Filtered[1:90])), "NBFIShareDom" = c(unlist(NBFIShareDom)))



##noholding



##overall

Edgelist.GerSub <- pblapply(1:length(Edgelist.List.Filtered), function(x) Edgelist.List.Filtered[[x]][apply(Edgelist.List.Filtered[[x]],1,function(y) Nodelist.List[[x]]$CompanyISO[match(last(y[!is.na(y)]),Nodelist.List[[x]]$CompanyBvDID)] == "DE"),])

NBFInoholding <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^641|^643|^649|^66") & Nodelist.List[[x]]$CompanyType != "Bank"))
GermanEnergy.List <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^35") & Nodelist.List[[x]]$CompanyISO == "DE"))

NBFInoholdingPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% NBFInoholding[[x]]$CompanyBvDID)))
GermanEnergyPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% GermanEnergy.List[[x]]$CompanyBvDID)))

NBFInoholdingFilter <- pblapply(1:53, function(x) lapply(1:length(NBFInoholdingPosition[[x]]), function (y) first(NBFInoholdingPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))

NBFInoholdingOwner <- pblapply(1:length(NBFInoholdingFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(NBFInoholdingFilter[[x]]),], !is.na(X1)))

NBFInoholdingShare <- sapply(1:length(NBFInoholdingOwner), function(x) nrow(NBFInoholdingOwner[[x]]) / nrow(Edgelist.GerSub[[x]]))

NBFInoholdingShareDF <- data.frame("Year" = c(names(Edgelist.List.Filtered[1:53])), "NBFInoholdingShare" = c(unlist(NBFInoholdingShare)))






##affiliates

Edgelist.GerSub <- pblapply(1:length(Edgelist.List.Filtered), function(x) Edgelist.List.Filtered[[x]][apply(Edgelist.List.Filtered[[x]],1,function(y) Nodelist.List[[x]]$CompanyISO[match(last(y[!is.na(y)]),Nodelist.List[[x]]$CompanyBvDID)] == "DE"),])

NBFInoholding <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^641|^643|^649|^66") & Nodelist.List[[x]]$CompanyType != "Bank"))
GermanEnergy.List <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^35") & Nodelist.List[[x]]$CompanyISO == "DE"))
List.Internationals <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], Nodelist.List[[x]]$CompanyISO != "DE"))

InternationalsPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% List.Internationals[[x]]$CompanyBvDID)))
GermanEnergyPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% GermanEnergy.List[[x]]$CompanyBvDID)))

AffiliatesFilter <- pblapply(1:154, function(x) lapply(1:length(InternationalsPosition[[x]]), function (y) first(InternationalsPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))
Affiliates.List <- pblapply(1:length(AffiliatesFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(AffiliatesFilter[[x]]),], !is.na(X1)))

NBFInoholdingPositionAffiliates <- pblapply(1:length(Affiliates.List), function(x) apply(Affiliates.List[[x]],1,function(y) which(y %in% NBFInoholding[[x]]$CompanyBvDID)))

NBFInoholdingAffiliatesFilter <- pblapply(1:22, function(x) lapply(1:length(NBFInoholdingPositionAffiliates[[x]]), function (y) first(NBFInoholdingPositionAffiliates[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))

NBFInoholdingAffiliatesOwner <- pblapply(1:length(NBFInoholdingAffiliatesFilter), function(x) subset(Affiliates.List[[x]][unlist(NBFInoholdingAffiliatesFilter[[x]]),], !is.na(X1)))

NBFInoholdingAffiliatesShare <- sapply(1:length(NBFInoholdingAffiliatesOwner), function(x) nrow(NBFInoholdingAffiliatesOwner[[x]]) / nrow(Affiliates.List[[x]]))

NBFInoholdingAffiliatesShareDF <- data.frame("Year" = c(names(Edgelist.List.Filtered[1:22])), "NBFInoholdingShare" = c(unlist(NBFInoholdingAffiliatesShare)))



##TaxHaven

NBFInoholdingTaxhaven <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^641|^643|^649|^66") & Nodelist.List[[x]]$CompanyType != "Bank" & Nodelist.List[[x]]$CompanyISO %in% Taxhavens))
TaxHavenCompanies <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], Nodelist.List[[x]]$CompanyISO %in% Taxhavens))

NBFInoholdingTaxhavenPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% NBFInoholdingTaxhaven[[x]]$CompanyBvDID)))
TaxHavenCompaniesPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% TaxHavenCompanies[[x]]$CompanyBvDID)))

NBFInoholdingTaxhavenFilter <- pblapply(1:17, function(x) lapply(1:length(NBFInoholdingTaxhavenPosition[[x]]), function (y) first(NBFInoholdingTaxhavenPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))
TaxHavenCompaniesFilter <- pblapply(1:37, function(x) lapply(1:length(TaxHavenCompaniesPosition[[x]]), function (y) first(TaxHavenCompaniesPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))

NBFInoholdingTaxhavenOwner <- pblapply(1:length(NBFInoholdingTaxhavenFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(NBFInoholdingTaxhavenFilter[[x]]),], !is.na(X1)))
TaxHavenCompaniesOwner <- pblapply(1:length(TaxHavenCompaniesFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(TaxHavenCompaniesFilter[[x]]),], !is.na(X1)))

TaxHavenNBFInoholdingShare <- sapply(1:length(NBFInoholdingTaxhavenOwner), function(x) nrow(NBFInoholdingTaxhavenOwner[[x]]) / nrow(TaxHavenCompaniesOwner[[x]]))

TaxHavenNBFInoholdingShareDF <- data.frame("Year" = c(names(Edgelist.List.Filtered[1:17])), "NBFInoholdingShare" = c(unlist(TaxHavenNBFInoholdingShare)))


##TaxHavenEU

NBFInoholdingTaxhavenEU <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^641|^643|^649|^66") & Nodelist.List[[x]]$CompanyType != "Bank" & Nodelist.List[[x]]$CompanyISO %in% TaxhavensEU))
TaxHavenEUCompanies <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], Nodelist.List[[x]]$CompanyISO %in% TaxhavensEU))

NBFInoholdingTaxhavenEUPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% NBFInoholdingTaxhavenEU[[x]]$CompanyBvDID)))
TaxHavenEUCompaniesPosition <- pblapply(1:length(Edgelist.GerSub), function(x) apply(Edgelist.GerSub[[x]],1,function(y) which(y %in% TaxHavenEUCompanies[[x]]$CompanyBvDID)))

NBFInoholdingTaxhavenEUFilter <- pblapply(1:18, function(x) lapply(1:length(NBFInoholdingTaxhavenEUPosition[[x]]), function (y) first(NBFInoholdingTaxhavenEUPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))
TaxHavenEUCompaniesFilter <- pblapply(1:113, function(x) lapply(1:length(TaxHavenEUCompaniesPosition[[x]]), function (y) first(TaxHavenEUCompaniesPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))

NBFInoholdingTaxhavenEUOwner <- pblapply(1:length(NBFInoholdingTaxhavenEUFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(NBFInoholdingTaxhavenEUFilter[[x]]),], !is.na(X1)))
TaxHavenEUCompaniesOwner <- pblapply(1:length(TaxHavenEUCompaniesFilter), function(x) subset(Edgelist.GerSub[[x]][unlist(TaxHavenEUCompaniesFilter[[x]]),], !is.na(X1)))

TaxHavenEUNBFInoholdingShare <- sapply(1:length(NBFInoholdingTaxhavenEUOwner), function(x) nrow(NBFInoholdingTaxhavenEUOwner[[x]]) / nrow(TaxHavenEUCompaniesOwner[[x]]))

TaxHavenEUNBFInoholdingShareDF <- data.frame("Year" = c(names(Edgelist.List.Filtered[1:18])), "NBFInoholdingShare" = c(unlist(TaxHavenEUNBFInoholdingShare)))


## domestic 2 level

DomControl <- lapply(1:length(EdgelistDeDom), function (x) EdgelistDeDom[[x]][!is.na(EdgelistDeDom[[x]]$X2),])

NBFInoholding <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^641|^643|^649|^66") & Nodelist.List[[x]]$CompanyType != "Bank"))
GermanEnergy.List <- pblapply(1:length(Nodelist.List), function(x) subset(Nodelist.List[[x]], str_detect(Nodelist.List[[x]]$CompanyNACECore, "^35") & Nodelist.List[[x]]$CompanyISO == "DE"))

NBFInoholdingPosition <- pblapply(1:length(DomControl), function(x) apply(DomControl[[x]],1,function(y) which(y %in% NBFInoholding[[x]]$CompanyBvDID)))
GermanEnergyPosition <- pblapply(1:length(DomControl), function(x) apply(DomControl[[x]],1,function(y) which(y %in% GermanEnergy.List[[x]]$CompanyBvDID)))

NBFInoholdingFilterDom <- pblapply(1:53, function(x) lapply(1:length(NBFInoholdingPosition[[x]]), function (y) first(NBFInoholdingPosition[[x]][[y]] < last(GermanEnergyPosition[[x]][[y]]))))

NBFInoholdingOwnerDom <- pblapply(1:length(NBFInoholdingFilterDom), function(x) subset(DomControl[[x]][unlist(NBFInoholdingFilterDom[[x]]),], !is.na(X1)))

NBFInoholdingShareDom <- sapply(1:length(NBFInoholdingOwnerDom), function(x) nrow(NBFInoholdingOwnerDom[[x]]) / nrow(DomControl[[x]]))

NBFInoholdingShareDomDF <- data.frame("Year" = c(names(Edgelist.List.Filtered[1:53])), "NBFInoholdingShareDom" = c(unlist(NBFInoholdingShareDom)))





## cleanup

NBFI.List <- list(
  "NBFI" <- NBFI,
  "NBFIOwner" = NBFIOwner,
  "NBFIOwnerDom" = NBFIOwnerDom,
  "List.Internationals" = List.Internationals,
  "Affiliates.List" = Affiliates.List,
  "NBFIAffiliatesOwner" = NBFIAffiliatesOwner,
  "NBFITaxhaven" = NBFITaxhaven,
  "NBFITaxHavenEU" = NBFITaxhavenEU,
  "TaxHavenCompanies" = TaxHavenCompanies,
  "NBFITaxhavenOwner" = NBFITaxhavenOwner,
  "TaxHavenCompaniesOwner" =TaxHavenCompaniesOwner,
  "TaxHavenEUCompanies" = TaxHavenEUCompanies,
  "NBFITaxhavenEUOwner" = NBFITaxhavenEUOwner,
  "TaxHavenEUCompaniesOwner" =TaxHavenEUCompaniesOwner,
  "NBFInoholding" <- NBFInoholding,
  "NBFInoholdingOwner" = NBFInoholdingOwner,
  "NBFInoholdingOwnerDom" = NBFInoholdingOwnerDom,
  "NBFInoholdingAffiliatesOwner" = NBFInoholdingAffiliatesOwner,
  "NBFInoholdingTaxhaven" = NBFInoholdingTaxhaven,
  "NBFInoholdingTaxhavenOwner" = NBFInoholdingTaxhavenOwner,
  "NBFInoholdingTaxhavenEUOwner" = NBFInoholdingTaxhavenEUOwner
)


NBFI.DF.List <- list(
  "NBFIShareDF" = NBFIShareDF,
  "NBFIShareDomDF" = NBFIShareDomDF,
  "NBFIAffiliatesShareDF" = NBFIAffiliatesShareDF,
  "TaxHavenNBFIShareDF" = TaxHavenNBFIShareDF,
  "TaxHavenEUNBFIShareDF" = TaxHavenEUNBFIShareDF,
  "NBFInoholdingShareDF" = NBFInoholdingShareDF,
  "NBFInoholdingShareDomDF" = NBFInoholdingShareDomDF,
  "NBFInoholdingAffiliatesShareDF" = NBFInoholdingAffiliatesShareDF,
  "TaxHavenNBFInoholdingShareDF" = TaxHavenNBFInoholdingShareDF,
  "TaxHavenEUNBFInoholdingShareDF" = TaxHavenEUNBFInoholdingShareDF
)

NBFI.Share.DF <- purrr::reduce(NBFI.DF.List, left_join, by = "Year")
colnames(NBFI.Share.DF)[2:11] <- names(NBFI.DF.List)




rm(NBFI,NBFIPosition,GermanEnergyPosition,NBFIFilter,NBFIOwner,NBFIShare,NBFIShareDF,NBFITaxhaven,TaxHavenCompanies,NBFITaxhavenPosition,TaxHavenCompaniesPosition,NBFITaxhavenFilter,TaxHavenCompaniesFilter,NBFITaxhavenOwner,TaxHavenCompaniesOwner,TaxHavenNBFIShare,TaxHavenNBFIShareDF,NBFITaxhavenEU,TaxHavenEUCompanies,NBFITaxhavenEUPosition,TaxHavenEUCompaniesPosition,NBFITaxhavenEUFilter,TaxHavenEUCompaniesFilter,NBFITaxhavenEUOwner,TaxHavenEUCompaniesOwner,TaxHavenEUNBFIShare,TaxHavenEUNBFIShareDF)
rm(NBFInoholding,NBFInoholdingPosition,GermanEnergyPosition,NBFInoholdingFilter,NBFInoholdingOwner,NBFInoholdingShare,NBFInoholdingShareDF,NBFInoholdingTaxhaven,TaxHavenCompanies,NBFInoholdingTaxhavenPosition,TaxHavenCompaniesPosition,NBFInoholdingTaxhavenFilter,TaxHavenCompaniesFilter,NBFInoholdingTaxhavenOwner,TaxHavenCompaniesOwner,TaxHavenNBFInoholdingShare,TaxHavenNBFInoholdingShareDF,NBFInoholdingTaxhavenEU,TaxHavenEUCompanies,NBFInoholdingTaxhavenEUPosition,TaxHavenEUCompaniesPosition,NBFInoholdingTaxhavenEUFilter,TaxHavenEUCompaniesFilter,NBFInoholdingTaxhavenEUOwner,TaxHavenEUCompaniesOwner,TaxHavenEUNBFInoholdingShare,TaxHavenEUNBFInoholdingShareDF)
rm(NBFI,GermanEnergy.List,List.Internationals,InternationalsPosition,GermanEnergyPosition,AffiliatesFilter,Affiliates.List,NBFIPositionAffiliates,NBFIAffiliatesFilter,NBFIAffiliatesOwner,NBFIAffiliatesShare,NBFIAffiliatesShareDF,NBFInoholding,GermanEnergy.List,List.Internationals,InternationalsPosition,GermanEnergyPosition,AffiliatesFilter,Affiliates.List,NBFInoholdingPositionAffiliates,NBFInoholdingAffiliatesFilter,NBFInoholdingAffiliatesOwner,NBFInoholdingAffiliatesShare,NBFInoholdingAffiliatesShareDF)
rm(DomControl,NBFIFilterDom,NBFInoholdingFilterDom,NBFInoholdingOwnerDom,NBFInoholdingShareDom,NBFIOwnerDom,NBFIShareDomDF,NBFInoholdingShareDomDF,NBFIShareDom)




names(Edgelist.GerSub) <- names(Edgelist.List.Filtered)[1:length(Edgelist.GerSub)]





melt(NBFI.Share.DF[,1:6], id="Year") |>
  subset(!is.na(value)) |>
  subset(Year > 1990) |>
  
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  scale_color_manual(values = c("darkgrey","darkgreen","blue","red","orange" ), labels = c("NBFIShare", "NBFIShareDomestic","NBFIShareAffiliates","NBFIShareSinks","NBFIShareConduits")) +
  labs( x = "Year" , y = "Share") + 
  theme(legend.position = "bottom") +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent)
  



melt(NBFI.Share.DF[,c(1,7:11)], id="Year") |>
  subset(!is.na(value)) |>
  subset(Year > 1990) |>
  
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  scale_color_manual(values = c("darkgrey","darkgreen","blue","red","orange" ), labels = c("NBFIShare", "NBFIShareDomestic","NBFIShareAffiliates","NBFIShareSinks","NBFIShareConduits")) +
  labs( x = "Year" , y = "Share - no holdings") + 
  theme(legend.position = "bottom") +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent)









