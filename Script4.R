


## In this script i will extract some descriptive information from the ownership chain lists


Edgelist.Countries <- pblapply(1:length(Edgelist.List.Filtered), function(x) as.data.frame({sapply(Edgelist.List.Filtered[[x]], function (y) y = NodelistALL$CompanyISO[match(y, NodelistALL$CompanyBvDID)])}))


Edgelist.Stat <- pblapply(1:length(Edgelist.Countries), function(x) data.frame("ISO" = unique(sapply(as.matrix(Edgelist.Countries[[x]]), function(y) y))))
Edgelist.Stat <- pblapply(1:length(Edgelist.Stat), function(x) as.data.frame(cbind("ISO" = Edgelist.Stat[[x]]$ISO, "Count" = NA)))
Edgelist.Stat <- pblapply(1:length(Edgelist.Stat), function(x) Edgelist.Stat[[x]][!is.na(Edgelist.Stat[[x]]$ISO),])

for (x in 1:length(Edgelist.Stat)) {
for (i in 1:nrow(Edgelist.Stat[[x]])) { 
  Edgelist.Stat[[x]][i,2] <- length(subset(Nodelist.List[[x]], Nodelist.List[[x]]$CompanyISO == Edgelist.Stat[[x]][i,1])$CompanyISO)
}
}
for (x in 1:length(Edgelist.Stat)) {
    Edgelist.Stat[[x]][,2] <- as.numeric(Edgelist.Stat[[x]][,2])
  }


Edgelist.Stat <- pblapply(1:length(Edgelist.Stat), function(x) as.data.frame(cbind(Edgelist.Stat[[x]], "AvgCountPerChain" = NA, "MaxCountPerChain" = NA)))



for (x in 1:length(Edgelist.Stat)) {
  for (i in 1:nrow(Edgelist.Stat[[x]])) { 
    Temp <- apply(Edgelist.Countries[[x]],1, function(y) length(which(y == Edgelist.Stat[[x]][i,1])))[!apply(Edgelist.Countries[[x]],1, function(y) length(which(y == Edgelist.Stat[[x]][i,1]))) == 0]
    Edgelist.Stat[[x]][i,3] <- mean(Temp)
    Edgelist.Stat[[x]][i,4] <- max(as.numeric(Temp))
  }
  print(x / length(Edgelist.Stat))
}



Edgelist.Cooc <- pblapply(1:length(Edgelist.Stat), function(x) data.frame(matrix(NA, ncol = nrow(Edgelist.Stat[[x]]), nrow =  nrow(Edgelist.Stat[[x]]))))
for(x in 1:length(Edgelist.Cooc)) {
  colnames(Edgelist.Cooc[[x]]) <- Edgelist.Stat[[x]]$ISO
  rownames(Edgelist.Cooc[[x]]) <- Edgelist.Stat[[x]]$ISO
} 



for (x in 1:length(Edgelist.Cooc)) {
  for (i in 1:nrow(Edgelist.Cooc[[x]])) {
    for (j in 1:ncol(Edgelist.Cooc[[x]])) {
      Edgelist.Cooc[[x]][i,j] <- nrow(Edgelist.Countries[[x]][apply(Edgelist.Countries[[x]],1, function (y) rownames(Edgelist.Cooc[[x]])[i] %in% y & colnames(Edgelist.Cooc[[x]])[j] %in% y),])
    }
  }
  print(paste0(x,"/",length(Edgelist.Cooc)))
}


Edgelist.Stat <- pblapply(1:length(Edgelist.Stat), function (x) full_join(Edgelist.Stat[[x]], BetwCentr[[x]], by = "ISO"))
Edgelist.Stat <- pblapply(1:length(Edgelist.Stat), function (x) full_join(Edgelist.Stat[[x]], GUOindicator[[x]], by = "ISO"))


DomesticFirmRatioTemp <- pblapply(1:length(Edgelist.List.Filtered), function (x) unique(Edgelist.List.Filtered[[x]][sapply(Edgelist.List.Filtered[[x]], function (y) Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] == "DE")]))

DomesticFirmRatioTemp2 <- pblapply(1:length(Edgelist.List.Filtered), function (x) subset(Edgelist.List.Filtered[[x]], Nodelist.List[[x]]$CompanyISO[match(Edgelist.List.Filtered[[x]]$X1, Nodelist.List[[x]]$CompanyBvDID)] != "DE"))
DomesticFirmRatioTemp2 <- DomesticFirmRatioTemp2[sapply(DomesticFirmRatioTemp2, function(x) dim(x)[1]) > 0]
DomesticFirmRatioTemp2 <- pblapply(1:length(DomesticFirmRatioTemp2), function (x) unique(DomesticFirmRatioTemp[[x]][sapply(DomesticFirmRatioTemp2[[x]], function (y) Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] == "DE")]))

DomesticFirmRatio <- pblapply(1:length(DomesticFirmRatioTemp2), function(x) length(DomesticFirmRatioTemp2[[x]]) / length(DomesticFirmRatioTemp[[x]]))

rm(DomesticFirmRatioTemp)
rm(DomesticFirmRatioTemp2)

