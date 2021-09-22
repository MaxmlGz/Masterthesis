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


## Create sample of domestic German firms

EdgelistDeDom <- pblapply(1:length(Edgelist.List.Filtered), function(x) Edgelist.List.Filtered[[x]][apply(Edgelist.List.Filtered[[x]],1, function (y)  all(Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] == "DE" | is.na(y))),])


## Create sample of internationally involved firms

EdgelistInt <- pblapply(1:length(Edgelist.List.Filtered), function(x) Edgelist.List.Filtered[[x]][apply(Edgelist.List.Filtered[[x]],1, function (y)  any(Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] != "DE" & !is.na(y))),])
names(EdgelistInt) <- names(Edgelist.List.Filtered)

EdgelistInt <- EdgelistInt[sapply(EdgelistInt, function(x) isTruthy(x))]
EdgelistInt <- EdgelistInt[sapply(EdgelistInt, function(x) dim(x)[1]) > 0]



## create sample for chains containing taxhavens 



Taxhavens <- c("AD","BM","BS","CW","CY","GG","GI","HK","IM","JE","JO","KN","KY","LB","LI","MC","MT","MU","PA","PH","SG","TW","VG")
TaxhavensEU <- c("BE","CH","CTL","LU","NL","IE")



EdgelistTH <- pblapply(1:length(EdgelistInt), function(x) Edgelist.Countries[[x]][apply(Edgelist.Countries[[x]],1, function (y) any(y %in% Taxhavens)),])
names(EdgelistTH) <- names(EdgelistInt)



EdgelistTHEU <- pblapply(1:length(EdgelistInt), function(x) Edgelist.Countries[[x]][apply(Edgelist.Countries[[x]],1, function (y) any(y %in% TaxhavensEU)),])
names(EdgelistTHEU) <- names(EdgelistInt)




## split sample by year and direct controlling shareholder

EdgelistByCSH <- pblapply(1:length(EdgelistInt), function (x) {

  ISOList <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))))
  
  pblapply(1:nrow(ISOList), function(i) {
      EdgelistInt[[x]][apply(
      sapply(2:ncol(EdgelistInt[[x]]), function(y) Nodelist.List[[x]]$CompanyISO[match(EdgelistInt[[x]][,y], Nodelist.List[[x]]$CompanyBvDID)] == "DE" &
                                                    Nodelist.List[[x]]$CompanyISO[match(EdgelistInt[[x]][,(y-1)], Nodelist.List[[x]]$CompanyBvDID)] == ISOList[i,1]
    ),1, function(z) any(z) == TRUE),]
  })})

names(EdgelistByCSH) <- names(EdgelistInt)
for (i in 1:length(EdgelistByCSH)) {names(EdgelistByCSH[[i]]) <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))))$ISO}


for (i in 1:length(EdgelistByCSH)) {
  for (j in 1:length(EdgelistByCSH[[i]])) {
      if (nrow(EdgelistByCSH[[i]][[j]]) == 0) {
        EdgelistByCSH[[i]][[j]][1,] <- NA
    }
  }
}



## split sample by year and GUO

EdgelistByGUO <- pblapply(1:length(EdgelistInt), function (x) {
  
  ISOList <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))))
  
  pblapply(1:nrow(ISOList), function(i) {
    EdgelistInt[[x]][apply(
      sapply(2:ncol(EdgelistInt[[x]]), function(y) Nodelist.List[[x]]$CompanyISO[match(EdgelistInt[[x]][,y], Nodelist.List[[x]]$CompanyBvDID)] == "DE" &
               Nodelist.List[[x]]$CompanyISO[match(EdgelistInt[[x]][,1], Nodelist.List[[x]]$CompanyBvDID)] == ISOList[i,1]
      ),1, function(z) any(z) == TRUE),]
  })})

names(EdgelistByGUO) <- names(EdgelistInt)
for (i in 1:length(EdgelistByGUO)) {names(EdgelistByGUO[[i]]) <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))))$ISO}

for (i in 1:length(EdgelistByGUO)) {
  for (j in 1:length(EdgelistByGUO[[i]])) {
    if (nrow(EdgelistByGUO[[i]][[j]]) == 0) {
      EdgelistByGUO[[i]][[j]][1,] <- NA
    }
  }
}






EdgelistByGUO.Gersub <- EdgelistByGUO
for(i in 1:length(EdgelistByGUO.Gersub)) {
  for(j in 1:length(EdgelistByGUO.Gersub[[i]])) {
    
    if (isTruthy(EdgelistByGUO.Gersub[[i]][[j]])) {
      EdgelistByGUO.Gersub[[i]][[j]] <- EdgelistByGUO.Gersub[[i]][[j]][apply(EdgelistByGUO.Gersub[[i]][[j]],1,function(y) last(y[!is.na(y)] %in% NodelistGermanEnergy$CompanyBvDID)),]
    }
  }
  
  print(paste(i,"/",length(EdgelistByGUO.Gersub)))
  
}











## split sample by year and owning country at any level

EdgelistByanyown <- pblapply(1:length(EdgelistInt), function (x) {
  
  ISOList <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))))
  
  pblapply(1:nrow(ISOList), function(i) {
    EdgelistInt[[x]][as.logical(sapply(apply(EdgelistInt[[x]],1, function (y) first(as.numeric(which(Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] == ISOList[i,1]))) < last(as.numeric(which(Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] == "DE")))), function(z) z == TRUE & !is.na(z))),]
  })})

EdgelistByanyown <- pblapply(1:length(EdgelistByanyown), function(x) {pblapply(1:length(EdgelistByanyown[[x]]), function(y) EdgelistByanyown[[x]][[y]][!is.na(EdgelistByanyown[[x]][[y]][,1]),])})

names(EdgelistByanyown) <- names(EdgelistInt)
for (i in 1:length(EdgelistByanyown)) {names(EdgelistByanyown[[i]]) <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))))$ISO}

for (i in 1:length(EdgelistByanyown)) {
  for (j in 1:length(EdgelistByanyown[[i]])) {
    if (nrow(EdgelistByanyown[[i]][[j]]) == 0) {
      EdgelistByanyown[[i]][[j]][1,] <- NA
    }
  }
}



EdgelistByanyown.Gersub <- EdgelistByanyown
for(i in 1:length(EdgelistByanyown.Gersub)) {
  for(j in 1:length(EdgelistByanyown.Gersub[[i]])) {
    
    if (isTruthy(EdgelistByanyown.Gersub[[i]][[j]])) {
    EdgelistByanyown.Gersub[[i]][[j]] <- EdgelistByanyown.Gersub[[i]][[j]][apply(EdgelistByanyown.Gersub[[i]][[j]],1,function(y) last(y[!is.na(y)] %in% NodelistGermanEnergy$CompanyBvDID)),]
  }
  }
  
  print(paste(i,"/",length(EdgelistByanyown.Gersub)))
  
  }




## split sample by year and owning country at intermediate level


EdgelistByintermed <- pblapply(1:length(EdgelistByanyown), function (x) {
  
  ISOList <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))))
  
  pblapply(1:nrow(ISOList), function(i) {
    dplyr::setdiff(EdgelistByanyown[[x]][[i]], EdgelistByGUO[[x]][[i]])
  })})

EdgelistByintermed <- pblapply(1:length(EdgelistByintermed), function(x) {pblapply(1:length(EdgelistByintermed[[x]]), function(y)  {if (isTruthy(EdgelistByintermed[[x]][[y]])) {  EdgelistByintermed[[x]][[y]][!is.na(EdgelistByintermed[[x]][[y]][,1]),]}})})

names(EdgelistByintermed) <- names(EdgelistInt)
for (i in 1:length(EdgelistByintermed)) {names(EdgelistByintermed[[i]]) <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))))$ISO}

for (i in 1:length(EdgelistByintermed)) {
  for (j in 1:length(EdgelistByintermed[[i]])) {
    if (nrow(EdgelistByintermed[[i]][[j]]) == 0) {
      EdgelistByintermed[[i]][[j]][1,] <- NA
    }
  }
}


EdgelistByintermed.Gersub <- EdgelistByintermed
for(i in 1:length(EdgelistByintermed.Gersub)) {
  for(j in 1:length(EdgelistByintermed.Gersub[[i]])) {
    
    if (isTruthy(EdgelistByintermed.Gersub[[i]][[j]])) {
      EdgelistByintermed.Gersub[[i]][[j]] <- EdgelistByintermed.Gersub[[i]][[j]][apply(EdgelistByintermed.Gersub[[i]][[j]],1,function(y) last(y[!is.na(y)] %in% NodelistGermanEnergy$CompanyBvDID)),]
    }
  }
  
  print(paste(i,"/",length(EdgelistByintermed.Gersub)))
  
}




## split sample by year and subsidiary country at any level

EdgelistByanysub <- pblapply(1:length(EdgelistInt), function (x) {
  
  ISOList <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))))
  
  pblapply(1:nrow(ISOList), function(i) {
    EdgelistInt[[x]][as.logical(sapply(apply(EdgelistInt[[x]],1, function (y) last(as.numeric(which(Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] == ISOList[i,1]))) > first(as.numeric(which(Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] == "DE")))), function(z) z == TRUE & !is.na(z))),]
  })})

EdgelistByanysub <- pblapply(1:length(EdgelistByanysub), function(x) {pblapply(1:length(EdgelistByanysub[[x]]), function(y) EdgelistByanysub[[x]][[y]][!is.na(EdgelistByanysub[[x]][[y]][,1]),])})

names(EdgelistByanysub) <- names(EdgelistInt)
for (i in 1:length(EdgelistByanysub)) {names(EdgelistByanysub[[i]]) <- data.frame("ISO" = c(na.omit(unique(NodelistALL$CompanyISO))))$ISO}

for (i in 1:length(EdgelistByanysub)) {
  for (j in 1:length(EdgelistByanysub[[i]])) {
    if (nrow(EdgelistByanysub[[i]][[j]]) == 0) {
      EdgelistByanysub[[i]][[j]][1,] <- NA
    }
  }
}







#direct owner links

for (x in 1:length(EdgelistByCSH)) {
  
  Edgelist.Stat[[x]]$directlinksown <- NA
    
  for(y in 1:nrow(Edgelist.Stat[[x]])) {
    
    if (Edgelist.Stat[[x]][y,1] %notin% names(EdgelistByCSH[[x]])) {next}
    if (nrow(EdgelistByCSH[[x]][[Edgelist.Stat[[x]][y,1]]]) == 0) {next}
    if (all(is.na(EdgelistByCSH[[x]][[Edgelist.Stat[[x]][y,1]]]))) {next}
    
    Edgelist.Stat[[x]][y,7] <- nrow(EdgelistByCSH[[x]][[Edgelist.Stat[[x]][y,1]]])
    
  }
}


#indirect owner links

for (x in 1:length(EdgelistByanyown)) {
  
  Edgelist.Stat[[x]]$indirectlinksown <- NA
  
  for(y in 1:nrow(Edgelist.Stat[[x]])) {
    
    if (!isTruthy(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]])) {next}
    if (Edgelist.Stat[[x]][y,1] %notin% names(EdgelistByanyown[[x]])) {next}
    if (nrow(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]]) == 0) {next}
    if (all(is.na(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]]))) {next}
    
    
    Edgelist.Stat[[x]][y,8] <- nrow(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]]) - nrow(EdgelistByCSH[[x]][[Edgelist.Stat[[x]][y,1]]])
    
  }
}


#intermediate position

for (x in 1:length(EdgelistByanyown)) {
  
  Edgelist.Stat[[x]]$intermed <- NA
  
  for(y in 1:nrow(Edgelist.Stat[[x]])) {
    
    if (!isTruthy(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]])) {next}
    if (Edgelist.Stat[[x]][y,1] %notin% names(EdgelistByanyown[[x]])) {next}
    if (nrow(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]]) == 0) {next}
    if (all(is.na(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]]))) {next}
    
    Edgelist.Stat[[x]][y,9] <- nrow(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]]) - nrow(EdgelistByGUO[[x]][[Edgelist.Stat[[x]][y,1]]])
    
  }
}


#GUO

for (x in 1:length(EdgelistByGUO)) {
  
  Edgelist.Stat[[x]]$GUO <- NA
  
  for(y in 1:nrow(Edgelist.Stat[[x]])) {
    

    if (Edgelist.Stat[[x]][y,1] %notin% names(EdgelistByGUO[[x]])) {next}
    if (nrow(EdgelistByGUO[[x]][[Edgelist.Stat[[x]][y,1]]]) == 0) {next}
    if (all(is.na(EdgelistByGUO[[x]][[Edgelist.Stat[[x]][y,1]]]))) {next}
    
    Edgelist.Stat[[x]][y,10] <- nrow(EdgelistByGUO[[x]][[Edgelist.Stat[[x]][y,1]]])
    
  }
}



# links over tax havens


for (x in 1:length(EdgelistByanyown)) {
  
  Edgelist.Stat[[x]]$linksownTH <- NA
  
  for(y in 1:nrow(Edgelist.Stat[[x]])) {
    
    if (Edgelist.Stat[[x]][y,1] %notin% names(EdgelistByanyown[[x]])) {next}
    if (!isTruthy(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]])) {next}
    

    EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]][is.na(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]])] <- "DUMMY"
    if (nrow(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]]) < 20) {EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]] <- rbind(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
        
        Temp1 <- apply(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]],1, function (z) as.numeric(which(Nodelist.List[[x]]$CompanyISO[match(z, Nodelist.List[[x]]$CompanyBvDID)] == Edgelist.Stat[[x]][y,1])))
        Temp2 <- apply(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]],1, function (z) as.numeric(which(Nodelist.List[[x]]$CompanyISO[match(z, Nodelist.List[[x]]$CompanyBvDID)] %in% Taxhavens[Taxhavens != Edgelist.Stat[[x]][y,1]])))
        if(!isTruthy(Temp2)) {next}
        Temp3 <- apply(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]],1, function (z) as.numeric(which(Nodelist.List[[x]]$CompanyISO[match(z, Nodelist.List[[x]]$CompanyBvDID)] == "DE")))
        
        Temp4 <- sapply(1:length(Temp1), function (z) any(Temp2[[z]]+1 > first(Temp1[[z]])+1 & Temp2[[z]]+1 < last(Temp3[[z]]+1)))
        
        Edgelist.Stat[[x]][[y,11]] <- length(Temp4[Temp4 == TRUE])
        
  }
  
  print(paste0(x,"/",length(EdgelistByanyown)))
  
}


# links over EU tax havens


for (x in 1:length(EdgelistByanyown)) {
  
  Edgelist.Stat[[x]]$linksownTHEU <- NA
  
  for(y in 1:nrow(Edgelist.Stat[[x]])) {
    
    if (Edgelist.Stat[[x]][y,1] %notin% names(EdgelistByanyown[[x]])) {next}
    if (!isTruthy(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]])) {next}
    
    
    EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]][is.na(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]])] <- "DUMMY"
    if (nrow(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]]) < 20) {EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]] <- rbind(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]],c("DUMMY"),c("DUMMY"),c("DUMMY"),c("DUMMY"))}
    
    Temp1 <- apply(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]],1, function (z) as.numeric(which(Nodelist.List[[x]]$CompanyISO[match(z, Nodelist.List[[x]]$CompanyBvDID)] == Edgelist.Stat[[x]][y,1])))
    Temp2 <- apply(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]],1, function (z) as.numeric(which(Nodelist.List[[x]]$CompanyISO[match(z, Nodelist.List[[x]]$CompanyBvDID)] %in% TaxhavensEU[TaxhavensEU != Edgelist.Stat[[x]][y,1]])))
    if(!isTruthy(Temp2)) {next}
    Temp3 <- apply(EdgelistByanyown[[x]][[Edgelist.Stat[[x]][y,1]]],1, function (z) as.numeric(which(Nodelist.List[[x]]$CompanyISO[match(z, Nodelist.List[[x]]$CompanyBvDID)] == "DE")))
    
    Temp4 <- sapply(1:length(Temp1), function (z) any(Temp2[[z]]+1 > first(Temp1[[z]])+1 & Temp2[[z]]+1 < last(Temp3[[z]]+1)))
    
    Edgelist.Stat[[x]][[y,12]] <- length(Temp4[Temp4 == TRUE])
    
  }
  
  print(paste0(x,"/",length(EdgelistByanyown)))
  
}


for (i in 1:length(EdgelistByanyown)) {
  for (j in 1:length(EdgelistByanyown[[i]])) {
    
    
    if(!isTruthy(EdgelistByanyown[[i]][[j]])) {next}
    EdgelistByanyown[[i]][[j]][EdgelistByanyown[[i]][[j]] == "DUMMY" ] <- NA
    EdgelistByanyown[[i]][[j]] <- EdgelistByanyown[[i]][[j]][rowSums(is.na(EdgelistByanyown[[i]][[j]])) != ncol(EdgelistByanyown[[i]][[j]]),]
    
  }
}







## additional graphics & tables




Internationality <- data.frame(
  "Year" = names(EdgelistInt),
  "ShareInt" = sapply(1:length(EdgelistInt), function (x) nrow(EdgelistInt[[x]]) / nrow(Edgelist.List.Filtered[[x]]) )
) 

melt(Internationality, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  labs(x = "Year", y = "Share of International Ownership Chains") +
  theme(legend.position = "bottom",
        text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1, size = 15),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = "darkblue", labels = "Share of International Chains")





TaxHavenImpact <- data.frame(
  "Year" = names(EdgelistInt),
  "ShareTH" = sapply(1:length(EdgelistInt), function (x) nrow(EdgelistTH[[x]]) / nrow(EdgelistInt[[x]])),
  "ShareTHEU" = sapply(1:length(EdgelistInt), function (x) nrow(EdgelistTHEU[[x]]) / nrow(EdgelistInt[[x]]))
) 

melt(TaxHavenImpact, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  labs(x = "Year", y = "Share of sink/conduit involvement \n within international Ownership Chains") +
  scale_color_manual(values = c("red","orange"), labels = c("Share_Sinks","Share_Conduits")) +
  theme(legend.position = "bottom",
        text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1, size = 15),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent)









#### Plots GUO



GUOPlot <- melt(GUOindicatorDF, id="ISO")
colnames(GUOPlot) <- c("ISO","Year","score")

GUOPlot$count <- as.numeric( pbsapply(1:nrow(GUOPlot), function (y) ChainCountByCountry[ChainCountByCountry$ISO == GUOPlot$ISO[y],][,c(paste(GUOPlot$Year[y]))]) )

GUOPlot <- GUOPlot[!is.na(GUOPlot$score),]

for (i in 1:nrow(GUOPlot)) {if (GUOPlot$ISO[i] != "DE") {next}
  GUOPlot$count[i] = max(as.numeric(subset(GUOPlot, as.character(Year) == as.character(GUOPlot$Year[i]))$count), na.rm = TRUE) * 1.3   
  }



ggplot(data=GUOPlot, aes(x=ISO, y=Year, size=(1/score), color = score, group=ISO)) +
  scale_color_gradient(low = "red", high = "green") +
  geom_line() +
  scale_y_discrete(breaks = unique(GUOPlot$Year)[c(FALSE,FALSE,FALSE,FALSE,TRUE)],
                   labels = unique(GUOPlot$Year)[c(FALSE,FALSE,FALSE,FALSE,TRUE)]) +
  labs(size = "1 / GUOScore", color = "GUOScore") +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        axis.text.x = element_text(size = 10))






GUOPlot_harmonized <- melt(GUOindicator_harmonizedDF, id="ISO")
colnames(GUOPlot_harmonized) <- c("ISO","Year","score")
GUOPlot_harmonized$score <- round(GUOPlot_harmonized$score, digits = 10)

GUOPlot_harmonized$count <- as.numeric( pbsapply(1:nrow(GUOPlot_harmonized), function (y) ChainCountByCountry[ChainCountByCountry$ISO == GUOPlot_harmonized$ISO[y],][,c(paste(GUOPlot_harmonized$Year[y]))]) )

GUOPlot_harmonized <- GUOPlot_harmonized[!is.na(GUOPlot_harmonized$score),]

for (i in 1:nrow(GUOPlot_harmonized)) {if (GUOPlot_harmonized$ISO[i] != "DE") {next}
  GUOPlot_harmonized$count[i] = max(as.numeric(subset(GUOPlot_harmonized, as.character(Year) == as.character(GUOPlot_harmonized$Year[i]))$count), na.rm = TRUE) * 1.3   
}




ggplot(data=GUOPlot_harmonized, aes(x=ISO, y=Year, size = count,  color= score, group=ISO)) +
  scale_color_gradient(low = "red", high = "green") +
  geom_line() +
  scale_y_discrete(breaks = unique(GUOPlot$Year)[c(FALSE,FALSE,FALSE,FALSE,TRUE)],
                   labels = unique(GUOPlot$Year)[c(FALSE,FALSE,FALSE,FALSE,TRUE)]) +
  labs(size = "Count", color = "GUOScore (harmonized)") +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        axis.text.x = element_text(size = 10))





## Plots Betweenness Centrality


BetwCentrPlot <- melt(BetwCentrDF, id="ISO")
colnames(BetwCentrPlot) <- c("ISO","Year","BetwCentr")

BetwCentrPlot$count <- as.numeric( pbsapply(1:nrow(BetwCentrPlot), function (y) ChainCountByCountry[ChainCountByCountry$ISO == BetwCentrPlot$ISO[y],][,c(paste(BetwCentrPlot$Year[y]))]) )

BetwCentrPlot <- BetwCentrPlot[!is.na(BetwCentrPlot$BetwCentr),]

for (i in 1:nrow(BetwCentrPlot)) {if (BetwCentrPlot$ISO[i] != "DE") {next}
  BetwCentrPlot$count[i] = max(as.numeric(subset(BetwCentrPlot, as.character(Year) == as.character(BetwCentrPlot$Year[i]))$count), na.rm = TRUE) * 1.3   
}

BetwCentrPlot$BetwCentr <- round(BetwCentrPlot$BetwCentr, digits = 10)
BetwCentrPlot[BetwCentrPlot$ISO == "DE"] <- NULL

ggplot(data=BetwCentrPlot[BetwCentrPlot$ISO != "DE",], aes(x=ISO, y=Year, size=BetwCentr, color= sqrt(BetwCentr), group=ISO)) +
  scale_color_gradient(low = "green", high = "red") +
  geom_line() +
  scale_y_discrete(breaks = unique(BetwCentrPlot$Year)[c(FALSE,FALSE,FALSE,TRUE,FALSE)],
                   labels = unique(BetwCentrPlot$Year)[c(FALSE,FALSE,FALSE,TRUE,FALSE)]) +
  labs(size = "Betwenness Centrality", color = "Betwenness Centrality (square root)") +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        axis.text.x = element_text(size = 10))








BetwCentrPlot_harmonized <- melt(BetwCentr_harmonizedDF, id="ISO")
colnames(BetwCentrPlot_harmonized) <- c("ISO","Year","BetwCentr")
BetwCentrPlot_harmonized$BetwCentr <- round(BetwCentrPlot_harmonized$BetwCentr, digits = 10)

BetwCentrPlot_harmonized$count <- as.numeric( pbsapply(1:nrow(BetwCentrPlot_harmonized), function (y) ChainCountByCountry[ChainCountByCountry$ISO == BetwCentrPlot_harmonized$ISO[y],][,c(paste(BetwCentrPlot_harmonized$Year[y]))]) )

BetwCentrPlot_harmonized <- BetwCentrPlot_harmonized[!is.na(BetwCentrPlot_harmonized$BetwCentr),]

for (i in 1:nrow(BetwCentrPlot_harmonized)) {if (BetwCentrPlot_harmonized$ISO[i] != "DE") {next}
  BetwCentrPlot_harmonized$count[i] = max(as.numeric(subset(BetwCentrPlot_harmonized, as.character(Year) == as.character(BetwCentrPlot_harmonized$Year[i]))$count), na.rm = TRUE) * 1.3   
}




ggplot(data=BetwCentrPlot_harmonized, aes(x=ISO, y=Year, size = count,  color= BetwCentr, group=ISO)) +
  scale_color_gradient(low = "green", high = "red") +
  geom_line() +
  scale_y_discrete(breaks = unique(BetwCentrPlot$Year)[c(FALSE,FALSE,FALSE,TRUE,FALSE)],
                   labels = unique(BetwCentrPlot$Year)[c(FALSE,FALSE,FALSE,TRUE,FALSE)]) +
  labs(size = "Count", color = "Betwenness Centrality (harmonized)") +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        axis.text.x = element_text(size = 10))





### GUO score comparison


AvgGUOScore <- data.frame( "Year" = names(Edgelist.List.Filtered)[ncol(GUOindicatorDF):1],
                           "score" = apply(subset(GUOindicatorDF, ISO != "DE"),2, function(y) mean(as.numeric(y), na.rm = TRUE)))
AvgGUOScore <- subset(AvgGUOScore, !is.na(as.numeric(score)))


ChainCountByCountry <- pblapply(1:length(EdgelistInt), function(x)
  data.frame(
    "ISO" = subset(GUOindicatorDF, ISO != "DE")$ISO,
    "Count" =   sapply(subset(GUOindicatorDF, ISO != "DE")$ISO, function(h) nrow(Edgelist.Countries[[x]][sapply(apply(Edgelist.Countries[[x]],1, function(y) y[!is.na(y)]), function(z) any(z == h)),]))
  ))
ChainCountByCountry <- ChainCountByCountry |> purrr::reduce(full_join, by = "ISO")
colnames(ChainCountByCountry) <- c("ISO",names(EdgelistInt))
ChainCountByCountry <- ChainCountByCountry[,order(ncol(ChainCountByCountry):1)]
ChainCountByCountry <- ChainCountByCountry[,c(ncol(ChainCountByCountry), 1:ncol(ChainCountByCountry)-1)]
ChainCountByCountry <- ChainCountByCountry[,apply(ChainCountByCountry,2, function(y) any(!is.na(y) & y > 0 & y != "NaN" ))]

AvgGUOScore$Avg_W <- pbsapply(2:ncol(ChainCountByCountry), function(z) sum(sapply(ChainCountByCountry$ISO, function(y) ChainCountByCountry[ChainCountByCountry$ISO == y,][,z] * GUOindicatorDF[GUOindicatorDF$ISO == y,][,z]), na.rm = TRUE) / sum(ChainCountByCountry[,z],na.rm = TRUE))
AvgGUOScore <- left_join(AvgGUOScore, subset(GUOPlot[,c(1:3)], GUOPlot[,c(1:3)]$ISO == "DE"), by = "Year")
AvgGUOScore$ISO <- NULL
colnames(AvgGUOScore) <- c("Year", "Avg", "Avg_W", "Germany")


melt(AvgGUOScore, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  labs(x = "Year", y = "GUO_Score") + 
  theme(legend.position = "bottom",
        text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1, size = 15),
        legend.title = element_blank()) +
  scale_color_manual(values = c("darkblue","black","darkgreen") ,labels= c("Average of Foreign Countries", "Average of Foreign Countries (Weighted by Number of Chains Per Country","Germany"))





AvgGUOScoreTH <- data.frame( "Year" = names(Edgelist.List.Filtered)[ncol(GUOindicatorDF):1],
                             "score" = apply(subset(GUOindicatorDF, ISO %in% Taxhavens),2, function(y) mean(as.numeric(y), na.rm = TRUE)))

AvgGUOScoreTH <- subset(AvgGUOScoreTH, !is.na(as.numeric(score)))

AvgGUOScoreTH$Avg_W <- pbsapply((ncol(ChainCountByCountry)-(nrow(AvgGUOScoreTH)-1)):ncol(ChainCountByCountry), function(z) sum(sapply(ChainCountByCountry$ISO[ChainCountByCountry$ISO %in% Taxhavens], function(y) ChainCountByCountry[ChainCountByCountry$ISO == y,][,z] * GUOindicatorDF[GUOindicatorDF$ISO == y,][,z]), na.rm = TRUE) / sum(subset(ChainCountByCountry, ChainCountByCountry$ISO %in% Taxhavens)[,z],na.rm = TRUE))

AvgGUOScoreTH <- left_join(AvgGUOScoreTH, subset(GUOPlot[,c(1:3)], GUOPlot[,c(1:3)]$ISO == "DE"), by = "Year")
AvgGUOScoreTH$ISO <- NULL
colnames(AvgGUOScoreTH) <- c("Year", "AvgTH", "AvgTH_W", "Germany")


melt(AvgGUOScoreTH, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  labs(x = "Year", y = "GUO_Score") +
  theme(legend.position = "bottom",
        text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1, size = 15),
        legend.title = element_blank()) 










GUOScoreCompare <- full_join(AvgGUOScore, AvgGUOScoreTH, by = "Year")
GUOScoreCompare <- GUOScoreCompare[,(c(1,2,3,5,6,4))]
GUOScoreCompare <- GUOScoreCompare[apply(GUOScoreCompare,1,function(y) all(!is.na(y))),]
colnames(GUOScoreCompare) <- c("Year","AvgGUOscore","AvgGUOscore_W","AvgGUOscore_TH","AvgGUOscore_TH_W","GUOscore_Germany")




melt(GUOScoreCompare, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, linetype=variable, group=variable), size = 1.5) +
  scale_color_manual(values = c("blue","blue","red","red","darkgreen"), labels = c("Sample_Average", "Sample_Average_Weighted","Average_Sinks","Average_Sinks_Weighted","Average_Germany")) +
  scale_linetype_manual(values= c("dotted","solid","dotted","solid","dashed"), labels = c("Sample_Average", "Sample_Average_Weighted","Average_Sinks","Average_Sinks_Weighted","Average_Germany")) +
  labs( x = "Year", y = "GUO_Score") +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        legend.title = element_blank())




Gersub_GerGUO <- data.frame(
  "Year" = names(EdgelistInt),
  "GUOCount" = pbsapply(1:length(EdgelistInt), function (x) nrow(Edgelist.Countries[[x]][apply(Edgelist.Countries[[x]],1,function(y) first(y[!is.na(y)]) == "DE"  & last(y[!is.na(y)]) != "DE"),])),
  "SubCount" = pbsapply(1:length(EdgelistInt), function (x) nrow(Edgelist.Countries[[x]][apply(Edgelist.Countries[[x]],1,function(y) first(y[!is.na(y)]) != "DE"  & last(y[!is.na(y)]) == "DE"),]))
) 


melt(Gersub_GerGUO, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  labs(x = "Year", y = "Count") + 
  theme(legend.position = "bottom",
        text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1, size = 15),
        legend.title = element_blank()) +
  scale_color_manual(values = c("blue","darkorange"), labels = c("Number of Subsidiaries Owned by German Energy Firms","Number of Foreign Owned Energy Subsidiaries in Germany"))





### BetwCentr comparison



AvgBetwCentr <- data.frame( "Year" = names(Edgelist.List.Filtered)[ncol(BetwCentrDF):1],
                            "BetwCentr" = apply(subset(BetwCentrDF, ISO != "DE"),2, function(y) mean(as.numeric(y), na.rm = TRUE)))
AvgBetwCentr <- subset(AvgBetwCentr, !is.na(as.numeric(BetwCentr)))


ChainCountByCountry <- pblapply(1:length(EdgelistInt), function(x)
  data.frame(
    "ISO" = subset(BetwCentrDF, ISO != "DE")$ISO,
    "Count" =   sapply(subset(BetwCentrDF, ISO != "DE")$ISO, function(h) nrow(Edgelist.Countries[[x]][sapply(apply(Edgelist.Countries[[x]],1, function(y) y[!is.na(y)]), function(z) any(z == h)),]))
  ))
ChainCountByCountry <- ChainCountByCountry |> purrr::reduce(full_join, by = "ISO")
colnames(ChainCountByCountry) <- c("ISO",names(EdgelistInt))
ChainCountByCountry <- ChainCountByCountry[,order(ncol(ChainCountByCountry):1)]
ChainCountByCountry <- ChainCountByCountry[,c(ncol(ChainCountByCountry), 1:ncol(ChainCountByCountry)-1)]
ChainCountByCountry <- ChainCountByCountry[,apply(ChainCountByCountry,2, function(y) any(!is.na(y) & y > 0 & y != "NaN" ))]

AvgBetwCentr$Avg_W <- pbsapply(2:ncol(BetwCentrDF), function(z) sum(sapply(ChainCountByCountry$ISO, function(y) ChainCountByCountry[ChainCountByCountry$ISO == y,][,(z + (ncol(ChainCountByCountry) - ncol(BetwCentrDF)))] * BetwCentrDF[BetwCentrDF$ISO == y,][,z]), na.rm = TRUE) / sum(ChainCountByCountry[,(z + (ncol(ChainCountByCountry) - ncol(BetwCentrDF)))],na.rm = TRUE))

colnames(AvgBetwCentr) <- c("Year", "Avg", "Avg_W")


melt(AvgBetwCentr, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  theme(legend.position = "bottom")




AvgBetwCentrTH <- data.frame( "Year" = names(Edgelist.List.Filtered)[ncol(BetwCentrDF):1],
                              "BetwCentr" = apply(subset(BetwCentrDF, ISO %in% TaxhavensEU),2, function(y) mean(as.numeric(y), na.rm = TRUE)))

AvgBetwCentrTH <- subset(AvgBetwCentrTH, !is.na(as.numeric(BetwCentr)))

AvgBetwCentrTH$Avg_W <- pbsapply((ncol(BetwCentrDF)-(nrow(AvgBetwCentrTH)-1)):ncol(BetwCentrDF), function(z) sum(sapply(ChainCountByCountry$ISO[ChainCountByCountry$ISO %in% TaxhavensEU], function(y) ChainCountByCountry[ChainCountByCountry$ISO == y,][,(z + (ncol(ChainCountByCountry) - ncol(BetwCentrDF)))] * BetwCentrDF[BetwCentrDF$ISO == y,][,z]), na.rm = TRUE) / sum(subset(ChainCountByCountry, ChainCountByCountry$ISO %in% TaxhavensEU)[,(z + (ncol(ChainCountByCountry) - ncol(BetwCentrDF)))],na.rm = TRUE))



melt(AvgBetwCentrTH, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  theme(legend.position = "bottom")





BetwCentrCompare <- full_join(AvgBetwCentr, AvgBetwCentrTH, by = "Year")
colnames(BetwCentrCompare) <- c("Year","AvgBetwCentr","AvgBetwCentr_W","AvgBetwCentr_TH","AvgBetwCentr_TH_W")




melt(BetwCentrCompare, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, linetype=variable, group=variable), size = 1.5) +
  scale_color_manual(values = c("blue","blue","red","red"), labels = c("Sample_Average","Sample_Average_Weighted","Average_Conduits","Average_Conduits_Weighted")) +
  scale_linetype_manual(values= c("dashed","solid","dashed","solid"), labels = c("Sample_Average","Sample_Average_Weighted","Average_Conduits","Average_Conduits_Weighted")) +
  labs (y = "Betweenness Centrality", x = "Year") +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        legend.title = element_blank())









## connectedness between sink and conduits






InterConnect <- data.frame(
  "Year" = names(EdgelistInt),
  "ShareTH-THEU" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1, function (y) any(y %in% Taxhavens)),]) / nrow(EdgelistTHEU[[x]])),
  "ShareTH-Int" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTH[[x]]) / nrow(EdgelistInt[[x]])),
  "ShareTH-THEU_length3" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 2 ),][apply(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 2 ),],1, function (y) any(y %in% Taxhavens)),]) / nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 2 ),])),
  "ShareTH-Int_length3" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 2 ),]) / nrow(EdgelistInt[[x]][apply(EdgelistInt[[x]],1,function(y) length(y[!is.na(y)]) > 2 ),]))
)

melt(InterConnect, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, linetype=variable, group=variable), size = 1.5) +
  scale_color_manual(values = c("gray","gray","darkgreen","darkgreen"), labels = c("within conduit involving chains", "within all international chains, dashed", "length > 2", "length > 2")) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed"), labels = c("within conduit involving chains", "within all international chains, dashed", "length > 2", "length > 2")) +
  labs(x = "Year", y = "Share of Chains involving Sinks") +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        legend.title = element_blank())




InterConnect2 <- data.frame(
  "Year" = names(EdgelistInt),
  "ShareTH-THEU_length5" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 4 ),][apply(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 4 ),],1, function (y) any(y %in% Taxhavens)),]) / nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 4 ),])),
  "ShareTH-Int_length5" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 4 ),]) / nrow(EdgelistInt[[x]][apply(EdgelistInt[[x]],1,function(y) length(y[!is.na(y)]) > 4 ),])),
  "ShareTH-THEU_length7" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 6 ),][apply(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 6 ),],1, function (y) any(y %in% Taxhavens)),]) / nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 6 ),])),
  "ShareTH-Int_length7" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 6 ),]) / nrow(EdgelistInt[[x]][apply(EdgelistInt[[x]],1,function(y) length(y[!is.na(y)]) > 6 ),])),
  "ShareTH-THEU_length9" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 8 ),][apply(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 8 ),],1, function (y) any(y %in% Taxhavens)),]) / nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 8 ),])),
  "ShareTH-Int_length9" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 8 ),]) / nrow(EdgelistInt[[x]][apply(EdgelistInt[[x]],1,function(y) length(y[!is.na(y)]) > 8 ),])))


melt(InterConnect2, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, linetype=variable, group=variable), size = 1.5) +
  scale_color_manual(values = c("blue","blue","orange","orange","red","red"), labels = c("within conduit involving chains, length > 4", "within all international chains, dashed, length > 4", "length > 6", "length > 6", "length > 8", "length > 8")) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed","solid","dashed"), labels = c("within conduit involving chains, length > 4", "within all international chains, dashed, length > 4", "length > 6", "length > 6", "length > 8", "length > 8")) +
  labs(x = "Year", y = "Share of Chains involving Sinks") +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        legend.title = element_blank())







InterConnect_reverse <- data.frame(
  "Year" = names(EdgelistInt),
  "ShareTHEU-TH" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1, function (y) any(y %in% TaxhavensEU)),]) / nrow(EdgelistTH[[x]])),
  "ShareTHEU-Int" = pbsapply(1:length(EdgelistInt), function (x) nrow(EdgelistTHEU[[x]]) / nrow(EdgelistInt[[x]])),
  "ShareTHEU-TH_length3" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 2 ),][apply(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 2 ),],1, function (y) any(y %in% TaxhavensEU)),]) / nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 2 ),])),
  "ShareTHEU-Int_length3" = pbsapply(1:length(EdgelistInt), function (x) nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 2 ),]) / nrow(EdgelistInt[[x]][apply(EdgelistInt[[x]],1,function(y) length(y[!is.na(y)]) > 2 ),]))
)

melt(InterConnect_reverse, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, linetype=variable, group=variable), size = 1.5) +
  scale_color_manual(values = c("gray","gray","darkgreen","darkgreen"), labels = c("within sink involving chains", "within all international chains, dashed", "length > 2", "length > 2")) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed"), labels = c("within sink involving chains", "within all international chains, dashed", "length > 2", "length > 2")) +
  labs(x = "Year", y = "Share of Chains involving Conduits") +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        legend.title = element_blank())




InterConnect_reverse2 <- data.frame(
  "Year" = names(EdgelistInt),
  "ShareTHEU-TH_length5" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 4 ),][apply(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 4 ),],1, function (y) any(y %in% TaxhavensEU)),]) / nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 4 ),])),
  "ShareTHEU-Int_length5" = pbsapply(1:length(EdgelistInt), function (x) nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 4 ),]) / nrow(EdgelistInt[[x]][apply(EdgelistInt[[x]],1,function(y) length(y[!is.na(y)]) > 4 ),])),
  "ShareTHEU-TH_length7" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 6 ),][apply(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 6 ),],1, function (y) any(y %in% TaxhavensEU)),]) / nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 6 ),])),
  "ShareTHEU-Int_length7" = pbsapply(1:length(EdgelistInt), function (x) nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 6 ),]) / nrow(EdgelistInt[[x]][apply(EdgelistInt[[x]],1,function(y) length(y[!is.na(y)]) > 6 ),])),
  "ShareTHEU-TH_length9" = pbsapply(1:length(EdgelistInt), function(x) nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 8 ),][apply(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 8 ),],1, function (y) any(y %in% TaxhavensEU)),]) / nrow(EdgelistTH[[x]][apply(EdgelistTH[[x]],1,function(y) length(y[!is.na(y)]) > 8 ),])),
  "ShareTHEU-Int_length9" = pbsapply(1:length(EdgelistInt), function (x) nrow(EdgelistTHEU[[x]][apply(EdgelistTHEU[[x]],1,function(y) length(y[!is.na(y)]) > 8 ),]) / nrow(EdgelistInt[[x]][apply(EdgelistInt[[x]],1,function(y) length(y[!is.na(y)]) > 8 ),]))
)


melt(InterConnect_reverse2, id = "Year") |>
  subset(Year > 1990) |>
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, linetype=variable, group=variable), size = 1.5) +
  scale_color_manual(values = c("blue","blue","orange","orange","red","red"), labels = c("within sink involving chains, length > 4", "within all international chains, dashed, length > 4", "length > 6", "length > 6", "length > 8", "length > 8")) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed","solid","dashed"), labels = c("within sink involving chains, length > 4", "within all international chains, dashed, length > 4", "length > 6", "length > 6", "length > 8", "length > 8")) +
  labs(x = "Year", y = "Share of Chains involving Conduits") +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        legend.title = element_blank())











rm(Network.List.Countries.Temp)






