


## Create sample of domestic German firms

EdgelistDeDom <- pblapply(1:length(Edgelist.List.Filtered), function(x) Edgelist.List.Filtered[[x]][apply(Edgelist.List.Filtered[[x]],1, function (y)  all(Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] == "DE" | is.na(y))),])


## Create sample of internationally involved firms

EdgelistInt <- pblapply(1:length(Edgelist.List.Filtered), function(x) Edgelist.List.Filtered[[x]][apply(Edgelist.List.Filtered[[x]],1, function (y)  any(Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] != "DE" & !is.na(y))),])
names(EdgelistInt) <- names(Edgelist.List.Filtered)

EdgelistInt <- EdgelistInt[sapply(EdgelistInt, function(x) isTruthy(x))]
EdgelistInt <- EdgelistInt[sapply(EdgelistInt, function(x) dim(x)[1]) > 0]


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

Taxhavens <- c("AD","BM","BS","CW","GG","GI","HK","IM","JE","KN","KY","LB","MC","MT","MU","PA","PH","TW","VG")
TaxhavensEU <- c("BE","NL","LU","CH","CY","IE","CTL")


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






