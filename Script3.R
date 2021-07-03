




setDT(NodelistALL)


## next, before I add geographic data, I replace the ISO code of non-nation tax havens (like guernsey, the city of london etc.) with an own unique code using postcodes, city/state names and company names


## City of London


CTLPostcodes <- rio::import("City of London postcodes.csv")
NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyPostcode %in% CTLPostcodes$Postcode, "CTL", CompanyISO))


## Jersey

NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('^JE', CompanyPostcode),]$CompanyBvDID, "JE" ,CompanyISO))
NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('JERSEY|HELIER|CLEMENT|SAVIOUR|MARTIN|GROUVILLE|MARY|JOHN|TRINITY|OUEN|LAWRENCE', CompanyCity),][CompanyISO == "GB"]$CompanyBvDID, "JE" ,CompanyISO))
NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('JERSEY', CompanyName),]$CompanyBvDID, "JE" ,CompanyISO))


## Isle of Man

NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('^IM', CompanyPostcode),]$CompanyBvDID, "IM" ,CompanyISO))
NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('ISLE OF MAN', CompanyName),]$CompanyBvDID, "IM" ,CompanyISO))
NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('PATRICK|DOUGLAS|RAMSEY|CASTLETOWN|ONCHAN|PEEL|BRADDAN|ERIN|BALLASALLA|MARY|LAXEY|SAINT|KIRK|SANTON|MARLEY|ARBORY|ERIN|BRADDAN|FOXDALE|SODERICK|ANDREAS|BALLAUGH|BRIDE|JURBY|LEZAYRE|JOHNS|JOHN|GREEBA|BALDRINE|LONAN|SULBY|MAUGHOLD', CompanyCity),][CompanyISO == "GB"]$CompanyBvDID, "IM" ,CompanyISO))


## Guernsey

NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('GY', CompanyPostcode)]$CompanyBvDID, "GG" ,CompanyISO))
NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('GUERNSEY', CompanyName)]$CompanyBvDID, "GG" ,CompanyISO))
NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('GUERNSEY|ALDERNEY|SARK|HERM|JETHOU|BRECQHOU|LIHOU|BURHOU|CASQUETS|BRECHOU|PIERRE|VALE|TORTEVAL|FOREST|ANDREW|MARTIN|SAMPSON|SAVIOUR|CASTEL', CompanyCity),][CompanyISO == "GB"]$CompanyBvDID, "GG" ,CompanyISO))


## Delaware

NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('^19', CompanyPostcode)][CompanyISO == "US"]$CompanyBvDID, "DEL" ,CompanyISO))
NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('DELAWARE', CompanyName)][CompanyISO == "US"]$CompanyBvDID, "DEL" ,CompanyISO))
NodelistALL <- NodelistALL %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('DELAWARE|DOVER|HARRINGTON|LEWES|METROPOLIS|MILFORD|NEW CASTLE|NEWARK|REHOBOTH|SEAFORD|WILMINGTON', CompanyCity)][CompanyISO == "US"]$CompanyBvDID, "DEL" ,CompanyISO))


## now I assign geocordinates to each country

Coordinates <- rio::import("Geocoordinates.xlsx")
Coordinates <- rbind(Coordinates, data.frame(country = "CTL", latitude = "51.51279", longitude = "0.09184", name = "City of London"))
Coordinates <- rbind(Coordinates, data.frame(country = "DEL", latitude = "39.00000", longitude = "-75.5000", name = "Delaware"))
Coordinates <- rbind(Coordinates, data.frame(country = "CW", latitude = "12.2135221", longitude = "-68.9495816", name = "Curacao"))
NodelistALL <- merge(x=NodelistALL, y=Coordinates, by.x = "CompanyISO", by.y = "country", all.x = TRUE)
NodelistALL <- subset(NodelistALL, select = c(2,3,1,4:ncol(NodelistALL)))


## create list of Nodelists for every year

Nodelist.List <- pblapply(1:length(Edgelist.List.Filtered), function (x) x = subset(NodelistALL, CompanyBvDID %in% as.matrix(Edgelist.List.Filtered[[x]][])))
Nodelist.List.Missing <- pblapply(1:length(Edgelist.List.Filtered), function(x) Edgelist.List.Filtered[[x]][sapply(Edgelist.List.Filtered[[x]], function(y) y %notin% Nodelist.List[[x]]$CompanyBvDID)])
Nodelist.List.Missing <- pblapply(1:length(Nodelist.List.Missing), function(x) data.frame(CompanyBvDID = c(Nodelist.List.Missing[[x]])))
Nodelist.List.Missing <- pblapply(1:length(Nodelist.List.Missing), function (x) Nodelist.List.Missing[[x]][!apply(Nodelist.List.Missing[[x]],1, function (y) {all  (is.na(y))}),])
Nodelist.List.Missing <- pblapply(1:length(Nodelist.List.Missing), function(x) data.frame(CompanyBvDID = c(Nodelist.List.Missing[[x]])))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) rbind(Nodelist.List[[x]],unique(Nodelist.List.Missing[[x]]), fill = TRUE))
Nodelist.List <- pblapply(1:length(Nodelist.List), function(x) rbind.fill(Nodelist.List[[x]], data.frame(NA)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]][,1:13])
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyISO = ifelse(is.na(Nodelist.List[[x]]$CompanyISO), OrbisCompanies$CSHISO[match(Nodelist.List[[x]]$CompanyBvDID, OrbisCompanies$CSHBvDID)],Nodelist.List[[x]]$CompanyISO)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyName = ifelse(is.na(Nodelist.List[[x]]$CompanyName), OrbisCompanies$CSHName[match(Nodelist.List[[x]]$CompanyBvDID, OrbisCompanies$CSHBvDID)],Nodelist.List[[x]]$CompanyName)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyISO = ifelse(is.na(Nodelist.List[[x]]$CompanyISO), OrbisCompanies$CompanyISO[match(Nodelist.List[[x]]$CompanyBvDID, OrbisCompanies$CompanyBvDID)],Nodelist.List[[x]]$CompanyISO)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyName = ifelse(is.na(Nodelist.List[[x]]$CompanyName), OrbisCompanies$CompanyName[match(Nodelist.List[[x]]$CompanyBvDID, OrbisCompanies$CompanyBvDID)],Nodelist.List[[x]]$CompanyName)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(longitude = ifelse(is.na(Nodelist.List[[x]]$longitude), Coordinates$longitude[match(Nodelist.List[[x]]$CompanyISO, Coordinates$country)],Nodelist.List[[x]]$longitude)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(latitude = ifelse(is.na(Nodelist.List[[x]]$latitude), Coordinates$latitude[match(Nodelist.List[[x]]$CompanyISO, Coordinates$country)],Nodelist.List[[x]]$latitude)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyISO = ifelse(is.na(Nodelist.List[[x]]$CompanyISO), "NA" , Nodelist.List[[x]]$CompanyISO)))


## create networks for each year - I create one version with non-aggregated vertices (BvDID) and one version with collapsed, aggregated nodes (ISO)

Edgelist.Network <- pblapply(1:(length(Edgelist.List.Filtered)-3), function (x) as.data.frame(do.call(bind_rows, lapply(1:(ncol(Edgelist.List.Filtered[[x]])-1), function(y) Edgelist.List.Filtered[[x]][,y:(y+1)]))))
Edgelist.Network <- pblapply(1:length(Edgelist.Network), function(x)  {t(apply(Edgelist.Network[[x]],1,function(y) {c(y[!is.na(y)],y[is.na(y)])}))})
Edgelist.Network <- pblapply(1:length(Edgelist.Network), function(x) Edgelist.Network[[x]][,!apply(Edgelist.Network[[x]],2, function(y) all(is.na(y)))])
Edgelist.Network <- pblapply(1:length(Edgelist.Network), function(x) as.data.frame(Edgelist.Network[[x]]))
Edgelist.Network <- c(Edgelist.Network, lapply((length(Edgelist.List.Filtered)-2):length(Edgelist.List.Filtered), function (x) x = cbind(Edgelist.List.Filtered[[x]], NA)))
Edgelist.Network <- pblapply(1:length(Edgelist.Network), function (x) Edgelist.Network[[x]][!apply(Edgelist.Network[[x]],1, function (y) {all  (is.na(y))}),])
names(Edgelist.Network) <- names(Edgelist.List.Filtered)



Network.List <- pblapply(1:length(Edgelist.Network), function(x) igraph::graph_from_data_frame(Edgelist.Network[[x]], vertices = Nodelist.List[[x]]))
Group.by.country <- pblapply(1:length(Network.List), function(x) x= as.factor(V(Network.List[[x]])$CompanyISO))
factor <- pblapply(1:length(Group.by.country), function(x) x= as.numeric(Group.by.country[[x]]))
Network.List.Countries <- pblapply(1:length(Network.List), function (x) x = contract.vertices(Network.List[[x]], factor[[x]], vertex.attr.comb = first))



## visualization with geo-mapping. Starting with creating temporary lists 


TempVis1 <- pblapply(1:length(Nodelist.List), function (x) plyr::count(Nodelist.List[[x]]$CompanyISO))
TempVis1 <- pblapply(1:length(Nodelist.List), function (x) setNames(TempVis1[[x]], c("CompanyISO", "freq")))
TempVis1 <- pblapply(1:length(TempVis1), function(x) TempVis1[[x]] %>% dplyr::mutate(freq = ifelse(TempVis1[[x]]$CompanyISO == "DE", 1 ,TempVis1[[x]]$freq)))
TempVis1 <- pblapply(1:length(TempVis1), function(x) TempVis1[[x]] %>% dplyr::mutate(freq = ifelse(TempVis1[[x]]$CompanyISO == "DE", max(TempVis1[[x]]$freq)*1.3 ,TempVis1[[x]]$freq)))

TempVis2 <- pblapply(1:length(TempVis1), function (x) dplyr::left_join(Nodelist.List[[x]], TempVis1[[x]], by = "CompanyISO"))
TempVis2 <- pblapply(1:length(TempVis2), function (x) dplyr::filter(TempVis2[[x]], CompanyISO != "NA"))
TempVis2 <- pblapply(1:length(TempVis2), function (x) dplyr::filter(TempVis2[[x]], CompanyISO != "n.a."))

TempVis3 <- pblapply(1:length(Network.List.Countries), function(x) set.vertex.attribute(Network.List.Countries[[x]], "name" , value = V(Network.List.Countries[[x]])$CompanyISO))
TempVis3 <- pblapply(1:length(TempVis3), function (x) igraph::as_data_frame(TempVis3[[x]], what = "edges"))
TempVis3 <- pblapply(1:length(TempVis3), function (x) plyr::count(TempVis3[[x]]))
TempVis3 <- pblapply(1:length(TempVis3), function (x) TempVis3[[x]] %>% dplyr::mutate(fromlat = as.numeric(Coordinates$latitude[match(from, Coordinates$country)])))
TempVis3 <- pblapply(1:length(TempVis3), function (x) TempVis3[[x]] %>% dplyr::mutate(fromlong = as.numeric(Coordinates$longitude[match(from, Coordinates$country)])))
TempVis3 <- pblapply(1:length(TempVis3), function (x) TempVis3[[x]] %>% dplyr::mutate(tolat = as.numeric(Coordinates$latitude[match(to, Coordinates$country)])))
TempVis3 <- pblapply(1:length(TempVis3), function (x) TempVis3[[x]] %>% dplyr::mutate(tolong = as.numeric(Coordinates$longitude[match(to, Coordinates$country)])))
TempVis3 <- pblapply(1:length(TempVis3), function (x) filter (TempVis3[[x]], !is.na(as.character(TempVis3[[x]]$fromlat))))
TempVis3 <- pblapply(1:length(TempVis3), function (x) filter (TempVis3[[x]], !is.na(as.character(TempVis3[[x]]$to))))
TempVis3 <- pblapply(1:length(TempVis3), function (x) filter (TempVis3[[x]], TempVis3[[x]]$to != "NA"))
TempVis3 <- pblapply(1:length(TempVis3), function (x) filter (TempVis3[[x]], TempVis3[[x]]$from != "NA"))
TempVis3 <- pblapply(1:length(TempVis3), function (x) filter (TempVis3[[x]], TempVis3[[x]]$from != TempVis3[[x]]$to))

TempVis4 <- pblapply(1:length(Network.List.Countries), function(x) set.vertex.attribute(Network.List.Countries[[x]], "name" , value = V(Network.List.Countries[[x]])$CompanyISO))
TempVis4 <- pblapply(1:length(TempVis4), function (x) data.frame("CompanyISO"= c(V(TempVis4[[x]])$name)))
TempVis4 <- pblapply(1:length(TempVis4), function (x) filter (TempVis4[[x]], CompanyISO != "NA"))
TempVis4 <- pblapply(1:length(TempVis4), function (x) filter (TempVis4[[x]], CompanyISO != "n.a."))

TempVis2 <- pblapply(1:length(TempVis2), function (x) TempVis2[[x]] %>% dplyr::rename(long = longitude, lat = latitude))
TempVis2 <- pblapply(1:length(TempVis2), function (x) TempVis2[[x]] %>% dplyr::mutate(long = as.numeric(TempVis2[[x]]$long)))
TempVis2 <- pblapply(1:length(TempVis2), function (x) TempVis2[[x]] %>% dplyr::mutate(lat = as.numeric(TempVis2[[x]]$lat)))

TempVis5 <- pblapply(1:length(Edgelist.List.Filtered), function(x)  as.data.frame(sapply(Edgelist.List.Filtered[[x]],function(y) y = Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)])))
TempVis5 <- pblapply(1:length(TempVis5), function(x) TempVis5[[x]] %>% na_if("NA"))



## here I calculate my own "GUO indicator". It is based on the average position of a country within each ownership chain on the respective year. I find it more appropriate for a visualization than the indicators included 
## in iGraph. It gives a very nice concise picture of which countries are more "GUO heavy" and which are more "Subsidiary heavy". Of course, indicators like different centrality measures and other network metrics are going
## to be included in the mathematical part of this research


TempVis4 <- pblapply(1:length(TempVis4), function(x) data.frame("ISO" = c(TempVis4[[x]]$CompanyISO), "score" = c(NA)))

TempVis4 <- pblapply(1:length(TempVis4), function (x) {
  

Temp7 <- TempVis4[[x]]

for(i in 1:nrow(TempVis4[[x]])) {
  
Temp <- TempVis5[[x]][apply(TempVis5[[x]], 1 , function(y) max(which(!is.na(y))) > 1),]
if (!isTruthy(Temp)) next
if (nrow(Temp)==0 | ncol(Temp)== 0) next

Temp <- Temp[apply(Temp, 1, function(y) distinct(y) > 1),]
if (!isTruthy(Temp)) next
if (nrow(Temp)==0 | ncol(Temp)== 0) next

Temp1 <- apply(Temp, 1 , function(y) which(y == TempVis4[[x]][i,1]))
if (!isTruthy(Temp1)) next
if (Temp1 %>% reduce(sum) == 0) next

Temp2 <- apply(Temp, 1 , function(y) max(which(!is.na(y))))
if (!isTruthy(Temp2)) next
if (Temp2 %>% reduce(sum) == 0) next

Temp3 <- lapply(1:length(Temp1), function(y) Temp1[[y]]/Temp2[y])
if (!isTruthy(Temp3)) next
if (Temp3 %>% reduce(sum) == 0) next

Temp4 <- lapply(1:length(Temp3), function (y) sum(Temp3[[y]]))
if (!isTruthy(Temp4)) next
if (Temp4 %>% reduce(sum) == 0) next
Temp5 <- lapply(1:length(Temp1), function (y) length(Temp1[[y]]))

if (!isTruthy(Temp5)) next
if (Temp5 %>% reduce(sum) == 0) next

Temp6 <- (Temp4 %>% reduce(sum)/(Temp5 %>% reduce(sum)))

Temp7[i,2] <- Temp6

}
x <- Temp7
}
)


GUOindicator <- TempVis4
for (i in 1:7) {rm (list=paste0("Temp",i))}



## assigning colors based on the GUO indicator score


for (i in 1:length(TempVis4)) {
rr <- range(TempVis4[[i]]$score)
svals <- (TempVis4[[i]]$score-rr[1])/diff(rr)
f <- colorRamp(c("red", "green"))
colors <- rgb(f(svals)/255)
TempVis4[[i]]$colors <- colors
}



for (i in 1:length(TempVis4)) {
if(nrow(TempVis4[[i]]) == 1)
{TempVis4[[i]]$colors <- "gray"}
}


TempVis3 <- lapply(1:length(TempVis3), function(x) TempVis3[[x]] %>% dplyr::mutate(colors = TempVis4[[x]]$colors[match(TempVis3[[x]]$from, TempVis4[[x]]$ISO)]))
TempVis2 <- lapply(1:length(TempVis2), function(x) TempVis2[[x]][!duplicated(TempVis2[[x]]$CompanyISO),])
TempVis2 <- lapply(1:length(TempVis2), function(x) TempVis2[[x]] %>% dplyr::mutate(colors = TempVis4[[x]]$colors[match(TempVis2[[x]]$CompanyISO, TempVis4[[x]]$ISO)]))



## here the actual geo-mapping happens


world_map <- map_data("world")
Network.Graph <- vector(mode = "list")

for (x in 1:length(TempVis3)) {
p <- ggplot() + geom_polygon(aes(x=long, y= lat, group=group), data=world_map, fill = "gray30", color = "gray50") + theme(plot.background = element_rect(fill = "black"), panel.background = element_rect(fill = "black"))
if (nrow(TempVis3[[x]]) > 0) {
marc <- TempVis3[[x]]
for(i in 1:nrow(marc)) {
      arc <- (gcIntermediate(c(marc[i,]$fromlong, marc[i,]$fromlat),
              c(marc[i,]$tolong, marc[i,]$tolat),
                                           n = 1000, addStartEnd = TRUE))
      barc <- data.frame("long" = c(arc[,1]), "lat" = c(arc[,2]))
      barc$size <- round(TempVis3[[x]][i,"freq"])
      barc$colors <- TempVis3[[x]][i,"colors"]
      p <- p + geom_path(aes(x=long,y=lat), data = barc, size = log(barc$size)/10, colour=barc$colors)
      }
    }
  p <- p + geom_point(aes(x=long, y=lat), data=TempVis2[[x]], size = log(TempVis2[[x]]$freq)/4, colour = TempVis2[[x]]$colors)
  p <- p + geom_text(aes(x=long, y=lat), data=TempVis2[[x]], label = TempVis2[[x]]$CompanyISO, size = 0.7, colour = "gray20")
  Network.Graph[[x]] <- p
  }

names(Network.Graph) <- names(Edgelist.List.Filtered)


## plot the networks

for (i in 1:length(Network.Graph)) 
{
svg(paste0("./plot/",names(Network.Graph[i]),".svg"))
plot(Network.Graph[[i]])
dev.off()
}




## cleanup


for (i in 1:5) {rm (list=paste0("TempVis",i))}
rm(p)
rm(i)
rm(colors)
rm(edge.col)
rm(edge.ind)
rm(x)
rm(j)
rm(rr)
rm(svals)
rm(f)
rm(arc)
rm(Backup)
rm(barc)
rm(Country.labels)
rm(factor)
rm(Edgelist.Network)
rm(marc)


### GUO indicator statistics

GUOindicatorDF <- GUOindicator |> purrr::reduce(left_join, by = "ISO")
colnames(GUOindicatorDF) <- c("ISO",names(Edgelist.List.Filtered))
GUOindicatorDF <- GUOindicatorDF[,order(ncol(GUOindicatorDF):1)]
GUOindicatorDF <- GUOindicatorDF[,c(ncol(GUOindicatorDF), 1:ncol(GUOindicatorDF)-1)]

GUOPlot <- melt(GUOindicatorDF, id="ISO")

colnames(GUOPlot) <- c("ISO","Year","score")

ggplot(data=GUOPlot, aes(x=ISO, y=Year, size=(1/score), color= score, group=ISO)) +
  scale_color_gradient(low = "red", high = "green") +
  theme(axis.text.y = element_text(size = 5)) +
  geom_line()


### betweenness centrality statistics

Network.List.Countries.Temp <- pblapply(1:length(Network.List.Countries), function(x) set.vertex.attribute(Network.List.Countries[[x]], "name" , value = V(Network.List.Countries[[x]])$CompanyISO))

BetwCentr <- pblapply(1:length(Network.List.Countries.Temp), function(x) as.data.frame(betweenness(Network.List.Countries.Temp[[x]],V(Network.List.Countries.Temp[[x]])$name, directed = TRUE, normalized = TRUE)))
BetwCentr <- pblapply(1:length(BetwCentr), function(x) setNames(BetwCentr[[x]],c("BetwCentr")))
BetwCentr <- pblapply(1:length(BetwCentr), function(x) data.frame("ISO" = c(rownames(BetwCentr[[x]])), "BetwCentr" = c(BetwCentr[[x]]$BetwCentr)))
BetwCentr <- pblapply(1:length(BetwCentr), function (x) filter (BetwCentr[[x]], ISO != "NA"))
BetwCentr <- pblapply(1:length(BetwCentr), function (x) filter (BetwCentr[[x]], ISO != "n.a."))

BetwCentrDF <- BetwCentr |> purrr::reduce(left_join, by ="ISO")
colnames(BetwCentrDF) <- c("ISO",names(Edgelist.List.Filtered))
BetwCentrDF <- BetwCentrDF[,order(ncol(BetwCentrDF):1)]
BetwCentrDF <- BetwCentrDF[,c(ncol(BetwCentrDF), 1:ncol(BetwCentrDF)-1)]

BetwCentrPlot <- melt(BetwCentrDF, id="ISO")

colnames(BetwCentrPlot) <- c("ISO", "Year", "BetwCentr")
BetwCentrPlot[BetwCentrPlot == "NaN"] <- NA
BetwCentrPlot$BetwCentr <- as.numeric(BetwCentrPlot$BetwCentr)
BetwCentrPlot$BetwCentr <- round(BetwCentrPlot$BetwCentr, digits = 10)
BetwCentrPlot <- BetwCentrPlot[!grepl("DE", BetwCentrPlot$ISO),]

ggplot(data=BetwCentrPlot, aes(x=ISO, y=Year, color= BetwCentr, size = BetwCentr, group=ISO)) +
  scale_color_gradient(low = "green", high = "red") +
  theme(axis.text.y = element_text(size = 5)) +
  geom_line()

rm(Network.List.Countries.Temp)






