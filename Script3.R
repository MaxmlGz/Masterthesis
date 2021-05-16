


## as I later also want to check the impact of the Energiewende and Orbis does not deliver detailes NACE codes, I decided to put a little merge function here and use NAICS codes as a proxy before creating the networks

## first, I export all necassary BvDID numbers again


rio::export(NodelistALL$CompanyBvDID, "ExportNAICS.xlsx")


ImportNAICs <- rio::import("ImportNAICS.xlsx", which = "Results")

ImportNAICs <- ImportNAICs[,3:4]

ImportNAICs <- ImportNAICs %>%
  fill(`BvD ID number`) %>%
  dplyr::group_by(`BvD ID number`) %>%
  dplyr::summarize(
    `NAICS 2017, primary code(s)` = paste2(`NAICS 2017, primary code(s)`, sep = ",", trim = TRUE),)


setDT(NodelistALL)
setDT(ImportNAICs)

setkey(NodelistALL, CompanyBvDID)
setkey(ImportNAICs, `BvD ID number`)

NodelistALLNAICs <- NodelistALL[ImportNAICs]


## next, before I add geographic data, I replace the ISO code of non-nation tax havens (like guernsey, the city of london etc.) with an own unique code using postcodes, city/state names and company names


## City of London

CTLPostcodes <- rio::import("City of London postcodes.csv")
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyPostcode %in% CTLPostcodes$Postcode, "CTL", CompanyISO))


## Jersey

NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('^JE', CompanyPostcode),]$CompanyBvDID, "JE" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('JERSEY|HELIER|CLEMENT|SAVIOUR|MARTIN|GROUVILLE|MARY|JOHN|TRINITY|OUEN|LAWRENCE', CompanyCity),][CompanyISO == "GB"]$CompanyBvDID, "JE" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALL[grepl('JERSEY', CompanyName),]$CompanyBvDID, "JE" ,CompanyISO))


## Isle of Man

NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('^IM', CompanyPostcode),]$CompanyBvDID, "IM" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('ISLE OF MAN', CompanyName),]$CompanyBvDID, "IM" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('PATRICK|DOUGLAS|RAMSEY|CASTLETOWN|ONCHAN|PEEL|BRADDAN|ERIN|BALLASALLA|MARY|LAXEY|SAINT|KIRK|SANTON|MARLEY|ARBORY|ERIN|BRADDAN|FOXDALE|SODERICK|ANDREAS|BALLAUGH|BRIDE|JURBY|LEZAYRE|JOHNS|JOHN|GREEBA|BALDRINE|LONAN|SULBY|MAUGHOLD', CompanyCity),][CompanyISO == "GB"]$CompanyBvDID, "IM" ,CompanyISO))


## Guernsey

NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('GY', CompanyPostcode)]$CompanyBvDID, "GG" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('GUERNSEY', CompanyName)]$CompanyBvDID, "GG" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('GUERNSEY|ALDERNEY|SARK|HERM|JETHOU|BRECQHOU|LIHOU|BURHOU|CASQUETS|BRECHOU|PIERRE|VALE|TORTEVAL|FOREST|ANDREW|MARTIN|SAMPSON|SAVIOUR|CASTEL', CompanyCity),][CompanyISO == "GB"]$CompanyBvDID, "GG" ,CompanyISO))


## Delaware

NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('^19', CompanyPostcode)][CompanyISO == "US"]$CompanyBvDID, "DEL" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('DELAWARE', CompanyName)][CompanyISO == "US"]$CompanyBvDID, "DEL" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('DELAWARE|DOVER|HARRINGTON|LEWES|METROPOLIS|MILFORD|NEW CASTLE|NEWARK|REHOBOTH|SEAFORD|WILMINGTON', CompanyCity)][CompanyISO == "US"]$CompanyBvDID, "DEL" ,CompanyISO))


## now I assign geocordinates to each country

Coordinates <- rio::import("Geocoordinates.xlsx")
Coordinates <- rbind(Coordinates, data.frame(country = "CTL", latitude = "51.51279", longitude = "0.09184", name = "City of London"))

NodelistALLNAICs <- merge(x=NodelistALLNAICs, y=Coordinates, by.x = "CompanyISO", by.y = "country", all.x = TRUE)
NodelistALLNAICs <- subset(NodelistALLNAICs, select = c(2,3,1,4:ncol(NodelistALLNAICs)))


## create list of Nodelists for every year

Nodelist.List <- pblapply(1:length(Edgelist.List.Filtered), function (x) x = subset(NodelistALLNAICs, CompanyBvDID %in% as.matrix(Edgelist.List.Filtered[[x]][])))

Nodelist.List.Missing <- pblapply(1:length(Edgelist.List.Filtered), function(x) Edgelist.List.Filtered[[x]][sapply(Edgelist.List.Filtered[[x]], function(y) y %notin% Nodelist.List[[x]]$CompanyBvDID)])
Nodelist.List.Missing <- pblapply(1:length(Nodelist.List.Missing), function(x) data.frame(CompanyBvDID = c(Nodelist.List.Missing[[x]])))
Nodelist.List.Missing <- pblapply(1:length(Nodelist.List.Missing), function (x) Nodelist.List.Missing[[x]][!apply(Nodelist.List.Missing[[x]],1, function (y) {all  (is.na(y))}),])
Nodelist.List.Missing <- pblapply(1:length(Nodelist.List.Missing), function(x) data.frame(CompanyBvDID = c(Nodelist.List.Missing[[x]])))

Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) rbind(Nodelist.List[[x]],unique(Nodelist.List.Missing[[x]]), fill = TRUE))
Nodelist.List <- pblapply(1:length(Nodelist.List), function(x) rbind.fill(Nodelist.List[[x]], data.frame(NA)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]][,1:13])

Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyISO = ifelse(is.na(Nodelist.List[[x]]$CompanyISO), OrbisCompanies$CSHISO[match(Nodelist.List[[x]]$CompanyBvDID, OrbisCompanies$CSHBvDID)],Nodelist.List[[x]]$CompanyISO)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyName = ifelse(is.na(Nodelist.List[[x]]$CompanyName), OrbisCompanies$CSHName[match(Nodelist.List[[x]]$CompanyBvDID, OrbisCompanies$CSHBvDID)],Nodelist.List[[x]]$CompanyName)))

Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyISO = ifelse(is.na(Nodelist.List[[x]]$CompanyISO), Merge1$CSHISO[match(Nodelist.List[[x]]$CompanyBvDID, Merge1$CSHBvDID)],Nodelist.List[[x]]$CompanyISO)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyName = ifelse(is.na(Nodelist.List[[x]]$CompanyName), Merge1$CSHName[match(Nodelist.List[[x]]$CompanyBvDID, Merge1$CSHBvDID)],Nodelist.List[[x]]$CompanyName)))

Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyISO = ifelse(is.na(Nodelist.List[[x]]$CompanyISO), Merge2$CSHISO[match(Nodelist.List[[x]]$CompanyBvDID, Merge2$CSHBvDID)],Nodelist.List[[x]]$CompanyISO)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyName = ifelse(is.na(Nodelist.List[[x]]$CompanyName), Merge2$CSHName[match(Nodelist.List[[x]]$CompanyBvDID, Merge2$CSHBvDID)],Nodelist.List[[x]]$CompanyName)))

Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyISO = ifelse(is.na(Nodelist.List[[x]]$CompanyISO), OrbisCompanies$CompanyISO[match(Nodelist.List[[x]]$CompanyBvDID, OrbisCompanies$CompanyBvDID)],Nodelist.List[[x]]$CompanyISO)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(CompanyName = ifelse(is.na(Nodelist.List[[x]]$CompanyName), OrbisCompanies$CompanyName[match(Nodelist.List[[x]]$CompanyBvDID, OrbisCompanies$CompanyBvDID)],Nodelist.List[[x]]$CompanyName)))

Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(longitude = ifelse(is.na(Nodelist.List[[x]]$longitude), Coordinates$longitude[match(Nodelist.List[[x]]$CompanyISO, Coordinates$country)],Nodelist.List[[x]]$longitude)))
Nodelist.List <- pblapply(1:length(Nodelist.List), function (x) Nodelist.List[[x]] %>% dplyr::mutate(latitude = ifelse(is.na(Nodelist.List[[x]]$latitude), Coordinates$latitude[match(Nodelist.List[[x]]$CompanyISO, Coordinates$country)],Nodelist.List[[x]]$latitude)))


## create networks for each year

Edgelist.Network <- pblapply(1:(length(Edgelist.List.Filtered)-3), function (x) as.data.frame(do.call(bind_rows, lapply(1:(ncol(Edgelist.List.Filtered[[x]])-1), function(y) Edgelist.List.Filtered[[x]][,y:(y+1)]))))
Edgelist.Network <- pblapply(1:length(Edgelist.Network), function(x)  {t(apply(Edgelist.Network[[x]],1,function(y) {c(y[!is.na(y)],y[is.na(y)])}))})
Edgelist.Network <- pblapply(1:length(Edgelist.Network), function(x) Edgelist.Network[[x]][,!apply(Edgelist.Network[[x]],2, function(y) all(is.na(y)))])
Edgelist.Network <- pblapply(1:length(Edgelist.Network), function(x) as.data.frame(Edgelist.Network[[x]]))
Edgelist.Network <- c(Edgelist.Network, lapply((length(Edgelist.List.Filtered)-2):length(Edgelist.List.Filtered), function (x) x = cbind(Edgelist.List.Filtered[[x]], NA)))
Edgelist.Network <- pblapply(1:length(Edgelist.Network), function (x) Edgelist.Network[[x]][!apply(Edgelist.Network[[x]],1, function (y) {all  (is.na(y))}),])
names(Edgelist.Network) <- names(Edgelist.List.Filtered)





Network.List <- pblapply(1:length(Edgelist.Network), function(x) igraph::graph_from_data_frame(Edgelist.Network[[x]], vertices = Nodelist.List[[x]]))








