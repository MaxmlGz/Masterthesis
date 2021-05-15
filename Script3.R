


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

NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('GY', CompanyPostcode)]$CompanyBvDID, "GY" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('GUERNSEY', CompanyName)]$CompanyBvDID, "GY" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('GUERNSEY|ALDERNEY|SARK|HERM|JETHOU|BRECQHOU|LIHOU|BURHOU|CASQUETS|BRECHOU|PIERRE|VALE|TORTEVAL|FOREST|ANDREW|MARTIN|SAMPSON|SAVIOUR|CASTEL', CompanyCity),][CompanyISO == "GB"]$CompanyBvDID, "GY" ,CompanyISO))

## Delaware

NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('^19', CompanyPostcode)][CompanyISO == "US"]$CompanyBvDID, "DEL" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('DELAWARE', CompanyName)][CompanyISO == "US"]$CompanyBvDID, "DEL" ,CompanyISO))
NodelistALLNAICs <- NodelistALLNAICs %>% mutate(CompanyISO = ifelse (CompanyBvDID %in% NodelistALLNAICs[grepl('DELAWARE|DOVER|HARRINGTON|LEWES|METROPOLIS|MILFORD|NEW CASTLE|NEWARK|REHOBOTH|SEAFORD|WILMINGTON', CompanyCity)][CompanyISO == "US"]$CompanyBvDID, "DEL" ,CompanyISO))






