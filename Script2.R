## to filter the edgelists by date of incorporation, I first extract all unique BvDID numbers for all years


BvDIDALL <- lapply(1:length(Edgelist.List), function (x) as.character(unique(unlist(Edgelist.List[[x]]))))



BvDIDALLExport <- vector(mode = "character")

for (i in 2:length(BvDIDALL)) {
  
BvDIDALLExport <- c(BvDIDALLExport,BvDIDALL[[i]])

}

BvDIDALLExport <- as.data.frame(unique(BvDIDALLExport))

rio::export(BvDIDALLExport, "BvDALLExport.xlsx")


NodelistALL <- rio::import("NodelistALL.xlsx", which = "Results")[-c(1)]

colnames(NodelistALL) <- colnames(OrbisCompanies[1:11])
  
NodelistALL <- NodelistALL %>%
  fill(CompanyBvDID, CompanyName) %>%
  dplyr::group_by(CompanyBvDID) %>%
  dplyr::summarize(
    CompanyName = first(CompanyName),
    CompanyISO = first(CompanyISO),
    CompanyNACECore = first(CompanyNACECore),
    CompanyNACE = paste2(CompanyNACE, sep = ",", trim = TRUE),
    CompanyPostcode = first(CompanyPostcode),
    CompanyCity = first(CompanyCity),
    CompanyStart = first(CompanyStart),
    CompanyStatus = first(CompanyStatus),
    CompanyEnd = first(CompanyEnd),
  )


Date.Of.Incorporation.List <- vector(mode = "list")

Date.Of.Incorporation.List <- lapply(1:length(Edgelist.List), function (x) {as.data.frame(sapply(Edgelist.List[[x]], function (y) NodelistALL$CompanyStart [match(y, NodelistALL$CompanyBvDID)]))})

DateOfDiscontinuationTemp <- subset(NodelistALL, str_detect(CompanyStatus, "Active") == FALSE)

Date.Of.Discontinuation.List <- vector(mode = "list")

Date.Of.Discontinuation.List <- lapply(1:length(Edgelist.List), function (x) {as.data.frame(sapply(Edgelist.List[[x]], function (y) DateOfDiscontinuationTemp$CompanyEnd [match(y, DateOfDiscontinuationTemp$CompanyBvDID)]))})

rm(DateOfDiscontinuationTemp)



Filter.Incorporation <- lapply(1:length(Date.Of.Incorporation.List), function (x) {as.data.frame(sapply(1:ncol(Date.Of.Incorporation.List[[x]]), function (y) Date.Of.Incorporation.List[[x]][,y] <= (2022-x)))})

for (i in 1:length(Filter.Incorporation)) {
  
  Filter.Incorporation[[i]][is.na(Filter.Incorporation[[i]])] <- TRUE
  
}



Filter.Discontinuation <- lapply(1:length(Date.Of.Discontinuation.List), function (x) {as.data.frame(sapply(1:ncol(Date.Of.Discontinuation.List[[x]]), function (y) Date.Of.Discontinuation.List[[x]][,y] >= (2022-x)))})

for (i in 1:length(Filter.Discontinuation)) {
  
  Filter.Discontinuation[[i]][is.na(Filter.Discontinuation[[i]])] <- TRUE
  
}









