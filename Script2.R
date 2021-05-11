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





