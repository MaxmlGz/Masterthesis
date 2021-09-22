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


## In this script i will extract some descriptive information from the ownership chain lists



Edgelist.Countries <- pblapply(1:length(Edgelist.List.Filtered), function(x) as.data.frame({sapply(Edgelist.List.Filtered[[x]], function (y) y = NodelistALL$CompanyISO[match(y, NodelistALL$CompanyBvDID)])}))
names(Edgelist.Countries) <- names(Edgelist.List.Filtered)


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



names(Edgelist.Stat) <- names(Edgelist.List.Filtered)







## network stats COMPARISON GROSSKURT


Test <- pblapply(1:length(Edgelist.Countries),function(x)   Edgelist.Countries[[x]][sapply(sapply(apply(Edgelist.Countries[[x]],1, function(y) y[!is.na(y)]), function(z) z[length(z)] != z[length(z) -1] &  z[length(z)] != first(z) & z[length(z) -1] == first(z)), function(h) isTruthy(h) ),])

Test2 <- pblapply(1:length(Edgelist.Countries),function(x)   Edgelist.Countries[[x]][sapply(sapply(apply(Edgelist.Countries[[x]],1, function(y) y[!is.na(y)]), function(z) z[length(z)] != z[length(z) -1] &  z[length(z)] != first(z) & z[length(z) -1] != first(z)), function(h) isTruthy(h) ),])

Test3 <- pblapply(1:length(Edgelist.Countries),function(x)    Edgelist.Countries[[x]][sapply(sapply(apply(Edgelist.Countries[[x]],1, function(y) y[!is.na(y)]), function(z) z[length(z)] != z[length(z) -1] &  z[length(z)] == first(z) & z[length(z) -1] != first(z)), function(h) isTruthy(h) ),])

Test4 <- pblapply(1:length(Edgelist.Countries),function(x)    Edgelist.Countries[[x]][sapply(sapply(apply(Edgelist.Countries[[x]],1, function(y) y[!is.na(y)]), function(z) z[length(z)] == z[length(z) -1] &  z[length(z)] != first(z) & z[length(z) -1] != first(z)), function(h) isTruthy(h) ),])



Test5 <- data.frame(
  "Year" = names(Edgelist.Countries[1:(length(Edgelist.Countries)-1)]),
  "Domestic Hub" = unlist(sapply(1:length(Edgelist.Countries), function(x) nrow(Test4[[x]]) / sum(nrow(Test[[x]]),nrow(Test2[[x]]),nrow(Test3[[x]]),nrow(Test4[[x]])))),
  "Round-Tripping" = unlist(sapply(1:length(Edgelist.Countries), function(x) nrow(Test3[[x]]) / sum(nrow(Test[[x]]),nrow(Test2[[x]]),nrow(Test3[[x]]),nrow(Test4[[x]])))),
  "Conduit Structure" = unlist(sapply(1:length(Edgelist.Countries), function(x) nrow(Test2[[x]]) / sum(nrow(Test[[x]]),nrow(Test2[[x]]),nrow(Test3[[x]]),nrow(Test4[[x]])))),
  "Plain Foreign" = unlist(sapply(1:length(Edgelist.Countries), function(x) nrow(Test[[x]]) / sum(nrow(Test[[x]]),nrow(Test2[[x]]),nrow(Test3[[x]]),nrow(Test4[[x]]))))
)



melt(Test5, id="Year") |>
  subset(Year > 1990) |>
  ggplot(aes(fill = variable, y=value, x=Year)) +
  geom_bar(position="fill", stat="identity", color = "#02070F", size = 0.8, width = 0.7) +
  labs( x = "Year" , y = "Share of total structures") + 
  scale_fill_manual(values = c("#E6EDEE","#924047","#C1D0DB","#466B83")) +
  guides(fill=guide_legend(nrow=2, byrow = TRUE, reverse = TRUE)) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 6), expand = c(0, 0)) +
  ggtitle("Special ownership stuctures over time") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.height = unit(10,"mm"),
        legend.key.width = unit(40,"mm"),
        text = element_text(size=40),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 0.5, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(angle=90, vjust = 0.5, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed", size = 1.5),
        plot.title = element_text(hjust = 0.5, margin = margin(t = 0, r = 0, b = 50, l = 0)),
        axis.line.x.bottom = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y.left = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 10, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0))
  )




Test6 <- data.frame(
  "Year" = names(Edgelist.Countries[1:(length(Edgelist.Countries)-1)]),
  "Domestic Hub" = unlist(sapply(1:length(Edgelist.Countries), function(x) nrow(Test4[[x]]))),
  "Round-Tripping" = unlist(sapply(1:length(Edgelist.Countries), function(x) nrow(Test3[[x]]))),
  "Conduit Structure" = unlist(sapply(1:length(Edgelist.Countries), function(x) nrow(Test2[[x]]))),
  "Plain Foreign" = unlist(sapply(1:length(Edgelist.Countries), function(x) nrow(Test[[x]])))
)



melt(Test6, id="Year") |>
  subset(Year > 1990) |>
  ggplot(aes(fill = variable, y=value, x=Year)) +
  geom_bar(position="stack", stat="identity", color = "#02070F", size = 0.8, width = 0.7) +
  labs( x = "Year" , y = "Number of total structures") + 
  scale_fill_manual(values = c("#E6EDEE","#924047","#C1D0DB","#466B83")) +
  guides(fill=guide_legend(nrow=2, byrow = TRUE, reverse = TRUE)) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 6), expand = c(0, 0)) +
  ggtitle("Special ownership stuctures over time") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.height = unit(10,"mm"),
        legend.key.width = unit(40,"mm"),
        text = element_text(size=40),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 0.5, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(angle=90, vjust = 0.5, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed", size = 1.5),
        plot.title = element_text(hjust = 0.5, margin = margin(t = 0, r = 0, b = 50, l = 0)),
        axis.line.x.bottom = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y.left = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 10, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0))
  )



## firm count

Count.DF <- data.frame(
  "Year" = names(Edgelist.List.Filtered),
  "Chains" = pbsapply(1:length(Edgelist.List.Filtered), function(x) nrow(Edgelist.List.Filtered[[x]])),
  "Firms" = pbsapply(1:length(Edgelist.List.Filtered), function(x) length(na.omit(unique(as.character(as.matrix(Edgelist.List.Filtered[[x]])))))), 
  "Firms_Ger_Energy" = pbsapply(1:length(Edgelist.List.Filtered), function(x) length(unique(Edgelist.List.Filtered[[x]][sapply(Edgelist.List.Filtered[[x]], function (y) Nodelist.List[[x]]$CompanyISO[match(y, Nodelist.List[[x]]$CompanyBvDID)] == "DE" & str_detect(Nodelist.List[[x]]$CompanyNACECore[match(y, Nodelist.List[[x]]$CompanyBvDID)], "^35"))])))
)


rio::export(Count.DF, "CountDF.xlsx")



melt(Count.DF, id="Year") |>
  subset(!is.na(value) & as.numeric(Year) >1950) |>
  
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  scale_x_discrete (breaks = Count.DF$Year,
                     labels = str_sub(as.character(Count.DF$Year),-2,-1) ) +
  ylab("Count") +
  theme(legend.position = "bottom",
        text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1, size = 15),
        legend.title = element_blank()) 


##change


Count.DF.Change <- data.frame(
  "Year" = names(Edgelist.List.Filtered)[-1],
  "Chains" = sapply(2:length(Count.DF$Chains), function(y) (Count.DF$Chains[y-1] - Count.DF$Chains[y])  / Count.DF$Chains[y] ) ,
  "Firms" = sapply(2:length(Count.DF$Firms), function(y) (Count.DF$Firms[y-1] - Count.DF$Firms[y])  / Count.DF$Firms[y] ) ,
  "Firms_Ger_Energy" = sapply(2:length(Count.DF$Firms_Ger_Energy), function(y) (Count.DF$Firms_Ger_Energy[y-1] - Count.DF$Firms_Ger_Energy[y])  / Count.DF$Firms_Ger_Energy[y] )
) 
  
  melt(Count.DF.Change, id="Year") |>
  subset(!is.na(value))  |>
  
  ggplot() +
  geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
  scale_x_discrete (breaks = Count.DF.Change$Year[c(TRUE,FALSE)],
                    labels = str_sub(as.character(Count.DF.Change$Year[c(TRUE,FALSE)]),-2,-1) ) +
  ylab("Change") +
  theme(legend.position = "bottom",
        text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1, size = 15),
        legend.title = element_blank()) 






  
  
## chain length

  
ChainLength <-  data.frame(
    "Year" = names(Edgelist.List.Filtered)[1:136],
    "LengthALL" = sapply(1:136, function(x) mean(apply(Edgelist.List.Filtered[[x]],1, function(y) length(y[!is.na(y)])))),
    "LengthINT" = sapply(1:136, function(x) mean(apply(EdgelistInt[[x]],1, function(y) length(y[!is.na(y)])))),
    "LengthDOM" = sapply(1:136, function(x) mean(apply(EdgelistDeDom[[x]],1, function(y) length(y[!is.na(y)])))))


ChainLength |>
  
    melt(id = "Year") |>
    subset(!is.na(value) & as.numeric(Year) >1950) |>
    
    ggplot() +
    geom_line(aes(x=Year, y=value, color=variable, group=variable), size = 1.5) +
    scale_x_discrete (breaks = names(Edgelist.List.Filtered)[1:136],
                      labels = str_sub(as.character(names(Edgelist.List.Filtered)[1:136]),-2,-1) ) +
  ylab("Average Chain Length") +
  theme(legend.position = "bottom",
        text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1, size = 15),
        legend.title = element_blank()) 
  
  
  









































