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





TTest.ETR <- vector(mode = "list")

TTest.ETR[["ConduitsU"]] <- tsum.test(mean.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$n,
          mean.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Conduits",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Conduits",]$sd, n.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Conduits",]$n, alternative = "greater")

TTest.ETR[["ConduitsW"]] <- tsum.test(mean.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$n,
          mean.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Conduits",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Conduits",]$sd, n.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Conduits",]$n, alternative = "greater")

TTest.ETR[["ConduitsProxyUW"]] <- tsum.test(mean.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$n,
                                           mean.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "ConduitsProxy",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "ConduitsProxy",]$sd, n.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "ConduitsProxy",]$n, alternative = "greater")

TTest.ETR[["ConduitsProxyW"]] <- tsum.test(mean.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$n,
                                      mean.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "ConduitsProxy",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "ConduitsProxy",]$sd, n.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "ConduitsProxy",]$n, alternative = "greater")

TTest.ETR[["SinksU"]] <- tsum.test(mean.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$n,
          mean.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Sinks",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Sinks",]$sd, n.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Sinks",]$n, alternative = "greater")

TTest.ETR[["SinksW"]] <- tsum.test(mean.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$n,
          mean.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Sinks",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Sinks",]$sd, n.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Sinks",]$n, alternative = "greater")







TTest.ETR2 <- vector(mode = "list")

TTest.ETR2[["ConduitsU"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$n,
                                      mean.y = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "Conduits",]$ETR2, s.y = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "Conduits",]$sd, n.y = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "Conduits",]$n, alternative = "greater")

TTest.ETR2[["ConduitsW"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$n,
                                      mean.y = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "Conduits",]$ETR2, s.y = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "Conduits",]$sd, n.y = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "Conduits",]$n, alternative = "greater")

TTest.ETR2[["ConduitsProxyUW"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "ConduitsProxy",]$ETR2, s.y = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "ConduitsProxy",]$sd, n.y = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "ConduitsProxy",]$n, alternative = "greater")

TTest.ETR2[["ConduitsProxyW"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$n,
                                           mean.y = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "ConduitsProxy",]$ETR2, s.y = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "ConduitsProxy",]$sd, n.y = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "ConduitsProxy",]$n, alternative = "greater")

TTest.ETR2[["SinksU"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$n,
                                    mean.y = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "Sinks",]$ETR2, s.y = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "Sinks",]$sd, n.y = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "Sinks",]$n, alternative = "greater")

TTest.ETR2[["SinksW"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$n,
                                    mean.y = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "Sinks",]$ETR2, s.y = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "Sinks",]$sd, n.y = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "Sinks",]$n, alternative = "greater")







TTest.ETR.Cross <- vector(mode = "list")

TTest.ETR.Cross[["ConduitsU"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Conduits",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Conduits",]$sd, n.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Conduits",]$n, alternative = "greater")

TTest.ETR.Cross[["ConduitsW"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Conduits",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Conduits",]$sd, n.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Conduits",]$n, alternative = "greater")

TTest.ETR.Cross[["ConduitsProxyUW"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "ConduitsProxy",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "ConduitsProxy",]$sd, n.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "ConduitsProxy",]$n, alternative = "greater")

TTest.ETR.Cross[["ConduitsProxyW"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "ConduitsProxy",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "ConduitsProxy",]$sd, n.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "ConduitsProxy",]$n, alternative = "greater")

TTest.ETR.Cross[["SinksU"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2unweighted"]][ETR2.List[["Byanyown"]][["ETR2unweighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Sinks",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Sinks",]$sd, n.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "Sinks",]$n, alternative = "greater")

TTest.ETR.Cross[["SinksW"]] <- tsum.test(mean.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$ETR2, s.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$sd, n.x = ETR2.List[["Byanyown"]][["ETR2weighted"]][ETR2.List[["Byanyown"]][["ETR2weighted"]]$ISO == "DEDOM",]$n,
                                          mean.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Sinks",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Sinks",]$sd, n.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "Sinks",]$n, alternative = "greater")


















TTest.EATR <- vector(mode = "list")

TTest.EATR[["ConduitsU"]] <- tsum.test(mean.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$n,
                                       mean.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "Conduits",]$EATR, s.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "Conduits",]$sd, n.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "Conduits",]$n, alternative = "greater")

TTest.EATR[["ConduitsW"]] <- tsum.test(mean.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$n,
                                       mean.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "Conduits",]$EATR, s.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "Conduits",]$sd, n.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "Conduits",]$n, alternative = "greater")

TTest.EATR[["ConduitsProxyUW"]] <- tsum.test(mean.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$n,
                                             mean.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "ConduitsProxy",]$EATR, s.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "ConduitsProxy",]$sd, n.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "ConduitsProxy",]$n, alternative = "greater")

TTest.EATR[["ConduitsProxyW"]] <- tsum.test(mean.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "ConduitsProxy",]$EATR, s.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "ConduitsProxy",]$sd, n.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "ConduitsProxy",]$n, alternative = "greater")

TTest.EATR[["SinksU"]] <- tsum.test(mean.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$n,
                                     mean.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "Sinks",]$EATR, s.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "Sinks",]$sd, n.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "Sinks",]$n, alternative = "greater")

TTest.EATR[["SinksW"]] <- tsum.test(mean.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$n,
                                     mean.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "Sinks",]$EATR, s.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "Sinks",]$sd, n.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "Sinks",]$n, alternative = "greater")







TTest.CD <- vector(mode = "list")


TTest.CD[["ConduitsU"]] <- tsum.test(mean.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentUW, s.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentUW.SD, n.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$n,
                                     mean.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Conduits",]$PercentUW, s.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Conduits",]$PercentUW.SD, n.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Conduits",]$n, alternative = "greater")

TTest.CD[["ConduitsW"]] <- tsum.test(mean.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentW, s.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentW.SD, n.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$n,
                                     mean.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Conduits",]$PercentW, s.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Conduits",]$PercentW.SD, n.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Conduits",]$n, alternative = "greater")

TTest.CD[["ConduitsProxyU"]] <- tsum.test(mean.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentUW, s.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentUW.SD, n.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$n,
                                   mean.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "ConduitsProxy",]$PercentUW, s.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "ConduitsProxy",]$PercentUW.SD, n.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "ConduitsProxy",]$n, alternative = "greater")

TTest.CD[["ConduitsProxyW"]] <- tsum.test(mean.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentW, s.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentW.SD, n.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$n,
                                          mean.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "ConduitsProxy",]$PercentW, s.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "ConduitsProxy",]$PercentW.SD, n.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "ConduitsProxy",]$n, alternative = "greater")

TTest.CD[["SinksU"]] <- tsum.test(mean.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentUW, s.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentUW.SD, n.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$n,
                                    mean.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Sinks",]$PercentUW, s.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Sinks",]$PercentUW.SD, n.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Sinks",]$n, alternative = "greater")

TTest.CD[["SinksW"]] <- tsum.test(mean.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentW, s.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$PercentW.SD, n.x = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "DEDOM",]$n,
                                   mean.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Sinks",]$PercentW, s.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Sinks",]$PercentW.SD, n.y = CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$ISO == "Sinks",]$n, alternative = "greater")










#EBT/EBIT ETR



full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO") |>
subset(ISO != "GerGUO" & ISO != "DeInt") |>
ggplot() +
geom_point(aes(x=reorder(ISO, ETR), y = ETR2), color = "blue", size = 4) +
geom_text(aes(x=reorder(ISO, ETR), y = ETR2, label = n.y), color = "blue", size = 6, nudge_x = -0.25, nudge_y = -0.01, check_overlap = TRUE) +
geom_point(aes(x=reorder(ISO, ETR), y= ETR), color = "darkblue", size = 4) +
geom_text(aes(x=reorder(ISO, ETR), y = ETR, label = n.x), color = "darkblue", size = 6, nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR2), color = "green", size = 4) +
geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR2, label = n.y), color = "green", size = 6, nudge_x = -0.25, nudge_y = -0.01, check_overlap = TRUE) +
geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkgreen", size = 4) +
geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR, label = n.x), color = "darkgreen", size = 6, nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, ETR), y= ETR2), color = "red", size = 4) +
geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, ETR), y= ETR2, label = n.y), color = "red", size = 6, nudge_x = -0.25, nudge_y = -0.01, check_overlap = TRUE) +
geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, ETR), y= ETR2), color = "red", size = 4) +  
geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, ETR), y= ETR2, label = n.y), color = "red", size = 6, nudge_x = -0.25, nudge_y = -0.01, check_overlap = TRUE) +
geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, ETR), y= ETR2), color = "red", size = 4) +
geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, ETR), y= ETR2, label = n.y), color = "red", size = 6, nudge_x = -0.25, nudge_y = -0.01, check_overlap = TRUE) +
geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 4) +
geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, ETR), y= ETR, label = n.x), color = "darkred",  size = 6, nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 4) +  
geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, ETR), y= ETR, label = n.x), color = "darkred", size = 6, nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 4) + 
geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRunweighted"]],ETR2.List[["Byanyown"]][["ETR2unweighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, ETR), y= ETR, label = n.x), color = "darkred", size = 6, nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
labs ( x = "Jurisdiction" , y = "ETR") +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_y_continuous(labels = scales::percent)
  



full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO") |>
  subset(ISO != "GerGUO" & ISO != "DeInt") |>
  ggplot() +
  geom_point(aes(x=reorder(ISO, ETR), y = ETR2), color = "blue", size = 4) +
  geom_text(aes(x=reorder(ISO, ETR), y = ETR2, label = n.y), color = "blue", size = 6, nudge_x = -0.25, nudge_y = -0.01, check_overlap = TRUE) +
  geom_point(aes(x=reorder(ISO, ETR), y= ETR), color = "darkblue", size = 4) +
  geom_text(aes(x=reorder(ISO, ETR), y = ETR, label = n.x), color = "darkblue", size = 6, nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
  geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR2), color = "green", size = 4) +
  geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR2, label = n.y), color = "green", size = 6, nudge_x = -0.25, nudge_y = -0.01, check_overlap = TRUE) +
  geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkgreen", size = 4) +
  geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR, label = n.x), color = "darkgreen", size = 6, nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
  geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, ETR), y= ETR2), color = "red", size = 4) +
  geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, ETR), y= ETR2, label = n.y), color = "red", size = 6, nudge_x = -0.25, nudge_y = -0.01, check_overlap = TRUE) +
  geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, ETR), y= ETR2), color = "red", size = 4) +  
  geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, ETR), y= ETR2, label = n.y), color = "red", size = 6, nudge_x = -0.25, nudge_y = -0.01, check_overlap = TRUE) +
  geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, ETR), y= ETR2), color = "red", size = 4) +
  geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, ETR), y= ETR2, label = n.y), color = "red", size = 6, nudge_x = -0.25, nudge_y = -0.01, check_overlap = TRUE) +
  geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 4) +
  geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, ETR), y= ETR, label = n.x), color = "darkred",  size = 6, nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
  geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 4) +  
  geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, ETR), y= ETR, label = n.x), color = "darkred", size = 6, nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
  geom_point(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 4) + 
  geom_text(data = subset(full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO"), full_join(ETR.List[["Byanyown"]][["ETRweighted"]],ETR2.List[["Byanyown"]][["ETR2weighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, ETR), y= ETR, label = n.x), color = "darkred", size = 6, nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
  labs ( x = "Jurisdiction" , y = "ETR") +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_y_continuous(labels = scales::percent)








#EATR


full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO") |>
  ggplot() +
  geom_point(aes(x=reorder(ISO, EATR.y), y = EATR.x), color = "blue", size = 4) +
  geom_text(aes(x=reorder(ISO, EATR.y), y = EATR.x, label = n.x), color = "blue", size = 6, nudge_x = 0.25, nudge_y = 0.005, check_overlap = TRUE) +
  geom_point(aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkblue", size = 4) +
  geom_text(aes(x=reorder(ISO, EATR.y), y = EATR.y, label = n.y), color = "darkblue", size = 6, nudge_x = -0.25, nudge_y = -0.005, check_overlap = TRUE) +
  geom_point(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, EATR.x), y= EATR.y), color = "darkgreen", size = 4) +
  geom_text(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, EATR.x), y= EATR.y, label = n.y), color = "darkgreen", size = 6, nudge_x = -0.25, nudge_y = -0.005, check_overlap = TRUE) +
  geom_point(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, EATR.x), y= EATR.x), color = "green", size = 4) +
  geom_text(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, EATR.x), y= EATR.x, label = n.x), color = "green", size = 6, nudge_x = 0.25, nudge_y = 0.005, check_overlap = TRUE) +
  geom_point(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, EATR.x), y= EATR.y), color = "darkred", size = 4) +
  geom_text(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, EATR.x), y= EATR.y, label = n.y), color = "darkred", size = 6, nudge_x = -0.25, nudge_y = -0.005, check_overlap = TRUE) +
  geom_point(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, EATR.x), y= EATR.y), color = "darkred", size = 4) +  
  geom_text(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, EATR.x), y= EATR.y, label = n.y), color = "darkred", size = 6, nudge_x = -0.25, nudge_y = -0.005, check_overlap = TRUE) +
  geom_point(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, EATR.x), y= EATR.y), color = "darkred", size = 4) +
  geom_text(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, EATR.x), y= EATR.y, label = n.y), color = "darkred", size = 6, nudge_x = -0.25, nudge_y = -0.005, check_overlap = TRUE) +
  geom_point(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, EATR.x), y= EATR.x), color = "red", size = 4) +
  geom_text(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "Sinks"), aes(x=reorder(ISO, EATR.x), y= EATR.x, label = n.x), color = "red",  size = 6, nudge_x = 0.25, nudge_y = 0.005, check_overlap = TRUE) +
  geom_point(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, EATR.x), y= EATR.x), color = "red", size = 4) +  
  geom_text(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "Conduits"), aes(x=reorder(ISO, EATR.x), y= EATR.x, label = n.x), color = "red", size = 6, nudge_x = 0.25, nudge_y = 0.005, check_overlap = TRUE) +
  geom_point(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, EATR.x), y= EATR.x), color = "red", size = 4) + 
  geom_text(data = subset(full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "ConduitsProxy"), aes(x=reorder(ISO, EATR.x), y= EATR.x, label = n.x), color = "red", size = 6, nudge_x = 0.25, nudge_y = 0.005, check_overlap = TRUE) +
  labs ( x = "Jurisdiction" , y = "EATR") +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_y_continuous(labels = scales::percent)








#CD

CobDou.List[["Byanyown"]][["CobDouunweighted"]] |>
  subset(ISO != "GerGUO" & ISO != "DEINT" & !is.na(PercentUW)) |>
  ggplot() +
  geom_point(aes(x=reorder(ISO, PercentW), y = PercentUW), color = "blue", size = 3) +
  geom_text(aes(x=reorder(ISO,PercentW), y = PercentUW, label = n), color = "blue", size = 6, nudge_x = -0.25, nudge_y = -0.2, check_overlap = TRUE) +
  geom_point(aes(x=reorder(ISO, PercentW), y= PercentW), color = "darkblue", size = 3) +
  geom_text(aes(x=reorder(ISO, PercentW), y= PercentW, label = n), color = "darkblue", size = 6, nudge_x = 0.25, nudge_y = 0.2, check_overlap = TRUE) +
  geom_point(data = subset(CobDou.List[["Byanyown"]][["CobDouunweighted"]], ISO == "DEDOM"), aes(x=reorder(ISO, PercentW), y = PercentW), color = "darkgreen", size = 3) +
  geom_text(data = subset(CobDou.List[["Byanyown"]][["CobDouunweighted"]], ISO == "DEDOM"), aes(x=reorder(ISO, PercentW), y = PercentW, label = n), color = "darkgreen", size = 6, nudge_x = 0.25, nudge_y = 0.2, check_overlap = TRUE) +
  geom_point(data = subset(CobDou.List[["Byanyown"]][["CobDouunweighted"]], ISO == "DEDOM"), aes(x=reorder(ISO, PercentW), y = PercentUW), color = "green", size = 3) +
  geom_text(data = subset(CobDou.List[["Byanyown"]][["CobDouunweighted"]], ISO == "DEDOM"), aes(x=reorder(ISO, PercentW), y = PercentUW, label = n), color = "green", size = 6, nudge_x = -0.25, nudge_y = -0.2, check_overlap = TRUE) +
  geom_point(data = subset(CobDou.List[["Byanyown"]][["CobDouunweighted"]], ISO == "Sinks" | ISO =="Conduits" | ISO == "ConduitsProxy"), aes(x=reorder(ISO, PercentW), y = PercentUW), color = "red", size = 3) +
  geom_text(data = subset(CobDou.List[["Byanyown"]][["CobDouunweighted"]], ISO == "Sinks" | ISO =="Conduits" | ISO == "ConduitsProxy"), aes(x=reorder(ISO, PercentW), y = PercentUW, label = n), color = "red", size = 6, nudge_x = -0.25, nudge_y = -0.2, check_overlap = TRUE) +
  geom_point(data = subset(CobDou.List[["Byanyown"]][["CobDouunweighted"]], ISO == "Sinks" | ISO =="Conduits" | ISO == "ConduitsProxy"), aes(x=reorder(ISO, PercentW), y = PercentW), color = "darkred", size = 3) +
  geom_text(data = subset(CobDou.List[["Byanyown"]][["CobDouunweighted"]], ISO == "Sinks" | ISO =="Conduits" | ISO == "ConduitsProxy"), aes(x=reorder(ISO, PercentW), y = PercentW, label = n), color = "darkred", size = 6, nudge_x = 0.25, nudge_y = 0.2, check_overlap = TRUE) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "tomato3", size = 2) + 
  geom_hline(yintercept = 1, linetype = "dotted", color = "olivedrab3", size = 2) +
  labs ( x = "Jurisdiction" , y = "reported profit / estimated real profit") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_y_continuous(labels = scales::percent, breaks = scales::breaks_extended(n = 12))








  
  
  