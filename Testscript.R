




TTest <- vector(mode = "list")



TTest[["EUHavensU"]] <- tsum.test(mean.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$n,
                                      mean.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavensEU",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavensEU",]$sd, n.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavensEU",]$n, alternative = "greater", conf.level = 0.9)




TTest[["EUHavensW"]] <- tsum.test(mean.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$n,
                                      mean.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavensEU",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavensEU",]$sd, n.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavensEU",]$n, alternative = "greater", conf.level = 0.95)



TTest[["HavensU"]] <- tsum.test(mean.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$n,
                                    mean.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavens",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavens",]$sd, n.y = ETR.List[["Byanyown"]][["ETRunweighted"]][ETR.List[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavens",]$n, alternative = "greater", conf.level = 0.95)




TTest[["HavensW"]] <- tsum.test(mean.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$n,
                                    mean.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavens",]$ETR, s.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavens",]$sd, n.y = ETR.List[["Byanyown"]][["ETRweighted"]][ETR.List[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavens",]$n, alternative = "greater", conf.level = 0.98)








TTest.Pos <- vector(mode = "list")



TTest.Pos[["EUHavensU"]] <- tsum.test(mean.x = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$n,
          mean.y = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavensEU",]$ETR, s.y = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavensEU",]$sd, n.y = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavensEU",]$n, alternative = "greater", conf.level = 0.85)




TTest.Pos[["EUHavensW"]] <- tsum.test(mean.x = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$n,
          mean.y = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavensEU",]$ETR, s.y = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavensEU",]$sd, n.y = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavensEU",]$n, alternative = "greater", conf.level = 0.95)


TTest.Pos[["EUHavensProxyW"]] <- tsum.test(mean.x = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$n,
                                      mean.y = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavensEUProxy",]$ETR, s.y = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavensEUProxy",]$sd, n.y = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavensEUProxy",]$n, alternative = "greater", conf.level = 0.99)



TTest.Pos[["HavensU"]] <- tsum.test(mean.x = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "DEDOM",]$n,
          mean.y = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavens",]$ETR, s.y = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavens",]$sd, n.y = ETR.List.Pos[["Byanyown"]][["ETRunweighted"]][ETR.List.Pos[["Byanyown"]][["ETRunweighted"]]$ISO == "TaxHavens",]$n, alternative = "greater", conf.level = 0.85)




TTest.Pos[["HavensW"]] <- tsum.test(mean.x = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$ETR, s.x = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "DEDOM",]$n,
          mean.y = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavens",]$ETR, s.y = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavens",]$sd, n.y = ETR.List.Pos[["Byanyown"]][["ETRweighted"]][ETR.List.Pos[["Byanyown"]][["ETRweighted"]]$ISO == "TaxHavens",]$n, alternative = "greater", conf.level = 0.99)







TTest.Pos2 <- vector(mode = "list")



TTest.Pos2[["EUHavensU"]] <- tsum.test(mean.x = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "DEDOM",]$ETR3, s.x = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "DEDOM",]$n,
                                      mean.y = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "TaxHavensEU",]$ETR3, s.y = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "TaxHavensEU",]$sd, n.y = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "TaxHavensEU",]$n, alternative = "greater", conf.level = 0.90)




TTest.Pos2[["EUHavensProxyW"]] <- tsum.test(mean.x = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "DEDOM",]$ETR3, s.x = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "DEDOM",]$sd, n.x = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "DEDOM",]$n,
                                           mean.y = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "TaxHavensEUProxy",]$ETR3, s.y = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "TaxHavensEUProxy",]$sd, n.y = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "TaxHavensEUProxy",]$n, alternative = "greater", conf.level = 0.98)



TTest.Pos2[["HavensU"]] <- tsum.test(mean.x = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "DEDOM",]$ETR3, s.x = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "DEDOM",]$sd, n.x = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "DEDOM",]$n,
                                    mean.y = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "TaxHavens",]$ETR3, s.y = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "TaxHavens",]$sd, n.y = ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]]$ISO == "TaxHavens",]$n, alternative = "greater", conf.level = 0.85)




TTest.Pos2[["HavensW"]] <- tsum.test(mean.x = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "DEDOM",]$ETR3, s.x = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "DEDOM",]$sd, n.x = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "DEDOM",]$n,
                                    mean.y = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "TaxHavens",]$ETR3, s.y = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "TaxHavens",]$sd, n.y = ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]][ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]]$ISO == "TaxHavens",]$n, alternative = "greater", conf.level = 0.99)







TTest.EATR <- vector(mode = "list")



TTest.EATR[["EUHavensU"]] <- tsum.test(mean.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$n,
                                       mean.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEU",]$EATR, s.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEU",]$sd, n.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEU",]$n, alternative = "greater", conf.level = 0.90)



TTest.EATR[["EUHavensProxyU"]] <- tsum.test(mean.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEUProxy",]$EATR, s.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEUProxy",]$sd, n.y = EATR.List[["Byanyown"]][["EATRunweighted"]][EATR.List[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEUProxy",]$n, alternative = "greater", conf.level = 0.98)



TTest.EATR[["EUHavensProxyW"]] <- tsum.test(mean.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavensEUProxy",]$EATR, s.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavensEUProxy",]$sd, n.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavensEUProxy",]$n, alternative = "greater", conf.level = 0.98)


TTest.EATR[["HavensW"]] <- tsum.test(mean.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$n,
                                     mean.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavens",]$EATR, s.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavens",]$sd, n.y = EATR.List[["Byanyown"]][["EATRweighted"]][EATR.List[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavens",]$n, alternative = "greater", conf.level = 0.99)











TTest.EATR.Reinv <- vector(mode = "list")



TTest.EATR.Reinv[["EUHavensU"]] <- tsum.test(mean.x = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$n,
                                       mean.y = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEU",]$EATR, s.y = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEU",]$sd, n.y = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEU",]$n, alternative = "greater", conf.level = 0.90)



TTest.EATR.Reinv[["EUHavensW"]] <- tsum.test(mean.x = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$n,
                                       mean.y = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavensEU",]$EATR, s.y = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavensEU",]$sd, n.y = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavensEU",]$n, alternative = "greater", conf.level = 0.90)


TTest.EATR.Reinv[["EUHavensProxyU"]] <- tsum.test(mean.x = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEUProxy",]$EATR, s.y = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEUProxy",]$sd, n.y = EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]]$ISO == "TaxHavensEUProxy",]$n, alternative = "greater", conf.level = 0.98)



TTest.EATR.Reinv[["EUHavensProxyW"]] <- tsum.test(mean.x = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$n,
                                            mean.y = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavensEUProxy",]$EATR, s.y = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavensEUProxy",]$sd, n.y = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavensEUProxy",]$n, alternative = "greater", conf.level = 0.98)


TTest.EATR.Reinv[["HavensW"]] <- tsum.test(mean.x = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$EATR, s.x = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$sd, n.x = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "DEDOM",]$n,
                                     mean.y = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavens",]$EATR, s.y = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavens",]$sd, n.y = EATR.List.Reinv[["Byanyown"]][["EATRweighted"]][EATR.List.Reinv[["Byanyown"]][["EATRweighted"]]$ISO == "TaxHavens",]$n, alternative = "greater", conf.level = 0.99)







#EBT/EBIT ETR



full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO") |>
ggplot() +
geom_point(aes(x=reorder(ISO, ETR), y = ETR3), color = "blue", size = 3) +
geom_point(aes(x=reorder(ISO, ETR), y= ETR), color = "darkblue", size = 3) +
geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR3), color = "green", size = 3) +
geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkgreen", size = 3) +
geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO")$ISO == "TaxHavens"), aes(x=reorder(ISO, ETR), y= ETR3), color = "red", size = 3) +
geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO")$ISO == "TaxHavensEU"), aes(x=reorder(ISO, ETR), y= ETR3), color = "red", size = 3) +  
geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO")$ISO == "TaxHavensEUProxy"), aes(x=reorder(ISO, ETR), y= ETR3), color = "red", size = 3) +
geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO")$ISO == "TaxHavens"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 3) +
geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO")$ISO == "TaxHavensEU"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 3) +  
geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRunweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3unweighted"]], by = "ISO")$ISO == "TaxHavensEUProxy"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 3)
  

full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO") |>
  ggplot() +
  geom_point(aes(x=reorder(ISO, ETR), y = ETR3), color = "blue", size = 3) +
  geom_point(aes(x=reorder(ISO, ETR), y= ETR), color = "darkblue", size = 3) +
  geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR3), color = "green", size = 3) +
  geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkgreen", size = 3) +
  geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO")$ISO == "TaxHavens"), aes(x=reorder(ISO, ETR), y= ETR3), color = "red", size = 3) +
  geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO")$ISO == "TaxHavensEU"), aes(x=reorder(ISO, ETR), y= ETR3), color = "red", size = 3) +  
  geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO")$ISO == "TaxHavensEUProxy"), aes(x=reorder(ISO, ETR), y= ETR3), color = "red", size = 3) +
  geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO")$ISO == "TaxHavens"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 3) +
  geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO")$ISO == "TaxHavensEU"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 3) +  
  geom_point(data = subset(full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO"), full_join(ETR.List.Pos[["Byanyown"]][["ETRweighted"]],ETR3.List.Pos[["Byanyown"]][["ETR3weighted"]], by = "ISO")$ISO == "TaxHavensEUProxy"), aes(x=reorder(ISO, ETR), y= ETR), color = "darkred", size = 3)



#EATR

full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO") |>
  ggplot() +
  geom_point(aes(x=reorder(ISO, EATR.y), y = EATR.x), color = "blue", size = 3) +
  geom_point(aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkblue", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, EATR.y), y= EATR.x), color = "green", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkgreen", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO")$ISO == "TaxHavens"), aes(x=reorder(ISO, EATR.y), y= EATR.x), color = "red", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO")$ISO == "TaxHavensEU"), aes(x=reorder(ISO, EATR.y), y= EATR.x), color = "red", size = 3) +  
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO")$ISO == "TaxHavensEUProxy"), aes(x=reorder(ISO, EATR.y), y= EATR.x), color = "red", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO")$ISO == "TaxHavens"), aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkred", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO")$ISO == "TaxHavensEU"), aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkred", size = 3) +  
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRunweighted"]],EATR.List[["Byanyown"]][["EATRunweighted"]], by = "ISO")$ISO == "TaxHavensEUProxy"), aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkred", size = 3)
  
  
full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO") |>
  ggplot() +
  geom_point(aes(x=reorder(ISO, EATR.y), y = EATR.x), color = "blue", size = 3) +
  geom_point(aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkblue", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, EATR.y), y= EATR.x), color = "green", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "DEDOM"), aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkgreen", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "TaxHavens"), aes(x=reorder(ISO, EATR.y), y= EATR.x), color = "red", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "TaxHavensEU"), aes(x=reorder(ISO, EATR.y), y= EATR.x), color = "red", size = 3) +  
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "TaxHavensEUProxy"), aes(x=reorder(ISO, EATR.y), y= EATR.x), color = "red", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "TaxHavens"), aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkred", size = 3) +
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "TaxHavensEU"), aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkred", size = 3) +  
  geom_point(data = subset(full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO"), full_join(EATR.List.Reinv[["Byanyown"]][["EATRweighted"]],EATR.List[["Byanyown"]][["EATRweighted"]], by = "ISO")$ISO == "TaxHavensEUProxy"), aes(x=reorder(ISO, EATR.y), y= EATR.y), color = "darkred", size = 3)











## CD estimate

CDDiagram1 <- CobDou.List[["Byanyown"]][["CobDouunweighted"]][CobDou.List[["Byanyown"]][["CobDouunweighted"]]$n > 0,]
CDDiagram1$Dif <- CDDiagram1$EBT - CDDiagram1$EBTCD


CDDiagram1 |>
  ggplot() + 
  geom_segment(aes(x=reorder(ISO,EBTCD), xend = reorder(ISO,EBT), y=EBT, yend = EBTCD, group = ISO, color = Dif), size = 2) +
  geom_point(aes(x=reorder(ISO,EBTCD), y=EBT, group = ISO), shape = 15, size = 3) +
  geom_point(aes(x=reorder(ISO,EBTCD), y=EBTCD, group = ISO), shape = 19, size = 3) +
  scale_color_gradient(low = "red", high = "green")

  
  




  
  
  