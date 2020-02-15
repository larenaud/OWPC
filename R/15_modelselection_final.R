# pheno, climat, weather, sheep
library(googledrive)
rm(list = ls())

  setwd("~/uSherbrooke/Hiver 2020/NDVI/OWPC")
  

  drive_download("OWPC/Analyses/data/Raw/Climat/season_climate_ram", type="csv", overwrite=T)
  clim<-read.csv("season_climate_ram.csv", header=T, sep=",")
  
  # Load dfweather.Rdata
  #load("C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/dfweather.Rdata")

  df<-merge(df_fec, clim, by="yr", all.x=T)
  
  #### RUN raw_repro models ####
  
  # 
  names(clim)
  mod.raw <- list()
  mod.raw$base <- glmer(raw_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID), 
                      data=df, 
                      family="binomial",
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000))) 
  
  mod.raw$PDO <- glmer(raw_repro ~ -1 + ageClass/PDO.spring + MassAutumn_tm1 + (1|ID), 
                      data=df, 
                      family="binomial",
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000))) 
  
  mod.raw$P.T.WIN.m1 <-glmer(raw_repro ~ -1 + ageClass/P.WIN.m1 +ageClass/T.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="nmkbw", 
                                                  optCtrl = list(maxfun = 2000000))) 
  
  mod.raw$combined <- glmer(raw_repro ~ -1 + ageClass/PDO.spring + ageClass/T.WIN.m1 + ageClass/P.WIN.m1 + MassAutumn_tm1 + (1|ID), 
                      data=df, 
                      family="binomial",
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000))) 
  
  x <- aictab(mod.raw)
  aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                     digits = NULL, display = NULL, nice.names = TRUE,
                     include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
  print.xtable(aictable, type="html", 
               file="C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/Raw_repro_combinedmodel.html") # open directly with Word

  summary(mod.raw$combined)
  
##### True Repro ####
  
  names(clim)
  mod.true <- list()
  mod.true$base <- glmer(true_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID), 
                        data=df, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 2000000))) 
  
  mod.true$clim.win <- glmer(true_repro ~ -1 + ageClass/PDO.winter + ageClass/SOI.winter + MassAutumn_tm1 + (1|ID), 
                       data=df, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000))) 
  
  mod.true$weather <-glmer(true_repro ~ -1 + ageClass/P.WIN.m1 +ageClass/T.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000))) 
  
  mod.true$combined <- glmer(true_repro ~ -1 + ageClass/PDO.winter + ageClass/SOI.winter + ageClass/T.WIN.m1 + ageClass/P.WIN.m1 + MassAutumn_tm1 + (1|ID), 
                            data=df, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000))) 
  
  x <- aictab(mod.true)
  aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                     digits = NULL, display = NULL, nice.names = TRUE,
                     include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
  print.xtable(aictable, type="html", 
               file="C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/true_repro_combinedmodel.html") # open directly with Word
  
  summary(mod.true$clim.win)