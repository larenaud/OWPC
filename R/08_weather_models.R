library(plyr)
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(cowplot)
library(googledrive)
library(xtable)
library(AICcmodavg)
library(readxl)

rm(list = ls())

setwd("") # where to download

drive_find(n_max = 10)

drive_download(
  "dataSurvivalModels.RData", # specify path and change to TRUE if desired
  overwrite = FALSE
)

drive_download(
 "dataFecundityModels.RData",
  overwrite = FALSE
)

# after moving in appropriate file try to import r objects

load("cache/dataSurvivalModels.RData") # get dataSurvScld
load("cache/dataFecundityModels.RData")



# variable explanations ---------------------------------------------------

# these have no time lags (are original data )

#T.WIN.m1
# P.WIN.m1
# T.SPRING.m1
# P.SPRING.m1
# T.SUMMER
# P.SUMMER
# T.FALL
# P.FALL


# these are time lags t+1

# "T.WIN", "P.WIN", "T.SPRING", "P.SPRING"


# replaced df_surv by dataSurvScld



# run survival models -----------------------------------------------------

colnames(dataSurvScld)



mod.l <- list()

# base
mod.l$base <- glm(alive_t1 ~ -1 + ageClass +  pred, 
                      data=dataSurvScld, 
                      family="binomial") 



# Winter (yr-1)
mod.l$P.T.Win.m1 <- glm(alive_t1 ~ -1 + ageClass/P.WIN.m1+ageClass/T.WIN.m1 +  pred, 
                     data=dataSurvScld, 
                     family="binomial")


mod.l$PxT.Win.m1 <- glm(alive_t1 ~ -1 + ageClass/(P.WIN.m1*T.WIN.m1) +  pred, 
                     data=dataSurvScld, 
                     family="binomial")

mod.l$T.Win.m1 <- glm(alive_t1 ~ -1 + ageClass/T.WIN.m1 +  pred, 
                   data=dataSurvScld, 
                   family="binomial")

mod.l$P.Win.m1 <- glm(alive_t1 ~ -1 + ageClass/P.WIN.m1 +  pred, 
                   data=dataSurvScld, 
                   family="binomial")

# Spring (yr-1)

mod.l$P.T.Spring.m1 <- glm(alive_t1 ~ -1 + ageClass/P.SPRING.m1+ageClass/T.SPRING.m1 +  pred, 
                        data=dataSurvScld, 
                        family="binomial")

mod.l$PxT.Spring.m1 <- glm(alive_t1 ~ -1 + ageClass/(P.SPRING.m1*T.SPRING.m1) +  pred, 
                        data=dataSurvScld, 
                        family="binomial")

mod.l$T.Spring.m1 <- glm(alive_t1 ~ -1 + ageClass/T.SPRING.m1 +  pred, 
                      data=dataSurvScld, 
                      family="binomial")

mod.l$P.Spring.m1 <- glm(alive_t1 ~ -1 + ageClass/P.SPRING.m1 +  pred, 
                      data=dataSurvScld, 
                      family="binomial")

# Winter
mod.l$P.T.Win <- glm(alive_t1 ~ -1 + ageClass/P.WIN+ageClass/T.WIN +  pred, 
                     data=dataSurvScld, 
                     family="binomial")


mod.l$PxT.Win <- glm(alive_t1 ~ -1 + ageClass/(P.WIN*T.WIN) +  pred, 
                     data=dataSurvScld, 
                     family="binomial")

mod.l$T.Win <- glm(alive_t1 ~ -1 + ageClass/T.WIN +  pred, 
                   data=dataSurvScld, 
                   family="binomial")

mod.l$P.Win <- glm(alive_t1 ~ -1 + ageClass/P.WIN +  pred, 
                   data=dataSurvScld, 
                   family="binomial")

# Spring

mod.l$P.T.Spring <- glm(alive_t1 ~ -1 + ageClass/P.SPRING+ageClass/T.SPRING +  pred, 
                        data=dataSurvScld, 
                        family="binomial")

mod.l$PxT.Spring <- glm(alive_t1 ~ -1 + ageClass/(P.SPRING*T.SPRING) +  pred, 
                        data=dataSurvScld, 
                        family="binomial")

mod.l$T.Spring <- glm(alive_t1 ~ -1 + ageClass/T.SPRING +  pred, 
                      data=dataSurvScld, 
                      family="binomial")

mod.l$P.Spring <- glm(alive_t1 ~ -1 + ageClass/P.SPRING +  pred, 
                      data=dataSurvScld, 
                      family="binomial")

# Summer
mod.l$P.T.Summer <- glm(alive_t1 ~ -1 + ageClass/P.SUMMER+ageClass/T.SUMMER +  pred, 
                        data=dataSurvScld, 
                        family="binomial")

mod.l$PxT.Summer <- glm(alive_t1 ~ -1 + ageClass/(P.SUMMER*T.SUMMER) +  pred, 
                        data=dataSurvScld, 
                        family="binomial")

mod.l$T.Summer <- glm(alive_t1 ~ -1 + ageClass/T.SUMMER +  pred, 
                      data=dataSurvScld, 
                      family="binomial")
# Fall

mod.l$P.Fall <- glm(alive_t1 ~ -1 + ageClass/P.FALL +  pred, 
                    data=dataSurvScld, 
                    family="binomial")

mod.l$P.T.Fall <- glm(alive_t1 ~ -1 + ageClass/P.FALL+ageClass/T.FALL +  pred, 
                      data=dataSurvScld, 
                      family="binomial")

mod.l$PxT.Fall <- glm(alive_t1 ~ -1 + ageClass/(P.FALL*T.FALL) +  pred, 
                      data=dataSurvScld, 
                      family="binomial")

mod.l$T.Fall <- glm(alive_t1 ~ -1 + ageClass/T.FALL +  pred, 
                    data=dataSurvScld, 
                    family="binomial")

mod.l$P.Fall <- glm(alive_t1 ~ -1 + ageClass/P.FALL +  pred, 
                    data=dataSurvScld, 
                    family="binomial")


## exporting AIC table
x <- aictab(mod.l)
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)


print.xtable(aictable, type="html", 
             file="graph/weather_surv_AIC.html") # open directly with Word




# MIGHT NOT BE GOOD ANYMORE




# results 
summary(mod.l$T.Fall)
results.T.Win <- data.frame(coef(summary(mod.l$T.Win)))
results.T.Win[, 1:4] <- round(results.T.Win[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$T.Win), digits = 3) #
#R2m   R2c
#theoretical 0.257 0.301
#delta       0.222 0.260
getwd()



results_weather_surv<- write.csv(results.T.Win, file = "C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/results_weather_surv.xls", row.names = TRUE)



# run RAW reproduction models ------------------------------------------------------



dataFecScld<-filter(dataFecScld, yr>=2000)


# variable explanations 



# these contain a time lag for fecundity 
# "TSummerFec", "PSummerFec", "TAutFec","PAutFec"

# these do not contain time lags 
# "TWin","PWin","TSpring","PSpring",



mod.l <- list()
mod.l$base <- glmer(raw_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID), # here write the model
                    data=dataFecScld, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 2000000))) 


# Winter yr-1


mod.l$PTWin <-glmer(raw_repro ~ -1 + ageClass/PWin +ageClass/TWin+  MassAutumn_tm1 + (1|ID), # here write the model
                            data=dataFecScld, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", # nmkbw
                                                   optCtrl = list(maxfun = 2000000))) 


mod.l$TxPWin <- glmer(raw_repro ~ -1 + ageClass/(TWin*PWin) +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=dataFecScld, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 


mod.l$TWin <- glmer(raw_repro ~ -1 + ageClass/TWin +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=dataFecScld, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 

mod.l$PWin <- glmer(raw_repro ~ -1 + ageClass/PWin +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=dataFecScld, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 

# Spring yr-1

mod.l$PTSpring <-glmer(raw_repro ~ -1 + ageClass/PSpring +ageClass/TSpring +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=dataFecScld, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 
mod.l$TxPSpring <- glmer(raw_repro ~ -1 + ageClass/(TSpring*PSpring) +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=dataFecScld, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 

mod.l$TSpring <- glmer(raw_repro ~ -1 + ageClass/TSpring +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=dataFecScld, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

mod.l$PSpring <- glmer(raw_repro ~ -1 + ageClass/PSpring +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=dataFecScld, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

# Summer yr-1
mod.l$PTSummerFec <-glmer(raw_repro ~ -1 + ageClass/PSummerFec +ageClass/TSummerFec +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=dataFecScld, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 
mod.l$TxPSummerFec <- glmer(raw_repro ~ -1 + ageClass/(TSummerFec*PSummerFec) +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=dataFecScld, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 

mod.l$TSummerFec <- glmer(raw_repro ~ -1 + ageClass/TSummerFec +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=dataFecScld, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

mod.l$PSummerFec <- glmer(raw_repro ~ -1 + ageClass/PSummerFec +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=dataFecScld, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

# Fall yr-1
mod.l$P.TAutFec <-glmer(raw_repro ~ -1 + ageClass/PAutFec +ageClass/TAutFec +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=dataFecScld, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 
mod.l$TxPAutFec <- glmer(raw_repro ~ -1 + ageClass/(TAutFec*PAutFec) +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=dataFecScld, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 

mod.l$TAutFec <- glmer(raw_repro ~ -1 + ageClass/TAutFec +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=dataFecScld, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

mod.l$PAutFec <- glmer(raw_repro ~ -1 + ageClass/PAutFec +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=dataFecScld, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

## exporting AIC table
x <- aictab(mod.l)
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

print.xtable(aictable, type="html", 
             file="C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/weather_rawrepro_AIC.html") # open directly with Word
getwd()



# THESE MIGHT NOT BE GOOD ANYMORE (LR)

# results 
summary(mod.l$P.T.WIN.m1)
library(boot)
inv.logit(coef(summary(mod.l$P.T.WIN.m1)))
results.T.Win <- data.frame(coef(summary(mod.l$T.Win)))
results.T.Win[, 1:4] <- round(results.T.Win[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$P.T.WIN.m1), digits = 3) #
#R2m   R2c
#theoretical 0.257 0.301
#delta       0.222 0.260



# Run TRUE reproduction models --------------------------------------------


mod.l <- list()
mod.l$base <- glmer(true_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID), # here write the model
                    data=dataFecScld, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 2000000))) 


# Winter yr-1


mod.l$PTWin <-glmer(true_repro ~ -1 + ageClass/PWin +ageClass/TWin+  MassAutumn_tm1 + (1|ID), # here write the model
                    data=dataFecScld, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", # nmkbw
                                           optCtrl = list(maxfun = 2000000))) 


mod.l$TxPWin <- glmer(true_repro ~ -1 + ageClass/(TWin*PWin) +  MassAutumn_tm1 + (1|ID), # here write the model
                      data=dataFecScld, 
                      family="binomial",
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000))) 


mod.l$TWin <- glmer(true_repro ~ -1 + ageClass/TWin +  MassAutumn_tm1 + (1|ID), # here write the model
                    data=dataFecScld, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 2000000))) 

mod.l$PWin <- glmer(true_repro ~ -1 + ageClass/PWin +  MassAutumn_tm1 + (1|ID), # here write the model
                    data=dataFecScld, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 2000000))) 

# Spring yr-1

mod.l$PTSpring <-glmer(true_repro ~ -1 + ageClass/PSpring +ageClass/TSpring +  MassAutumn_tm1 + (1|ID), # here write the model
                       data=dataFecScld, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000))) 
mod.l$TxPSpring <- glmer(true_repro ~ -1 + ageClass/(TSpring*PSpring) +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=dataFecScld, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 

mod.l$TSpring <- glmer(true_repro ~ -1 + ageClass/TSpring +  MassAutumn_tm1 + (1|ID), # here write the model
                       data=dataFecScld, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 1000000))) 

mod.l$PSpring <- glmer(true_repro ~ -1 + ageClass/PSpring +  MassAutumn_tm1 + (1|ID), # here write the model
                       data=dataFecScld, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 1000000))) 

# Summer yr-1
mod.l$PTSummerFec <-glmer(true_repro ~ -1 + ageClass/PSummerFec +ageClass/TSummerFec +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=dataFecScld, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 
mod.l$TxPSummerFec <- glmer(true_repro ~ -1 + ageClass/(TSummerFec*PSummerFec) +  MassAutumn_tm1 + (1|ID), # here write the model
                            data=dataFecScld, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000))) 

mod.l$TSummerFec <- glmer(true_repro ~ -1 + ageClass/TSummerFec +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=dataFecScld, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 1000000))) 

mod.l$PSummerFec <- glmer(true_repro ~ -1 + ageClass/PSummerFec +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=dataFecScld, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 1000000))) 

# Fall yr-1
mod.l$P.TAutFec <-glmer(true_repro ~ -1 + ageClass/PAutFec +ageClass/TAutFec +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=dataFecScld, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 2000000))) 
mod.l$TxPAutFec <- glmer(true_repro ~ -1 + ageClass/(TAutFec*PAutFec) +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=dataFecScld, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 

mod.l$TAutFec <- glmer(true_repro ~ -1 + ageClass/TAutFec +  MassAutumn_tm1 + (1|ID), # here write the model
                       data=dataFecScld, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 1000000))) 

mod.l$PAutFec <- glmer(true_repro ~ -1 + ageClass/PAutFec +  MassAutumn_tm1 + (1|ID), # here write the model
                       data=dataFecScld, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 1000000))) 



## exporting AIC table
x <- aictab(mod.l)
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)


print.xtable(aictable, type="html", 
             file="weather_truerepro_AIC.html") # Change path if nec. 
getwd()



# MIGHT NOT BE GOOD ANYMORE (LR)



# results 
summary(mod.l$P.T.WIN.m1)
library(boot)
inv.logit(coef(summary(mod.l$P.T.WIN.m1)))
results.T.Win <- data.frame(coef(summary(mod.l$T.Win)))
results.T.Win[, 1:4] <- round(results.T.Win[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$P.T.WIN.m1), digits = 3) #
# R2m   R2c
# theoretical 0.327 0.777
# delta       0.280 0.664

