# script for model selection of weather models

# Cleaning R environment
rm(list = ls())

# Loading required libraries
library(lme4)
library(AICcmodavg)
library(xtable)
library(googledrive)

# Set working directory DIFFERENT FOR EACH PERSON
setwd("")
#Ex: setwd("C:/Users/Proprietaire/Documents/uni poc/Phd/OWPC/Analyses")

# Accessing google drive
drive_find(n_max = 10)
# Select a pre-authorised account by entering the corresponding number in the console or enter '0' to obtain a new token.

##### Survival ~ Weather #############################################################################################
# Setting up and importing data ----

# Download RData from drive
drive_download("OWPC/Analyses/cache/dataSurvivalModels.RData",overwrite=T)
# Import in R environment
load("dataSurvivalModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_surv<-droplevels(subset(dataSurvScld,!(yr %in% c("1999","2000","2016"))))
# Remove unnecessary objects for the environment
rm(clim_surv,pheno_surv,weather_surv,sheep_data,dataSurvUnscld,dataSurvScld)

# variable explanations

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

# Survival model selection -----------------------------------------------------

mod.surv <- list()

# Base model
mod.surv$Base <- glm(alive_t1 ~ -1 + ageClass +  pred, 
                      data=df_surv, family="binomial") 

# Winter (yr-1)
mod.surv$Winter_tm1_T <- glm(alive_t1 ~ -1 + ageClass/T.WIN.m1 + pred, 
                   data=df_surv, family="binomial")

mod.surv$Winter_tm1_P <- glm(alive_t1 ~ -1 + ageClass/P.WIN.m1 + pred, 
                   data=df_surv, family="binomial")

mod.surv$Winter_tm1_PT <- glm(alive_t1 ~ -1 + ageClass/P.WIN.m1 + ageClass/T.WIN.m1 + pred, 
                              data=df_surv, family="binomial")

mod.surv$Winter_tm1_PxT <- glm(alive_t1 ~ -1 + ageClass/(P.WIN.m1*T.WIN.m1) + pred, 
                           data=df_surv, family="binomial")

# Winter
mod.surv$Winter_T <- glm(alive_t1 ~ -1 + ageClass/T.WIN + pred, 
                         data=df_surv, family="binomial")

mod.surv$Winter_P <- glm(alive_t1 ~ -1 + ageClass/P.WIN + pred, 
                         data=df_surv, family="binomial")

mod.surv$Winter_PT <- glm(alive_t1 ~ -1 + ageClass/P.WIN + ageClass/T.WIN + pred, 
                          data=df_surv, family="binomial")

mod.surv$Winter_PxT <- glm(alive_t1 ~ -1 + ageClass/(P.WIN*T.WIN) + pred, 
                           data=df_surv, family="binomial")

# Spring
mod.surv$Spring_T <- glm(alive_t1 ~ -1 + ageClass/T.SPRING + pred,
                         data=df_surv, family="binomial")

mod.surv$Spring_P <- glm(alive_t1 ~ -1 + ageClass/P.SPRING + pred,
                         data=df_surv, family="binomial")

mod.surv$Spring_PT <- glm(alive_t1 ~ -1 + ageClass/P.SPRING + ageClass/T.SPRING + pred, 
                          data=df_surv, family="binomial")

mod.surv$Spring_PxT <- glm(alive_t1 ~ -1 + ageClass/(P.SPRING*T.SPRING) + pred,
                           data=df_surv, family="binomial")

# Summer
mod.surv$Summer_T <- glm(alive_t1 ~ -1 + ageClass/T.SUMMER + pred,
                         data=df_surv, family="binomial")

mod.surv$Summer_P <- glm(alive_t1 ~ -1 + ageClass/P.SUMMER + pred,
                         data=df_surv, family="binomial")

mod.surv$Summer_PT <- glm(alive_t1 ~ -1 + ageClass/P.SUMMER + ageClass/T.SUMMER + pred,
                          data=df_surv, family="binomial")

mod.surv$Summer_PxT <- glm(alive_t1 ~ -1 + ageClass/(P.SUMMER*T.SUMMER) + pred,
                           data=df_surv, family="binomial")

# Autumn
mod.surv$Autumn_T <- glm(alive_t1 ~ -1 + ageClass/T.FALL +  pred,
                         data=df_surv, family="binomial")

mod.surv$Autumn_P <- glm(alive_t1 ~ -1 + ageClass/P.FALL + pred,
                         data=df_surv, family="binomial")

mod.surv$Autumn_PT <- glm(alive_t1 ~ -1 + ageClass/P.FALL + ageClass/T.FALL + pred,
                          data=df_surv, family="binomial")

mod.surv$Autumn_PxT <- glm(alive_t1 ~ -1 + ageClass/(P.FALL*T.FALL) + pred,
                           data=df_surv, family="binomial")

# Creating a list to store the results
results.surv<-list()

## Creating and exporting AIC table to results list
results.surv$aictable.surv <- xtable(aictab(mod.surv), caption = NULL, label = NULL, align = NULL,
                                     digits = NULL, display = NULL, nice.names = TRUE,
                                     include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.surv$aictable.surv[,3:6] <-round(results.surv[["aictable.surv"]][,3:6],digits=3)

# Options to save AIC table
#print.xtable(results.surv[["aictable.surv"]], type="html", file="surv_climate_aic_table.html")
#write.table(results.surv[["aictable.surv"]], file="surv_climate_aic_table.csv")

# Survival results from best models -------------------------------------------------------------------------------------- 

results.surv$coefs.surv.best <- data.frame(coef(summary(mod.surv[[as.character(results.surv[["aictable.surv"]][1,1])]])))
results.surv$coefs.surv.best[, 1:4] <- round(results.surv[["coefs.surv.best"]][, 1:4], digits = 3)
results.surv$r2.surv.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv[[as.character(results.surv[["aictable.surv"]][1,1])]]), digits = 3))

results.surv$coefs.surv.2ndbest <- data.frame(coef(summary(mod.surv[[as.character(results.surv[["aictable.surv"]][2,1])]])))
results.surv$coefs.surv.2ndbest[, 1:4] <- round(results.surv[["coefs.surv.2ndbest"]][, 1:4], digits = 3)
results.surv$r2.surv.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv[[as.character(results.surv[["aictable.surv"]][2,1])]]), digits = 3))

results.surv$coefs.surv.3rdbest <- data.frame(coef(summary(mod.surv[[as.character(results.surv[["aictable.surv"]][3,1])]])))
results.surv$coefs.surv.3rdbest[, 1:4] <- round(results.surv[["coefs.surv.3rdbest"]][, 1:4], digits = 3)
results.surv$r2.surv.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv[[as.character(results.surv[["aictable.surv"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
# save(df_surv,mod.surv,results.surv,file = "surv_weather.Rdata")

##### Reproduction ~ Weather #########################################################################################
# Setting up and importing data ----------------------------------------------------------------------------------------

# Download RData from drive
drive_download("OWPC/Analyses/cache/dataFecundityModels.RData",overwrite=T)
# Import in R environment
load("dataFecundityModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_fec<-droplevels(subset(dataFecScld,!(yr %in% c("1999","2000","2001"))))
# Remove unnecessary objects for the environment
rm(clim_fec,pheno_fec,weather_fec,sheep_data,dataFecUnscld,dataFecScld)

# variable explanations 

# these contain a time lag for fecundity 
# "TSummerFec", "PSummerFec", "TAutFec","PAutFec"

# these do not contain time lags 
# "TWin","PWin","TSpring","PSpring",

# Raw repro model selection ------------------------------------------------------
mod.raw.repro <- list()

# Base model
mod.raw.repro$base <- glmer(raw_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID),
                            data=df_fec, family="binomial",
                            control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Winter
mod.raw.repro$Winter_T <- glmer(raw_repro ~ -1 + ageClass/TWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.raw.repro$Winter_P <- glmer(raw_repro ~ -1 + ageClass/PWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.raw.repro$Winter_PT <-glmer(raw_repro ~ -1 + ageClass/PWin + ageClass/TWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.raw.repro$Winter_PxT <- glmer(raw_repro ~ -1 + ageClass/(TWin*PWin) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Spring
mod.raw.repro$Spring_T <- glmer(raw_repro ~ -1 + ageClass/TSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.raw.repro$Spring_P <- glmer(raw_repro ~ -1 + ageClass/PSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.raw.repro$Spring_PT <-glmer(raw_repro ~ -1 + ageClass/PSpring + ageClass/TSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

mod.raw.repro$Spring_PxT <- glmer(raw_repro ~ -1 + ageClass/(TSpring*PSpring) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Summer
mod.raw.repro$Summer_T <- glmer(raw_repro ~ -1 + ageClass/TSummerFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.raw.repro$Summer_P <- glmer(raw_repro ~ -1 + ageClass/PSummerFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.raw.repro$Summer_PT<-glmer(raw_repro ~ -1 + ageClass/PSummerFec + ageClass/TSummerFec + MassAutumn_tm1 + (1|ID),
                               data=df_fec, family="binomial",
                               control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.raw.repro$Summer_PxT <- glmer(raw_repro ~ -1 + ageClass/(TSummerFec*PSummerFec) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Autumn
mod.raw.repro$Autumn_T <- glmer(raw_repro ~ -1 + ageClass/TAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.raw.repro$Autumn_P <- glmer(raw_repro ~ -1 + ageClass/PAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.raw.repro$Autumn_PT <-glmer(raw_repro ~ -1 + ageClass/PAutFec + ageClass/TAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

mod.raw.repro$Autumn_PxT <- glmer(raw_repro ~ -1 + ageClass/(TAutFec*PAutFec) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

# Creating a list to store the results
results.raw.repro<-list()

## Creating and exporting AIC table to results list
results.raw.repro$aictable.raw.repro <- xtable(aictab(mod.raw.repro), caption = NULL, label = NULL, align = NULL,
                                               digits = NULL, display = NULL, nice.names = TRUE,
                                               include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.raw.repro$aictable.raw.repro[,3:6] <-round(results.raw.repro[["aictable.raw.repro"]][,3:6],digits=3)

# Raw repro results from best models --------------------------------------------------------------------------------------------- 

results.raw.repro$coefs.raw.repro.best <- data.frame(coef(summary(mod.raw.repro[[as.character(results.raw.repro[["aictable.raw.repro"]][1,1])]])))
results.raw.repro$coefs.raw.repro.best[, 1:4] <- round(results.raw.repro[["coefs.raw.repro.best"]][, 1:4], digits = 3)
results.raw.repro$r2.raw.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro[[as.character(results.raw.repro[["aictable.raw.repro"]][1,1])]]), digits = 3))

results.raw.repro$coefs.raw.repro.2ndbest <- data.frame(coef(summary(mod.raw.repro[[as.character(results.raw.repro[["aictable.raw.repro"]][2,1])]])))
results.raw.repro$coefs.raw.repro.2ndbest[, 1:4] <- round(results.raw.repro[["coefs.raw.repro.2ndbest"]][, 1:4], digits = 3)
results.raw.repro$r2.raw.repro.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro[[as.character(results.raw.repro[["aictable.raw.repro"]][2,1])]]), digits = 3))

results.raw.repro$coefs.raw.repro.3rdbest <- data.frame(coef(summary(mod.raw.repro[[as.character(results.raw.repro[["aictable.raw.repro"]][3,1])]])))
results.raw.repro$coefs.raw.repro.3rdbest[, 1:4] <- round(results.raw.repro[["coefs.raw.repro.3rdbest"]][, 1:4], digits = 3)
results.raw.repro$r2.raw.repro.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro[[as.character(results.raw.repro[["aictable.raw.repro"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
# save(df_fec,mod.raw.repro,results.raw.repro,file = "raw.repro_weather.Rdata")



# True repro model selection ------------------------------------------------------
mod.true.repro <- list()

# Base model
mod.true.repro$base <- glmer(true_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID),
                            data=df_fec, family="binomial",
                            control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Winter
mod.true.repro$Winter_T <- glmer(true_repro ~ -1 + ageClass/TWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.true.repro$Winter_P <- glmer(true_repro ~ -1 + ageClass/PWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.true.repro$Winter_PT <-glmer(true_repro ~ -1 + ageClass/PWin + ageClass/TWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.true.repro$Winter_PxT <- glmer(true_repro ~ -1 + ageClass/(TWin*PWin) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Spring
mod.true.repro$Spring_T <- glmer(true_repro ~ -1 + ageClass/TSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro$Spring_P <- glmer(true_repro ~ -1 + ageClass/PSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro$Spring_PT <-glmer(true_repro ~ -1 + ageClass/PSpring + ageClass/TSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

mod.true.repro$Spring_PxT <- glmer(true_repro ~ -1 + ageClass/(TSpring*PSpring) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Summer
mod.true.repro$Summer_T <- glmer(true_repro ~ -1 + ageClass/TSummerFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro$Summer_P <- glmer(true_repro ~ -1 + ageClass/PSummerFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro$Summer_PT<-glmer(true_repro ~ -1 + ageClass/PSummerFec + ageClass/TSummerFec + MassAutumn_tm1 + (1|ID),
                               data=df_fec, family="binomial",
                               control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.true.repro$Summer_PxT <- glmer(true_repro ~ -1 + ageClass/(TSummerFec*PSummerFec) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Autumn
mod.true.repro$Autumn_T <- glmer(true_repro ~ -1 + ageClass/TAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro$Autumn_P <- glmer(true_repro ~ -1 + ageClass/PAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro$Autumn_PT <-glmer(true_repro ~ -1 + ageClass/PAutFec + ageClass/TAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

mod.true.repro$Autumn_PxT <- glmer(true_repro ~ -1 + ageClass/(TAutFec*PAutFec) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

# Creating a list to store the results
results.true.repro<-list()

## Creating and exporting AIC table to results list
results.true.repro$aictable.true.repro <- xtable(aictab(mod.true.repro), caption = NULL, label = NULL, align = NULL,
                                                 digits = NULL, display = NULL, nice.names = TRUE,
                                                 include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.true.repro$aictable.true.repro[,3:6] <-round(results.true.repro[["aictable.true.repro"]][,3:6],digits=3)

# True repro results from best models --------------------------------------------------------------------------------------------- 

results.true.repro$coefs.true.repro.best <- data.frame(coef(summary(mod.true.repro[[as.character(results.true.repro[["aictable.true.repro"]][1,1])]])))
results.true.repro$coefs.true.repro.best[, 1:4] <- round(results.true.repro[["coefs.true.repro.best"]][, 1:4], digits = 3)
results.true.repro$r2.true.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro[[as.character(results.true.repro[["aictable.true.repro"]][1,1])]]), digits = 3))

results.true.repro$coefs.true.repro.2ndbest <- data.frame(coef(summary(mod.true.repro[[as.character(results.true.repro[["aictable.true.repro"]][2,1])]])))
results.true.repro$coefs.true.repro.2ndbest[, 1:4] <- round(results.true.repro[["coefs.true.repro.2ndbest"]][, 1:4], digits = 3)
results.true.repro$r2.true.repro.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro[[as.character(results.true.repro[["aictable.true.repro"]][2,1])]]), digits = 3))

results.true.repro$coefs.true.repro.3rdbest <- data.frame(coef(summary(mod.true.repro[[as.character(results.true.repro[["aictable.true.repro"]][3,1])]])))
results.true.repro$coefs.true.repro.3rdbest[, 1:4] <- round(results.true.repro[["coefs.true.repro.3rdbest"]][, 1:4], digits = 3)
results.true.repro$r2.true.repro.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro[[as.character(results.true.repro[["aictable.true.repro"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
# save(df_fec,mod.true.repro,results.true.repro,file = "true.repro_weather.Rdata")
