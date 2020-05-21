# script for model selection of weather models

# Cleaning R environment
rm(list = ls())

# Loading required libraries
library(lme4)
library(AICcmodavg)
library(xtable)
library(googledrive)

# Accessing google drive
drive_find(n_max = 10)
# Select a pre-authorised account by entering the corresponding number in the console or enter '0' to obtain a new token.

# Set working directory DIFFERENT FOR EACH PERSON
setwd("")
#Ex: setwd("C:/Users/Proprietaire/Documents/uni poc/Phd/OWPC/Analyses")

##### Survival ~ Weather #############################################################################################
# Setting up and importing data ----

# Download RData from drive
drive_download("OWPC/Analyses/cache/dataSurvivalModels.RData",overwrite=T)
# Import in R environment
load("dataSurvivalModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_surv<-droplevels(subset(dataSurvScld,!(yr %in% c("1999","2000","2016"))))
# Remove unnecessary objects for the environment
rm(clim_surv,pheno_surv,weather_surv,sheep_data,dataSurvUnscld,dataSurvScld,fullSurvDataScled)

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

mod.surv.weat <- list()

# Base model
mod.surv.weat$Base <- glm(alive_t1 ~ -1 + ageClass +  pred, 
                      data=df_surv, family="binomial") 

# Winter (yr-1)
mod.surv.weat$Winter_tm1_T <- glm(alive_t1 ~ -1 + ageClass/T.WIN.m1 + pred, 
                   data=df_surv, family="binomial")

mod.surv.weat$Winter_tm1_P <- glm(alive_t1 ~ -1 + ageClass/P.WIN.m1 + pred, 
                   data=df_surv, family="binomial")

mod.surv.weat$Winter_tm1_PT <- glm(alive_t1 ~ -1 + ageClass/P.WIN.m1 + ageClass/T.WIN.m1 + pred, 
                              data=df_surv, family="binomial")

mod.surv.weat$Winter_tm1_PxT <- glm(alive_t1 ~ -1 + ageClass/(P.WIN.m1*T.WIN.m1) + pred, 
                           data=df_surv, family="binomial")

# Winter
mod.surv.weat$Winter_T <- glm(alive_t1 ~ -1 + ageClass/T.WIN + pred, 
                         data=df_surv, family="binomial")

mod.surv.weat$Winter_P <- glm(alive_t1 ~ -1 + ageClass/P.WIN + pred, 
                         data=df_surv, family="binomial")

mod.surv.weat$Winter_PT <- glm(alive_t1 ~ -1 + ageClass/P.WIN + ageClass/T.WIN + pred, 
                          data=df_surv, family="binomial")

mod.surv.weat$Winter_PxT <- glm(alive_t1 ~ -1 + ageClass/(P.WIN*T.WIN) + pred, 
                           data=df_surv, family="binomial")

# Spring
mod.surv.weat$Spring_T <- glm(alive_t1 ~ -1 + ageClass/T.SPRING + pred,
                         data=df_surv, family="binomial")

mod.surv.weat$Spring_P <- glm(alive_t1 ~ -1 + ageClass/P.SPRING + pred,
                         data=df_surv, family="binomial")

mod.surv.weat$Spring_PT <- glm(alive_t1 ~ -1 + ageClass/P.SPRING + ageClass/T.SPRING + pred, 
                          data=df_surv, family="binomial")

mod.surv.weat$Spring_PxT <- glm(alive_t1 ~ -1 + ageClass/(P.SPRING*T.SPRING) + pred,
                           data=df_surv, family="binomial")

# Summer
mod.surv.weat$Summer_T <- glm(alive_t1 ~ -1 + ageClass/T.SUMMER + pred,
                         data=df_surv, family="binomial")

mod.surv.weat$Summer_P <- glm(alive_t1 ~ -1 + ageClass/P.SUMMER + pred,
                         data=df_surv, family="binomial")

mod.surv.weat$Summer_PT <- glm(alive_t1 ~ -1 + ageClass/P.SUMMER + ageClass/T.SUMMER + pred,
                          data=df_surv, family="binomial")

mod.surv.weat$Summer_PxT <- glm(alive_t1 ~ -1 + ageClass/(P.SUMMER*T.SUMMER) + pred,
                           data=df_surv, family="binomial")

# Fall
mod.surv.weat$Fall_T <- glm(alive_t1 ~ -1 + ageClass/T.FALL +  pred,
                         data=df_surv, family="binomial")

mod.surv.weat$Fall_P <- glm(alive_t1 ~ -1 + ageClass/P.FALL + pred,
                         data=df_surv, family="binomial")

mod.surv.weat$Fall_PT <- glm(alive_t1 ~ -1 + ageClass/P.FALL + ageClass/T.FALL + pred,
                          data=df_surv, family="binomial")

mod.surv.weat$Fall_PxT <- glm(alive_t1 ~ -1 + ageClass/(P.FALL*T.FALL) + pred,
                           data=df_surv, family="binomial")

# Creating a list to store the results
results.surv.weat<-list()

## Creating and exporting AIC table to results list
results.surv.weat$aictable.surv <- xtable(aictab(mod.surv.weat), caption = NULL, label = NULL, align = NULL,
                                     digits = NULL, display = NULL, nice.names = TRUE,
                                     include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.surv.weat$aictable.surv[,3:6] <-round(results.surv.weat[["aictable.surv"]][,3:6],digits=3)

# Options to save AIC table
#print.xtable(results.surv.weat[["aictable.surv"]], type="html", file="surv_climate_aic_table.html")
#write.table(results.surv.weat[["aictable.surv"]], file="surv_climate_aic_table.csv")

# Survival results from best models -------------------------------------------------------------------------------------- 

results.surv.weat$coefs.surv.best <- data.frame(coef(summary(mod.surv.weat[[as.character(results.surv.weat[["aictable.surv"]][1,1])]])))
results.surv.weat$coefs.surv.best[, 1:4] <- round(results.surv.weat[["coefs.surv.best"]][, 1:4], digits = 3)
results.surv.weat$r2.surv.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv.weat[[as.character(results.surv.weat[["aictable.surv"]][1,1])]]), digits = 3))

results.surv.weat$coefs.surv.2ndbest <- data.frame(coef(summary(mod.surv.weat[[as.character(results.surv.weat[["aictable.surv"]][2,1])]])))
results.surv.weat$coefs.surv.2ndbest[, 1:4] <- round(results.surv.weat[["coefs.surv.2ndbest"]][, 1:4], digits = 3)
results.surv.weat$r2.surv.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv.weat[[as.character(results.surv.weat[["aictable.surv"]][2,1])]]), digits = 3))

results.surv.weat$coefs.surv.3rdbest <- data.frame(coef(summary(mod.surv.weat[[as.character(results.surv.weat[["aictable.surv"]][3,1])]])))
results.surv.weat$coefs.surv.3rdbest[, 1:4] <- round(results.surv.weat[["coefs.surv.3rdbest"]][, 1:4], digits = 3)
results.surv.weat$r2.surv.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv.weat[[as.character(results.surv.weat[["aictable.surv"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
#save(df_surv,mod.surv.weat,results.surv.weat,file = "surv_weather.Rdata")
# Upload RData on drive
#drive_upload("surv_weather.Rdata","OWPC/Analyses/results/surv_weather.RData",overwrite=T)

##### Reproduction ~ Weather #########################################################################################
# Setting up and importing data ----------------------------------------------------------------------------------------

# Download RData from drive
drive_download("OWPC/Analyses/cache/dataFecundityModels.RData",overwrite=T)
# Import in R environment
load("dataFecundityModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_fec<-droplevels(subset(dataFecScld,!(yr %in% c("1999","2000","2001"))))
# Remove unnecessary objects for the environment
rm(clim_fec,pheno_fec,weather_fec,sheep_data,dataFecUnscld,dataFecScld,fullFecDataScld)

# variable explanations 

# these contain a time lag for fecundity 
# "TSummerFec", "PSummerFec", "TAutFec","PAutFec"

# these do not contain time lags 
# "TWin","PWin","TSpring","PSpring",

# Raw repro model selection ------------------------------------------------------
mod.raw.repro.weat <- list()

# Base model
mod.raw.repro.weat$base <- glmer(raw_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID),
                            data=df_fec, family="binomial",
                            control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Winter
mod.raw.repro.weat$Winter_T <- glmer(raw_repro ~ -1 + ageClass/TWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.raw.repro.weat$Winter_P <- glmer(raw_repro ~ -1 + ageClass/PWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.raw.repro.weat$Winter_PT <-glmer(raw_repro ~ -1 + ageClass/PWin + ageClass/TWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

#mod.raw.repro.weat$Winter_PxT <- glmer(raw_repro ~ -1 + ageClass/(TWin*PWin) + MassAutumn_tm1 + (1|ID),
#                                  data=df_fec, family="binomial",
#                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 4000000))) 

# Spring
mod.raw.repro.weat$Spring_T <- glmer(raw_repro ~ -1 + ageClass/TSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.raw.repro.weat$Spring_P <- glmer(raw_repro ~ -1 + ageClass/PSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.raw.repro.weat$Spring_PT <-glmer(raw_repro ~ -1 + ageClass/PSpring + ageClass/TSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

mod.raw.repro.weat$Spring_PxT <- glmer(raw_repro ~ -1 + ageClass/(TSpring*PSpring) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Summer
mod.raw.repro.weat$Summer_T <- glmer(raw_repro ~ -1 + ageClass/TSummerFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.raw.repro.weat$Summer_P <- glmer(raw_repro ~ -1 + ageClass/PSummerFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.raw.repro.weat$Summer_PT<-glmer(raw_repro ~ -1 + ageClass/PSummerFec + ageClass/TSummerFec + MassAutumn_tm1 + (1|ID),
                               data=df_fec, family="binomial",
                               control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

#mod.raw.repro.weat$Summer_PxT <- glmer(raw_repro ~ -1 + ageClass/(TSummerFec*PSummerFec) + MassAutumn_tm1 + (1|ID),
#                                  data=df_fec, family="binomial",
#                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Fall
mod.raw.repro.weat$Fall_T <- glmer(raw_repro ~ -1 + ageClass/TAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

#mod.raw.repro.weat$Fall_P <- glmer(raw_repro ~ -1 + ageClass/PAutFec + MassAutumn_tm1 + (1|ID),
#                                data=df_fec, family="binomial",
#                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 4000000))) 

mod.raw.repro.weat$Fall_PT <-glmer(raw_repro ~ -1 + ageClass/PAutFec + ageClass/TAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

#mod.raw.repro.weat$Fall_PxT <- glmer(raw_repro ~ -1 + ageClass/(TAutFec*PAutFec) + MassAutumn_tm1 + (1|ID),
#                                  data=df_fec, family="binomial",
#                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 4000000)))

# Creating a list to store the results
results.raw.repro.weat<-list()

## Creating and exporting AIC table to results list
results.raw.repro.weat$aictable.raw.repro <- xtable(aictab(mod.raw.repro.weat), caption = NULL, label = NULL, align = NULL,
                                               digits = NULL, display = NULL, nice.names = TRUE,
                                               include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.raw.repro.weat$aictable.raw.repro[,3:6] <-round(results.raw.repro.weat[["aictable.raw.repro"]][,3:6],digits=3)

# Raw repro results from best models --------------------------------------------------------------------------------------------- 

results.raw.repro.weat$coefs.raw.repro.best <- data.frame(coef(summary(mod.raw.repro.weat[[as.character(results.raw.repro.weat[["aictable.raw.repro"]][1,1])]])))
results.raw.repro.weat$coefs.raw.repro.best[, 1:4] <- round(results.raw.repro.weat[["coefs.raw.repro.best"]][, 1:4], digits = 3)
results.raw.repro.weat$r2.raw.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro.weat[[as.character(results.raw.repro.weat[["aictable.raw.repro"]][1,1])]]), digits = 3))

results.raw.repro.weat$coefs.raw.repro.2ndbest <- data.frame(coef(summary(mod.raw.repro.weat[[as.character(results.raw.repro.weat[["aictable.raw.repro"]][2,1])]])))
results.raw.repro.weat$coefs.raw.repro.2ndbest[, 1:4] <- round(results.raw.repro.weat[["coefs.raw.repro.2ndbest"]][, 1:4], digits = 3)
results.raw.repro.weat$r2.raw.repro.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro.weat[[as.character(results.raw.repro.weat[["aictable.raw.repro"]][2,1])]]), digits = 3))

results.raw.repro.weat$coefs.raw.repro.3rdbest <- data.frame(coef(summary(mod.raw.repro.weat[[as.character(results.raw.repro.weat[["aictable.raw.repro"]][3,1])]])))
results.raw.repro.weat$coefs.raw.repro.3rdbest[, 1:4] <- round(results.raw.repro.weat[["coefs.raw.repro.3rdbest"]][, 1:4], digits = 3)
results.raw.repro.weat$r2.raw.repro.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro.weat[[as.character(results.raw.repro.weat[["aictable.raw.repro"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
#save(df_fec,mod.raw.repro.weat,results.raw.repro.weat,file = "raw_repro_weather.Rdata")
# Upload RData on drive
#drive_upload("raw_repro_weather.Rdata","OWPC/Analyses/results/raw_repro_weather.RData",overwrite=T)


# True repro model selection ------------------------------------------------------
mod.true.repro.weat <- list()

# Base model
mod.true.repro.weat$base <- glmer(true_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID),
                            data=df_fec, family="binomial",
                            control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Winter
mod.true.repro.weat$Winter_T <- glmer(true_repro ~ -1 + ageClass/TWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.true.repro.weat$Winter_P <- glmer(true_repro ~ -1 + ageClass/PWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.true.repro.weat$Winter_PT <-glmer(true_repro ~ -1 + ageClass/PWin + ageClass/TWin + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.true.repro.weat$Winter_PxT <- glmer(true_repro ~ -1 + ageClass/(TWin*PWin) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Spring
mod.true.repro.weat$Spring_T <- glmer(true_repro ~ -1 + ageClass/TSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro.weat$Spring_P <- glmer(true_repro ~ -1 + ageClass/PSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro.weat$Spring_PT <-glmer(true_repro ~ -1 + ageClass/PSpring + ageClass/TSpring + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

mod.true.repro.weat$Spring_PxT <- glmer(true_repro ~ -1 + ageClass/(TSpring*PSpring) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Summer
mod.true.repro.weat$Summer_T <- glmer(true_repro ~ -1 + ageClass/TSummerFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro.weat$Summer_P <- glmer(true_repro ~ -1 + ageClass/PSummerFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro.weat$Summer_PT<-glmer(true_repro ~ -1 + ageClass/PSummerFec + ageClass/TSummerFec + MassAutumn_tm1 + (1|ID),
                               data=df_fec, family="binomial",
                               control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

mod.true.repro.weat$Summer_PxT <- glmer(true_repro ~ -1 + ageClass/(TSummerFec*PSummerFec) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000))) 

# Fall
mod.true.repro.weat$Fall_T <- glmer(true_repro ~ -1 + ageClass/TAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro.weat$Fall_P <- glmer(true_repro ~ -1 + ageClass/PAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000))) 

mod.true.repro.weat$Fall_PT <-glmer(true_repro ~ -1 + ageClass/PAutFec + ageClass/TAutFec + MassAutumn_tm1 + (1|ID),
                                data=df_fec, family="binomial",
                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

mod.true.repro.weat$Fall_PxT <- glmer(true_repro ~ -1 + ageClass/(TAutFec*PAutFec) + MassAutumn_tm1 + (1|ID),
                                  data=df_fec, family="binomial",
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

# Creating a list to store the results
results.true.repro.weat<-list()

## Creating and exporting AIC table to results list
results.true.repro.weat$aictable.true.repro <- xtable(aictab(mod.true.repro.weat), caption = NULL, label = NULL, align = NULL,
                                                 digits = NULL, display = NULL, nice.names = TRUE,
                                                 include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.true.repro.weat$aictable.true.repro[,3:6] <-round(results.true.repro.weat[["aictable.true.repro"]][,3:6],digits=3)

# True repro results from best models --------------------------------------------------------------------------------------------- 

results.true.repro.weat$coefs.true.repro.best <- data.frame(coef(summary(mod.true.repro.weat[[as.character(results.true.repro.weat[["aictable.true.repro"]][1,1])]])))
results.true.repro.weat$coefs.true.repro.best[, 1:4] <- round(results.true.repro.weat[["coefs.true.repro.best"]][, 1:4], digits = 3)
results.true.repro.weat$r2.true.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro.weat[[as.character(results.true.repro.weat[["aictable.true.repro"]][1,1])]]), digits = 3))

results.true.repro.weat$coefs.true.repro.2ndbest <- data.frame(coef(summary(mod.true.repro.weat[[as.character(results.true.repro.weat[["aictable.true.repro"]][2,1])]])))
results.true.repro.weat$coefs.true.repro.2ndbest[, 1:4] <- round(results.true.repro.weat[["coefs.true.repro.2ndbest"]][, 1:4], digits = 3)
results.true.repro.weat$r2.true.repro.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro.weat[[as.character(results.true.repro.weat[["aictable.true.repro"]][2,1])]]), digits = 3))

results.true.repro.weat$coefs.true.repro.3rdbest <- data.frame(coef(summary(mod.true.repro.weat[[as.character(results.true.repro.weat[["aictable.true.repro"]][3,1])]])))
results.true.repro.weat$coefs.true.repro.3rdbest[, 1:4] <- round(results.true.repro.weat[["coefs.true.repro.3rdbest"]][, 1:4], digits = 3)
results.true.repro.weat$r2.true.repro.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro.weat[[as.character(results.true.repro.weat[["aictable.true.repro"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
#save(df_fec,mod.true.repro.weat,results.true.repro.weat,file = "true_repro_weather.Rdata")
# Upload RData on drive
#drive_upload("true_repro_weather.Rdata","OWPC/Analyses/results/true_repro_weather.RData",overwrite=T)
