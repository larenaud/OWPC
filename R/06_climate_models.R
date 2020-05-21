# script for model selection of climate models

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

##### Survival ~ Climate ###############################################################################################
# Setting up and importing data  ---------------------------------------------------------------------------------------

# Download RData from drive
drive_download("OWPC/Analyses/cache/dataSurvivalModels.RData",overwrite=T)
# Import in R environment
load("dataSurvivalModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_surv<-droplevels(subset(dataSurvScld,!(yr %in% c("1999","2000","2016"))))
# Remove unnecessary objects for the environment
rm(clim_surv,pheno_surv,weather_surv,sheep_data,dataSurvUnscld,dataSurvScld,fullSurvDataScled)
# Model selection -----------------------------------------------------------------------------------------------------------
#colnames(df_surv)
# List of candidate models
mod.surv.clim <- list()

# Base model
mod.surv.clim$base <- glm(alive_t1 ~ -1 + ageClass + pred, data=df_surv, family="binomial")

# Summer 
mod.surv.clim$Summer_PDO <- glm(alive_t1 ~ -1 + ageClass/PDO.summer_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv.clim$Summer_SOI <- glm(alive_t1 ~ -1 + ageClass/SOI.summer_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv.clim$Summer_PDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.summer_surv + ageClass/SOI.summer_surv + pred, 
                          data=df_surv, family="binomial")

mod.surv.clim$Summer_Int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_summer + pred, 
                        data=df_surv, family="binomial")

# Fall
mod.surv.clim$Fall_PDO <- glm(alive_t1 ~ -1 + ageClass/PDO.fall_surv + pred, 
                     data=df_surv, family="binomial")

mod.surv.clim$Fall_SOI <- glm(alive_t1 ~ -1 + ageClass/SOI.fall_surv + pred, 
                     data=df_surv, family="binomial")

mod.surv.clim$Fall_PDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.fall_surv + ageClass/SOI.fall_surv + pred, 
                        data=df_surv, family="binomial") 

mod.surv.clim$Fall_Int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_fall + pred, 
                      data=df_surv, family="binomial")

# Winter
mod.surv.clim$Winter_PDO <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv.clim$Winter_SOI <- glm(alive_t1 ~ -1 + ageClass/SOI.winter_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv.clim$Winter_PDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_surv+ageClass/SOI.winter_surv + pred, 
                          data=df_surv, family="binomial") 

mod.surv.clim$Winter_Int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_winter + pred, 
                        data=df_surv, family="binomial")

# Spring
mod.surv.clim$Spring_PDO <- glm(alive_t1 ~ -1 + ageClass/PDO.spring_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv.clim$Spring_SOI <- glm(alive_t1 ~ -1 + ageClass/SOI.spring_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv.clim$Spring_PDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.spring_surv+ageClass/SOI.spring_surv + pred, 
                          data=df_surv, family="binomial") 

mod.surv.clim$Spring_Int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_spring + pred, 
                        data=df_surv, family="binomial")

# Winter t-1
mod.surv.clim$Winter_tm1_PDO <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_tm1 + pred, 
                            data=df_surv, family="binomial")

mod.surv.clim$Winter_tm1_SOI <- glm(alive_t1 ~ -1 + ageClass/SOI.winter_tm1 + pred, 
                            data=df_surv, family="binomial")

mod.surv.clim$Winter_tm1_PDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_tm1+ageClass/SOI.winter_tm1 + pred, 
                               data=df_surv, family="binomial") 

mod.surv.clim$Winter_tm1_Int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_winter_tm1 + pred, 
                            data=df_surv, family="binomial")

# Creating a list to store the results
results.surv.clim<-list()

## Creating and exporting AIC table to results list
results.surv.clim$aictable.surv <- xtable(aictab(mod.surv.clim), caption = NULL, label = NULL, align = NULL,
                                     digits = NULL, display = NULL, nice.names = TRUE,
                                     include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.surv.clim$aictable.surv[,3:6] <-round(results.surv.clim[["aictable.surv"]][,3:6],digits=3)

# Options to save AIC table
#print.xtable(results.surv.clim[["aictable.surv"]], type="html", file="surv_climate_aic_table.html")
#write.table(results.surv.clim[["aictable.surv"]], file="surv_climate_aic_table.csv")

# Results from best models -------------------------------------------------------------------------------------- 

results.surv.clim$coefs.surv.best <- data.frame(coef(summary(mod.surv.clim[[as.character(results.surv.clim[["aictable.surv"]][1,1])]])))
results.surv.clim$coefs.surv.best[, 1:4] <- round(results.surv.clim[["coefs.surv.best"]][, 1:4], digits = 3)
results.surv.clim$r2.surv.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv.clim[[as.character(results.surv.clim[["aictable.surv"]][1,1])]]), digits = 3))

results.surv.clim$coefs.surv.2ndbest <- data.frame(coef(summary(mod.surv.clim[[as.character(results.surv.clim[["aictable.surv"]][2,1])]])))
results.surv.clim$coefs.surv.2ndbest[, 1:4] <- round(results.surv.clim[["coefs.surv.2ndbest"]][, 1:4], digits = 3)
results.surv.clim$r2.surv.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv.clim[[as.character(results.surv.clim[["aictable.surv"]][2,1])]]), digits = 3))

results.surv.clim$coefs.surv.3rdbest <- data.frame(coef(summary(mod.surv.clim[[as.character(results.surv.clim[["aictable.surv"]][3,1])]])))
results.surv.clim$coefs.surv.3rdbest[, 1:4] <- round(results.surv.clim[["coefs.surv.3rdbest"]][, 1:4], digits = 3)
results.surv.clim$r2.surv.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv.clim[[as.character(results.surv.clim[["aictable.surv"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
#save(df_surv,mod.surv.clim,results.surv.clim,file = "surv_clim.Rdata")
# Upload RData on drive
#drive_upload("surv_clim.Rdata","OWPC/Analyses/results/surv_clim.RData",overwrite=T)

##### Repro ~ Climate ##################################################################################################
# Setting up and importing data ----------------------------------------------------------------------------------------

# Download RData from drive
drive_download("OWPC/Analyses/cache/dataFecundityModels.RData",overwrite=T)
# Import in R environment
load("dataFecundityModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_fec<-droplevels(subset(dataFecScld,!(yr %in% c("1999","2000","2001"))))
# Remove unnecessary objects for the environment
rm(clim_fec,pheno_fec,weather_fec,sheep_data,dataFecUnscld,dataFecScld,fullFecDataScld)

# Raw repro model selection  -------------------------------------------------------------------------------------------
mod.raw.repro.clim <- list()

#Base model
mod.raw.repro.clim$Base <- glmer(raw_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID),
                            family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                            optCtrl = list(maxfun = 100000)))

# Summer
mod.raw.repro.clim$Summer_PDO <- glmer(raw_repro ~ -1 + ageClass/PDOSummerFec + MassAutumn_tm1 + (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Summer_SOI <- glmer(raw_repro ~ -1 + ageClass/SOISummerFec + MassAutumn_tm1 + (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Summer_PDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDOSummerFec + ageClass/SOISummerFec + MassAutumn_tm1 + (1|ID), 
                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Summer_Int <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_summer + MassAutumn_tm1+ (1|ID),
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 100000)))

# Fall
mod.raw.repro.clim$Fall_PDO <- glmer(raw_repro ~ -1 + ageClass/PDOFallFec + MassAutumn_tm1 + (1|ID), 
                               family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                               optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Fall_SOI <- glmer(raw_repro ~ -1 + ageClass/SOIFallFec + MassAutumn_tm1 + (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Fall_PDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDOFallFec + ageClass/SOIFallFec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                  optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Fall_Int <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_fall + MassAutumn_tm1 + (1|ID),
                               family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                               optCtrl = list(maxfun = 100000)))

# Winter
mod.raw.repro.clim$Winter_PDO <- glmer(raw_repro ~ -1 + ageClass/PDOWinterFec + MassAutumn_tm1+  (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Winter_SOI <- glmer(raw_repro ~ -1 + ageClass/SOIWinterFec + MassAutumn_tm1 + (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Winter_PDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDOWinterFec + ageClass/SOIWinterFec + MassAutumn_tm1 + (1|ID),
                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Winter_Int <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_winter + MassAutumn_tm1 + (1|ID),
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))
# Spring
mod.raw.repro.clim$Spring_PDO <- glmer(raw_repro ~ -1 + ageClass/PDOSpringFec + MassAutumn_tm1 + (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Spring_SOI <- glmer(raw_repro ~ -1 + ageClass/SOISpringFec + MassAutumn_tm1 + (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

#mod.raw.repro.clim$Spring_PDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDOSpringFec + ageClass/SOISpringFec + MassAutumn_tm1 + (1|ID),
#                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
#                                    optCtrl = list(maxfun = 100000)))

mod.raw.repro.clim$Spring_Int <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_spring  + MassAutumn_tm1 + (1|ID),
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

# Creating a list to store the results
results.raw.repro.clim<-list()

## Creating and exporting AIC table to results list
results.raw.repro.clim$aictable.raw.repro <- xtable(aictab(mod.raw.repro.clim), caption = NULL, label = NULL, align = NULL,
                                               digits = NULL, display = NULL, nice.names = TRUE,
                                               include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.raw.repro.clim$aictable.raw.repro[,3:6] <-round(results.raw.repro.clim[["aictable.raw.repro"]][,3:6],digits=3)

# Raw repro results from best models --------------------------------------------------------------------------------------------- 

results.raw.repro.clim$coefs.raw.repro.best <- data.frame(coef(summary(mod.raw.repro.clim[[as.character(results.raw.repro.clim[["aictable.raw.repro"]][1,1])]])))
results.raw.repro.clim$coefs.raw.repro.best[, 1:4] <- round(results.raw.repro.clim[["coefs.raw.repro.best"]][, 1:4], digits = 3)
results.raw.repro.clim$r2.raw.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro.clim[[as.character(results.raw.repro.clim[["aictable.raw.repro"]][1,1])]]), digits = 3))

results.raw.repro.clim$coefs.raw.repro.2ndbest <- data.frame(coef(summary(mod.raw.repro.clim[[as.character(results.raw.repro.clim[["aictable.raw.repro"]][2,1])]])))
results.raw.repro.clim$coefs.raw.repro.2ndbest[, 1:4] <- round(results.raw.repro.clim[["coefs.raw.repro.2ndbest"]][, 1:4], digits = 3)
results.raw.repro.clim$r2.raw.repro.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro.clim[[as.character(results.raw.repro.clim[["aictable.raw.repro"]][2,1])]]), digits = 3))

results.raw.repro.clim$coefs.raw.repro.3rdbest <- data.frame(coef(summary(mod.raw.repro.clim[[as.character(results.raw.repro.clim[["aictable.raw.repro"]][3,1])]])))
results.raw.repro.clim$coefs.raw.repro.3rdbest[, 1:4] <- round(results.raw.repro.clim[["coefs.raw.repro.3rdbest"]][, 1:4], digits = 3)
results.raw.repro.clim$r2.raw.repro.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro.clim[[as.character(results.raw.repro.clim[["aictable.raw.repro"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
#save(df_fec,mod.raw.repro.clim,results.raw.repro.clim,file = "raw_repro_clim.Rdata")
# Upload RData on drive
#drive_upload("raw_repro_clim.Rdata","OWPC/Analyses/results/raw_repro_clim.RData",overwrite=T)

# True repro model selection  -------------------------------------------------------------------------------------------
mod.true.repro.clim <- list()

#Base model
mod.true.repro.clim$Base <- glmer(true_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID),
                            family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                   optCtrl = list(maxfun = 100000)))

# Summer
mod.true.repro.clim$Summer_PDO <- glmer(true_repro ~ -1 + ageClass/PDOSummerFec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Summer_SOI <- glmer(true_repro ~ -1 + ageClass/SOISummerFec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Summer_PDOSOI <- glmer(true_repro ~ -1 + ageClass/PDOSummerFec + ageClass/SOISummerFec + MassAutumn_tm1 + (1|ID), 
                                     family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                            optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Summer_Int <- glmer(true_repro ~ -1 + ageClass/PDOSOI_summer + MassAutumn_tm1+ (1|ID),
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

# Fall
mod.true.repro.clim$Fall_PDO <- glmer(true_repro ~ -1 + ageClass/PDOFallFec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Fall_SOI <- glmer(true_repro ~ -1 + ageClass/SOIFallFec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Fall_PDOSOI <- glmer(true_repro ~ -1 + ageClass/PDOFallFec + ageClass/SOIFallFec + MassAutumn_tm1 + (1|ID), 
                                     family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                            optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Fall_Int <- glmer(true_repro ~ -1 + ageClass/PDOSOI_fall + MassAutumn_tm1 + (1|ID),
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

# Winter
mod.true.repro.clim$Winter_PDO <- glmer(true_repro ~ -1 + ageClass/PDOWinterFec + MassAutumn_tm1+  (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Winter_SOI <- glmer(true_repro ~ -1 + ageClass/SOIWinterFec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Winter_PDOSOI <- glmer(true_repro ~ -1 + ageClass/PDOWinterFec + ageClass/SOIWinterFec + MassAutumn_tm1 + (1|ID),
                                     family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                            optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Winter_Int <- glmer(true_repro ~ -1 + ageClass/PDOSOI_winter + MassAutumn_tm1 + (1|ID),
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))
# Spring
mod.true.repro.clim$Spring_PDO <- glmer(true_repro ~ -1 + ageClass/PDOSpringFec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Spring_SOI <- glmer(true_repro ~ -1 + ageClass/SOISpringFec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Spring_PDOSOI <- glmer(true_repro ~ -1 + ageClass/PDOSpringFec + ageClass/SOISpringFec + MassAutumn_tm1 + (1|ID),
                                     family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                            optCtrl = list(maxfun = 100000)))

mod.true.repro.clim$Spring_Int <- glmer(true_repro ~ -1 + ageClass/PDOSOI_spring  + MassAutumn_tm1 + (1|ID),
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

# Creating a list to store the results
results.true.repro.clim<-list()

## Creating and exporting AIC table to results list
results.true.repro.clim$aictable.true.repro <- xtable(aictab(mod.true.repro.clim), caption = NULL, label = NULL, align = NULL,
                                               digits = NULL, display = NULL, nice.names = TRUE,
                                               include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.true.repro.clim$aictable.true.repro[,3:6] <-round(results.true.repro.clim[["aictable.true.repro"]][,3:6],digits=3)

# True repro results from best models --------------------------------------------------------------------------------------------- 

results.true.repro.clim$coefs.true.repro.best <- data.frame(coef(summary(mod.true.repro.clim[[as.character(results.true.repro.clim[["aictable.true.repro"]][1,1])]])))
results.true.repro.clim$coefs.true.repro.best[, 1:4] <- round(results.true.repro.clim[["coefs.true.repro.best"]][, 1:4], digits = 3)
results.true.repro.clim$r2.true.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro.clim[[as.character(results.true.repro.clim[["aictable.true.repro"]][1,1])]]), digits = 3))

results.true.repro.clim$coefs.true.repro.2ndbest <- data.frame(coef(summary(mod.true.repro.clim[[as.character(results.true.repro.clim[["aictable.true.repro"]][2,1])]])))
results.true.repro.clim$coefs.true.repro.2ndbest[, 1:4] <- round(results.true.repro.clim[["coefs.true.repro.2ndbest"]][, 1:4], digits = 3)
results.true.repro.clim$r2.true.repro.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro.clim[[as.character(results.true.repro.clim[["aictable.true.repro"]][2,1])]]), digits = 3))

results.true.repro.clim$coefs.true.repro.3rdbest <- data.frame(coef(summary(mod.true.repro.clim[[as.character(results.true.repro.clim[["aictable.true.repro"]][3,1])]])))
results.true.repro.clim$coefs.true.repro.3rdbest[, 1:4] <- round(results.true.repro.clim[["coefs.true.repro.3rdbest"]][, 1:4], digits = 3)
results.true.repro.clim$r2.true.repro.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro.clim[[as.character(results.true.repro.clim[["aictable.true.repro"]][3,1])]]), digits = 3))


# Option to create and save RData file with data, candidate models and results
#save(df_fec,mod.true.repro.clim,results.true.repro.clim,file = "true_repro_clim.Rdata")
# Upload RData on drive
#drive_upload("true_repro_clim.Rdata","OWPC/Analyses/results/true_repro_clim.RData",overwrite=T)

