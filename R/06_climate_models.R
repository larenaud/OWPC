# script for model selection of climate models

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

##### Survival ~ Climate ###############################################################################################
# Setting up and importing data  ---------------------------------------------------------------------------------------

# Download RData from drive
drive_download("OWPC/Analyses/cache/dataSurvivalModels.RData",overwrite=T)
# Import in R environment
load("dataSurvivalModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_surv<-droplevels(subset(dataSurvScld,!(yr %in% c("1999","2000","2016"))))
# Remove unnecessary objects for the environment
rm(clim_surv,pheno_surv,weather_surv,sheep_data,dataSurvUnscld,dataSurvScld)
# Model selection -----------------------------------------------------------------------------------------------------------
#colnames(df_surv)
# List of candidate models
mod.surv <- list()

mod.surv$base <- glm(alive_t1 ~ -1 + ageClass + pred, data=df_surv, family="binomial")

mod.surv$summerPDO <- glm(alive_t1 ~ -1 + ageClass/PDO.summer_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv$summerSOI <- glm(alive_t1 ~ -1 + ageClass/SOI.summer_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv$summer_int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_summer + pred, 
                        data=df_surv, family="binomial")

mod.surv$fallPDO <- glm(alive_t1 ~ -1 + ageClass/PDO.fall_surv + pred, 
                     data=df_surv, family="binomial")

mod.surv$fallSOI <- glm(alive_t1 ~ -1 + ageClass/SOI.fall_surv + pred, 
                     data=df_surv, family="binomial")

mod.surv$fallPDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.fall_surv+ageClass/SOI.fall_surv + pred, 
                        data=df_surv, family="binomial") 

mod.surv$fall_int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_fall + pred, 
                      data=df_surv, family="binomial")

mod.surv$winterPDO <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv$winterSOI <- glm(alive_t1 ~ -1 + ageClass/SOI.winter_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv$winterPDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_surv+ageClass/SOI.winter_surv + pred, 
                          data=df_surv, family="binomial") 

mod.surv$winter_int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_winter + pred, 
                        data=df_surv, family="binomial")

mod.surv$springPDO <- glm(alive_t1 ~ -1 + ageClass/PDO.spring_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv$springSOI <- glm(alive_t1 ~ -1 + ageClass/SOI.spring_surv + pred, 
                       data=df_surv, family="binomial")

mod.surv$springPDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.spring_surv+ageClass/SOI.spring_surv + pred, 
                          data=df_surv, family="binomial") 

mod.surv$spring_int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_spring + pred, 
                        data=df_surv, family="binomial")

mod.surv$winter.tm1.PDO <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_tm1 + pred, 
                            data=df_surv, family="binomial")

mod.surv$winter.tm1.SOI <- glm(alive_t1 ~ -1 + ageClass/SOI.winter_tm1 + pred, 
                            data=df_surv, family="binomial")

mod.surv$winter.tm1.PDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_tm1+ageClass/SOI.winter_tm1 + pred, 
                               data=df_surv, family="binomial") 

mod.surv$winter.tm1.int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_winter_tm1 + pred, 
                            data=df_surv, family="binomial")

# female density and mass not included as control variables because correlated with age classes 

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

# Results from best models -------------------------------------------------------------------------------------- 

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
# save(df_surv,mod.surv,results.surv,file = "surv_clim.Rdata")

##### Repro ~ Climate ##################################################################################################
# Setting up and importing data ----------------------------------------------------------------------------------------

# Download RData from drive
drive_download("OWPC/Analyses/cache/dataFecundityModels.RData",overwrite=T)
# Import in R environment
load("dataFecundityModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_fec<-droplevels(subset(dataFecScld,!(yr %in% c("1999","2000","2001"))))
# Remove unnecessary objects for the environment
rm(clim_fec,pheno_fec,weather_fec,sheep_data,dataFecUnscld,dataFecScld)

# Raw repro model selection  -------------------------------------------------------------------------------------------
mod.raw.repro <- list()

mod.raw.repro$summerPDO <- glmer(raw_repro ~ -1 + ageClass/PDOSummerFec + MassAutumn_tm1 + (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro$fallPDO <- glmer(raw_repro ~ -1 + ageClass/PDOFallFec + MassAutumn_tm1 + (1|ID), 
                               family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                               optCtrl = list(maxfun = 100000)))

mod.raw.repro$winterPDO <- glmer(raw_repro ~ -1 + ageClass/PDOWinterFec + MassAutumn_tm1+  (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro$springPDO <- glmer(raw_repro ~ -1 + ageClass/PDOSpringFec + MassAutumn_tm1+ (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro$summerPDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDOSummerFec + ageClass/SOISummerFec + MassAutumn_tm1 + (1|ID), 
                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 100000)))

mod.raw.repro$fallPDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDOFallFec + ageClass/SOIFallFec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                  optCtrl = list(maxfun = 100000)))

mod.raw.repro$winterPDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDOWinterFec + ageClass/SOIWinterFec + MassAutumn_tm1 + (1|ID),
                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 100000)))

#mod.raw.repro$springPDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDOSpringFec + ageClass/SOISpringFec + MassAutumn_tm1 + (1|ID),
#                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
#                                    optCtrl = list(maxfun = 100000)))

mod.raw.repro$summerInt <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_summer + MassAutumn_tm1+ (1|ID),
                            family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                            optCtrl = list(maxfun = 100000)))

mod.raw.repro$fallInt <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_fall + MassAutumn_tm1 + (1|ID),
                               family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                               optCtrl = list(maxfun = 100000)))

mod.raw.repro$winterInt <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_winter + MassAutumn_tm1 + (1|ID),
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro$springInt <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_spring  + MassAutumn_tm1 + (1|ID),
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro$base <- glmer(raw_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID),
                            family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                            optCtrl = list(maxfun = 100000)))

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
# save(df_raw.repro,mod.raw.repro,results.raw.repro,file = "raw.repro_clim.Rdata")


# True repro model selection -------------------------------------------------------
mod.true.repro <- list()

mod.true.repro$summerPDO <- glmer(true_repro ~ -1 + ageClass/PDOSummerFec + MassAutumn_tm1 + (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 100000)))

mod.true.repro$fallPDO <- glmer(true_repro ~ -1 + ageClass/PDOFallFec + MassAutumn_tm1 + (1|ID), 
                               family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                      optCtrl = list(maxfun = 100000)))

mod.true.repro$winterPDO <- glmer(true_repro ~ -1 + ageClass/PDOWinterFec + MassAutumn_tm1+  (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 100000)))

mod.true.repro$springPDO <- glmer(true_repro ~ -1 + ageClass/PDOSpringFec + MassAutumn_tm1+ (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 100000)))

mod.true.repro$summerPDOSOI <- glmer(true_repro ~ -1 + ageClass/PDOSummerFec + ageClass/SOISummerFec + MassAutumn_tm1 + (1|ID), 
                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                           optCtrl = list(maxfun = 100000)))

mod.true.repro$fallPDOSOI <- glmer(true_repro ~ -1 + ageClass/PDOFallFec + ageClass/SOIFallFec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 100000)))

mod.true.repro$winterPDOSOI <- glmer(true_repro ~ -1 + ageClass/PDOWinterFec + ageClass/SOIWinterFec + MassAutumn_tm1 + (1|ID),
                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                           optCtrl = list(maxfun = 100000)))

mod.true.repro$springPDOSOI <- glmer(true_repro ~ -1 + ageClass/PDOSpringFec + ageClass/SOISpringFec + MassAutumn_tm1 + (1|ID),
                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 100000)))

mod.true.repro$summerInt <- glmer(true_repro ~ -1 + ageClass/PDOSOI_summer + MassAutumn_tm1+ (1|ID),
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 100000)))

mod.true.repro$fallInt <- glmer(true_repro ~ -1 + ageClass/PDOSOI_fall + MassAutumn_tm1 + (1|ID),
                               family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                      optCtrl = list(maxfun = 100000)))

mod.true.repro$winterInt <- glmer(true_repro ~ -1 + ageClass/PDOSOI_winter + MassAutumn_tm1 + (1|ID),
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 100000)))

mod.true.repro$springInt <- glmer(true_repro ~ -1 + ageClass/PDOSOI_spring  + MassAutumn_tm1 + (1|ID),
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 100000)))

mod.true.repro$base <- glmer(true_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID),
                            family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                                                                   optCtrl = list(maxfun = 100000)))

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
