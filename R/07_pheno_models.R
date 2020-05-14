# script for model selection of phenology models

# Cleaning R environment
rm(list = ls())

# Loading required libraries
library(lme4)
library(AICcmodavg)
library(xtable)
library(googledrive)
library(lme4)
require(optimx)
require(nloptr)
require(dfoptim)

# Set working directory DIFFERENT FOR EACH PERSON
setwd("")
#Ex: setwd("C:/Users/Proprietaire/Documents/uni poc/Phd/OWPC/Analyses")

# Accessing google drive
drive_find(n_max = 10)
# Select a pre-authorised account by entering the corresponding number in the console or enter '0' to obtain a new token.

##### Survival ~ Phenology #############################################################################################
# Setting up and importing data ----

# Download RData from drive
drive_download("OWPC/Analyses/cache/dataSurvivalModels.RData",overwrite=T)
# Import in R environment
load("dataSurvivalModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_surv<-droplevels(subset(dataSurvScld,!(yr %in% c("1999","2000","2016")))) # n = 448
# Remove unnecessary objects for the environment
rm(clim_surv,pheno_surv,weather_surv,sheep_data,dataSurvUnscld,dataSurvScld)

# Model selection ----------------------------------------------------------
mod.surv <- list()

# Base model
mod.surv$base <- glm(alive_t1 ~ -1 + ageClass + pred, data=df_surv, family="binomial")

# Lenght with vegetation
mod.surv$Summer_NDVI <- glm(alive_t1 ~ -1 + ageClass/SummerNDVI + pred, data=df_surv, family="binomial")# this is summer season t

mod.surv$Summer_PC1 <- glm(alive_t1 ~ -1 + ageClass/PC1Summer + pred, data=df_surv, family="binomial")

mod.surv$Summer_PC2 <- glm(alive_t1 ~ -1 + ageClass/PC2Summer + pred, data=df_surv, family="binomial")

mod.surv$Summer_PC1PC2 <- glm(alive_t1 ~ -1 + ageClass/PC1Summer + ageClass/PC2Summer + pred, data=df_surv, family="binomial")

# Lenght without vegetation
mod.surv$Winter_NDVI <- glm(alive_t1 ~ -1 + ageClass/WinNDVIsurvT1 + pred, data=df_surv, family="binomial")# this is Winter season t

mod.surv$Winter_PC1 <- glm(alive_t1 ~ -1 + ageClass/PC1Winter + pred, data=df_surv, family="binomial")

mod.surv$Winter_PC2 <- glm(alive_t1 ~ -1 + ageClass/PC2Winter + pred, data=df_surv, family="binomial")

mod.surv$Winter_PC1PC2 <- glm(alive_t1 ~ -1 + ageClass/PC1Winter + ageClass/PC2Winter + pred, data=df_surv, family="binomial")

# Timing (green-up date)
mod.surv$Date_NDVI <- glm(alive_t1 ~ -1 + ageClass/NDVIsurvT + pred, data=df_surv, family="binomial") # this is green-up t

mod.surv$Date_PC1 <- glm(alive_t1 ~ -1 + ageClass/PC1Date + pred, data=df_surv, family="binomial") # these are PC of timing 

mod.surv$Date_PC2 <- glm(alive_t1 ~ -1 + ageClass/PC2Date + pred, data=df_surv, family="binomial")

mod.surv$Date_PC1PC2 <- glm(alive_t1 ~ -1 + ageClass/PC1Date + ageClass/PC2Date + pred, data=df_surv, family="binomial")

# Snow
mod.surv$Snow_melt <- glm() # this is date of snow_log_up 

mod.surv$Snow_in <- glm() # this is date of snow_log_down

mod.surv$Snow_present <- glm() # this is nb of days with snow

mod.surv$Snow_absent <- glm() # this is nb of days without snow

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
# save(df_surv,mod.surv,results.surv,file = "surv_pheno.Rdata")

##### Reproduction ~ Phenology #########################################################################################
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

# Base model
mod.raw.repro$base <- glmer(raw_repro ~ -1 +  ageClass + MassAutumn_tm1 + (1|ID), 
                            data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                   optCtrl = list(maxfun = 1000000))) 

# Lenght with vegetation
mod.raw.repro$Summer_NDVI <- glmer(raw_repro ~ -1 + ageClass/SummerNDVIfec + MassAutumn_tm1 + (1|ID), 
                                      data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                             optCtrl = list(maxfun = 2000000)))

mod.raw.repro$Summer_PC1 <- glmer(raw_repro ~ -1 + ageClass/PC1Summer + MassAutumn_tm1+ (1|ID), 
                           data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                 optCtrl = list(maxfun = 2000000)))

mod.raw.repro$Summer_PC2 <- glmer(raw_repro ~ -1 + ageClass/PC2Summer + MassAutumn_tm1 + (1|ID), 
                              data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                     optCtrl = list(maxfun = 2000000)))

mod.raw.repro$Summer_PC1PC2 <- glmer(raw_repro ~ -1 + ageClass/PC1Summer + ageClass/PC2Summer + MassAutumn_tm1 + (1|ID), 
                           data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                  optCtrl = list(maxfun = 2000000)))

# Lenght without vegetation
mod.raw.repro$Winter_NDVI <- glmer(raw_repro ~ -1 + ageClass/WinNDVIfecT + MassAutumn_tm1 + (1|ID), 
                               data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                      optCtrl = list(maxfun = 2000000)))

mod.raw.repro$Winter_PC1 <- glmer(raw_repro ~ -1 + ageClass/PC1Winter + MassAutumn_tm1+ (1|ID), 
                                  data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 2000000)))

mod.raw.repro$Winter_PC2 <- glmer(raw_repro ~ -1 + ageClass/PC2Winter + MassAutumn_tm1 + (1|ID), 
                                  data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 2000000)))

mod.raw.repro$Winter_PC1PC2 <- glmer(raw_repro ~ -1 + ageClass/PC1Winter + ageClass/PC2Winter + MassAutumn_tm1 + (1|ID), 
                                     data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                            optCtrl = list(maxfun = 2000000)))

# Timing (green-up)
mod.raw.repro$Date_NDVI<- glmer(raw_repro ~ -1 + ageClass/NDVIfecT + MassAutumn_tm1 + (1|ID), 
                                  data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 2000000)))

mod.raw.repro$Date_PC1 <- glmer(raw_repro ~ -1 + ageClass/PC1Date +  MassAutumn_tm1+ (1|ID), 
                              data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                    optCtrl = list(maxfun = 2000000)))

mod.raw.repro$Date_PC2 <- glmer(raw_repro ~ -1 + ageClass/PC2Date + MassAutumn_tm1 + (1|ID), 
                                  data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 4000000)))
# modelfit.all <- lme4::allFit(mod.raw.repro$pc1pc2tim)
# ss <- summary(modelfit.all)
 
mod.raw.repro$Date_PC1PC2 <- glmer(raw_repro ~ -1 + ageClass/PC1Date + ageClass/PC2Date + MassAutumn_tm1 + (1|ID), 
                               data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                      optCtrl = list(maxfun = 4000000)))
# modelfit.all <- lme4::allFit(model)
# ss <- summary(modelfit.all)

# Snow
mod.surv$Snow_melt <- glm() # this is date of snow_log_up 

mod.surv$Snow_in <- glm() # this is date of snow_log_down

mod.surv$Snow_present <- glm() # this is nb of days with snow

mod.surv$Snow_absent <- glm() # this is nb of days without snow

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
# save(df_fec,mod.raw.repro,results.raw.repro,file = "raw.repro_pheno.Rdata")

# True repro model selection  -------------------------------------------------------------------------------------------
mod.true.repro <- list()

# Base model
mod.true.repro$base <- glmer(true_repro ~ -1 +  ageClass + MassAutumn_tm1 + (1|ID), 
                            data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                   optCtrl = list(maxfun = 1000000))) 

# Lenght with vegetation
mod.true.repro$Summer_NDVI <- glmer(true_repro ~ -1 + ageClass/SummerNDVIfec + MassAutumn_tm1 + (1|ID), 
                                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                          optCtrl = list(maxfun = 2000000)))

mod.true.repro$Summer_PC1 <- glmer(true_repro ~ -1 + ageClass/PC1Summer + MassAutumn_tm1+ (1|ID), 
                                  data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 2000000)))

mod.true.repro$Summer_PC2 <- glmer(true_repro ~ -1 + ageClass/PC2Summer + MassAutumn_tm1 + (1|ID), 
                                  data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 2000000)))

mod.true.repro$Summer_PC1PC2 <- glmer(true_repro ~ -1 + ageClass/PC1Summer + ageClass/PC2Summer + MassAutumn_tm1 + (1|ID), 
                                     data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                            optCtrl = list(maxfun = 2000000)))

# Lenght without vegetation
mod.true.repro$Winter_NDVI <- glmer(true_repro ~ -1 + ageClass/WinNDVIfecT + MassAutumn_tm1 + (1|ID), 
                                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                          optCtrl = list(maxfun = 2000000)))

mod.true.repro$Winter_PC1 <- glmer(true_repro ~ -1 + ageClass/PC1Winter + MassAutumn_tm1+ (1|ID), 
                                  data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 2000000)))

mod.true.repro$Winter_PC2 <- glmer(true_repro ~ -1 + ageClass/PC2Winter + MassAutumn_tm1 + (1|ID), 
                                  data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 2000000)))

mod.true.repro$Winter_PC1PC2 <- glmer(true_repro ~ -1 + ageClass/PC1Winter + ageClass/PC2Winter + MassAutumn_tm1 + (1|ID), 
                                     data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                            optCtrl = list(maxfun = 2000000)))

# Timing (green-up)
mod.true.repro$Date_NDVI<- glmer(true_repro ~ -1 + ageClass/NDVIfecT + MassAutumn_tm1 + (1|ID), 
                                data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                       optCtrl = list(maxfun = 2000000)))

mod.true.repro$Date_PC1 <- glmer(true_repro ~ -1 + ageClass/PC1Date +  MassAutumn_tm1+ (1|ID), 
                                data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                      optCtrl = list(maxfun = 2000000)))

mod.true.repro$Date_PC2 <- glmer(true_repro ~ -1 + ageClass/PC2Date + MassAutumn_tm1 + (1|ID), 
                                data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                       optCtrl = list(maxfun = 4000000)))
# modelfit.all <- lme4::allFit(mod.true.repro$pc1pc2tim)
# ss <- summary(modelfit.all)

mod.true.repro$Date_PC1PC2 <- glmer(true_repro ~ -1 + ageClass/PC1Date + ageClass/PC2Date + MassAutumn_tm1 + (1|ID), 
                                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                          optCtrl = list(maxfun = 4000000)))
# modelfit.all <- lme4::allFit(model)
# ss <- summary(modelfit.all)

# Snow
mod.surv$Snow_melt <- glm() # this is date of snow_log_up 

mod.surv$Snow_in <- glm() # this is date of snow_log_down

mod.surv$Snow_present <- glm() # this is nb of days with snow

mod.surv$Snow_absent <- glm() # this is nb of days without snow

# Creating a list to store the results
results.true.repro<-list()

## Creating and exporting AIC table to results list
results.true.repro$aictable.true.repro <- xtable(aictab(mod.true.repro), caption = NULL, label = NULL, align = NULL,
                                                 digits = NULL, display = NULL, nice.names = TRUE,
                                                 include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.true.repro$aictable.true.repro[,3:6] <-round(results.true.repro[["aictable.true.repro"]][,3:6],digits=3)

# true repro results from best models --------------------------------------------------------------------------------------------- 

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
# save(df_fec,mod.true.repro,results.true.repro,file = "true.repro_pheno.Rdata")
