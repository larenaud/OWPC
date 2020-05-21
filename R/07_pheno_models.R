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

# Accessing google drive
drive_find(n_max = 10)
# Select a pre-authorised account by entering the corresponding number in the console or enter '0' to obtain a new token.

# Set working directory DIFFERENT FOR EACH PERSON
setwd("")
#Ex: setwd("C:/Users/Proprietaire/Documents/uni poc/Phd/OWPC/Analyses")

##### Survival ~ Phenology #############################################################################################
# Setting up and importing data ----

# Download RData from drive
drive_download("OWPC/Analyses/cache/dataSurvivalModels.RData",overwrite=T)
# Import in R environment
load("dataSurvivalModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_surv<-droplevels(subset(dataSurvScld,!(yr %in% c("1999","2000","2016")))) # n = 448
# Remove unnecessary objects for the environment
rm(clim_surv,pheno_surv,weather_surv,sheep_data,dataSurvUnscld,dataSurvScld,fullSurvDataScled)

# Model selection ----------------------------------------------------------
mod.surv.pheno <- list()

# Base model
mod.surv.pheno$base <- glm(alive_t1 ~ -1 + ageClass + pred, data=df_surv, family="binomial")

# Lenght with vegetation
mod.surv.pheno$Summer_NDVI <- glm(alive_t1 ~ -1 + ageClass/SummerNDVI + pred, data=df_surv, family="binomial")# this is summer season t

mod.surv.pheno$Summer_PC1 <- glm(alive_t1 ~ -1 + ageClass/PC1Summer + pred, data=df_surv, family="binomial")

mod.surv.pheno$Summer_PC2 <- glm(alive_t1 ~ -1 + ageClass/PC2Summer + pred, data=df_surv, family="binomial")

mod.surv.pheno$Summer_PC1PC2 <- glm(alive_t1 ~ -1 + ageClass/PC1Summer + ageClass/PC2Summer + pred, data=df_surv, family="binomial")

# Lenght without vegetation
mod.surv.pheno$Winter_NDVI <- glm(alive_t1 ~ -1 + ageClass/WinNDVIsurvT1 + pred, data=df_surv, family="binomial")# this is Winter season t

mod.surv.pheno$Winter_PC1 <- glm(alive_t1 ~ -1 + ageClass/PC1Winter + pred, data=df_surv, family="binomial")

mod.surv.pheno$Winter_PC2 <- glm(alive_t1 ~ -1 + ageClass/PC2Winter + pred, data=df_surv, family="binomial")

mod.surv.pheno$Winter_PC1PC2 <- glm(alive_t1 ~ -1 + ageClass/PC1Winter + ageClass/PC2Winter + pred, data=df_surv, family="binomial")

# Timing (green-up date)
mod.surv.pheno$Date_NDVI <- glm(alive_t1 ~ -1 + ageClass/NDVIsurvT + pred, data=df_surv, family="binomial") # this is green-up t

mod.surv.pheno$Date_PC1 <- glm(alive_t1 ~ -1 + ageClass/PC1Date + pred, data=df_surv, family="binomial") # these are PC of timing 

mod.surv.pheno$Date_PC2 <- glm(alive_t1 ~ -1 + ageClass/PC2Date + pred, data=df_surv, family="binomial")

mod.surv.pheno$Date_PC1PC2 <- glm(alive_t1 ~ -1 + ageClass/PC1Date + ageClass/PC2Date + pred, data=df_surv, family="binomial")

# Snow
mod.surv.pheno$Snow_melt <- glm(alive_t1 ~ -1 + ageClass/SNOWMELTsurvT + pred, data=df_surv, family="binomial") # this is date of snow_log_up 

mod.surv.pheno$Snow_cover <- glm(alive_t1 ~ -1 + ageClass/SNOWCOVERsurvT + pred, data=df_surv, family="binomial") # this is date of snow_log_down

mod.surv.pheno$Snow_present <- glm(alive_t1 ~ -1 + ageClass/WinSnowsurvT1 + pred, data=df_surv, family="binomial") # this is nb of days with snow

mod.surv.pheno$Snow_absent <- glm(alive_t1 ~ -1 + ageClass/SummerSnow + pred, data=df_surv, family="binomial") # this is nb of days without snow

# Creating a list to store the results
results.surv.pheno<-list()

## Creating and exporting AIC table to results list
results.surv.pheno$aictable.surv <- xtable(aictab(mod.surv.pheno), caption = NULL, label = NULL, align = NULL,
                                     digits = NULL, display = NULL, nice.names = TRUE,
                                     include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.surv.pheno$aictable.surv[,3:6] <-round(results.surv.pheno[["aictable.surv"]][,3:6],digits=3)

# Options to save AIC table
#print.xtable(results.surv.pheno[["aictable.surv"]], type="html", file="surv_climate_aic_table.html")
#write.table(results.surv.pheno[["aictable.surv"]], file="surv_climate_aic_table.csv")

# Results from best models -------------------------------------------------------------------------------------- 

results.surv.pheno$coefs.surv.best <- data.frame(coef(summary(mod.surv.pheno[[as.character(results.surv.pheno[["aictable.surv"]][1,1])]])))
results.surv.pheno$coefs.surv.best[, 1:4] <- round(results.surv.pheno[["coefs.surv.best"]][, 1:4], digits = 3)
results.surv.pheno$r2.surv.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv.pheno[[as.character(results.surv.pheno[["aictable.surv"]][1,1])]]), digits = 3))

results.surv.pheno$coefs.surv.2ndbest <- data.frame(coef(summary(mod.surv.pheno[[as.character(results.surv.pheno[["aictable.surv"]][2,1])]])))
results.surv.pheno$coefs.surv.2ndbest[, 1:4] <- round(results.surv.pheno[["coefs.surv.2ndbest"]][, 1:4], digits = 3)
results.surv.pheno$r2.surv.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv.pheno[[as.character(results.surv.pheno[["aictable.surv"]][2,1])]]), digits = 3))

results.surv.pheno$coefs.surv.3rdbest <- data.frame(coef(summary(mod.surv.pheno[[as.character(results.surv.pheno[["aictable.surv"]][3,1])]])))
results.surv.pheno$coefs.surv.3rdbest[, 1:4] <- round(results.surv.pheno[["coefs.surv.3rdbest"]][, 1:4], digits = 3)
results.surv.pheno$r2.surv.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv.pheno[[as.character(results.surv.pheno[["aictable.surv"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
#save(df_surv,mod.surv.pheno,results.surv.pheno,file = "surv_pheno.Rdata")
# Upload RData on drive
#drive_upload("surv_pheno.Rdata","OWPC/Analyses/results/surv_pheno.RData",overwrite=T)

##### Reproduction ~ Phenology #########################################################################################
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
mod.raw.repro.pheno <- list()

# Base model
mod.raw.repro.pheno$base <- glmer(raw_repro ~ -1 +  ageClass + MassAutumn_tm1 + (1|ID), 
                            data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                   optCtrl = list(maxfun = 1000000))) 

# Lenght with vegetation
mod.raw.repro.pheno$Summer_NDVI <- glmer(raw_repro ~ -1 + ageClass/SummerNDVIfec + MassAutumn_tm1 + (1|ID), 
                                      data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                             optCtrl = list(maxfun = 2000000)))

mod.raw.repro.pheno$Summer_PC1 <- glmer(raw_repro ~ -1 + ageClass/PC1Summer + MassAutumn_tm1+ (1|ID), 
                           data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                 optCtrl = list(maxfun = 2000000)))

#mod.raw.repro.pheno$Summer_PC2 <- glmer(raw_repro ~ -1 + ageClass/PC2Summer + MassAutumn_tm1 + (1|ID), 
#                              data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
#                                                                                     optCtrl = list(maxfun = 2000000)))

#mod.raw.repro.pheno$Summer_PC1PC2 <- glmer(raw_repro ~ -1 + ageClass/PC1Summer + ageClass/PC2Summer + MassAutumn_tm1 + (1|ID), 
#                           data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
#                                                                                  optCtrl = list(maxfun = 2000000)))

# Lenght without vegetation
mod.raw.repro.pheno$Winter_NDVI <- glmer(raw_repro ~ -1 + ageClass/WinNDVIfecT + MassAutumn_tm1 + (1|ID), 
                               data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                      optCtrl = list(maxfun = 2000000)))

mod.raw.repro.pheno$Winter_PC1 <- glmer(raw_repro ~ -1 + ageClass/PC1Winter + MassAutumn_tm1+ (1|ID), 
                                  data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 2000000)))

mod.raw.repro.pheno$Winter_PC2 <- glmer(raw_repro ~ -1 + ageClass/PC2Winter + MassAutumn_tm1 + (1|ID), 
                                  data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 2000000)))

mod.raw.repro.pheno$Winter_PC1PC2 <- glmer(raw_repro ~ -1 + ageClass/PC1Winter + ageClass/PC2Winter + MassAutumn_tm1 + (1|ID), 
                                     data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                            optCtrl = list(maxfun = 2000000)))

# Timing (green-up)
mod.raw.repro.pheno$Date_NDVI<- glmer(raw_repro ~ -1 + ageClass/NDVIfecT + MassAutumn_tm1 + (1|ID), 
                                  data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 2000000)))

mod.raw.repro.pheno$Date_PC1 <- glmer(raw_repro ~ -1 + ageClass/PC1Date +  MassAutumn_tm1+ (1|ID), 
                              data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                    optCtrl = list(maxfun = 2000000)))

mod.raw.repro.pheno$Date_PC2 <- glmer(raw_repro ~ -1 + ageClass/PC2Date + MassAutumn_tm1 + (1|ID), 
                                  data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 4000000)))

#mod.raw.repro.pheno$Date_PC1PC2 <- glmer(raw_repro ~ -1 + ageClass/PC1Date + ageClass/PC2Date + MassAutumn_tm1 + (1|ID), 
#                               data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
#                                                                                      optCtrl = list(maxfun = 4000000)))
# modelfit.all <- lme4::allFit(model)
# ss <- summary(modelfit.all)

# Snow
mod.raw.repro.pheno$Snow_melt <- glmer(raw_repro ~ -1 + ageClass/SNOWMELTfecT + MassAutumn_tm1 + (1|ID),  
                            data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                            optCtrl = list(maxfun = 4000000))) # this is date of snow_log_up

mod.raw.repro.pheno$Snow_in <- glmer(raw_repro ~ -1 + ageClass/SNOWCOVERfec + MassAutumn_tm1 + (1|ID),  
                               data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                               optCtrl = list(maxfun = 4000000))) # this is date of snow_log_down

mod.raw.repro.pheno$Snow_present <- glmer(raw_repro ~ -1 + ageClass/WinSnowfecT + MassAutumn_tm1 + (1|ID), 
                                    data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 4000000))) # this is nb of days with snow

mod.raw.repro.pheno$Snow_absent <- glmer(raw_repro ~ -1 + ageClass/SummerSnowfec + MassAutumn_tm1 + (1|ID), 
                                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                   optCtrl = list(maxfun = 4000000))) # this is nb of days without snow

# Creating a list to store the results
results.raw.repro.pheno<-list()

## Creating and exporting AIC table to results list
results.raw.repro.pheno$aictable.raw.repro <- xtable(aictab(mod.raw.repro.pheno), caption = NULL, label = NULL, align = NULL,
                                               digits = NULL, display = NULL, nice.names = TRUE,
                                               include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.raw.repro.pheno$aictable.raw.repro[,3:6] <-round(results.raw.repro.pheno[["aictable.raw.repro"]][,3:6],digits=3)

# Raw repro results from best models --------------------------------------------------------------------------------------------- 

results.raw.repro.pheno$coefs.raw.repro.best <- data.frame(coef(summary(mod.raw.repro.pheno[[as.character(results.raw.repro.pheno[["aictable.raw.repro"]][1,1])]])))
results.raw.repro.pheno$coefs.raw.repro.best[, 1:4] <- round(results.raw.repro.pheno[["coefs.raw.repro.best"]][, 1:4], digits = 3)
results.raw.repro.pheno$r2.raw.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro.pheno[[as.character(results.raw.repro.pheno[["aictable.raw.repro"]][1,1])]]), digits = 3))

results.raw.repro.pheno$coefs.raw.repro.2ndbest <- data.frame(coef(summary(mod.raw.repro.pheno[[as.character(results.raw.repro.pheno[["aictable.raw.repro"]][2,1])]])))
results.raw.repro.pheno$coefs.raw.repro.2ndbest[, 1:4] <- round(results.raw.repro.pheno[["coefs.raw.repro.2ndbest"]][, 1:4], digits = 3)
results.raw.repro.pheno$r2.raw.repro.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro.pheno[[as.character(results.raw.repro.pheno[["aictable.raw.repro"]][2,1])]]), digits = 3))

results.raw.repro.pheno$coefs.raw.repro.3rdbest <- data.frame(coef(summary(mod.raw.repro.pheno[[as.character(results.raw.repro.pheno[["aictable.raw.repro"]][3,1])]])))
results.raw.repro.pheno$coefs.raw.repro.3rdbest[, 1:4] <- round(results.raw.repro.pheno[["coefs.raw.repro.3rdbest"]][, 1:4], digits = 3)
results.raw.repro.pheno$r2.raw.repro.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro.pheno[[as.character(results.raw.repro.pheno[["aictable.raw.repro"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
#save(df_fec,mod.raw.repro.pheno,results.raw.repro.pheno,file = "raw_repro_pheno.Rdata")
# Upload RData on drive
#drive_upload("raw_repro_pheno.Rdata","OWPC/Analyses/results/raw_repro_pheno.RData",overwrite=T)

# True repro model selection  -------------------------------------------------------------------------------------------
mod.true.repro.pheno <- list()

# Base model
mod.true.repro.pheno$base <- glmer(true_repro ~ -1 +  ageClass + MassAutumn_tm1 + (1|ID), 
                            data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                   optCtrl = list(maxfun = 1000000))) 

# Lenght with vegetation
mod.true.repro.pheno$Summer_NDVI <- glmer(true_repro ~ -1 + ageClass/SummerNDVIfec + MassAutumn_tm1 + (1|ID), 
                                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                          optCtrl = list(maxfun = 2000000)))

mod.true.repro.pheno$Summer_PC1 <- glmer(true_repro ~ -1 + ageClass/PC1Summer + MassAutumn_tm1+ (1|ID), 
                                  data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 2000000)))

mod.true.repro.pheno$Summer_PC2 <- glmer(true_repro ~ -1 + ageClass/PC2Summer + MassAutumn_tm1 + (1|ID), 
                                  data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 2000000)))

mod.true.repro.pheno$Summer_PC1PC2 <- glmer(true_repro ~ -1 + ageClass/PC1Summer + ageClass/PC2Summer + MassAutumn_tm1 + (1|ID), 
                                     data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                            optCtrl = list(maxfun = 2000000)))

# Lenght without vegetation
mod.true.repro.pheno$Winter_NDVI <- glmer(true_repro ~ -1 + ageClass/WinNDVIfecT + MassAutumn_tm1 + (1|ID), 
                                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                          optCtrl = list(maxfun = 2000000)))

mod.true.repro.pheno$Winter_PC1 <- glmer(true_repro ~ -1 + ageClass/PC1Winter + MassAutumn_tm1+ (1|ID), 
                                  data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                        optCtrl = list(maxfun = 2000000)))

mod.true.repro.pheno$Winter_PC2 <- glmer(true_repro ~ -1 + ageClass/PC2Winter + MassAutumn_tm1 + (1|ID), 
                                  data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                         optCtrl = list(maxfun = 2000000)))

mod.true.repro.pheno$Winter_PC1PC2 <- glmer(true_repro ~ -1 + ageClass/PC1Winter + ageClass/PC2Winter + MassAutumn_tm1 + (1|ID), 
                                     data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                            optCtrl = list(maxfun = 2000000)))

# Timing (green-up)
mod.true.repro.pheno$Date_NDVI<- glmer(true_repro ~ -1 + ageClass/NDVIfecT + MassAutumn_tm1 + (1|ID), 
                                data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                       optCtrl = list(maxfun = 2000000)))

mod.true.repro.pheno$Date_PC1 <- glmer(true_repro ~ -1 + ageClass/PC1Date +  MassAutumn_tm1+ (1|ID), 
                                data=df_fec, family="binomial",control = glmerControl(optimizer="bobyqa", 
                                                                                      optCtrl = list(maxfun = 2000000)))

mod.true.repro.pheno$Date_PC2 <- glmer(true_repro ~ -1 + ageClass/PC2Date + MassAutumn_tm1 + (1|ID), 
                                data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                       optCtrl = list(maxfun = 4000000)))

mod.true.repro.pheno$Date_PC1PC2 <- glmer(true_repro ~ -1 + ageClass/PC1Date + ageClass/PC2Date + MassAutumn_tm1 + (1|ID), 
                                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                                                                          optCtrl = list(maxfun = 4000000)))

# Snow
mod.true.repro.pheno$Snow_melt <- glmer(true_repro ~ -1 + ageClass/SNOWMELTfecT + MassAutumn_tm1 + (1|ID),  
                                 data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 4000000))) # this is date of snow_log_up

mod.true.repro.pheno$Snow_in <- glmer(true_repro ~ -1 + ageClass/SNOWCOVERfec + MassAutumn_tm1 + (1|ID),  
                               data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                               optCtrl = list(maxfun = 4000000))) # this is date of snow_log_down

mod.true.repro.pheno$Snow_present <- glmer(true_repro ~ -1 + ageClass/WinSnowfecT + MassAutumn_tm1 + (1|ID), 
                                    data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 4000000))) # this is nb of days with snow

mod.true.repro.pheno$Snow_absent <- glmer(true_repro ~ -1 + ageClass/SummerSnowfec + MassAutumn_tm1 + (1|ID), 
                                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                   optCtrl = list(maxfun = 4000000))) # this is nb of days without snow

# Creating a list to store the results
results.true.repro.pheno<-list()

## Creating and exporting AIC table to results list
results.true.repro.pheno$aictable.true.repro <- xtable(aictab(mod.true.repro.pheno), caption = NULL, label = NULL, align = NULL,
                                                 digits = NULL, display = NULL, nice.names = TRUE,
                                                 include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.true.repro.pheno$aictable.true.repro[,3:6] <-round(results.true.repro.pheno[["aictable.true.repro"]][,3:6],digits=3)


# true repro results from best models --------------------------------------------------------------------------------------------- 

results.true.repro.pheno$coefs.true.repro.best <- data.frame(coef(summary(mod.true.repro.pheno[[as.character(results.true.repro.pheno[["aictable.true.repro"]][1,1])]])))
results.true.repro.pheno$coefs.true.repro.best[, 1:4] <- round(results.true.repro.pheno[["coefs.true.repro.best"]][, 1:4], digits = 3)
results.true.repro.pheno$r2.true.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro.pheno[[as.character(results.true.repro.pheno[["aictable.true.repro"]][1,1])]]), digits = 3))

results.true.repro.pheno$coefs.true.repro.2ndbest <- data.frame(coef(summary(mod.true.repro.pheno[[as.character(results.true.repro.pheno[["aictable.true.repro"]][2,1])]])))
results.true.repro.pheno$coefs.true.repro.2ndbest[, 1:4] <- round(results.true.repro.pheno[["coefs.true.repro.2ndbest"]][, 1:4], digits = 3)
results.true.repro.pheno$r2.true.repro.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro.pheno[[as.character(results.true.repro.pheno[["aictable.true.repro"]][2,1])]]), digits = 3))

results.true.repro.pheno$coefs.true.repro.3rdbest <- data.frame(coef(summary(mod.true.repro.pheno[[as.character(results.true.repro.pheno[["aictable.true.repro"]][3,1])]])))
results.true.repro.pheno$coefs.true.repro.3rdbest[, 1:4] <- round(results.true.repro.pheno[["coefs.true.repro.3rdbest"]][, 1:4], digits = 3)
results.true.repro.pheno$r2.true.repro.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro.pheno[[as.character(results.true.repro.pheno[["aictable.true.repro"]][3,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
#save(df_fec,mod.true.repro.pheno,results.true.repro.pheno,file = "true_repro_pheno.Rdata")
# Upload RData on drive
#drive_upload("true_repro_pheno.Rdata","OWPC/Analyses/results/true_repro_pheno.RData",overwrite=T)
