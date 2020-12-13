# this script is a combination of former 15_modelselection_final, 16_Final_models and 14_model_figures
# it is intended to select a final model combining all environmental variables for surv, raw repro and true reproduction. 
# modified by L. Renaud march 31 2020


# models with AIC < 2 were considered equivalent and included in the final selection 

# Cleaning R environment
rm(list = ls())

# Loading required libraries
library(lme4)
library(AICcmodavg)
library(xtable)
library(googledrive)
library(plyr)
library(dplyr)
library(MuMIn)
library(ggplot2)
library(cowplot)
library(xtable)
library(readxl)
library(boot)
library(ggthemes)

library(cowplot)

#install.packages("pander")
library(pander)

# Accessing google drive
drive_find(n_max = 10)
# Select a pre-authorised account by entering the corresponding number in the console or enter '0' to obtain a new token.

# Set working directory DIFFERENT FOR EACH PERSON
setwd("")
#Ex: setwd("C:/Users/Proprietaire/Documents/uni poc/Phd/OWPC/Analyses/FinalModels")

##### Survival ############################################################################################################
# Cleaning R environment -----------------------------------------------------------------------
rm(list = ls())
# Importing data -------------------------------------------------------------------------------

# Climate
drive_download("OWPC/Analyses/results/surv_clim.RData",overwrite = T)
load("surv_clim.RData")

# Pheno
drive_download("OWPC/Analyses/results/surv_pheno.RData",overwrite = T)
load("surv_pheno.RData")

# Weather
drive_download("OWPC/Analyses/results/surv_weather.RData",overwrite = T)
load("surv_weather.RData")

# Selecting models for each categories of environmental variable -------------------------------

# Climate
View(results.surv.clim[["aictable.surv"]])
# Best model is base model
# No model is selected

# Pheno
View(results.surv.pheno[["aictable.surv"]])
# Best model is Snow_present with a delta AICc >2 with base model.
# Other models "beat" the base model but none with a delta AICc >2.
# Snow_present is selected.

# Weather
View(results.surv.weat[["aictable.surv"]])
# Best model is Fall_T, but with delta AICc <2 with base model which is the second best model.

anova(mod.surv.weat[["Base"]], mod.surv.weat[["Fall_T"]], test = "LRT")

#  Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
#  1       442      375.8                       
#  2       437      364.6  5     11.2  0.04756 *

# Likelihood ratio test suggest our data significally "better" fit the Fall_T model.
# Fall_T model is selected.

# Final survival model -------------------------------------------------------------------------

# List of candidate models
mod.surv <- list()

# Base model
mod.surv$Base<- mod.surv.clim[["base"]]

# Climate
# no selected model

# Pheno
mod.surv$Snow_present<- mod.surv.pheno[["Snow_present"]]

# Weather
mod.surv$Fall_T<- mod.surv.weat[["Fall_T"]]

# Pheno + weather
mod.surv$Snowpres_FallT<- glm(alive_t1 ~ -1 + ageClass/WinSnowsurvT1 + ageClass/T.FALL + pred,
                              data=df_surv, family="binomial")

# Creating a list to store the results
results.surv<-list()

## Creating and exporting AIC table to results list
results.surv$aictable.surv <- xtable(aictab(mod.surv), caption = NULL, label = NULL, align = NULL,
                                          digits = NULL, display = NULL, nice.names = TRUE,
                                          include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.surv$aictable.surv[,3:6] <-round(results.surv[["aictable.surv"]][,3:6],digits=3)

# Results from survival best model -------------------------------------------------------------------------------------- 

# Snow_present
results.surv$coefs.surv.best <- data.frame(coef(summary(mod.surv[[as.character(results.surv[["aictable.surv"]][1,1])]])))
results.surv$coefs.surv.best[, 1:4] <- round(results.surv[["coefs.surv.best"]][, 1:4], digits = 3)
results.surv$r2.surv.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv[[as.character(results.surv[["aictable.surv"]][1,1])]]), digits = 3))

# Save results ---------------------------------------------------------------------------------------------------------- 

save(df_surv,mod.surv,results.surv,file = "final_surv.Rdata")


library(kableExtra)

kable(results.surv$aictable.surv) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "FinalAICSurv.html", self_contained = T) 

kable(results.surv$coefs.surv.best) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "FinalSurvCoef.html", self_contained = T) 

##### True repro ##########################################################################################################
# Cleaning R environment -----------------------------------------------------------------------
rm(list = ls())
# Importing data -------------------------------------------------------------------------------

# Climate
drive_download("OWPC/Analyses/results/true_repro_clim.RData",overwrite = T)
load("true_repro_clim.RData")

# Pheno
drive_download("OWPC/Analyses/results/true_repro_pheno.RData",overwrite = T)
load("true_repro_pheno.RData")

# Weather
drive_download("OWPC/Analyses/results/true_repro_weather.RData",overwrite = T)
load("true_repro_weather.RData")

# Selecting models for each categories of environmental variable -------------------------------

# Climate
View(results.true.repro.clim[["aictable.true.repro"]])
# Best model is base model
# No model is selected

# Pheno
View(results.true.repro.pheno[["aictable.true.repro"]])
# Best model is Date_PC2, but with a delta AICc <2 with base model which is the second best model.

anova(mod.true.repro.pheno[["base"]], mod.true.repro.pheno[["Date_PC2"]], test = "LRT")

#                                    Df    AIC    BIC     logLik  deviance  Chisq Chi Df Pr(>Chisq)  
#mod.true.repro.pheno[["base"]]      5   301.47  319.54  -145.74   291.47                           
#mod.true.repro.pheno[["Date_PC2"]]  8   300.02  328.93  -142.01   284.02     7.4456  3   0.05897 .

# Likelihood ratio test suggest our data marginally "better" fit the Date_PC2 model.
# Date_PC2 model is selected.

# Weather
View(results.true.repro.weat[["aictable.true.repro"]])
# Best model is Winter_PxT with delta AICc >2 with base model.
# Other models "beat" the base model, but Winter_PxT is cleary the best model with a delta AICc of 6.477 with the second best model.
# Winter_PxT is selected

# Final true repro model -------------------------------------------------------------------------

# List of candidate models
mod.true.repro <- list()

# Base model
mod.true.repro$Base<- mod.true.repro.clim[["Base"]]

# Climate
# no selected model

# Pheno
mod.true.repro$Date_PC2<- mod.true.repro.pheno[["Date_PC2"]]

# Weather
mod.true.repro$Winter_PxT<- mod.true.repro.weat[["Winter_PxT"]]

# Pheno + weather
mod.true.repro$DatePC2_WinterPxT<- glmer(true_repro ~ -1 + ageClass/PC2Date + ageClass/(TWin*PWin) + MassAutumn_tm1 + (1|ID), 
                                         data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                         optCtrl = list(maxfun = 4000000)))
  
# Creating a list to store the results
results.true.repro<-list()

## Creating and exporting AIC table to results list
results.true.repro$aictable.true.repro <- xtable(aictab(mod.true.repro), caption = NULL, label = NULL, align = NULL,
                                     digits = NULL, display = NULL, nice.names = TRUE,
                                     include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.true.repro$aictable.true.repro[,3:6] <-round(results.true.repro[["aictable.true.repro"]][,3:6],digits=3)

# Results from true repro best model -------------------------------------------------------------------------------------- 

# Winter_PxT
results.true.repro$coefs.true.repro.best <- data.frame(coef(summary(mod.true.repro[[as.character(results.true.repro[["aictable.true.repro"]][1,1])]])))
results.true.repro$coefs.true.repro.best[, 1:4] <- round(results.true.repro[["coefs.true.repro.best"]][, 1:4], digits = 3)
results.true.repro$r2.true.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro[[as.character(results.true.repro[["aictable.true.repro"]][1,1])]]), digits = 3))


# Save results ---------------------------------------------------------------------------------------------------------- 
save(df_fec,mod.true.repro,results.true.repro,file = "final_true_models.Rdata")

kable(results.true.repro$aictable.true.repro) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "FinalAicTrue.html", self_contained = T) 

kable(results.true.repro$coefs.true.repro.best[, 1:4]) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "FinalTrueCoef.html", self_contained = T) 

#### verifications ##########################################################################################################
# Cleaning R environment --------------------------------------------------------------------------------------------------
rm(list = ls())
# Importing data ----------------------------------------------------------------------------------------------------------

# Survival
# Download RData from drive
drive_download("OWPC/Analyses/cache/dataSurvivalModels.RData",overwrite=T)
# Import in R environment
load("dataSurvivalModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_surv<-droplevels(subset(dataSurvScld,!(yr %in% c("1999","2000","2016"))))
# Remove unnecessary objects for the environment
#rm(clim_surv,pheno_surv,weather_surv,sheep_data,dataSurvUnscld,dataSurvScld,fullSurvDataScled)

# True repro
# Download RData from drive
drive_download("OWPC/Analyses/cache/dataFecundityModels.RData",overwrite=T)
# Import in R environment
load("dataFecundityModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_fec<-droplevels(subset(dataFecScld,!(yr %in% c("1999","2000","2001"))))
# Remove unnecessary objects for the environment
#rm(clim_fec,pheno_fec,weather_fec,sheep_data,dataFecUnscld,dataFecScld,fullFecDataScld)

# Final models ------------------------------------------------------------------------------------------------------------
finalSurv <- glm(alive_t1 ~ -1 + ageClass/WinSnowsurvT1 + pred, data=df_surv, family="binomial")
testfinal <- glm(alive_t1 ~ -1 + MassAutumn + ageClass/WinSnowsurvT1 + pred, data=df_surv, family="binomial")
summary(testfinal)

finalTrue <- glmer(true_repro ~ -1 + ageClass/(TWin*PWin) + MassAutumn_tm1 + (1|ID),
                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa",
                   optCtrl = list(maxfun = 2000000)))




# final figures on separate script 
