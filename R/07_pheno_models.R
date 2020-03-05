# script for model selection of phenology models

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

# Data tyding (script qui doit être déplacé) --------------------
load("~/Documents/PhD/Analyses/OWPC/OWPC/cache/surv_data.RData")
load("~/Documents/PhD/Analyses/OWPC/OWPC/cache/repro_data.RData")
df_surv = df_surv_pca
df_fec = df_fec_pca
df_fec$yr <- as.factor(df_fec$yr)
df_fec$true_repro <- as.factor(df_fec$true_repro)
df_fec$raw_repro <- as.factor(df_fec$raw_repro)
df_fec$true_repro_tm1 <- as.factor(df_fec$true_repro_tm1)
df_surv$alive_t1 = as.factor(df_surv$alive_t1)

#New column ageClass (0,1,2,37,8)

df_surv$ageClass <- ifelse(df_surv$age >= 8, 8, df_surv$age)
c37 <- c(3:7)
df_surv$ageClass <- ifelse(df_surv$age %in% c37 , 37, df_surv$ageClass)
df_surv$ageClass <- as.factor(df_surv$ageClass)
df_surv<- df_surv[-which(df_surv$first_yr_trans==1),]

head(df_fec)
# df _reprd
C48 <-4:8 

df_fec$ageClass_r <- ifelse(df_fec$age == 3, 3, df_fec$age)
df_fec$ageClass_r <- ifelse(df_fec$ageClass_r >=9, 9, df_fec$ageClass_r)
df_fec$ageClass_r <- ifelse(df_fec$ageClass_r %in% C48, 48, df_fec$ageClass_r)

df_fec <- df_fec[which(df_fec$age>=3),]

head(df_fec)

df_fec <- df_fec[-which(df_fec$first_yr_trans==1),]
df_fec$MassAutumn_tm1 <- scale(df_fec$MassAutumn_tm1)

df_fec$ID <- as.factor(df_fec$ID)
df_fec$ageClass_r <- as.factor(df_fec$ageClass_r)
df_fec$pred_tm1 <- as.factor(df_fec$pred_tm1)
df_fec$yr <- as.factor(df_fec$yr)
colnames(df_fec)
df_fec[c(3,13:28)] <- scale(df_fec[c(3,13:28)])# CHANGE COLUMN NUMBER IF MODIFY df_fec!! 

df_fec <- df_fec[!is.na(df_fec$MassAutumn_tm1),] # n = 263


##### Survival ~ Phenology #############################################################################################
# Setting up and importing data ----

# Download data from drive
drive_download("OWPC/Analyses/data/df_surv_pca.csv",overwrite=T)#Path à modifier une fois base de données finale complété
# Import dataframe in R environment
df_surv <- read.csv2("df_surv_pca.csv", sep=",")#Path à modifier une fois base de données finale complété

# Model selection ----------------------------------------------------------
colnames(df_surv)

mod.surv <- list()

mod.surv$base <- glm(alive_t1 ~ -1 + ageClass + pred, data=df_surv, family="binomial")

mod.surv$pc1 <- glm(alive_t1 ~ -1 + ageClass/PC1 + pred, data=df_surv, family="binomial")

mod.surv$pc1pc2 <- glm(alive_t1 ~ -1 + ageClass/PC1 + ageClass/PC2 + pred, data=df_surv, family="binomial")

mod.surv$pc2 <- glm(alive_t1 ~ -1 + ageClass/PC2 + pred, data=df_surv, family="binomial")

mod.surv$summerNDVI <- glm(alive_t1 ~ -1 + ageClass/SummerNDVI + pred, data=df_surv, family="binomial")

mod.surv$summerEVI <- glm(alive_t1 ~ -1 + ageClass/SummerEVI + pred, data=df_surv, family="binomial")

mod.surv$summerLAI <- glm(alive_t1 ~ -1 + ageClass/SummerLAI + pred, data=df_surv, family="binomial")

mod.surv$summerGPP <- glm(alive_t1 ~ -1 + ageClass/SummerGPP + pred, data=df_surv, family="binomial")

mod.surv$summerSnow <- glm(alive_t1 ~ -1 + ageClass/SummerSnow + pred, data=df_surv, family="binomial")

mod.surv$summerPSNET <- glm(alive_t1 ~ -1 + ageClass/SummerPSNNET + pred, data=df_surv, family="binomial")

mod.surv$summerFPAR <- glm(alive_t1 ~ -1 + ageClass/SummerFPAR + pred, data=df_surv, family="binomial")

mod.surv$WinNDVI_surv <- glm(alive_t1 ~ -1 + ageClass/WinNDVI_surv + pred, data=df_surv, family="binomial")

mod.surv$WinEVI_surv <- glm(alive_t1 ~ -1 + ageClass/WinEVI_surv + pred, data=df_surv, family="binomial")

mod.surv$WinLAI_surv <- glm(alive_t1 ~ -1 + ageClass/WinLAI_surv + pred, data=df_surv, family="binomial")

mod.surv$WinGPP_surv <- glm(alive_t1 ~ -1 + ageClass/WinGPP_surv + pred, data=df_surv, family="binomial")

mod.surv$WinSnow_surv <- glm(alive_t1 ~ -1 + ageClass/WinSnow_surv + pred, data=df_surv, family="binomial")

mod.surv$WinPSNET_surv <- glm(alive_t1 ~ -1 + ageClass/WinPSNNET_surv + pred, data=df_surv, family="binomial")

mod.surv$WinFPAR_surv <- glm(alive_t1 ~ -1 + ageClass/WinFPAR_surv + pred, data=df_surv, family="binomial")

mod.surv$WinNDVI_tm1 <- glm(alive_t1 ~ -1 + ageClass/WinNDVI_tm1 + pred, data=df_surv, family="binomial")

mod.surv$WinEVI_tm1 <- glm(alive_t1 ~ -1 + ageClass/WinEVI_tm1 + pred, data=df_surv, family="binomial")

mod.surv$WinLAI_tm1 <- glm(alive_t1 ~ -1 + ageClass/WinLAI_tm1 + pred, data=df_surv, family="binomial")

mod.surv$WinGPP_tm1 <- glm(alive_t1 ~ -1 + ageClass/WinGPP_tm1 + pred, data=df_surv, family="binomial")

mod.surv$WinSnow_tm1 <- glm(alive_t1 ~ -1 + ageClass/WinSnow_tm1 + pred, data=df_surv, family="binomial")

mod.surv$WinPSNET_tm1 <- glm(alive_t1 ~ -1 + ageClass/WinPSNNET_tm1 + pred, data=df_surv, family="binomial")

mod.surv$WinFPAR_tm1 <- glm(alive_t1 ~ -1 + ageClass/WinFPAR_tm1 + pred, data=df_surv, family="binomial")


#aic table
x.surv <- aictab(mod.surv)
## exporting AIC table
aictable.surv <- xtable(x.surv, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
#print.xtable(aictable.surv, type="html", 
#             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/surv_pheno_aic_table.html") # open directly with Word
#write.table(aictable.surv,"surv_pheno_aic_table.csv")
#save(df_surv,aictable.surv,mod.surv,file = "surv_pheno.RData")
#getwd()

# Results from best models ---------------------------------------------
summary(mod.surv$pc2)
#results.pc2 <- data.frame(coef(summary(mod.surv$pc2 )))
#results.pc2[, 1:4] <- round(results.pc2[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.surv$pc2 ), digits = 3)
# R2m   R2c
# theoretical 0.174 0.309
# delta       0.112 0.198

#getwd()
#results_pheno_surv<- write.csv(results.pc2, file = "graph/results_pheno_surv.csv", row.names = FALSE)

summary(mod.surv$base)
round(MuMIn::r.squaredGLMM(mod.surv$base ), digits = 3)

##### Reproduction ~ Phenology #########################################################################################
# Setting up and importing data ----

# Download data from drive
drive_download("OWPC/Analyses/data/df_fec_pca.csv",overwrite=T) #Path à modifier une fois base de données finale complété
# Import dataframe in R environment
df_fec<-read.csv("df_fec_pca.csv",sep = ",") #Path à modifier une fois base de données finale complété

# Raw repro model selection  -------------------------------------------------------------------------------------------
mod.raw.repro <- list()

mod.raw.repro$base <- glmer(raw_repro ~ -1 +  ageClass_r + MassAutumn_tm1 + (1|ID), 
                    data=df_fec, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 1000000))) 
mod.raw.repro$pc1 <- glmer(raw_repro ~ -1 + ageClass_r/PC1 +  MassAutumn_tm1+ (1|ID), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.raw.repro$pc1pc2 <- glmer(raw_repro ~ -1 + ageClass_r/PC1 + ageClass_r/PC2 +   MassAutumn_tm1 + (1|ID), 
                      data=df_fec, 
                      family="binomial",
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000))) 
mod.raw.repro$pc2 <- glmer(raw_repro ~ -1 + ageClass_r/PC2 +   MassAutumn_tm1 + (1|ID), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.raw.repro$WinNDVI_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinNDVI_fec +   MassAutumn_tm1 + (1|ID), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.raw.repro$WinEVI_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinEVI_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000)))
#mod.raw.repro$WinLAI_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinLAI_fec +   MassAutumn_tm1 + (1|ID), 
 #                         data=df_fec, 
  #                        family="binomial",
   #                       control = glmerControl(optimizer="bobyqa", 
                                                # optCtrl = list(maxfun = 3000000)))
mod.raw.repro$WinGPP_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinGPP_fec +   MassAutumn_tm1 + (1|ID), 
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))
mod.raw.repro$WinSnow_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinSnow_fec +   MassAutumn_tm1 + (1|ID), 
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))
mod.raw.repro$WinPSNNET_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinPSNNET_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000)))
mod.raw.repro$WinFPAR_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinFPAR_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.raw.repro$SummerNDVI_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerNDVI_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000)))
mod.raw.repro$SummerEVI_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerEVI_fec +   MassAutumn_tm1 + (1|ID), 
                              data=df_fec, 
                              family="binomial",
                              control = glmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 2000000)))
mod.raw.repro$SummerLAI_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerLAI_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.raw.repro$SummerGPP_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerGPP_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.raw.repro$SummerSnow_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerSnow_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.raw.repro$SummerPSNNET_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerPSNNET_fec +   MassAutumn_tm1 + (1|ID), 
                              data=df_fec, 
                              family="binomial",
                              control = glmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 2000000)))
mod.raw.repro$SummerFPAR_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerFPAR_fec +   MassAutumn_tm1 + (1|ID), 
                                data=df_fec, 
                                family="binomial",
                                control = glmerControl(optimizer="bobyqa", 
                                                       optCtrl = list(maxfun = 2000000)))
#aic table
x.raw.repro <- aictab(mod.raw.repro)
## exporting AIC table
aictable.raw.repro <- xtable(x.raw.repro, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

#print.xtable(aictable.raw.repro, type="html",
#             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/raw_fec_pheno_aic_table.html") # open directly with Word
#getwd()

# Raw repro results from best models --------------------------------------------------------------------------------------------- 
summary(mod.raw.repro$base)
#resultsraw_fec <- data.frame(coef(summary(mod.raw.repro$base)))
#resultsraw_fec[, 1:4] <- round(resultsraw_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.raw.repro$base), digits = 3) #
# R2m   R2c
# theoretical 0.523 0.855
# delta       0.458 0.749
#getwd()
#results_pheno_fec<- write.csv(resultsraw_fec, file = "raw_results_pheno_fec.csv", row.names = FALSE)

summary(mod.raw.repro$SummerPSNNET_fec)

resultsraw_fec <- data.frame(coef(summary(mod.raw.repro$SummerPSNNET_fec)))
resultsraw_fec[, 1:4] <- round(resultsraw_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.raw.repro$SummerPSNNET_fec), digits = 3) #
# R2m   R2c
# theoretical 0.542 0.881
# delta       0.486 0.790
#getwd()
#results_pheno_fec<- write.csv(resultsraw_fec, file = "raw_results_pheno_fec.csv", row.names = FALSE)

summary(mod.raw.repro$SummerGPP_fec)

round(MuMIn::r.squaredGLMM(mod.raw.repro$SummerGPP_fec), digits = 3) #
# R2m   R2c
# theoretical 0.543 0.869
# delta       0.481 0.771
getwd()
#results_pheno_fec<- write.csv(resultsraw_fec, file = "raw_results_pheno_fec.csv", row.names = FALSE)


# True repro model selection -------------------------------------------------------
mod.true.repro <- list()
mod.true.repro$base <- glmer(true_repro ~ -1 +  ageClass_r + MassAutumn_tm1 + (1|ID), 
                    data=df_fec, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 1000000))) 
mod.true.repro$pc1 <- glmer(true_repro ~ -1 + ageClass_r/PC1 +  MassAutumn_tm1+ (1|ID), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.true.repro$pc1pc2 <- glmer(true_repro ~ -1 + ageClass_r/PC1 + ageClass_r/PC2 +   MassAutumn_tm1 + (1|ID), 
                      data=df_fec, 
                      family="binomial",
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000))) 
mod.true.repro$pc2 <- glmer(true_repro ~ -1 + ageClass_r/PC2 +   MassAutumn_tm1 + (1|ID), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.true.repro$WinNDVI_fec <- glmer(true_repro ~ -1 + ageClass_r/WinNDVI_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000))) 
mod.true.repro$WinEVI_fec <- glmer(true_repro ~ -1 + ageClass_r/WinEVI_fec +   MassAutumn_tm1 + (1|ID), 
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))
mod.true.repro$WinLAI_fec <- glmer(true_repro ~ -1 + ageClass_r/WinLAI_fec +   MassAutumn_tm1 + (1|ID), 
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 3000000)))
mod.true.repro$WinGPP_fec <- glmer(true_repro ~ -1 + ageClass_r/WinGPP_fec +   MassAutumn_tm1 + (1|ID), 
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))
mod.true.repro$WinSnow_fec <- glmer(true_repro ~ -1 + ageClass_r/WinSnow_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000)))
mod.true.repro$WinPSNNET_fec <- glmer(true_repro ~ -1 + ageClass_r/WinPSNNET_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.true.repro$WinFPAR_fec <- glmer(true_repro ~ -1 + ageClass_r/WinFPAR_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000)))
mod.true.repro$SummerNDVI_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerNDVI_fec +   MassAutumn_tm1 + (1|ID), 
                              data=df_fec, 
                              family="binomial",
                              control = glmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 2000000)))
mod.true.repro$SummerEVI_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerEVI_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.true.repro$SummerLAI_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerLAI_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.true.repro$SummerGPP_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerGPP_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.true.repro$SummerSnow_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerSnow_fec +   MassAutumn_tm1 + (1|ID), 
                              data=df_fec, 
                              family="binomial",
                              control = glmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 2000000)))
mod.true.repro$SummerPSNNET_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerPSNNET_fec +   MassAutumn_tm1 + (1|ID), 
                                data=df_fec, 
                                family="binomial",
                                control = glmerControl(optimizer="bobyqa", 
                                                       optCtrl = list(maxfun = 2000000)))
mod.true.repro$SummerFPAR_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerFPAR_fec +   MassAutumn_tm1 + (1|ID), 
                              data=df_fec, 
                              family="binomial",
                              control = glmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 2000000)))
#aic table
x.true.repro <- aictab(mod.true.repro)
## exporting AIC table
aictable.true.repro <- xtable(x.true.repro, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
#print.xtable(aictable.true.repro, type="html",
#             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/true_fec_pheno_aic_table.html") # open directly with Word
#getwd()

# Raw repro results from best models -------------------------------------------------------------------

# 
summary(mod.true.repro$SummerPSNNET_fec)
#resultstrue_fec <- data.frame(coef(summary(mod.true.repro$SummerPSNNET_fec)))
#resultstrue_fec[, 1:4] <- round(resultstrue_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.true.repro$SummerPSNNET_fec), digits = 3) #
# R2m   R2c
# theoretical 0.301 0.323
# delta       0.231 0.247
#getwd()
#results_pheno_fec<- write.csv(resultstrue_fec, file = "true_results_pssnet_fec.csv", row.names = FALSE)

# results 
summary(mod.true.repro$SummerGPP_fec)
#resultstrue_fec <- data.frame(coef(summary(mod.true.repro$SummerGPP_fec)))
#resultstrue_fec[, 1:4] <- round(resultstrue_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.true.repro$SummerGPP_fec), digits = 3) #
#R2m   R2c
#theoretical 0.292 0.313
#delta       0.223 0.239
#results_pheno_fec<- write.csv(resultstrue_fec, file = "true_results_gpp_fec.csv", row.names = FALSE)
