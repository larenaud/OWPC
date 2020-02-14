# phenoogy variables - survival ------------------------------------------------------
rm(list = ls())

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

# model pheno survival ----------------------------------------------------------
colnames(df_surv)

# check model PO

#aic table
x <- aictab(mod.l)
## exporting AIC table
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html", 
             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/surv_pheno_aic_table.html") # open directly with Word
getwd()
# results 
summary(mod.l$pc2)
results.pc2 <- data.frame(coef(summary(mod.l$pc2 )))
results.pc2[, 1:4] <- round(results.pc2[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$pc2 ), digits = 3) #
# R2m   R2c
# theoretical 0.174 0.309
# delta       0.112 0.198
getwd()
#results_pheno_surv<- write.csv(results.pc2, file = "graph/results_pheno_surv.csv", row.names = FALSE)

# prepare  fecundity data ---------------------------------------------------------

# run raw fun models  -----------------------------------------------------
# set your data output
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph")
# run models
mod.l <- list()
mod.l$base <- glmer(raw_repro ~ -1 +  ageClass_r + MassAutumn_tm1 + (1|ID), 
                    data=df_fec, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 1000000))) 
mod.l$pc1 <- glmer(raw_repro ~ -1 + ageClass_r/PC1 +  MassAutumn_tm1+ (1|ID), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.l$pc1pc2 <- glmer(raw_repro ~ -1 + ageClass_r/PC1 + ageClass_r/PC2 +   MassAutumn_tm1 + (1|ID), 
                      data=df_fec, 
                      family="binomial",
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000))) 
mod.l$pc2 <- glmer(raw_repro ~ -1 + ageClass_r/PC2 +   MassAutumn_tm1 + (1|ID), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.l$WinNDVI_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinNDVI_fec +   MassAutumn_tm1 + (1|ID), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.l$WinEVI_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinEVI_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000)))
#mod.l$WinLAI_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinLAI_fec +   MassAutumn_tm1 + (1|ID), 
 #                         data=df_fec, 
  #                        family="binomial",
   #                       control = glmerControl(optimizer="bobyqa", 
                                                # optCtrl = list(maxfun = 3000000)))
mod.l$WinGPP_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinGPP_fec +   MassAutumn_tm1 + (1|ID), 
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))
mod.l$WinSnow_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinSnow_fec +   MassAutumn_tm1 + (1|ID), 
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))
mod.l$WinPSNNET_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinPSNNET_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000)))
mod.l$WinFPAR_fec <- glmer(raw_repro ~ -1 + ageClass_r/WinFPAR_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.l$SummerNDVI_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerNDVI_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000)))
mod.l$SummerEVI_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerEVI_fec +   MassAutumn_tm1 + (1|ID), 
                              data=df_fec, 
                              family="binomial",
                              control = glmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 2000000)))
mod.l$SummerLAI_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerLAI_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.l$SummerGPP_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerGPP_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.l$SummerSnow_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerSnow_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.l$SummerPSNNET_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerPSNNET_fec +   MassAutumn_tm1 + (1|ID), 
                              data=df_fec, 
                              family="binomial",
                              control = glmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 2000000)))
mod.l$SummerFPAR_fec <- glmer(raw_repro ~ -1 + ageClass_r/SummerFPAR_fec +   MassAutumn_tm1 + (1|ID), 
                                data=df_fec, 
                                family="binomial",
                                control = glmerControl(optimizer="bobyqa", 
                                                       optCtrl = list(maxfun = 2000000)))
#aic table
x <- aictab(mod.l)
## exporting AIC table
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html",
             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/raw_fec_pheno_aic_table.html") # open directly with Word
getwd()
# results 
summary(mod.l$base)
resultsraw_fec <- data.frame(coef(summary(mod.l$base)))
resultsraw_fec[, 1:4] <- round(resultsraw_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$base), digits = 3) #
# R2m   R2c
# theoretical 0.523 0.855
# delta       0.458 0.749
getwd()
#results_pheno_fec<- write.csv(resultsraw_fec, file = "raw_results_pheno_fec.csv", row.names = FALSE)

summary(mod.l$SummerPSNNET_fec)

resultsraw_fec <- data.frame(coef(summary(mod.l$SummerPSNNET_fec)))
resultsraw_fec[, 1:4] <- round(resultsraw_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$SummerPSNNET_fec), digits = 3) #
# R2m   R2c
# theoretical 0.542 0.881
# delta       0.486 0.790
getwd()
#results_pheno_fec<- write.csv(resultsraw_fec, file = "raw_results_pheno_fec.csv", row.names = FALSE)

summary(mod.l$SummerGPP_fec)

round(MuMIn::r.squaredGLMM(mod.l$SummerGPP_fec), digits = 3) #
# R2m   R2c
# theoretical 0.543 0.869
# delta       0.481 0.771
getwd()
#results_pheno_fec<- write.csv(resultsraw_fec, file = "raw_results_pheno_fec.csv", row.names = FALSE)


# true reproduction -------------------------------------------------------
mod.l <- list()
mod.l$base <- glmer(true_repro ~ -1 +  ageClass_r + MassAutumn_tm1 + (1|ID), 
                    data=df_fec, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 1000000))) 
mod.l$pc1 <- glmer(true_repro ~ -1 + ageClass_r/PC1 +  MassAutumn_tm1+ (1|ID), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.l$pc1pc2 <- glmer(true_repro ~ -1 + ageClass_r/PC1 + ageClass_r/PC2 +   MassAutumn_tm1 + (1|ID), 
                      data=df_fec, 
                      family="binomial",
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000))) 
mod.l$pc2 <- glmer(true_repro ~ -1 + ageClass_r/PC2 +   MassAutumn_tm1 + (1|ID), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.l$WinNDVI_fec <- glmer(true_repro ~ -1 + ageClass_r/WinNDVI_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000))) 
mod.l$WinEVI_fec <- glmer(true_repro ~ -1 + ageClass_r/WinEVI_fec +   MassAutumn_tm1 + (1|ID), 
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))
mod.l$WinLAI_fec <- glmer(true_repro ~ -1 + ageClass_r/WinLAI_fec +   MassAutumn_tm1 + (1|ID), 
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 3000000)))
mod.l$WinGPP_fec <- glmer(true_repro ~ -1 + ageClass_r/WinGPP_fec +   MassAutumn_tm1 + (1|ID), 
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))
mod.l$WinSnow_fec <- glmer(true_repro ~ -1 + ageClass_r/WinSnow_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000)))
mod.l$WinPSNNET_fec <- glmer(true_repro ~ -1 + ageClass_r/WinPSNNET_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.l$WinFPAR_fec <- glmer(true_repro ~ -1 + ageClass_r/WinFPAR_fec +   MassAutumn_tm1 + (1|ID), 
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000)))
mod.l$SummerNDVI_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerNDVI_fec +   MassAutumn_tm1 + (1|ID), 
                              data=df_fec, 
                              family="binomial",
                              control = glmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 2000000)))
mod.l$SummerEVI_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerEVI_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.l$SummerLAI_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerLAI_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.l$SummerGPP_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerGPP_fec +   MassAutumn_tm1 + (1|ID), 
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000)))
mod.l$SummerSnow_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerSnow_fec +   MassAutumn_tm1 + (1|ID), 
                              data=df_fec, 
                              family="binomial",
                              control = glmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 2000000)))
mod.l$SummerPSNNET_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerPSNNET_fec +   MassAutumn_tm1 + (1|ID), 
                                data=df_fec, 
                                family="binomial",
                                control = glmerControl(optimizer="bobyqa", 
                                                       optCtrl = list(maxfun = 2000000)))
mod.l$SummerFPAR_fec <- glmer(true_repro ~ -1 + ageClass_r/SummerFPAR_fec +   MassAutumn_tm1 + (1|ID), 
                              data=df_fec, 
                              family="binomial",
                              control = glmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 2000000)))
#aic table
x <- aictab(mod.l)
## exporting AIC table
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html",
             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/true_fec_pheno_aic_table.html") # open directly with Word
getwd()
# results 
summary(mod.l$SummerPSNNET_fec)
resultstrue_fec <- data.frame(coef(summary(mod.l$SummerPSNNET_fec)))
resultstrue_fec[, 1:4] <- round(resultstrue_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$SummerPSNNET_fec), digits = 3) #
# R2m   R2c
# theoretical 0.301 0.323
# delta       0.231 0.247
getwd()
results_pheno_fec<- write.csv(resultstrue_fec, file = "true_results_pssnet_fec.csv", row.names = FALSE)

# results 
summary(mod.l$SummerGPP_fec)
resultstrue_fec <- data.frame(coef(summary(mod.l$SummerGPP_fec)))
resultstrue_fec[, 1:4] <- round(resultstrue_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$SummerGPP_fec), digits = 3) #
#R2m   R2c
#theoretical 0.292 0.313
#delta       0.223 0.239
results_pheno_fec<- write.csv(resultstrue_fec, file = "true_results_gpp_fec.csv", row.names = FALSE)
