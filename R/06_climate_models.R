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

########################################################################################################################
# Data tyding survival (script qui doit être déplacé) ---------------------------------------------------------------------------
# Prepare data for survival models

# Remove lines for translocated individuals in their 1st year on Ram (year of translocation)
df_surv<-subset(df_surv,!(first_yr_trans==1))
df_surv$first_yr_trans<-as.factor(df_surv$first_yr_trans)
df_surv$first_yr_trans<-droplevels(df_surv$first_yr_trans)

# New column ageClass (0,1,2,37,8)
df_surv$ageClass <- ifelse(df_surv$age >= 8, 8, df_surv$age)
c37 <- c(3:7)
df_surv$ageClass <- ifelse(df_surv$age %in% c37 , 37, df_surv$ageClass)
df_surv$ageClass <- as.factor(df_surv$ageClass)

# Formatting variables
df_surv$yr<-as.factor(df_surv$yr)
df_surv$pred<-as.factor(df_surv$pred)
df_surv$alive_t1<-as.factor(df_surv$alive_t1)
df_surv$MassSpring<-as.numeric(as.character(df_surv$MassSpring))
df_surv$MassAutumn<-as.numeric(as.character(df_surv$MassAutumn))
df_surv$PDO.winter_surv<-as.numeric(as.character(df_surv$PDO.winter_surv))
df_surv$PDO.summer_surv<-as.numeric(as.character(df_surv$PDO.summer_surv))
df_surv$PDO.spring_surv<-as.numeric(as.character(df_surv$PDO.spring_surv))
df_surv$PDO.fall_surv<-as.numeric(as.character(df_surv$PDO.fall_surv))
df_surv$SOI.winter_surv<-as.numeric(as.character(df_surv$SOI.winter_surv))
df_surv$SOI.summer_surv<-as.numeric(as.character(df_surv$SOI.summer_surv))
df_surv$SOI.spring_surv<-as.numeric(as.character(df_surv$SOI.spring_surv))
df_surv$SOI.fall_surv<-as.numeric(as.character(df_surv$SOI.fall_surv))
df_surv$PDO.winter_tm1<-as.numeric(as.character(df_surv$PDO.winter_tm1))
df_surv$SOI.winter_tm1<-as.numeric(as.character(df_surv$SOI.winter_tm1))

# Add new column for the combined effect of PDO and SOI. Combined effect = PDO -SOI
df_surv$PDOSOI_winter <- df_surv$PDO.winter_surv - df_surv$SOI.winter_surv
df_surv$PDOSOI_spring <- df_surv$PDO.spring_surv - df_surv$SOI.spring_surv
df_surv$PDOSOI_summer <- df_surv$PDO.summer_surv - df_surv$SOI.summer_surv
df_surv$PDOSOI_fall <- df_surv$PDO.fall_surv - df_surv$SOI.fall_surv
df_surv$PDOSOI_winter_tm1 <- df_surv$PDO.winter_tm1 - df_surv$SOI.winter_tm1

# Scale explanatory variables
colnames(df_surv)
df_surv[c("MassSpring","MassAutumn","PDO.summer_surv", "PDO.fall_surv","SOI.summer_surv",
          "SOI.fall_surv","PDO.winter_surv", "PDO.spring_surv", "SOI.winter_surv", "SOI.spring_surv",
          "PDO.winter_tm1", "SOI.winter_tm1", "PDOSOI_winter_tm1")] <- 
  scale(df_surv[c("MassSpring","MassAutumn","PDO.summer_surv", "PDO.fall_surv","SOI.summer_surv",
                  "SOI.fall_surv","PDO.winter_surv", "PDO.spring_surv", "SOI.winter_surv", "SOI.spring_surv",
                  "PDO.winter_tm1", "SOI.winter_tm1", "PDOSOI_winter_tm1")]) 


# Data tyding reproduction (script qui doit être déplacé) --------------------------------------------------
load("~/Documents/PhD/Analyses/OWPC/OWPC/cache/repro_data.RData")
df_fec = fecun_climate_data
df_fec$yr <- as.factor(df_fec$yr)
df_fec$true_repro <- as.factor(df_fec$true_repro)
df_fec$raw_repro <- as.factor(df_fec$raw_repro)
df_fec$true_repro_tm1 <- as.factor(df_fec$true_repro_tm1)

# Remove lines for translocated individuals in their 1st year on Ram (year of translocation)
df_fec<-subset(df_fec,!(first_yr_trans==1))
df_fec$first_yr_trans<-as.factor(df_fec$first_yr_trans)
df_fec$first_yr_trans<-droplevels(df_fec$first_yr_trans)

# New column ageClass (0,1,2,3,48,9)
C48 <-4:8 
df_fec$ageClass_r <- ifelse(df_fec$age == 3, 3, df_fec$age)
df_fec$ageClass_r <- ifelse(df_fec$ageClass_r >=9, 9, df_fec$ageClass_r)
df_fec$ageClass_r <- ifelse(df_fec$ageClass_r %in% C48, 48, df_fec$ageClass_r)
df_fec <- df_fec[which(df_fec$age>=3),]

head(df_fec)

# Add new column for the combined effect of PDO and SOI. Combined effect = PDO - SOI
df_fec$PDOSOI_winter <- df_fec$PDO.winter_fec - df_fec$SOI.winter_fec
df_fec$PDOSOI_spring <- df_fec$PDO.spring_fec - df_fec$SOI.spring_fec
df_fec$PDOSOI_summer <- df_fec$PDO.summer_fec - df_fec$SOI.summer_fec
df_fec$PDOSOI_fall <- df_fec$PDO.fall_fec - df_fec$SOI.fall_fec


# Scale explanatory variables
colnames(df_fec)
df_fec[c( "MassAutumn_tm1","PDO.winter_fec", "PDO.spring_fec", "PDO.summer_fec", "PDO.fall_fec" ,  "SOI.winter_fec", "SOI.spring_fec",
          "SOI.summer_fec", "SOI.fall_fec" ,"PDOSOI_winter"  ,"PDOSOI_spring" , "PDOSOI_summer" ,
          "PDOSOI_fall")] <- 
  scale(df_fec[c("MassAutumn_tm1","PDO.winter_fec", "PDO.spring_fec", "PDO.summer_fec", "PDO.fall_fec" ,  "SOI.winter_fec", "SOI.spring_fec",
                 "SOI.summer_fec", "SOI.fall_fec" ,"PDOSOI_winter"  ,"PDOSOI_spring" , "PDOSOI_summer" ,
                 "PDOSOI_fall" )]) 

df_fec <- df_fec[-which(df_fec$first_yr_trans==1),]
df_fec$MassAutumn_tm1 <- scale(df_fec$MassAutumn_tm1)

df_fec$ID <- as.factor(df_fec$ID)
df_fec$ageClass_r <- as.factor(df_fec$ageClass_r)
df_fec$pred_tm1 <- as.factor(df_fec$pred_tm1)
df_fec$yr <- as.factor(df_fec$yr)

#df_fec <- na.omit(df_fec) # ATTENTION MIGHT NOT BE NECESSARY EVERYWHERE

########################################################################################################################


##### Survival ~ Climate ###############################################################################################
# Setting up and importing data  ---------------------------------------------------------------------------------------

# Download data from drive
drive_download("OWPC/Analyses/data/surv_climate_data.csv",overwrite=T)#Path à modifier une fois base de données finale complétée
# Import in R environment
df_surv<-read.csv("surv_climate_data.csv",sep = ",")#Path à modifier une fois base de données finale complétée

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

# Options to save AIC table
#print.xtable(results.surv[[aictable.surv]], type="html", file="surv_climate_aic_table.html")
#write.table(results.surv[[aictable.surv]], file="surv_climate_aic_table.csv")

# Results from best models -------------------------------------------------------------------------------------- 

results.surv$coefs.surv.best <- data.frame(coef(summary(mod.surv[[aictable.surv[1,1]]])))
results.surv$coefs.surv.best[, 1:4] <- round(results.surv[["coefs.surv.best"]][, 1:4], digits = 3)
results.surv$r2.surv.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv[[aictable.surv[1,1]]]), digits = 3))

results.surv$coefs.surv.2ndbest <- data.frame(coef(summary(mod.surv[[aictable.surv[2,1]]])))
results.surv$coefs.surv.2ndbest[, 1:4] <- round(results.surv[["coefs.surv.2ndbest"]][, 1:4], digits = 3)
results.surv$r2.surv.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv[[aictable.surv[2,1]]]), digits = 3))

results.surv$coefs.surv.3rdbest <- data.frame(coef(summary(mod.surv[[aictable.surv[3,1]]])))
results.surv$coefs.surv.3rdbest[, 1:4] <- round(results.surv[["coefs.surv.2ndbest"]][, 1:4], digits = 3)
results.surv$r2.surv.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv[[aictable.surv[3,1]]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
# save(df_surv,mod.surv,results.surv,file = "surv_clim.Rdata")

##### Repro ~ Climate ##################################################################################################
# Setting up and importing data ----------------------------------------------------------------------------------------

# Download data from drive
drive_download("OWPC/Analyses/data/fecun_climate_data.csv",overwrite=T)
    #Path à modifier une fois base de données finale complétée

# Import in R environment
df_fec<-read.csv("fecun_climate_data.csv",sep = ",")
    #Path à modifier une fois base de données finale complétée

# Raw repro model selection  -------------------------------------------------------------------------------------------
mod.raw.repro <- list()

mod.raw.repro$summerPDO <- glmer(raw_repro ~ -1 + ageClass_r/PDO.summer_fec + MassAutumn_tm1 + (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro$fallPDO <- glmer(raw_repro ~ -1 + ageClass_r/PDO.fall_fec + MassAutumn_tm1 + (1|ID), 
                               family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                               optCtrl = list(maxfun = 100000)))

mod.raw.repro$winterPDO <- glmer(raw_repro ~ -1 + ageClass_r/PDO.winter_fec + MassAutumn_tm1+  (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro$springPDO <- glmer(raw_repro ~ -1 + ageClass_r/PDO.spring_fec + MassAutumn_tm1+ (1|ID), 
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro$summerPDOSOI <- glmer(raw_repro ~ -1 + ageClass_r/PDO.summer_fec + ageClass_r/SOI.summer_fec + MassAutumn_tm1 + (1|ID), 
                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 100000)))

mod.raw.repro$fallPDOSOI <- glmer(raw_repro ~ -1 + ageClass_r/PDO.fall_fec + ageClass_r/SOI.fall_fec + MassAutumn_tm1 + (1|ID), 
                                  family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                  optCtrl = list(maxfun = 100000)))

mod.raw.repro$winterPDOSOI <- glmer(raw_repro ~ -1 + ageClass_r/PDO.winter_fec + ageClass_r/SOI.winter_fec + MassAutumn_tm1 + (1|ID),
                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 100000)))

mod.raw.repro$springPDOSOI <- glmer(raw_repro ~ -1 + ageClass_r/PDO.spring_fec + ageClass_r/SOI.spring_fec + MassAutumn_tm1 + (1|ID),
                                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 200000)))

mod.raw.repro$summerInt <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_summer + MassAutumn_tm1+ (1|ID),
                            family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                            optCtrl = list(maxfun = 100000)))

mod.raw.repro$fallInt <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_fall + MassAutumn_tm1 + (1|ID),
                               family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                               optCtrl = list(maxfun = 100000)))

mod.raw.repro$winterInt <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_winter + MassAutumn_tm1 + (1|ID),
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro$springInt <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_spring  + MassAutumn_tm1 + (1|ID),
                                 family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                                 optCtrl = list(maxfun = 100000)))

mod.raw.repro$base <- glmer(raw_repro ~ -1 + ageClass_r + MassAutumn_tm1 + (1|ID),
                            family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                            optCtrl = list(maxfun = 100000)))
# Creating a list to store the results
results.raw.repro<-list()

## Creating and exporting AIC table to results list
results.raw.repro$aictable.raw.repro <- xtable(aictab(mod.raw.repro), caption = NULL, label = NULL, align = NULL,
                                               digits = NULL, display = NULL, nice.names = TRUE,
                                               include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
# Raw repro results from best models --------------------------------------------------------------------------------------------- 

results.raw.repro$coefs.raw.repro.best <- data.frame(coef(summary(mod.raw.repro[[aictable.raw.repro[1,1]]])))
results.raw.repro$coefs.raw.repro.best[, 1:4] <- round(results.raw.repro[["coefs.raw.repro.best"]][, 1:4], digits = 3)
results.raw.repro$r2.raw.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro[[aictable.raw.repro[1,1]]]), digits = 3))

results.raw.repro$coefs.raw.repro.2ndbest <- data.frame(coef(summary(mod.raw.repro[[aictable.raw.repro[2,1]]])))
results.raw.repro$coefs.raw.repro.2ndbest[, 1:4] <- round(results.raw.repro[["coefs.raw.repro.2ndbest"]][, 1:4], digits = 3)
results.raw.repro$r2.raw.repro.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro[[aictable.raw.repro[2,1]]]), digits = 3))

results.raw.repro$coefs.raw.repro.3rdbest <- data.frame(coef(summary(mod.raw.repro[[aictable.raw.repro[3,1]]])))
results.raw.repro$coefs.raw.repro.3rdbest[, 1:4] <- round(results.raw.repro[["coefs.raw.repro.2ndbest"]][, 1:4], digits = 3)
results.raw.repro$r2.raw.repro.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw.repro[[aictable.raw.repro[3,1]]]), digits = 3))

# True repro model selection -------------------------------------------------------
mod.true.repro <- list()

mod.l$summerPDO <- glmer(true_repro ~ -1 + ageClass_r/PDO.summer_fec + MassAutumn_tm1 + (1|ID),
                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                    optCtrl = list(maxfun = 100000)))

mod.l$fallPDO <- glmer(true_repro ~ -1 + ageClass_r/PDO.fall_fec + MassAutumn_tm1 + (1|ID),
                       family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                       optCtrl = list(maxfun = 100000)))

mod.l$winterPDO <- glmer(true_repro ~ -1 + ageClass_r/PDO.winter_fec + MassAutumn_tm1 + (1|ID),
                         family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                         optCtrl = list(maxfun = 100000)))

mod.l$springPDO <- glmer(true_repro ~ -1 + ageClass_r/PDO.spring_fec + MassAutumn_tm1 + (1|ID),
                         family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                         optCtrl = list(maxfun = 100000)))


mod.l$summerPDOSOI <- glmer(true_repro ~ -1 + ageClass_r/PDO.summer_fec + ageClass_r/SOI.summer_fec + MassAutumn_tm1 + (1|ID),
                            family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                            optCtrl = list(maxfun = 100000)))

mod.l$fallPDOSOI <- glmer(true_repro ~ -1 + ageClass_r/PDO.fall_fec + ageClass_r/SOI.fall_fec + MassAutumn_tm1 + (1|ID),
                          family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                          optCtrl = list(maxfun = 100000)))

mod.l$winterPDOSOI <- glmer(true_repro ~ -1 + ageClass_r/PDO.winter_fec + ageClass_r/SOI.winter_fec + MassAutumn_tm1 + (1|ID),
                            family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                            optCtrl = list(maxfun = 100000)))

mod.l$springPDOSOI <- glmer(true_repro ~ -1 + ageClass_r/PDO.spring_fec + ageClass_r/SOI.spring_fec + MassAutumn_tm1 + (1|ID),
                            family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                            optCtrl = list(maxfun = 100000)))

mod.l$summerInt <- glmer(true_repro ~ -1 + ageClass_r/PDOSOI_summer + MassAutumn_tm1 + (1|ID),
                         family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                         optCtrl = list(maxfun = 100000)))

mod.l$fallInt <- glmer(true_repro ~ -1 + ageClass_r/PDOSOI_fall + MassAutumn_tm1 + (1|ID),
                       family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                       optCtrl = list(maxfun = 100000)))

mod.l$winterInt <- glmer(true_repro ~ -1 + ageClass_r/PDOSOI_winter + MassAutumn_tm1 + (1|ID),
                         family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                         optCtrl = list(maxfun = 100000)))

mod.l$springInt <- glmer(true_repro ~ -1 + ageClass_r/PDOSOI_spring + MassAutumn_tm1 + (1|ID),
                         family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                         optCtrl = list(maxfun = 100000)))

mod.l$base <- glmer(true_repro ~ -1 + ageClass_r + MassAutumn_tm1+ (1|ID),
                    family="binomial", data=df_fec, control = glmerControl(optimizer="bobyqa", 
                    optCtrl = list(maxfun = 100000)))

# Creating a list to store the results
results.true.repro<-list()

## Creating and exporting AIC table to results list
results.true.repro$aictable.true.repro <- xtable(aictab(mod.true.repro), caption = NULL, label = NULL, align = NULL,
                                                 digits = NULL, display = NULL, nice.names = TRUE,
                                                 include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

# True repro results from best models --------------------------------------------------------------------------------------------- 

results.true.repro$coefs.true.repro.best <- data.frame(coef(summary(mod.true.repro[[aictable.true.repro[1,1]]])))
results.true.repro$coefs.true.repro.best[, 1:4] <- round(results.true.repro[["coefs.true.repro.best"]][, 1:4], digits = 3)
results.true.repro$r2.true.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro[[aictable.true.repro[1,1]]]), digits = 3))

results.true.repro$coefs.true.repro.2ndbest <- data.frame(coef(summary(mod.true.repro[[aictable.true.repro[2,1]]])))
results.true.repro$coefs.true.repro.2ndbest[, 1:4] <- round(results.true.repro[["coefs.true.repro.2ndbest"]][, 1:4], digits = 3)
results.true.repro$r2.true.repro.2ndbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro[[aictable.true.repro[2,1]]]), digits = 3))

results.true.repro$coefs.true.repro.3rdbest <- data.frame(coef(summary(mod.true.repro[[aictable.true.repro[3,1]]])))
results.true.repro$coefs.true.repro.3rdbest[, 1:4] <- round(results.true.repro[["coefs.true.repro.2ndbest"]][, 1:4], digits = 3)
results.true.repro$r2.true.repro.3rdbest<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro[[aictable.true.repro[3,1]]]), digits = 3))
