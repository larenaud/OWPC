# script for survival models

# Loading required libraries
library(lme4)
library(plyr)
library(dplyr)
library(AICcmodavg)
library(xtable)
library(googledrive)
library(car)

# Set working directory DIFFERENT FOR EACH PERSON
setwd("")
#Ex: setwd("C:/Users/Proprietaire/Documents/uni poc/Phd/OWPC/Analyses")

##### Survival ~ Climate ############################################################
# Setting up R environment ----
# Cleaning R environment
rm(list = ls())

# Download data from drive
drive_download("OWPC/Analyses/data/surv_climate_data.csv",overwrite=T)
# Import in R environment
df_surv<-read.csv("surv_climate_data.csv",sep = ",")

# Prepare data for models----

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

# Run models ----
#colnames(df_surv)
# List of candidate models
mod.l <- list()
mod.l$base <- glm(alive_t1 ~ -1 + ageClass + pred, data=df_surv, family="binomial")

mod.l$summerPDO <- glm(alive_t1 ~ -1 + ageClass/PDO.summer_surv + pred, 
                       data=df_surv, family="binomial")

mod.l$summerSOI <- glm(alive_t1 ~ -1 + ageClass/SOI.summer_surv + pred, 
                       data=df_surv, family="binomial")

mod.l$summer_int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_summer + pred, 
                        data=df_surv, family="binomial")

mod.l$fallPDO <- glm(alive_t1 ~ -1 + ageClass/PDO.fall_surv + pred, 
                     data=df_surv, family="binomial")

mod.l$fallSOI <- glm(alive_t1 ~ -1 + ageClass/SOI.fall_surv + pred, 
                     data=df_surv, family="binomial")

mod.l$fallPDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.fall_surv+ageClass/SOI.fall_surv + pred, 
                        data=df_surv, family="binomial") 

mod.l$fall_int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_fall + pred, 
                      data=df_surv, family="binomial")

mod.l$winterPDO <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_surv + pred, 
                       data=df_surv, family="binomial")

mod.l$winterSOI <- glm(alive_t1 ~ -1 + ageClass/SOI.winter_surv + pred, 
                       data=df_surv, family="binomial")

mod.l$winterPDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_surv+ageClass/SOI.winter_surv + pred, 
                          data=df_surv, family="binomial") 

mod.l$winter_int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_winter + pred, 
                        data=df_surv, family="binomial")

mod.l$springPDO <- glm(alive_t1 ~ -1 + ageClass/PDO.spring_surv + pred, 
                       data=df_surv, family="binomial")

mod.l$springSOI <- glm(alive_t1 ~ -1 + ageClass/SOI.spring_surv + pred, 
                       data=df_surv, family="binomial")

mod.l$springPDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.spring_surv+ageClass/SOI.spring_surv + pred, 
                          data=df_surv, family="binomial") 

mod.l$spring_int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_spring + pred, 
                        data=df_surv, family="binomial")

mod.l$winter.tm1.PDO <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_tm1 + pred, 
                            data=df_surv, family="binomial")

mod.l$winter.tm1.SOI <- glm(alive_t1 ~ -1 + ageClass/SOI.winter_tm1 + pred, 
                            data=df_surv, family="binomial")

mod.l$winter.tm1.PDOSOI <- glm(alive_t1 ~ -1 + ageClass/PDO.winter_tm1+ageClass/SOI.winter_tm1 + pred, 
                               data=df_surv, family="binomial") 

mod.l$winter.tm1.int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_winter_tm1 + pred, 
                            data=df_surv, family="binomial")

# female density and mass not included as control variables because correlated with age classes 

# Create AIC table
x <- aictab(mod.l)
# Exporting AIC table
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

# Options to save AIC table
#print.xtable(aictable, type="html", file="surv_climate_aic_table.html")
#write.table(aictable, file="surv_climate_aic_table.csv")

# Option to create and save RData file with dataframe, AIC table and canidate models list
# save(df_surv,aictable,mod.l,file = "surv_clim.Rdata")

# Results ---- 

# Best model
summary(mod.l$base)
# Calculate R squared
round(MuMIn::r.squaredGLMM(mod.l$base), digits = 3)
# Save results
# results.base <- data.frame(coef(summary(mod.l$base)))
# results.base[, 1:4] <- round(results.base[, 1:4], digits = 3)
# write.csv(results.base, file = "results_BestMod_climat_surv.csv", row.names = T)

# Second best model 
summary(mod.l$winterSOI)
# Calculate R squared
round(MuMIn::r.squaredGLMM(mod.l$winterSOI), digits = 3) #
# Save results
#results.winterSOI <- data.frame(coef(summary(mod.l$winterSOI)))
#results.winterSOI[, 1:4] <- round(results.winterSOI[, 1:4], digits = 3)
#write.csv(results.winter_int, file = "results_SecBestMod_climat_surv.csv", row.names = T)

##### Survival ~ Climate ############################################################
# Setting up R environment ----
# Cleaning R environment
rm(list = ls())

# Download data from drive
#drive_download("OWPC/Analyses/data/fecun_climate_data.csv",overwrite=T)
# Import in R environment
#df_fec<-read.csv("fecun_climate_data.csv",sep = ",")

load("~/Documents/PhD/Analyses/OWPC/OWPC/cache/repro_data.RData")
df_fec = fecun_climate_data
df_fec$yr <- as.factor(df_fec$yr)
df_fec$true_repro <- as.factor(df_fec$true_repro)
df_fec$raw_repro <- as.factor(df_fec$raw_repro)
df_fec$true_repro_tm1 <- as.factor(df_fec$true_repro_tm1)

# prepare fecundity data --------------------------------------------------

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
# run survival models -----------------------------------------------------

# prendre les modèles de PO

#aic table
x <- aictab(mod.l)
## exporting AIC table
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html", 
             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/surv_climate_aic_table.html") # open directly with Word
getwd()
# results 
summary(mod.l$fall_int)
results.fall_int <- data.frame(coef(summary(mod.l$fall_int)))
results.fall_int[, 1:4] <- round(results.fall_int[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$fall_int), digits = 3) #
# R2m   R2c
#theoretical 0.187 0.217
#delta       0.102 0.118
getwd()
results_clim_surv<- write.csv(results.fall_int, file = "graph/results_fall_surv.csv", row.names = FALSE)


summary(mod.l$winter_int)
results.winter_int <- data.frame(coef(summary(mod.l$winter_int)))
results.winter_int[, 1:4] <- round(results.winter_int[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$winter_int), digits = 3) #
# R2m   R2c
# theoretical 0.316 0.349
# delta       0.187 0.206
getwd()
results_clim_surv_winter<- write.csv(results.winter_int, file = "graph/results_winter_surv.csv", row.names = FALSE)

# run raw fecundity models ---------------------------------
mod.l <- list()
mod.l$mod1 <- glmer(raw_repro ~ -1 + ageClass_r/PDO.summer_fec + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod2 <- glmer(raw_repro ~ -1 + ageClass_r/PDO.fall_fec + MassAutumn_tm1 + (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod3 <- glmer(raw_repro ~ -1 + ageClass_r/PDO.winter_fec + MassAutumn_tm1+  (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod4 <- glmer(raw_repro ~ -1 + ageClass_r/PDO.spring_fec + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))


mod.l$mod5 <- glmer(raw_repro ~ -1 + ageClass_r/PDO.summer_fec + ageClass_r/SOI.summer_fec + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod6 <- glmer(raw_repro ~ -1 + ageClass_r/PDO.fall_fec + ageClass_r/SOI.fall_fec + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod7 <- glmer(raw_repro ~ -1 + ageClass_r/PDO.winter_fec + ageClass_r/SOI.winter_fec + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

#mod.l$mod8 <- glmer(raw_repro ~ -1 + ageClass_r/PDO.spring_fec + ageClass_r/SOI.spring_fec + (1|ID), family="binomial", data=df_fec,
  #                  control = glmerControl(optimizer="bobyqa", 
   #                                        optCtrl = list(maxfun = 200000)))

# vif(mod.l$mod8)
# mod.l$mod8 <- glm(raw_repro ~ -1 + ageClass_r/PDOSOI_summer + MassAutumn_tm1, family="binomial", data=df_fec)


mod.l$mod9 <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_summer + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod10 <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_fall + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 100000)))

mod.l$mod11 <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_winter +MassAutumn_tm1+  (1|ID), family="binomial", data=df_fec,
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 100000)))

mod.l$mod12 <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_spring  + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 100000)))

mod.l$mod13 <- glmer(raw_repro ~ -1 + ageClass_r + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 100000)))
x <- aictab(mod.l)

summary(mod.l$mod12)
summary(mod.l$mod4)

## exporting AIC table
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html", 
             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/RAW_fec_climate_aic_table.html") # open directly with Word
getwd()
# results 
summary(mod.l$mod4)
resultsraw_fec <- data.frame(coef(summary(mod.l$mod4)))
resultsraw_fec[, 1:4] <- round(resultsraw_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$mod4), digits = 3) #
#R2m   R2c
#theoretical 0.385 0.990
#delta       0.382 0.982
getwd()
write.csv(resultsraw_fec, file = "graph/results_clim_raw_fec.csv", row.names = FALSE)

# run true reproduction models  --------------------------------------
mod.l <- list()
mod.l$mod1 <- glmer(true_repro ~ -1 + ageClass_r/PDO.summer_fec + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod2 <- glmer(true_repro ~ -1 + ageClass_r/PDO.fall_fec + MassAutumn_tm1 + (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod3 <- glmer(true_repro ~ -1 + ageClass_r/PDO.winter_fec + MassAutumn_tm1+  (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod4 <- glmer(true_repro ~ -1 + ageClass_r/PDO.spring_fec + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))


mod.l$mod5 <- glmer(true_repro ~ -1 + ageClass_r/PDO.summer_fec + ageClass_r/SOI.summer_fec + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod6 <- glmer(true_repro ~ -1 + ageClass_r/PDO.fall_fec + ageClass_r/SOI.fall_fec + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod7 <- glmer(true_repro ~ -1 + ageClass_r/PDO.winter_fec + ageClass_r/SOI.winter_fec + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod8 <- glmer(true_repro ~ -1 + ageClass_r/PDOSOI_summer + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod9 <- glmer(true_repro ~ -1 + ageClass_r/PDOSOI_fall + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod10 <- glmer(true_repro ~ -1 + ageClass_r/PDOSOI_winter +MassAutumn_tm1+  (1|ID), family="binomial", data=df_fec,
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 100000)))

mod.l$mod11 <- glmer(true_repro ~ -1 + ageClass_r/PDOSOI_spring  + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 100000)))

mod.l$mod12 <- glmer(true_repro ~ -1 + ageClass_r + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 100000)))
mod.l$mod13 <- glmer(true_repro ~ -1 + ageClass_r/PDO.spring_fec + ageClass_r/SOI.spring_fec + (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun = 200000)))

#aic table
x <- aictab(mod.l)
summary(mod.l$mod7)


# #vif.lme <- function (mod.l$mod7) {
#   ## adapted from rms::vif
#   v <- vcov(mod.l$mod7)
#   nam <- names(fixef(mod.l$mod7))
#   ## exclude intercepts
#   ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
#   if (ns > 0) {
#     v <- v[-(1:ns), -(1:ns), drop = FALSE]
#     nam <- nam[-(1:ns)] }
#   d <- diag(v)^0.5
#   v <- diag(solve(v/(d %o% d)))
#   names(v) <- nam
#   v }

## exporting AIC table
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html", 
             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/TRUE_fec_climate_aic_table.html") # open directly with Word
getwd()
# results 
resultsTRUE_fec <- data.frame(coef(summary(mod.l$mod7)))
resultsTRUE_fec[, 1:4] <- round(resultsTRUE_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$mod7), digits = 3) #
# R2m   R2c
# theoretical 0.320 0.389
# delta       0.265 0.322
getwd()
resultsTRUE_fec <- write.csv(resultsTRUE_fec, file = "graph/results_clim_TRUE_fec.csv", row.names = FALSE)
