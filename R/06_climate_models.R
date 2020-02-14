library(lme4)
library(plyr)
library(dplyr)
library(AICcmodavg)
library(xtable)
library(googledrive)
library(car)
# script pour analyses de survie

rm(list = ls())
load("~/Documents/PhD/Analyses/OWPC/OWPC/cache/surv_data.RData")
load("~/Documents/PhD/Analyses/OWPC/OWPC/cache/repro_data.RData")
df_surv = surv_climate_data
df_fec = fecun_climate_data
df_fec$yr <- as.factor(df_fec$yr)
df_fec$true_repro <- as.factor(df_fec$true_repro)
df_fec$raw_repro <- as.factor(df_fec$raw_repro)
df_fec$true_repro_tm1 <- as.factor(df_fec$true_repro_tm1)
df_surv$alive_t1 = as.factor(df_surv$alive_t1)

# prepare surv data for models --------------------------------------------
df_surv$ageClass <- ifelse(df_surv$age >= 8, 8, df_surv$age)
c37 <- c(3:7)
df_surv$ageClass <- ifelse(df_surv$age %in% c37 , 37, df_surv$ageClass)
df_surv$ageClass <- as.factor(df_surv$ageClass)

df_surv$yr<-as.factor(df_surv$yr)
df_surv$pred<-as.factor(df_surv$pred)
df_surv$alive_t1<-as.factor(df_surv$alive_t1)

# add new column of PDO+ SOI

df_surv$PDOSOI_winter <- df_surv$PDO.winter_surv - df_surv$SOI.winter_surv
df_surv$PDOSOI_spring <- df_surv$PDO.spring_surv - df_surv$SOI.spring_surv
df_surv$PDOSOI_summer <- df_surv$PDO.summer_surv - df_surv$SOI.summer_surv
df_surv$PDOSOI_fall <- df_surv$PDO.fall_surv - df_surv$SOI.fall_surv

df_surv$PDOSOI_winter_tm1 <- df_surv$PDO.winter_tm1 - df_surv$SOI.winter_tm1

# scale
colnames(df_surv)
df_surv[c("MassSpring","MassAutumn",  "PDO.summer_surv", "PDO.fall_surv","SOI.summer_surv", "SOI.fall_surv",
          "PDO.winter_tm1" , "SOI.winter_tm1" , "PDO.winter_surv" ,"PDO.spring_surv", "SOI.winter_surv",
          "SOI.spring_surv","PDOSOI_winter" ,  "PDOSOI_spring" ,  "PDOSOI_summer" ,  "PDOSOI_fall", "PDOSOI_winter_tm1" )] <- 
  scale(df_surv[c("MassSpring","MassAutumn",  "PDO.summer_surv", "PDO.fall_surv","SOI.summer_surv", "SOI.fall_surv",
                  "PDO.winter_tm1" , "SOI.winter_tm1" , "PDO.winter_surv" ,"PDO.spring_surv", "SOI.winter_surv",
                  "SOI.spring_surv","PDOSOI_winter" ,  "PDOSOI_spring" ,  "PDOSOI_summer" ,  "PDOSOI_fall", "PDOSOI_winter_tm1" )]) 
df_surv<- df_surv[-which(df_surv$first_yr_trans==1),]

#df_surv<-na.omit(df_surv)

# prepare fecundity data --------------------------------------------------


# age classes

C48 <-4:8 

df_fec$ageClass_r <- ifelse(df_fec$age == 3, 3, df_fec$age)
df_fec$ageClass_r <- ifelse(df_fec$ageClass_r >=9, 9, df_fec$ageClass_r)
df_fec$ageClass_r <- ifelse(df_fec$ageClass_r %in% C48, 48, df_fec$ageClass_r)
df_fec <- df_fec[which(df_fec$age>=3),]

head(df_fec)


# add new column of PDO+ SOI
colnames(df_fec)

df_fec$PDOSOI_winter <- df_fec$PDO.winter_fec - df_fec$SOI.winter_fec
df_fec$PDOSOI_spring <- df_fec$PDO.spring_fec - df_fec$SOI.spring_fec
df_fec$PDOSOI_summer <- df_fec$PDO.summer_fec - df_fec$SOI.summer_fec
df_fec$PDOSOI_fall <- df_fec$PDO.fall_fec - df_fec$SOI.fall_fec


# scale
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

df_fec <- na.omit(df_fec) # ATTENTION MIGHT NOT BE NECESSARY EVERYWHERE
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
 #                   control = glmerControl(optimizer="bobyqa", 
  #                                         optCtrl = list(maxfun = 200000)))

# vif(mod.l$mod8)
# mod.l$mod8 <- glm(raw_repro ~ -1 + ageClass_r/PDOSOI_summer + MassAutumn_tm1, family="binomial", data=df_fec)


mod.l$mod8 <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_summer + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 100000)))

mod.l$mod9 <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_fall + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 100000)))

mod.l$mod10 <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_winter +MassAutumn_tm1+  (1|ID), family="binomial", data=df_fec,
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 100000)))

mod.l$mod11 <- glmer(raw_repro ~ -1 + ageClass_r/PDOSOI_spring  + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 100000)))

mod.l$mod12 <- glmer(raw_repro ~ -1 + ageClass_r + MassAutumn_tm1+ (1|ID), family="binomial", data=df_fec,
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
