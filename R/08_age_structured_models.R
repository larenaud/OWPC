library(MCMCglmm)
detach("package:MCMCglmm", unload=TRUE)

library(lme4)
library(plyr)
library(dplyr)
library(AICcmodavg)
library(xtable)
library(googledrive)
# script pour analyses de survie
rm(list = ls())

df_surv <- read.csv("~/Documents/PhD/Analyses/OWPC/OWPC/data/surv_climate_data.csv", sep=";")
df_fec <- read.csv("~/Documents/PhD/Analyses/OWPC/OWPC/data/fecun_climate_data.csv", sep=";")

#New column ageClass (0,1,2,37,8)

df_surv$ageClass <- ifelse(df_surv$age >= 8, 8, df_surv$age)
c37 <- c(3:7)
df_surv$ageClass <- ifelse(df_surv$age %in% c37 , 37, df_surv$ageClass)

df_fec$ageClass <- ifelse(df_fec$age >= 8, 8, df_fec$age)
c37 <- c(3:7)
df_fec$ageClass <- ifelse(df_fec$age %in% c37 , 37, df_fec$ageClass)

df_fec$ageClass <- as.factor(df_fec$ageClass)
df_surv$ageClass <- as.factor(df_surv$ageClass)

# add density
all_surv <- read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/SURV20170328.csv", sep = ",")

df_surv <- merge(unique(all_surv[, c("yr", "fem")]), 
                 df_surv, 
                 by.x = "yr", 
                 by.y = "yr", 
                 all.y = T)

#df_fec <- merge(unique(all_surv[, c("yr", "fem")]), 
 #            df_fec, 
  #           by.x = "yr", 
   #          by.y = "yr", 
    #         all.y = T)
rm(all_surv, tmp)


# prepare surv data for models --------------------------------------------
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

# add new column of PDO+ SOI
df_surv$PDOSOI_winter <- df_surv$PDO.winter_surv + df_surv$SOI.winter_surv
df_surv$PDOSOI_spring <- df_surv$PDO.spring_surv + df_surv$SOI.spring_surv
df_surv$PDOSOI_summer <- df_surv$PDO.summer_surv + df_surv$SOI.summer_surv
df_surv$PDOSOI_fall <- df_surv$PDO.fall_surv + df_surv$SOI.fall_surv

# scale
colnames(df_surv)
df_surv[c("MassSpring","MassAutumn","PDO.summer_surv", "PDO.fall_surv","SOI.summer_surv",
          "SOI.fall_surv","PDO.winter_surv", "PDO.spring_surv", "SOI.winter_surv", "SOI.spring_surv")] <- 
  scale(df_surv[c("MassSpring","MassAutumn","PDO.summer_surv", "PDO.fall_surv","SOI.summer_surv",
                  "SOI.fall_surv","PDO.winter_surv", "PDO.spring_surv", "SOI.winter_surv", "SOI.spring_surv")]) 

df_surv<-na.omit(df_surv)

# prepare fecundity data --------------------------------------------------
df_fec$yr<-as.factor(df_fec$yr)
df_fec$raw_repro <- as.factor(df_fec$raw_repro)
df_fec$true_repro <- as.factor(df_fec$true_repro)

df_fec$MassSpring<-as.numeric(as.character(df_fec$MassSpring))
df_fec$MassAutumn<-as.numeric(as.character(df_fec$MassAutumn))

df_fec$PDO.winter_fec<-as.numeric(as.character(df_fec$PDO.winter_fec))
df_fec$PDO.summer_fec<-as.numeric(as.character(df_fec$PDO.summer_fec))
df_fec$PDO.spring_fec<-as.numeric(as.character(df_fec$PDO.spring_fec))
df_fec$PDO.fall_fec<-as.numeric(as.character(df_fec$PDO.fall_fec))
df_fec$SOI.winter_fec<-as.numeric(as.character(df_fec$SOI.winter_fec))
df_fec$SOI.summer_fec<-as.numeric(as.character(df_fec$SOI.summer_fec))
df_fec$SOI.spring_fec<-as.numeric(as.character(df_fec$SOI.spring_fec))
df_fec$SOI.fall_fec<-as.numeric(as.character(df_fec$SOI.fall_fec))

# add new column of PDO+ SOI
df_fec$PDOSOI_winter <- df_fec$PDO.winter_fec + df_fec$SOI.winter_fec
df_fec$PDOSOI_spring <- df_fec$PDO.spring_fec + df_fec$SOI.spring_fec
df_fec$PDOSOI_summer <- df_fec$PDO.summer_fec + df_fec$SOI.summer_fec
df_fec$PDOSOI_fall <- df_fec$PDO.fall_fec + df_fec$SOI.fall_fec

# scale
colnames(df_fec)
df_fec[c("MassSpring","MassAutumn","PDO.summer_fec", "PDO.fall_fec","SOI.summer_fec",
         "SOI.fall_fec","PDO.winter_fec", "PDO.spring_fec", "SOI.winter_fec", "SOI.spring_fec")] <- 
  scale(df_fec[c("MassSpring","MassAutumn","PDO.summer_fec", "PDO.fall_fec","SOI.summer_fec",
                 "SOI.fall_fec","PDO.winter_fec", "PDO.spring_fec", "SOI.winter_fec", "SOI.spring_fec")]) 

# add time lags for control variables 
colnames(df_fec)
tmp <- df_fec[, c("yr", "ID", "MassAutumn", "pred", "true_repro")]
tmp$yr <-as.numeric(as.character(tmp$yr))

tmp$yr <- tmp$yr + 1

tmp <- tmp %>% 
  rename(MassAutumn_tm1= MassAutumn, 
         pred_tm1= pred, 
         true_repro_tm1 = true_repro)

df_fec<- merge(tmp, 
               df_fec, 
               by.x= c("yr", "ID"), 
               by.y=c('yr', "ID"), 
               all.y = T)

str(df_fec)
df_fec$yr <- as.factor(df_fec$yr)

# remove age classes 0-1-2 
df_fec <- subset(df_fec, ageClass %in% c(37,8))
df_fec$ageClass <- droplevels(df_fec$ageClass)
df_fec$pred_tm1 <- as.factor(df_fec$pred_tm1)

df_fec<-na.omit(df_fec)

# run survival models -----------------------------------------------------

colnames(df_surv)
mod.l <- list()
mod.l$base <- glmer(alive_t1 ~ -1 +  pred+ (1|ID) + (1|yr), 
                         data=df_surv, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 1000000))) 
mod.l$summerPDOSOI <- glmer(alive_t1 ~ -1 + ageClass/PDO.summer_surv+ageClass/(PDOSOI_summer) +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                     optCtrl = list(maxfun = 2000000))) 
mod.l$summerPDO <- glmer(alive_t1 ~ -1 + ageClass/PDO.summer_surv +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000))) 
mod.l$summer_int <- glmer(alive_t1 ~ -1 + ageClass/PDOSOI_summer +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 
mod.l$fallPDOSOI <- glmer(alive_t1 ~ -1 + ageClass/PDO.fall_surv+ageClass/(PDOSOI_fall) +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 
mod.l$fallPDO <- glmer(alive_t1 ~ -1 + ageClass/PDO.fall_surv +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 
mod.l$fall_int <- glmer(alive_t1 ~ -1 + ageClass/PDOSOI_fall +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 

mod.l$winterPDOSOI <- glmer(alive_t1 ~ -1 + ageClass/PDO.winter_surv+ageClass/(PDOSOI_winter) +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 
mod.l$winterPDO<- glmer(alive_t1 ~ -1 + ageClass/PDO.winter_surv +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 
mod.l$winter_int <- glmer(alive_t1 ~ -1 + ageClass/PDOSOI_winter +  pred+ (1|ID) + (1|yr), #
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000))) 
mod.l$springPDOSOI <- glmer(alive_t1 ~ -1 + ageClass/PDO.spring_surv+ageClass/(PDOSOI_spring) +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 
mod.l$springPDO<- glmer(alive_t1 ~ -1 + ageClass/PDO.spring_surv +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa",
                                                   optCtrl = list(maxfun = 2000000))) 
mod.l$spring_int <- glmer(alive_t1 ~ -1 +  ageClass/PDOSOI_spring +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 

# female density and mass removed because correlated with age classes 

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

# NE CONVERGENT TOUJOURS PAS 
mod.l <- list()
mod.l$base <- glmer(raw_repro ~ -1 + MassAutumn_tm1 + (1|ID) + (1|yr), # here write the model
                    data=df_fec, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 2000000))) 
mod.l$summerPDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDO.summer_fec+ageClass/(PDOSOI_summer) +  MassAutumn_tm1 + (1|ID) + (1|yr), # here write the model
                            data=df_fec, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000))) 
mod.l$summerPDO <- glmer(raw_repro ~ -1 + ageClass/PDO.summer_fec +  MassAutumn_tm1 + (1|ID) + (1|yr), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 
mod.l$summer_int <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_summer +  MassAutumn_tm1 + (1|ID) + (1|yr), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 1000000))) 
mod.l$fallPDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDO.fall_fec+ageClass/(PDOSOI_fall) +  MassAutumn_tm1 + (1|ID) + (1|yr), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 1000000))) 
mod.l$fallPDO <- glmer(raw_repro ~ -1 + ageClass/PDO.fall_fec +  MassAutumn_tm1 +  (1|ID) + (1|yr), # here write the model
                       data=df_fec, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 1000000))) 
mod.l$fall_int <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_fall +  MassAutumn_tm1 +  (1|ID) + (1|yr), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

mod.l$winterPDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDO.winter_fec+ageClass/(PDOSOI_winter) +  MassAutumn_tm1 +  (1|ID) + (1|yr), # here write the model
                            data=df_fec, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 
mod.l$winterPDO<- glmer(raw_repro ~ -1 + ageClass/PDO.winter_fec +  MassAutumn_tm1 +  (1|ID) + (1|yr), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 
mod.l$winter_int <- glmer(raw_repro ~ -1 + ageClass/PDOSOI_winter +  MassAutumn_tm1 +  (1|ID) + (1|yr), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 
mod.l$springPDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDO.spring_fec+ageClass/(PDOSOI_spring) +  MassAutumn_tm1 + (1|ID) + (1|yr), # here write the model
                            data=df_fec, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 
mod.l$springPDO<- glmer(raw_repro ~ -1 + ageClass/PDO.spring_fec +  MassAutumn_tm1 +  (1|ID) + (1|yr), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun = 2000000))) 
mod.l$spring_int <- glmer(raw_repro ~ -1 +  ageClass/PDOSOI_spring +  MassAutumn_tm1 +  (1|ID) + (1|yr), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer=list("bobyqa"),
                                                 optCtrl = list(maxfun = 1000000))) 

prior1 <- list(R = list(V = 1,fix= 1),
               G = list(G1 = list(V = 1,nu = 1, alpha.mu = 0, alpha.V = 1000),
                        G2 = list(V = 1,nu = 1, alpha.mu = 0, alpha.V = 1000)))
spring_int <- MCMCglmm(as.factor(raw_repro)~ -1 + ageClass/PDOSOI_spring +  MassAutumn_tm1 + as.factor(pred_tm1) + 
                         as.factor(true_repro_tm1), 
                       random = ~ID+yr,  
                       family="categorical", 
                       prior = prior1,
                       nitt=2500000, thin=2500,burnin=100000, verbose=T,
                       data = na.omit(df_fec))
plot(spring_int$Sol)

# trying all Fit
mod.l$spring_int <- glmer(raw_repro ~ -1 +  ageClass/PDOSOI_spring +  MassAutumn_tm1 +  (1|ID) + (1|yr), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer= "bobyqa",
                                                 optCtrl = list(maxfun = 2000000)))


source(system.file("utils", "allFit.R", package = "lme4"))
#install.packages("dfoptim")
spring_int_all<- allFit(mod.l$spring_int)
ss <- summary(spring_int_all)
ss$fixef
ss$which.OK

# run true reproduction models  --------------------------------------
mod.l <- list()
mod.l$base <- glmer(true_repro ~ -1 + MassAutumn_tm1 + pred_tm1 +true_repro_tm1 + (1|ID) + (1|yr), # here write the model
                    data=df_fec, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 1000000))) 
mod.l$summerPDOSOI <- glmer(true_repro ~ -1 + ageClass/PDO.summer_fec+ageClass/(PDOSOI_summer) +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 +(1|ID) + (1|yr), # here write the model
                            data=df_fec, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000))) 
mod.l$summerPDO <- glmer(true_repro ~ -1 + ageClass/PDO.summer_fec +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 +(1|ID) + (1|yr), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 
mod.l$summer_int <- glmer(true_repro ~ -1 + ageClass/PDOSOI_summer +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 +(1|ID) + (1|yr), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 1000000))) 
mod.l$fallPDOSOI <- glmer(true_repro ~ -1 + ageClass/PDO.fall_fec+ageClass/(PDOSOI_fall) +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 +(1|ID) + (1|yr), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 1000000))) 
mod.l$fallPDO <- glmer(true_repro ~ -1 + ageClass/PDO.fall_fec +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 + (1|ID) + (1|yr), # here write the model
                       data=df_fec, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 1000000))) 
mod.l$fall_int <- glmer(true_repro ~ -1 + ageClass/PDOSOI_fall +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 + (1|ID) + (1|yr), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

mod.l$winterPDOSOI <- glmer(true_repro ~ -1 + ageClass/PDO.winter_fec+ageClass/(PDOSOI_winter) +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 + (1|ID) + (1|yr), # here write the model
                            data=df_fec, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 
mod.l$winterPDO<- glmer(true_repro ~ -1 + ageClass/PDO.winter_fec +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 + (1|ID) + (1|yr), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 
mod.l$winter_int <- glmer(true_repro ~ -1 + ageClass/PDOSOI_winter +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 + (1|ID) + (1|yr), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 
mod.l$springPDOSOI <- glmer(true_repro ~ -1 + ageClass/PDO.spring_fec+ageClass/(PDOSOI_spring) +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 +(1|ID) + (1|yr), # here write the model
                            data=df_fec, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 1000000))) 
mod.l$springPDO<- glmer(true_repro ~ -1 + ageClass/PDO.spring_fec +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 + (1|ID) + (1|yr), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun = 2000000))) 
mod.l$spring_int <- glmer(true_repro ~ -1 +  ageClass/PDOSOI_spring +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 + (1|ID) + (1|yr), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))
# prior1=list(R = list(V = 1, fix = 1))
# spring_int <- MCMCglmm(as.factor(true_repro)~ -1 + ageClass/PDOSOI_spring +  MassAutumn_tm1 + as.factor(pred_tm1) + 
#                          as.factor(true_repro_tm1), 
#                              #random = ~ID, 
#                              family="categorical", 
#                              prior = prior1,
#                              nitt=2600005, thin=2500,burnin=100000, verbose=T,
#                              data = na.omit(df_fec))
# rcov=~us(trait):units ??? 
# Warning message:
# In MCMCglmm(as.factor(true_repro) ~ -1 + ageClass/PDOSOI_spring +  :
#                some fixed effects are not estimable and have been removed. Use singular.ok=TRUE to sample these effects, but use an informative prior!

#aic table
x <- aictab(mod.l)
summary(mod.l$base)
summary(mod.l$summer_int)

## exporting AIC table
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html", 
             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/RAW_fec_climate_aic_table.html") # open directly with Word
getwd()
# results 
summary(mod.l$base)
resultsTRUE_fec <- data.frame(coef(summary(mod.l$base)))
resultsTRUE_fec[, 1:4] <- round(resultsTRUE_fec[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$base), digits = 3) #
# R2m   R2c
# theoretical 0.282 0.292
# delta       0.222 0.229
getwd()
resultsTRUE_fec <- write.csv(resultsTRUE_fec, file = "graph/results_clim_TRUE_fec.csv", row.names = FALSE)


# phenoogy variables - survival ------------------------------------------------------
rm(list = ls())

# dataframe with PCA variable 
df_surv <- read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/df_surv_pca.csv", sep = ",")

# scale variables 
colnames(df_surv)
df_surv$yr<-as.factor(df_surv$yr)
df_surv$pred<-as.factor(df_surv$pred)
df_surv$alive_t1<-as.factor(df_surv$alive_t1)

df_surv$MassSpring<-as.numeric(as.character(df_surv$MassSpring))
df_surv$MassAutumn<-as.numeric(as.character(df_surv$MassAutumn))
df_surv$PC1<-as.numeric(as.character(df_surv$PC1))
df_surv$PC2<-as.numeric(as.character(df_surv$PC2))

df_surv[c("MassSpring","MassAutumn","SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR",
         "WinNDVI_surv","WinEVI_surv","WinLAI_surv","WinGPP_surv","WinSnow_surv" ,"WinPSNNET_surv", "WinFPAR_surv" )] <- 
  scale(df_surv[c("MassSpring","MassAutumn","SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET",
                 "SummerFPAR","WinNDVI_surv","WinEVI_surv","WinLAI_surv","WinGPP_surv","WinSnow_surv","WinPSNNET_surv", "WinFPAR_surv")]) 

#New column ageClass (0,1,2,37,8)

df_surv$ageClass <- ifelse(df_surv$age >= 8, 8, df_surv$age)
c37 <- c(3:7)
df_surv$ageClass <- ifelse(df_surv$age %in% c37 , 37, df_surv$ageClass)
df_surv$ageClass <- as.factor(df_surv$ageClass)

df_surv<-na.omit(df_surv)

# model pheno survival ----------------------------------------------------------
colnames(df_surv)
mod.l <- list()
mod.l$base <- glmer(alive_t1 ~ -1 +  pred + (1|ID) + (1|yr), 
                    data=df_surv, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 1000000))) 
mod.l$pc1 <- glmer(alive_t1 ~ -1 + ageClass/PC1 +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000))) 
mod.l$pc1pc2 <- glmer(alive_t1 ~ -1 + ageClass/PC1 + ageClass/PC2 +   pred+ (1|ID) + (1|yr), 
                   data=df_surv, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.l$pc2 <- glmer(alive_t1 ~ -1 + ageClass/PC2 +   pred+ (1|ID) + (1|yr), 
                   data=df_surv, 
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
results_pheno_surv<- write.csv(results.pc2, file = "graph/results_pheno_surv.csv", row.names = FALSE)

# prepare  fecundity data ---------------------------------------------------------
rm(list = ls())

# dataframe with PCA variable 
library(readr)
df_fec <- read_csv("data/df_fec_pca.csv",col_types = cols(X1 = col_skip()))
# scale variables 
colnames(df_fec)
df_fec$yr<-as.factor(df_fec$yr)
df_fec$pred<-as.factor(df_fec$pred)
df_fec$raw_repro<-as.factor(df_fec$raw_repro)
df_fec$true_repro<-as.factor(df_fec$true_repro)

df_fec$MassSpring<-as.numeric(as.character(df_fec$MassSpring))
df_fec$MassAutumn<-as.numeric(as.character(df_fec$MassAutumn))
df_fec$PC1<-as.numeric(as.character(df_fec$PC1))
df_fec$PC2<-as.numeric(as.character(df_fec$PC2))

colnames(df_fec)
df_fec[c("MassSpring", "MassAutumn","WinNDVI_fec","WinEVI_fec" ,"WinLAI_fec","WinGPP_fec","WinSnow_fec","WinPSNNET_fec","WinFPAR_fec","SummerNDVI_fec" , 
         "SummerEVI_fec","SummerLAI_fec","SummerGPP_fec","SummerSnow_fec","SummerPSNNET_fec", "SummerFPAR_fec"  )] <- 
  scale(df_fec[c("MassSpring", "MassAutumn","WinNDVI_fec","WinEVI_fec" ,"WinLAI_fec","WinGPP_fec","WinSnow_fec","WinPSNNET_fec","WinFPAR_fec","SummerNDVI_fec" , 
                 "SummerEVI_fec","SummerLAI_fec","SummerGPP_fec","SummerSnow_fec","SummerPSNNET_fec", "SummerFPAR_fec" )]) 

#New column ageClass (0,1,2,37,8)
# age class
df_fec$ageClass <- ifelse(df_fec$age >= 8, 8, df_fec$age)
c37 <- c(3:7)
df_fec$ageClass <- ifelse(df_fec$age %in% c37 , 37, df_fec$ageClass)
df_fec$ageClass <- as.factor(df_fec$ageClass)
# add time lags for control variables 
colnames(df_fec)
tmp <- df_fec[, c("yr", "ID", "MassAutumn", "pred", "true_repro")]
tmp$yr <-as.numeric(as.character(tmp$yr))

tmp$yr <- tmp$yr + 1

tmp <- tmp %>% 
  rename(MassAutumn_tm1= MassAutumn, 
         pred_tm1= pred, 
         true_repro_tm1 = true_repro)

df_fec<- merge(tmp, 
               df_fec, 
               by.x= c("yr", "ID"), 
               by.y=c('yr', "ID"), 
               all.y = T)

str(df_fec)
df_fec$yr <- as.factor(df_fec$yr)

# remove age classes 0-1-2 
df_fec <- subset(df_fec, ageClass %in% c(37,8))
df_fec$ageClass <- droplevels(df_fec$ageClass)
df_fec$pred_tm1 <- as.factor(df_fec$pred_tm1)

df_fec<-na.omit(df_fec)

# run raw fun models  -----------------------------------------------------
# colnames(df_fec)
# mod.l <- list()
# mod.l$base <- glmer(raw_repro ~ -1 +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 + (1|ID) + (1|yr), 
#                     data=df_fec, 
#                     family="binomial",
#                     control = glmerControl(optimizer="bobyqa", 
#                                            optCtrl = list(maxfun = 1000000))) 
# mod.l$pc1 <- glmer(raw_repro ~ -1 + ageClass/PC1 +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1+ (1|ID) + (1|yr), 
#                    data=df_fec, 
#                    family="binomial",
#                    control = glmerControl(optimizer="bobyqa", 
#                                           optCtrl = list(maxfun = 2000000))) 
# mod.l$pc1pc2 <- glmer(raw_repro ~ -1 + ageClass/PC1 + ageClass/PC2 +   MassAutumn_tm1 + pred_tm1 +true_repro_tm1+ (1|ID) + (1|yr), 
#                       data=df_fec, 
#                       family="binomial",
#                       control = glmerControl(optimizer="bobyqa", 
#                                              optCtrl = list(maxfun = 2000000))) 
# mod.l$pc2 <- glmer(raw_repro ~ -1 + ageClass/PC2 +   MassAutumn_tm1 + pred_tm1 +true_repro_tm1+ (1|ID) + (1|yr), 
#                    data=df_fec, 
#                    family="binomial",
#                    control = glmerControl(optimizer="bobyqa", 
#                                           optCtrl = list(maxfun = 2000000))) 
# 
# #aic table
# x <- aictab(mod.l)
# ## exporting AIC table
# aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
#                    digits = NULL, display = NULL, nice.names = TRUE,
#                    include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
# print.xtable(aictable, type="html", 
#              file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/fec_pheno_aic_table.html") # open directly with Word
# getwd()
# # results 
# summary(mod.l$pc1)
# results.pc1 <- data.frame(coef(summary(mod.l$pc1 )))
# results.pc1[, 1:4] <- round(results.pc1[, 1:4], digits = 3)
# 
# round(MuMIn::r.squaredGLMM(mod.l$pc1), digits = 3) #
# # R2m   R2c
# # theoretical 0.264 0.363
# # delta       0.232 0.319
# getwd()
# results_pheno_fec<- write.csv(results.pc1, file = "graph/results_pheno_fec.csv", row.names = FALSE)


# true reproduction -------------------------------------------------------
colnames(df_fec)
mod.l <- list()
mod.l$base <- glmer(true_repro ~ -1 +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1 + (1|ID) + (1|yr), 
                    data=df_fec, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 1000000))) 
mod.l$pc1 <- glmer(true_repro ~ -1 + ageClass/PC1 +  MassAutumn_tm1 + pred_tm1 +true_repro_tm1+ (1|ID) + (1|yr), 
                   data=df_fec, 
                   family="binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000))) 
mod.l$pc1pc2 <- glmer(true_repro ~ -1 + ageClass/PC1 + ageClass/PC2 +   MassAutumn_tm1 + pred_tm1 +true_repro_tm1+ (1|ID) + (1|yr), 
                      data=df_fec, 
                      family="binomial",
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000))) 
mod.l$pc2 <- glmer(true_repro ~ -1 + ageClass/PC2 +   MassAutumn_tm1 + pred_tm1 +true_repro_tm1+ (1|ID) + (1|yr), 
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
             file="/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/fec_pheno_aic_table.html") # open directly with Word
getwd()
# results 
summary(mod.l$pc2)
results.pc2 <- data.frame(coef(summary(mod.l$pc2 )))
results.pc2[, 1:4] <- round(results.pc2[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$pc2), digits = 3) #
# R2m   R2c
# theoretical 0.359 0.359
# delta       0.274 0.274
getwd()
results_pheno_fec<- write.csv(results.pc2, file = "graph/results_pheno_fec.csv", row.names = FALSE)

