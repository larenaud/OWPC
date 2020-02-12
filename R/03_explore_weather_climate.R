library(plyr)
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(cowplot)
library(googledrive)
library(xtable)
library(AICcmodavg)
library(readxl)

rm(list = ls())
# merging to existing dataframe # would be nice to have a RData and not tons of .csv
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data")

sheep_data <- read_excel("sheep_data.xlsx")
clim = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/season_climate_ram.csv",
                  na.string = c("", "NA"),sep = ",")

# add time lag # careful this is tricky
clim$yr <- as.numeric(as.character(clim$yr))
colnames(clim)

# PDO/ENSO for for survival to t1
tmp <- clim[, c("yr","PDO.winter", "PDO.spring", "SOI.winter","SOI.spring")]

tmp$yr <- tmp$yr - 1
dim(tmp)
tmp <- tmp %>% 
  rename(PDO.winter_surv = PDO.winter,
         PDO.spring_surv =PDO.spring, 
         SOI.winter_surv =SOI.winter,
         SOI.spring_surv =SOI.spring)
head(tmp)

colnames(clim)
clim_surv <- merge(clim[, c("yr","PDO.summer", "PDO.fall", "SOI.summer","SOI.fall")],
              tmp,
              by.x = c("yr"), 
              by.y = c("yr"))
clim_surv <- clim_surv %>% 
  rename(PDO.summer_surv = PDO.summer,
         PDO.fall_surv=PDO.fall,
         SOI.summer_surv = SOI.summer,
         SOI.fall_surv = SOI.fall)
getwd()

# merge dataframes 
colnames(clim_surv)
colnames(sheep_data)
df_surv= merge(sheep_data[c("yr","ID", "alive_t1", "MassSpring","MassAutumn","age","pred", "first_yr_trans")],
          clim_surv,
          by.x = "yr", 
          by.y =  "yr", 
          all.x=T) # keep all years even if NA
#write.csv(df_surv, "surv_climate_data.csv", row.names = FALSE)
#drive_upload("surv_climate_data.csv", path = "OWPC/Analyses/data/surv_climate_data.csv", overwrite = T)


# create fecundity data ---------------------------------------------------
sheep_data <- read_excel("sheep_data.xlsx")
clim = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/season_climate_ram.csv",
                 na.string = c("", "NA"),sep = ",")

# add time lag # careful this is tricky
clim$yr <- as.numeric(as.character(clim$yr))

# PDO/ENSO for for survival to t1
colnames(clim)
tmp <- clim[, c("yr", "PDO.summer", "PDO.fall", "SOI.summer" ,"SOI.fall")]

#now add backward time lag for fall and summer seasons 
tmp$yr <- tmp$yr+1
dim(tmp)
tmp <- tmp %>% 
  rename(PDO.summer_fec= PDO.summer,
         PDO.fall_fec=PDO.fall, 
         SOI.summer_fec =SOI.summer,
         SOI.fall_fec =SOI.fall)
head(tmp)

colnames(clim)
clim_fec <- merge(clim,
                   tmp,
                   by.x = c("yr"), 
                   by.y = c("yr"))

clim_fec <- clim_fec %>% 
  rename(PDO.winter_fec = PDO.winter,
         PDO.spring_fec =PDO.spring, 
         SOI.winter_fec =SOI.winter,
         SOI.spring_fec =SOI.spring)
getwd()

colnames(clim_fec)
clim_fec <- clim_fec[, c("yr", "PDO.winter_fec", "PDO.spring_fec", "SOI.winter_fec", "SOI.spring_fec",
                         "PDO.summer_fec", "PDO.fall_fec",   "SOI.summer_fec", "SOI.fall_fec")]
# merge dataframes 
colnames(clim_fec)
colnames(sheep_data)
df_fec= merge(sheep_data[c("yr","ID", "raw_repro", "true_repro", "MassSpring","MassAutumn","age","pred", "first_yr_trans")],
               clim_fec,
               by.x = "yr", 
               by.y =  "yr", 
               all.x=T) # keep all years even if NA

#write.csv(df_fec, "fecun_climate_data.csv", row.names = FALSE)
#drive_upload("fecun_climate_data.csv", path = "OWPC/Analyses/data/fecun_climate_data.csv", overwrite = T)

# survival : selection of relevant variables  ----------------------------------------

# check variable distribution
# tidy variables 
rm(clim, repro, tmp)

df_surv$yr<-as.factor(df_surv$yr)
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

df_surv$PDO.winter_surv<-as.numeric(as.character(df_surv$PDO.winter_surv))
df_surv$PDO.summer_surv<-as.numeric(as.character(df_surv$PDO.summer_surv))
df_surv$PDO.spring_surv<-as.numeric(as.character(df_surv$PDO.spring_surv))
df_surv$PDO.fall_surv<-as.numeric(as.character(df_surv$PDO.fall_surv))
df_surv$SOI.winter_surv<-as.numeric(as.character(df_surv$SOI.winter_surv))
df_surv$SOI.summer_surv<-as.numeric(as.character(df_surv$SOI.summer_surv))
df_surv$SOI.spring_surv<-as.numeric(as.character(df_surv$SOI.spring_surv))
df_surv$SOI.fall_surv<-as.numeric(as.character(df_surv$SOI.fall_surv))

df_surv$alive_t1<-as.factor(df_surv$alive_t1)

# scale
df_surv[c(4,5, 9:16)] <- scale(df_surv[c(4,5, 9:16)])# CHANGE COLUMN NUMBER IF MODIFY df_surv!! 

# run models
df_surv$alive_t1<-as.factor(df_surv$alive_t1)
colnames(df_surv)

ml=which(colnames(df_surv) %in% 
           c("PDO.summer_surv", "PDO.fall_surv","SOI.summer_surv", "SOI.fall_surv",
             "PDO.winter_surv","PDO.spring_surv", "SOI.winter_surv" ,"SOI.spring_surv"))

res2 <- ldply(ml,function(i){ # prepare dataframe of results
  
  # autumn mass and age are correlated 
  mod1 <- glmer(alive_t1 ~ df_surv[,i] + MassSpring + age + pred+ (1|ID) + (1|yr), # here write the model
               data=df_surv, 
               family="binomial",
               control = glmerControl(optimizer="bobyqa", 
                                     optCtrl = list(maxfun = 100000))) 
  #sjt.glmer(mod1, file="sjt_linear.doc")
  var=as.data.frame(VarCorr(mod1))[,4] # to get the variance, not SD
  r1=data.frame(colnames(df_surv)[i],
                aic = AICc(mod1), # or AIC(logLik(mod2)) ?? 
                V.id=var[1],
                V.yr=var[2],
                dev = mod1@devcomp[["cmp"]][["dev"]],
                R2c=r.squaredGLMM(mod1)[1,2], # conditional theoretical
                R2m=r.squaredGLMM(mod1)[1,1], # marginal theoretical # TO CHECK WHAT THESE 2 MEANS
                estimate= coef(summary(mod1))[2,1], # pheno is the 2rd term in model output
                SE= coef(summary(mod1))[2,2], 
                p_value= coef(summary(mod1))[2,4])
  
  return(r1)
})

# export a nice table
results_surv <- xtable(res2)
results_surv[, 2:10] <- round(results_surv[, 2:10], digits = 3)
results_surv$aic <-sort(results_surv$aic, decreasing = F)

getwd()
print.xtable(results_surv,type="html",
             file= "/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/climate_surv_model_outputs.html") # 

# double check one model 

mod1 <- glmer(alive_t1 ~ PDO.fall_surv + MassSpring + age + pred+ (1|ID) + (1|yr), # here write the model
              data=df_surv, 
              family="binomial",
              control = glmerControl(optimizer="bobyqa", 
                                     optCtrl = list(maxfun = 100000))) 
coef(summary(mod1))
r.squaredGLMM(mod1)[1,2]
r.squaredGLMM(mod1)

# end verification 

# inv logit results since it is a log link
# check out how to get deviance - what's important to report for glmer 
# is AIC appropriate for glmer ? is it ML or REML? 


# fecundity ---------------------------------------------------------------
df_fec$yr<-as.factor(df_fec$yr)
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

df_fec$PDO.winter_fec<-as.numeric(as.character(df_fec$PDO.winter_fec))
df_fec$PDO.summer_fec<-as.numeric(as.character(df_fec$PDO.summer_fec))
df_fec$PDO.spring_fec<-as.numeric(as.character(df_fec$PDO.spring_fec))
df_fec$PDO.fall_fec<-as.numeric(as.character(df_fec$PDO.fall_fec))
df_fec$SOI.winter_fec<-as.numeric(as.character(df_fec$SOI.winter_fec))
df_fec$SOI.summer_fec<-as.numeric(as.character(df_fec$SOI.summer_fec))
df_fec$SOI.spring_fec<-as.numeric(as.character(df_fec$SOI.spring_fec))
df_fec$SOI.fall_fec<-as.numeric(as.character(df_fec$SOI.fall_fec))

df_fec$raw_repro<-as.factor(df_fec$raw_repro)
df_fec$true_repro<-as.factor(df_fec$true_repro)

df_fec<- df_fec[!is.na(df_fec$PDO.winter_fec),]
df_fec<- df_fec[!is.na(df_fec$MassSpring),]
df_fec<- df_fec[!is.na(df_fec$MassAutumn),]

# scale
df_fec[c(5,6, 10:17)] <- scale(df_fec[c(5,6, 10:17)])# CHANGE COLUMN NUMBER IF MODIFY df_fec!! 
# run models and add time lags but keep current spring 

colnames(df_fec)
ml=which(colnames(df_fec) %in% 
           c("PDO.winter_fec", "PDO.spring_fec", "PDO.summer_fec", "PDO.fall_fec",
             "SOI.winter_fec","SOI.spring_fec","SOI.summer_fec", "SOI.fall_fec"))

res2 <- ldply(ml,function(i){ # prepare dataframe of results
  
  # autumn mass and age are correlated 
  mod1 <- glmer(raw_repro ~ df_fec[,i] + MassSpring + age + pred+ (1|ID) + (1|yr), # here write the model
                data=df_fec, 
                family="binomial",
                control = glmerControl(optimizer="bobyqa", 
                                       optCtrl = list(maxfun = 1000000))) 
  #sjt.glmer(mod1, file="sjt_linear.doc")
  var=as.data.frame(VarCorr(mod1))[,4] # to get the variance, not SD
  r1=data.frame(colnames(df_fec)[i],
                aic = AICc(mod1), # or AIC(logLik(mod2)) ?? 
                V.id=var[1],
                V.yr=var[2],
                dev = mod1@devcomp[["cmp"]][["dev"]],
                R2c=r.squaredGLMM(mod1)[1,2], # conditional theoretical
                R2m=r.squaredGLMM(mod1)[1,1], # marginal theoretical # TO CHECK WHAT THESE 2 MEANS
                estimate= coef(summary(mod1))[2,1], # pheno is the 2rd term in model output
                SE= coef(summary(mod1))[2,2], 
                p_value= coef(summary(mod1))[2,4])
  
  return(r1)
})

# export a nice table
results_fec <- xtable(res2)
results_fec[, 2:10] <- round(results_fec[, 2:10], digits = 3)
results_fec$aic <-sort(results_fec$aic, decreasing = F)

getwd()
print.xtable(results_fec,type="html",
             file= "/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/climate_fec_model_outputs.html") # 

# true reproduction -------------------------------------------------------

df_fec$true_repro<-as.factor(df_fec$true_repro)
# run models
colnames(df_fec)

ml=which(colnames(df_fec) %in% 
           c("PDO.winter_fec", "PDO.spring_fec", "SOI.winter_fec", "SOI.spring_fec",
             "PDO.summer_fec", "PDO.fall_fec" ,  "SOI.summer_fec", "SOI.fall_fec"))

res2 <- ldply(ml,function(i){ # prepare dataframe of results
  
  # autumn mass and age are correlated 
  mod1 <- glmer(true_repro ~ df_fec[,i] + MassSpring + age + pred+ (1|ID) + (1|yr), # here write the model
                data=df_fec, 
                family="binomial",
                control = glmerControl(optimizer="bobyqa", 
                                       optCtrl = list(maxfun = 100000))) 
  #sjt.glmer(mod1, file="sjt_linear.doc")
  var=as.data.frame(VarCorr(mod1))[,4] # to get the variance, not SD
  r1=data.frame(colnames(df_fec)[i],
                aic = AICc(mod1), # or AIC(logLik(mod2)) ?? 
                V.id=var[1],
                V.yr=var[2],
                dev = mod1@devcomp[["cmp"]][["dev"]],
                R2c=r.squaredGLMM(mod1)[1,2], # conditional theoretical
                R2m=r.squaredGLMM(mod1)[1,1], # marginal theoretical # TO CHECK WHAT THESE 2 MEANS
                estimate= coef(summary(mod1))[2,1], # pheno is the 2rd term in model output
                SE= coef(summary(mod1))[2,2], 
                p_value= coef(summary(mod1))[2,4])
  
  return(r1)
})

# export a nice table
results_true_rep <- xtable(res2)
results_true_rep[, 2:10] <- round(results_true_rep[, 2:10], digits = 3)
results_true_rep$aic <-sort(results_true_rep$aic, decreasing = F)

getwd()
print.xtable(results_fec,type="html",
             file= "/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/climate_true_repro_model_outputs.html") # 

