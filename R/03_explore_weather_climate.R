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


#survival: selection of relevant climate variables  ----------------------------------------
# get data
df_surv = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/surv_climate_data.csv",
                    na.string = c("", "NA"),sep = ";")
# scale

df_surv$MassSpring<-as.numeric(as.character(df_surv$MassSpring))
df_surv$MassAutumn<-as.numeric(as.character(df_surv$MassAutumn))

df_surv[c(4,5, 9:16)] <- scale(df_surv[c(4,5, 9:16)])# CHANGE COLUMN NUMBER IF MODIFY df_surv!! 

# run models
colnames(df_surv)
ml=which(colnames(df_surv) %in% 
           c( "PDO.summer_surv", "PDO.fall_surv",   "SOI.summer_surv", "SOI.fall_surv",  
              "PDO.winter_surv" ,"PDO.spring_surv", "SOI.winter_surv" ,"SOI.spring_surv"))

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


# fecundity: selection of relevant climate variables ---------------------------------------------------------------
df_fec = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/fecun_climate_data.csv",
                    na.string = c("", "NA"),sep = ";")
# scale
colnames(df_fec)
df_fec[c(5,6, 10:17)] <- scale(df_fec[c(5,6, 10:17)])# CHANGE COLUMN NUMBER IF MODIFY df_fec!! 
# run models and add time lags but keep current spring 

colnames(df_fec)

ml=which(colnames(df_fec) %in% 
           c("PDO.winter_fec", "PDO.spring_fec", "SOI.winter_fec", "SOI.spring_fec",
             "PDO.summer_fec", "PDO.fall_fec" ,  "SOI.summer_fec", "SOI.fall_fec"))

res2 <- ldply(ml,function(i){ # prepare dataframe of results
  
  # autumn mass and age are correlated 
  mod1 <- glmer(raw_repro ~ df_fec[,i] + MassSpring + age + pred+ (1|ID) + (1|yr), # here write the model
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
results_fec <- xtable(res2)
results_fec[, 2:10] <- round(results_fec[, 2:10], digits = 3)
results_fec$aic <-sort(results_fec$aic, decreasing = F)

getwd()
print.xtable(results_fec,type="html",
             file= "/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/climate_fec_model_outputs.html") # 

# true reproduction -------------------------------------------------------

# check if all variables are in the right category
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

