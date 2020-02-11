library(plyr)
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(cowplot)
library(googledrive)
library(xtable)
library(AICcmodavg)

rm(list = ls())
# merging to existing dataframe # would be nice to have a RData and not tons of .csv
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data")

#drive_download("~/OWPC/Analyses/data/repro_mass.csv")
repro = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/repro_mass.csv",
                       na.string = c("", "NA"),sep = ",")

#get climate date 
#drive_download("~/OWPC/Analyses/data/Climat/season_climate_ram")
# add .csv
clim = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/season_climate_ram.csv",
                  na.string = c("", "NA"),sep = ",")

# merge dataframes 
colnames(clim)
df= merge(unique(clim[, c("yr","PDO.winter", "PDO.spring", "PDO.summer", "PDO.fall","SOI.winter", "SOI.spring",
                          "SOI.summer","SOI.fall")]),
           repro,
           by.x = "yr", 
           by.y =  "yr")

# selection of relevant variables  ----------------------------------------

# try individual model
df$yr<-as.factor(df$yr)
df$MassSpring<-as.numeric(as.character(df$MassSpring))
df$MassAutumn<-as.numeric(as.character(df$MassAutumn))
df$PDO.winter<-as.numeric(as.character(df$PDO.winter))
df$PDO.summer<-as.numeric(as.character(df$PDO.summer))
df$PDO.spring<-as.numeric(as.character(df$PDO.spring))
df$PDO.fall<-as.numeric(as.character(df$PDO.fall))
df$SOI.winter<-as.numeric(as.character(df$SOI.winter))
df$SOI.summer<-as.numeric(as.character(df$SOI.summer))
df$SOI.spring<-as.numeric(as.character(df$SOI.spring))
df$SOI.fall<-as.numeric(as.character(df$SOI.fall))


# scale
df[c(2:9, 11:12)] <- scale(df[c(2:9, 11:12)])# CHANGE COLUMN NUMBER IF MODIFY DF!! 

# run models
colnames(df)

ml=which(colnames(df) %in% 
           c("PDO.winter", "PDO.spring", "PDO.summer", "PDO.fall","SOI.winter", "SOI.spring", "SOI.summer",
             "SOI.fall"))

res2 <- ldply(ml,function(i){ # prepare dataframe of results
  
  # autumn mass and age are correlated 
  mod1 <- glmer(alive_t1 ~ df[,i] + MassSpring + age + pred+ (1|ID) + (1|yr), # here write the model
               data=df, 
               family="binomial",
               control = glmerControl(optimizer="bobyqa", 
                                     optCtrl = list(maxfun = 100000))) 
  #sjt.glmer(mod1, file="sjt_linear.doc")
  var=as.data.frame(VarCorr(mod1))[,4] # to get the variance, not SD
  r1=data.frame(colnames(df)[i],
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
             file= "/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/climate_model_outputs.html") # 

# double check one model 

mod1 <- glmer(alive_t1 ~ PDO.fall + MassSpring + age + pred+ (1|ID) + (1|yr), # here write the model
              data=df, 
              family="binomial",
              control = glmerControl(optimizer="bobyqa", 
                                     optCtrl = list(maxfun = 100000))) 
coef(summary(mod1))

# inv logit results since it is a log link
# check out how to get deviance - what's important to report for glmer 
# is AIC appropriate for glmer ? is it ML or REML? 

