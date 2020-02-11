library(plyr)
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(cowplot)
library(googledrive)
library(xtable)
library(AICcmodavg)

setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data")
#drive_download("~/OWPC/Analyses/data/repro_mass.csv")

repro = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/repro_mass.csv",
                  na.string = c("", "NA"),sep = ",")
#drive_download("~/OWPC/Analyses/data/pheno_surv2.csv")
pheno = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/pheno_surv2.csv",
                  na.string = c("", "NA"),sep = ",")
# first part to this df is not good anymore - select needed only
colnames(pheno)

# merge dataframes # no need to have duplicated data in pheno (inflated...)
df= merge(unique(pheno[, c("yr","SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET",
                           "SummerFPAR","WinNDVI","WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET","WinFPAR")]),
          repro,
          by.x = "yr", 
          by.y =  "yr")
# selection of relevant variables  ----------------------------------------
# set where you want results to be 
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph")

# try individual model
df$yr<-as.factor(df$yr)
df$MassSpring<-as.numeric(df$MassSpring)
df$MassAutumn<-as.numeric(df$MassAutumn)

# scale
df[c(2:15, 17:18)] <- scale(df[c(2:15, 17:18)])# CHANGE COLUMN NUMBER IF MODIFY DF!! 

# run models
colnames(df)

ml=which(colnames(df) %in% 
           c("SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","Sum,merPSNNET", "SummerFPAR",
             "WinNDVI","WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET","WinFPAR"))

res2 <- ldply(ml,function(i){ # prepare dataframe of results
  
  # autumn mass and age are correlated 
  mod1 <- glmer(alive_t1 ~ df[,i] + MassSpring + age + pred + (1|ID) + (1|yr), # here write the model
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
             file= "/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/pheno_model_outputs.html") # 

# inv logit results since it is a log link
# check out how to get deviance - what's important to report for glmer 
# is AIC appropriate for glmer ? is it ML or REML? 

