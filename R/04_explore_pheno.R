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
rm(list = ls())
# survival dataframe ------------------------------------------------------
sheep_data <- read_excel("sheep_data.xlsx")
pheno = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/pheno_surv2.csv",
                  na.string = c("", "NA"),sep = ",")


# first part to this df is not good anymore - select needed only
colnames(pheno)
pheno <- unique(pheno[, c("yr", "SummerNDVI","SummerEVI","SummerLAI",
                        "SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR","WinNDVI",
                        "WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET",
                        "WinFPAR")])
    
# survival - add time lag for winter 
pheno$yr <- as.numeric(as.character(pheno$yr))
colnames(pheno)

colnames(pheno) # here only winter season is a problem - summer is on same year than surv
tmp1 <- unique(pheno[, c("yr", "WinNDVI","WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET","WinFPAR")])
tmp1$yr <- tmp1$yr - 1 # on leur met l'année de la survie 
dim(tmp1)
tmp1 <- tmp1 %>% 
  rename(WinNDVI_surv = WinNDVI,
         WinEVI_surv =WinEVI, 
         WinLAI_surv =WinLAI,
         WinGPP_surv =WinGPP,
         WinSnow_surv =WinSnow,
         WinPSNNET_surv =WinPSNNET,
         WinFPAR_surv =WinFPAR)
head(tmp1)

colnames(pheno)
pheno_surv <- merge(pheno[, c("yr","SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET", "SummerFPAR")],
                   tmp1,
                   by.x = c("yr"), 
                   by.y = c("yr"), 
                   all.x= T)
# merge dataframes 
colnames(pheno_surv)
colnames(sheep_data)
df_pheno_surv= merge(sheep_data[c("yr","ID", "alive_t1", "MassSpring","MassAutumn","age","pred", "first_yr_trans")],
               pheno_surv,
               by.x = "yr", 
               by.y =  "yr", 
               all.x=T) # keep all years even if NA
#write.csv(df_pheno_surv, "surv_pheno_data.csv", row.names = FALSE)
#drive_upload("surv_pheno_data.csv", path = "OWPC/Analyses/data/surv_pheno_data.csv", overwrite = T)

# fecundity dataframe -----------------------------------------------------
sheep_data <- read_excel("sheep_data.xlsx")
pheno = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/pheno_surv2.csv",
                  na.string = c("", "NA"),sep = ",")

# select needed only
colnames(pheno)
pheno <- unique(pheno[, c("yr", "SummerNDVI","SummerEVI","SummerLAI",
                          "SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR","WinNDVI",
                          "WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET",
                          "WinFPAR")])
pheno$yr <- as.numeric(as.character(pheno$yr))
colnames(pheno)

tmp1 <- unique(pheno[, c("yr", "SummerNDVI","SummerEVI","SummerLAI",
                 "SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR")])

# add time lag for summer lengths (real time lag t-1)
tmp1$yr <- tmp1$yr + 1
tmp1 <- tmp1 %>% 
  rename(SummerNDVI_fec= SummerNDVI,
         SummerEVI_fec=SummerEVI, 
         SummerLAI_fec=SummerLAI,
         SummerGPP_fec=SummerGPP,
         SummerSnow_fec =SummerSnow,
         SummerPSNNET_fec =SummerPSNNET,
         SummerFPAR_fec =SummerFPAR)

pheno_fec <- merge(unique(pheno[, c("yr","WinNDVI","WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET","WinFPAR")]), # no need for all duplicated data per ID
               tmp1,
               by.x = c("yr"), 
               by.y = c("yr"), 
               all.x = T)

pheno_fec<- pheno_fec%>%
  rename(WinNDVI_fec = WinNDVI,
         WinEVI_fec =WinEVI ,
         WinLAI_fec = WinLAI ,
         WinGPP_fec = WinGPP,
         WinSnow_fec = WinSnow,
         WinPSNNET_fec = WinPSNNET,
         WinFPAR_fec = WinFPAR)

# merge dataframes 
df= merge(sheep_data[c("yr","ID", "raw_repro", "true_repro", "MassSpring","MassAutumn","age","pred", "first_yr_trans")],
          pheno_fec,
          by.x = "yr", 
          by.y =  "yr", 
          all.x=T)

#write.csv(df, "fecun_pheno_data.csv", row.names = FALSE)
#drive_upload("fecun_pheno_data.csv", path = "OWPC/Analyses/data/fecun_pheno_data.csv", overwrite = T)

# survival : selection of relevant variables  ----------------------------------------
# set where you want results to be 
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph")

# try individual model
rm(list = ls())
df_pheno_surv = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/surv_pheno_data.csv",
                  na.string = c("", "NA"),sep = ",")

df_pheno_surv$yr<-as.factor(df_pheno_surv$yr)
df_pheno_surv$MassSpring<-as.numeric(df_pheno_surv$MassSpring)
df_pheno_surv$MassAutumn<-as.numeric(df_pheno_surv$MassAutumn)

df_pheno_surv$alive_t1<-as.factor(df_pheno_surv$alive_t1)
# df_pheno_surv$raw_repro<-as.factor(df_pheno_surv$raw_repro)
# df_pheno_surv$true_repro<-as.factor(df_pheno_surv$true_repro)

# scale
colnames(df_pheno_surv)
df_pheno_surv[c(4,5,9:22)] <- scale(df_pheno_surv[c(4,5, 9:22)])# CHANGE COLUMN NUMBER IF MODIFY df_pheno_surv!! 

# remove NAs
df_pheno_surv <- df_pheno_surv[!is.na(df_pheno_surv$MassSpring),]
df_pheno_surv <- df_pheno_surv[!is.na(df_pheno_surv$SummerNDVI),] # n =  
df_pheno_surv <- df_pheno_surv[!is.na(df_pheno_surv$WinGPP_surv),] # n = 
df_pheno_surv <- df_pheno_surv[!is.na(df_pheno_surv$MassAutumn),] # n = 460

# run models
colnames(df_pheno_surv)
ml=which(colnames(df_pheno_surv) %in% 
           c("SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET",
            "SummerFPAR","WinNDVI_surv","WinEVI_surv","WinLAI_surv", "WinGPP_surv","WinSnow_surv",
            "WinPSNNET_surv", "WinFPAR_surv"))

res2 <- ldply(ml,function(i){ # prepare dataframe of results
  
  # autumn mass and age are correlated 
  mod1 <- glmer(alive_t1 ~ df_pheno_surv[,i] + MassSpring + age + pred + (1|ID) + (1|yr), # here write the model
                data=df_pheno_surv, 
                family="binomial",
                control = glmerControl(optimizer="bobyqa", 
                                       optCtrl = list(maxfun = 100000))) 
  #sjt.glmer(mod1, file="sjt_linear.doc")
  var=as.data.frame(VarCorr(mod1))[,4] # to get the variance, not SD
  r1=data.frame(colnames(df_pheno_surv)[i],
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
             file= "/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/pheno_surv_model_outputs.html") # 

# inv logit results since it is a log link
# check out how to get deviance - what's important to report for glmer 
# is AIC appropriate for glmer ? is it ML or REML? 


# fecundity variable selection ---------------------------------------------------------
rm(list = ls())
df_pheno_fec = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/fecun_pheno_data.csv",
                          na.string = c("", "NA"),sep = ",")

df_pheno_fec$yr<-as.factor(df_pheno_fec$yr)
df_pheno_fec$MassSpring<-as.numeric(df_pheno_fec$MassSpring)
df_pheno_fec$MassAutumn<-as.numeric(df_pheno_fec$MassAutumn)

#df_pheno_fec$alive_t1<-as.factor(df_pheno_fec$alive_t1)
df_pheno_fec$raw_repro<-as.factor(df_pheno_fec$raw_repro)
df_pheno_fec$true_repro<-as.factor(df_pheno_fec$true_repro)

# scale
colnames(df_pheno_fec)
df_pheno_fec[c(5,6,10:23)] <- scale(df_pheno_fec[c(5,6, 10:23)])# CHANGE COLUMN NUMBER IF MODIFY df_pheno_fec!! 

# remove NAs
df_pheno_fec <- df_pheno_fec[!is.na(df_pheno_fec$MassSpring),]
df_pheno_fec <- df_pheno_fec[!is.na(df_pheno_fec$SummerNDVI_fec),] # n =  
df_pheno_fec <- df_pheno_fec[!is.na(df_pheno_fec$WinGPP_fec),] # n = 
df_pheno_fec <- df_pheno_fec[!is.na(df_pheno_fec$MassAutumn),] # n = 461

# set your data output
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph")

# run models
colnames(df_pheno_fec)
ml=which(colnames(df_pheno_fec) %in% 
           c("WinNDVI_fec","WinEVI_fec","WinLAI_fec","WinGPP_fec","WinSnow_fec","WinPSNNET_fec",
             "WinFPAR_fec","SummerNDVI_fec","SummerEVI_fec","SummerLAI_fec","SummerGPP_fec",
             "SummerSnow_fec","SummerPSNNET_fec", "SummerFPAR_fec"))

res2 <- ldply(ml,function(i){ # prepare dataframe of results
  
  # autumn mass and age are correlated 
  mod1 <- glmer(raw_repro ~ df_pheno_fec[,i] + MassSpring + age + pred + (1|ID) + (1|yr), # here write the model
                data=df_pheno_fec, 
                family="binomial",
                control = glmerControl(optimizer="bobyqa", 
                                       optCtrl = list(maxfun = 100000))) 
  #sjt.glmer(mod1, file="sjt_linear.doc")
  var=as.data.frame(VarCorr(mod1))[,4] # to get the variance, not SD
  r1=data.frame(colnames(df_pheno_fec)[i],
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
results_raw_fec <- xtable(res2)
results_raw_fec[, 2:10] <- round(results_raw_fec[, 2:10], digits = 3)
results_raw_fec$aic <-sort(results_raw_fec$aic, decreasing = F)

getwd()
print.xtable(results_raw_fec,type="html",
             file= "/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/pheno_raw_fec_model_outputs.html") # 

# inv logit results since it is a log link
# check out how to get deviance - what's important to report for glmer 
# is AIC appropriate for glmer ? is it ML or REML? 


# true reproction ---------------------------------------------------------
rm(list = ls())
df_pheno_fec = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/fecun_pheno_data.csv",
                         na.string = c("", "NA"),sep = ",")

df_pheno_fec$yr<-as.factor(df_pheno_fec$yr)
df_pheno_fec$MassSpring<-as.numeric(df_pheno_fec$MassSpring)
df_pheno_fec$MassAutumn<-as.numeric(df_pheno_fec$MassAutumn)

#df_pheno_fec$alive_t1<-as.factor(df_pheno_fec$alive_t1)
df_pheno_fec$true_repro<-as.factor(df_pheno_fec$true_repro)
df_pheno_fec$true_repro<-as.factor(df_pheno_fec$true_repro)

# scale
colnames(df_pheno_fec)
df_pheno_fec[c(5,6,10:23)] <- scale(df_pheno_fec[c(5,6, 10:23)])# CHANGE COLUMN NUMBER IF MODIFY df_pheno_fec!! 

# remove NAs
df_pheno_fec <- df_pheno_fec[!is.na(df_pheno_fec$MassSpring),]
df_pheno_fec <- df_pheno_fec[!is.na(df_pheno_fec$SummerNDVI_fec),] # n =  
df_pheno_fec <- df_pheno_fec[!is.na(df_pheno_fec$WinGPP_fec),] # n = 
df_pheno_fec <- df_pheno_fec[!is.na(df_pheno_fec$MassAutumn),] # n = 461

# set your data output
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph")

# run models
colnames(df_pheno_fec)
ml=which(colnames(df_pheno_fec) %in% 
           c("WinNDVI_fec","WinEVI_fec","WinLAI_fec","WinGPP_fec","WinSnow_fec","WinPSNNET_fec",
             "WinFPAR_fec","SummerNDVI_fec","SummerEVI_fec","SummerLAI_fec","SummerGPP_fec",
             "SummerSnow_fec","SummerPSNNET_fec", "SummerFPAR_fec"))

res2 <- ldply(ml,function(i){ # prepare dataframe of results
  
  # autumn mass and age are correlated 
  mod1 <- glmer(true_repro ~ df_pheno_fec[,i] + MassSpring + age + pred + (1|ID) + (1|yr), # here write the model
                data=df_pheno_fec, 
                family="binomial",
                control = glmerControl(optimizer="bobyqa", 
                                       optCtrl = list(maxfun = 100000))) 
  #sjt.glmer(mod1, file="sjt_linear.doc")
  var=as.data.frame(VarCorr(mod1))[,4] # to get the variance, not SD
  r1=data.frame(colnames(df_pheno_fec)[i],
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
results_true_fec <- xtable(res2)
results_true_fec[, 2:10] <- round(results_true_fec[, 2:10], digits = 3)
results_true_fec$aic <-sort(results_true_fec$aic, decreasing = F)

getwd()
print.xtable(results_true_fec,type="html",
             file= "/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/graph/pheno_true_fec_model_outputs.html") # 

# inv logit results since it is a log link
# check out how to get deviance - what's important to report for glmer 
# is AIC appropriate for glmer ? is it ML or REML? 
