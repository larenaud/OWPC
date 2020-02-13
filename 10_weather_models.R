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

setwd("C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC")

drive_download("OWPC/Analyses/data/Raw/sheep_data", type="csv", overwrite=T)
sheep<-read.csv("sheep_data.csv", header=T, sep=",")
names(sheep)

drive_download("OWPC/Analyses/data/Raw/Climat/Localweather_seasons",type="csv", overwrite=T)
weather<-read.csv("Localweather_seasons.csv", header=T, sep=",")

# add time lags

  # No time lag for survival
  # Add Summer(t-1) and Fall(t-1) for fecundity
weather$yr <- as.numeric(as.character(weather$yr))
colnames(weather)

tmp <- weather[, c("yr","T.SUMMER", "P.SUMMER", "T.AUT","P.AUT")]

tmp$yr <- tmp$yr+1

head(tmp)

weather_fec<-weather[, c("yr","T.Win","P.Win","T.SPRING","P.SPRING")]
head(weather_fec)
weather_fec <- merge(weather_fec,
                  tmp,
                  by.x = c("yr"), 
                  by.y = c("yr"))
head(weather_fec)


  # Sheep surv data


# merge dataframes 
colnames(clim_fec)
colnames(sheep_data)
df_fec= merge(sheep,
              weather_fec,
              by.x = "yr", 
              by.y =  "yr", 
              all.x=T) # keep all years even if NA

df_surv<-merge(sheep,
               weather,
               by.x = "yr", 
               by.y =  "yr", 
               all.x=T) # keep all years even if NA


#New column ageClass (0,1,2,37,8)

df_surv$ageClass <- ifelse(df_surv$age >= 8, 8, df_surv$age)
c37 <- c(3:7)
df_surv$ageClass <- ifelse(df_surv$age %in% c37 , 37, df_surv$ageClass)

df_fec$ageClass <- ifelse(df_fec$age >= 8, 8, df_fec$age)
c37 <- c(3:7)
df_fec$ageClass <- ifelse(df_fec$age %in% c37 , 37, df_fec$ageClass)

df_fec$ageClass <- as.factor(df_fec$ageClass)
df_surv$ageClass <- as.factor(df_surv$ageClass)

# prepare surv data for models --------------------------------------------
df_surv$yr<-as.factor(df_surv$yr)
df_surv$pred<-as.factor(df_surv$pred)
df_surv$alive_t1<-as.factor(df_surv$alive_t1)

df_surv$MassSpring<-as.numeric(as.character(df_surv$MassSpring))
df_surv$MassAutumn<-as.numeric(as.character(df_surv$MassAutumn))

str(df_surv)
names(df_surv)
colnames(df_surv)

df_surv[c("MassSpring","MassAutumn","T.Win","P.Win","T.SPRING" ,"P.SPRING", "T.SUMMER","P.SUMMER",
          "T.AUT","P.AUT")] <- 
  scale(df_surv[c("MassSpring","MassAutumn","T.Win","P.Win","T.SPRING" ,"P.SPRING", "T.SUMMER","P.SUMMER",
                  "T.AUT","P.AUT")]) 

#df_surv<-na.omit(df_surv)
# 

# run survival models -----------------------------------------------------

colnames(df_surv)
mod.l <- list()
mod.l$base <- glmer(alive_t1 ~ -1 +  pred+ (1|ID) + (1|yr), 
                    data=df_surv, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 1000000))) 

# Winter
mod.l$P.T.Win <- glmer(alive_t1 ~ -1 + ageClass/P.Win+ageClass/T.Win +  pred+ (1|ID) + (1|yr), 
                            data=df_surv, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000)))


mod.l$PxT.Win <- glmer(alive_t1 ~ -1 + ageClass/(P.Win*T.Win) +  pred+ (1|ID) + (1|yr), 
                       data=df_surv, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000)))

mod.l$T.Win <- glmer(alive_t1 ~ -1 + ageClass/T.Win +  pred+ (1|ID) + (1|yr), 
                       data=df_surv, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000)))

mod.l$P.Win <- glmer(alive_t1 ~ -1 + ageClass/P.Win +  pred+ (1|ID) + (1|yr), 
                       data=df_surv, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000)))

  # Spring

mod.l$P.T.Spring <- glmer(alive_t1 ~ -1 + ageClass/P.SPRING+ageClass/T.SPRING +  pred+ (1|ID) + (1|yr), 
                       data=df_surv, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000)))

mod.l$PxT.Spring <- glmer(alive_t1 ~ -1 + ageClass/(P.SPRING*T.SPRING) +  pred+ (1|ID) + (1|yr), 
                       data=df_surv, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000)))

mod.l$T.Spring <- glmer(alive_t1 ~ -1 + ageClass/T.SPRING +  pred+ (1|ID) + (1|yr), 
                     data=df_surv, 
                     family="binomial",
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 2000000)))

mod.l$P.Spring <- glmer(alive_t1 ~ -1 + ageClass/P.SPRING +  pred+ (1|ID) + (1|yr), 
                    data=df_surv, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 2000000)))

  # Summer
mod.l$P.T.Summer <- glmer(alive_t1 ~ -1 + ageClass/P.SUMMER+ageClass/T.SUMMER +  pred+ (1|ID) + (1|yr), 
                       data=df_surv, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000)))

mod.l$PxT.Summer <- glmer(alive_t1 ~ -1 + ageClass/(P.SUMMER*T.SUMMER) +  pred+ (1|ID) + (1|yr), 
                       data=df_surv, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000)))

mod.l$T.Summer <- glmer(alive_t1 ~ -1 + ageClass/T.SUMMER +  pred+ (1|ID) + (1|yr), 
                     data=df_surv, 
                     family="binomial",
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 2000000)))
  # Fall

mod.l$P.Fall <- glmer(alive_t1 ~ -1 + ageClass/P.AUT +  pred+ (1|ID) + (1|yr), 
                    data=df_surv, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 2000000)))

mod.l$P.T.Fall <- glmer(alive_t1 ~ -1 + ageClass/P.AUT+ageClass/T.AUT +  pred+ (1|ID) + (1|yr), 
                       data=df_surv, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000)))

mod.l$PxT.Fall <- glmer(alive_t1 ~ -1 + ageClass/(P.AUT*T.AUT) +  pred+ (1|ID) + (1|yr), 
                       data=df_surv, 
                       family="binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000)))

mod.l$T.Fall <- glmer(alive_t1 ~ -1 + ageClass/T.AUT +  pred+ (1|ID) + (1|yr), 
                     data=df_surv, 
                     family="binomial",
                     control = glmerControl(optimizer="bobyqa", 
                                            optCtrl = list(maxfun = 2000000)))

mod.l$P.Fall <- glmer(alive_t1 ~ -1 + ageClass/P.AUT +  pred+ (1|ID) + (1|yr), 
                    data=df_surv, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 2000000)))



## exporting AIC table
x <- aictab(mod.l)
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html", 
             file="C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/weather_AIC.html") # open directly with Word
getwd()
# results 

results.T.Win <- data.frame(coef(summary(mod.l$T.Win)))
results.T.Win[, 1:4] <- round(results.T.Win[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$T.Win), digits = 3) #
#R2m   R2c
#theoretical 0.257 0.301
#delta       0.222 0.260
getwd()

results_weather_surv<- write.csv(results.T.Win, file = "C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/results_weather_surv.xls", row.names = TRUE)

