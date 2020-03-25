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



<<<<<<< HEAD
=======
drive_download("OWPC/Analyses/data/Raw/sheep_data", type="csv", overwrite=T)
sheep<-read.csv("sheep_data.csv", header=T, sep=",")
names(sheep)

drive_download("OWPC/Analyses/data/Raw/Climat/Localweather_seasons",type="csv", overwrite=T)
weather<-read.csv("Localweather_seasons.csv", header=T, sep=",")

# add time lags

  # No time lag for survival
  # Add Summer(t-1) and Fall(t-1) for fecundity
weather$yr <- as.numeric(as.character(weather$yr))
names(weather)<-c("yr", "T.WIN.m1", "P.WIN.m1", "T.SPRING.m1", "P.SPRING.m1", "T.SUMMER", "P.SUMMER", "T.FALL", "P.FALL")

colnames(weather)

 # Surv (win + spring t+1)
tmp <- weather[, c("yr","T.WIN.m1", "P.WIN.m1", "T.SPRING.m1","P.SPRING.m1")]

tmp$yr <- tmp$yr-1
names(tmp)<-c("yr", "T.WIN", "P.WIN", "T.SPRING", "P.SPRING")
head(tmp)

#weather_surv<-weather[, c("yr","T.Win","P.Win","T.SPRING","P.SPRING")]
head(weather_surv)
weather_surv <- merge(weather,
                     tmp,
                     by.x = c("yr"), 
                     by.y = c("yr"))
weather_surv<-filter(weather_surv, yr>=2000)


df_surv<-merge(sheep,
               weather_surv,
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

df_surv[c("MassSpring","MassAutumn","T.WIN.m1","P.WIN.m1","T.SPRING.m1" ,"P.SPRING.m1","T.WIN","P.WIN","T.SPRING" ,"P.SPRING", "T.SUMMER","P.SUMMER",
          "T.FALL","P.FALL")] <- 
  scale(df_surv[c("MassSpring","MassAutumn","T.WIN.m1","P.WIN.m1","T.SPRING.m1" ,"P.SPRING.m1","T.WIN","P.WIN","T.SPRING" ,"P.SPRING", "T.SUMMER","P.SUMMER",
                  "T.FALL","P.FALL")]) 
names(df_surv)
#df_surv<-na.omit(df_surv)
# 
head(df_surv)
>>>>>>> 6f6c66643942f0ea1a857d3f02b56580724b6fb5
# run survival models -----------------------------------------------------

colnames(df_surv)
mod.l <- list()

# base
mod.l$base <- glm(alive_t1 ~ -1 + ageClass +  pred, 
                      data=df_surv, 
                      family="binomial") 



# Winter (yr-1)
mod.l$P.T.Win.m1 <- glm(alive_t1 ~ -1 + ageClass/P.WIN.m1+ageClass/T.WIN.m1 +  pred, 
                     data=df_surv, 
                     family="binomial")


mod.l$PxT.Win.m1 <- glm(alive_t1 ~ -1 + ageClass/(P.WIN.m1*T.WIN.m1) +  pred, 
                     data=df_surv, 
                     family="binomial")

mod.l$T.Win.m1 <- glm(alive_t1 ~ -1 + ageClass/T.WIN.m1 +  pred, 
                   data=df_surv, 
                   family="binomial")

mod.l$P.Win.m1 <- glm(alive_t1 ~ -1 + ageClass/P.WIN.m1 +  pred, 
                   data=df_surv, 
                   family="binomial")

# Spring (yr-1)

mod.l$P.T.Spring.m1 <- glm(alive_t1 ~ -1 + ageClass/P.SPRING.m1+ageClass/T.SPRING.m1 +  pred, 
                        data=df_surv, 
                        family="binomial")

mod.l$PxT.Spring.m1 <- glm(alive_t1 ~ -1 + ageClass/(P.SPRING.m1*T.SPRING.m1) +  pred, 
                        data=df_surv, 
                        family="binomial")

mod.l$T.Spring.m1 <- glm(alive_t1 ~ -1 + ageClass/T.SPRING.m1 +  pred, 
                      data=df_surv, 
                      family="binomial")

mod.l$P.Spring.m1 <- glm(alive_t1 ~ -1 + ageClass/P.SPRING.m1 +  pred, 
                      data=df_surv, 
                      family="binomial")

# Winter
mod.l$P.T.Win <- glm(alive_t1 ~ -1 + ageClass/P.WIN+ageClass/T.WIN +  pred, 
                     data=df_surv, 
                     family="binomial")


mod.l$PxT.Win <- glm(alive_t1 ~ -1 + ageClass/(P.WIN*T.WIN) +  pred, 
                     data=df_surv, 
                     family="binomial")

mod.l$T.Win <- glm(alive_t1 ~ -1 + ageClass/T.WIN +  pred, 
                   data=df_surv, 
                   family="binomial")

mod.l$P.Win <- glm(alive_t1 ~ -1 + ageClass/P.WIN +  pred, 
                   data=df_surv, 
                   family="binomial")

# Spring

mod.l$P.T.Spring <- glm(alive_t1 ~ -1 + ageClass/P.SPRING+ageClass/T.SPRING +  pred, 
                        data=df_surv, 
                        family="binomial")

mod.l$PxT.Spring <- glm(alive_t1 ~ -1 + ageClass/(P.SPRING*T.SPRING) +  pred, 
                        data=df_surv, 
                        family="binomial")

mod.l$T.Spring <- glm(alive_t1 ~ -1 + ageClass/T.SPRING +  pred, 
                      data=df_surv, 
                      family="binomial")

mod.l$P.Spring <- glm(alive_t1 ~ -1 + ageClass/P.SPRING +  pred, 
                      data=df_surv, 
                      family="binomial")

# Summer
mod.l$P.T.Summer <- glm(alive_t1 ~ -1 + ageClass/P.SUMMER+ageClass/T.SUMMER +  pred, 
                        data=df_surv, 
                        family="binomial")

mod.l$PxT.Summer <- glm(alive_t1 ~ -1 + ageClass/(P.SUMMER*T.SUMMER) +  pred, 
                        data=df_surv, 
                        family="binomial")

mod.l$T.Summer <- glm(alive_t1 ~ -1 + ageClass/T.SUMMER +  pred, 
                      data=df_surv, 
                      family="binomial")
# Fall

mod.l$P.Fall <- glm(alive_t1 ~ -1 + ageClass/P.FALL +  pred, 
                    data=df_surv, 
                    family="binomial")

mod.l$P.T.Fall <- glm(alive_t1 ~ -1 + ageClass/P.FALL+ageClass/T.FALL +  pred, 
                      data=df_surv, 
                      family="binomial")

mod.l$PxT.Fall <- glm(alive_t1 ~ -1 + ageClass/(P.FALL*T.FALL) +  pred, 
                      data=df_surv, 
                      family="binomial")

mod.l$T.Fall <- glm(alive_t1 ~ -1 + ageClass/T.FALL +  pred, 
                    data=df_surv, 
                    family="binomial")

mod.l$P.Fall <- glm(alive_t1 ~ -1 + ageClass/P.FALL +  pred, 
                    data=df_surv, 
                    family="binomial")
get
save(df_surv, df_fec, file="C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/cache/df_weather.Rdata")

## exporting AIC table
x <- aictab(mod.l)
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html", 
             file="C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/weather_surv_AIC.html") # open directly with Word
getwd()

# results 
summary(mod.l$T.Fall)
results.T.Win <- data.frame(coef(summary(mod.l$T.Win)))
results.T.Win[, 1:4] <- round(results.T.Win[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$T.Win), digits = 3) #
#R2m   R2c
#theoretical 0.257 0.301
#delta       0.222 0.260
getwd()

results_weather_surv<- write.csv(results.T.Win, file = "C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/results_weather_surv.xls", row.names = TRUE)


#### Repro time lags -------------------------------------------


tmp <- weather[, c("yr","T.SUMMER", "P.SUMMER", "T.FALL","P.FALL")]
names(weather)
tmp$yr <- tmp$yr+1

tmp <- tmp %>% 
  rename(T.SUMMER.m1 = T.SUMMER, 
         P.SUMMER.m1 = P.SUMMER, 
         T.FALL.m1 = T.FALL,
         P.FALL.m1 = P.FALL)

head(tmp)

weather_fec<-weather[, c("yr","T.WIN.m1","P.WIN.m1","T.SPRING.m1","P.SPRING.m1")]
head(weather_fec)
weather_fec <- merge(weather_fec,
                     tmp,
                     by.x = c("yr"), 
                     by.y = c("yr"))

tmp<-sheep[, c("yr", "ID", "MassAutumn", "pred", "true_repro")]
tmp$yr<-as.numeric(tmp$yr)
tmp$yr<-tmp$yr+1

tmp <- tmp %>% 
  rename(MassAutumn_tm1= MassAutumn, 
    pred_tm1= pred, 
     true_repro_tm1 = true_repro)
tmp<-filter(tmp, yr>=2001)

df_fec= merge(tmp,
              weather_fec,
              by.x = "yr", 
              by.y =  "yr", 
              all.x=T) # keep all years even if NA
sheep_fec<-sheep[, c("age", "alive_t1", "raw_repro", "true_repro", "pred", "first_yr_trans")]
df_fec=cbind(df_fec, sheep_fec)
head(df_fec)
df_fec$ageClass<-NULL
str(df_fec)
# classes d'age 3,4-8,9
df_fec$ageClass <- ifelse(df_fec$age == 3, 3, df_fec$age)
df_fec$ageClass <- ifelse(df_fec$age >= 9, 9, df_fec$age)
c48 <- c(4:8)
df_fec$ageClass <- ifelse(df_fec$age %in% c48 , 48, df_fec$ageClass)

df_fec$ageClass <- as.factor(df_fec$ageClass)

df_fec<-filter(df_fec, yr<=2016)
df_fec$yr <- as.factor(df_fec$yr)
df_fec$ID <- as.factor(df_fec$ID)
df_fec$alive_t1 <- as.factor(df_fec$alive_t1)
df_fec$raw_repro <- as.factor(df_fec$raw_repro)
df_fec$true_repro <- as.factor(df_fec$true_repro)

df_fec<-filter(df_fec, first_yr_trans==0)
df_fec<-filter(df_fec, age>=3)
# remove age classes 0-1-2 
#df_fec <- subset(df_fec, ageClass %in% c(3,48,9))
#df_fec$ageClass <- droplevels(df_fec$ageClass)
#df_fec$pred_tm1 <- as.factor(df_fec$pred_tm1)

# Scale variables
names(df_fec)
df_fec[c("MassAutumn_tm1","T.WIN.m1","P.WIN.m1","T.SPRING.m1" ,"P.SPRING.m1", "T.SUMMER.m1","P.SUMMER.m1",
          "T.FALL.m1","P.FALL.m1")] <- 
  scale(df_fec[c("MassAutumn_tm1","T.WIN.m1","P.WIN.m1","T.SPRING.m1" ,"P.SPRING.m1", "T.SUMMER.m1","P.SUMMER.m1",
                  "T.FALL.m1","P.FALL.m1")]) 

df_fec<-na.omit(df_fec)

#save(df_fec, df_surv, file="dfweather.Rdata")
#### RUN RAW REPRO MODELS  ####---------------------

mod.l <- list()
mod.l$base <- glmer(raw_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID), # here write the model
                    data=df_fec, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 2000000))) 


# Winter yr-1



mod.l$P.T.WIN.m1 <-glmer(raw_repro ~ -1 + ageClass/P.WIN.m1 +ageClass/T.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                            data=df_fec, 
                            family="binomial",
                            control = glmerControl(optimizer="nmkbw", 
                                                   optCtrl = list(maxfun = 2000000))) 


mod.l$TxP.WIN.m1 <- glmer(raw_repro ~ -1 + ageClass/(T.WIN.m1*P.WIN.m1) +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 


mod.l$T.WIN.m1 <- glmer(raw_repro ~ -1 + ageClass/T.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 

mod.l$P.WIN.m1 <- glmer(raw_repro ~ -1 + ageClass/P.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 

# Spring yr-1

mod.l$P.T.SPRING.m1 <-glmer(raw_repro ~ -1 + ageClass/P.SPRING.m1 +ageClass/T.SPRING.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 
mod.l$TxP.SPRING.m1 <- glmer(raw_repro ~ -1 + ageClass/(T.SPRING.m1*P.SPRING.m1) +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 

mod.l$T.SPRING.m1 <- glmer(raw_repro ~ -1 + ageClass/T.SPRING.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

mod.l$P.SPRING.m1 <- glmer(raw_repro ~ -1 + ageClass/P.SPRING.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

# Summer yr-1
mod.l$P.T.SUMMER.m1 <-glmer(raw_repro ~ -1 + ageClass/P.SUMMER.m1 +ageClass/T.SUMMER.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 
mod.l$TxP.SUMMER.m1 <- glmer(raw_repro ~ -1 + ageClass/(T.SUMMER.m1*P.SUMMER.m1) +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 

mod.l$T.SUMMER.m1 <- glmer(raw_repro ~ -1 + ageClass/T.SUMMER.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

mod.l$P.SUMMER.m1 <- glmer(raw_repro ~ -1 + ageClass/P.SUMMER.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

# Fall yr-1
mod.l$P.T.FALL.m1 <-glmer(raw_repro ~ -1 + ageClass/P.FALL.m1 +ageClass/T.FALL.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 
mod.l$TxP.FALL.m1 <- glmer(raw_repro ~ -1 + ageClass/(T.FALL.m1*P.FALL.m1) +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 

mod.l$T.FALL.m1 <- glmer(raw_repro ~ -1 + ageClass/T.FALL.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

mod.l$P.FALL.m1 <- glmer(raw_repro ~ -1 + ageClass/P.FALL.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 1000000))) 

## exporting AIC table
x <- aictab(mod.l)
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html", 
             file="C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/weather_rawrepro_AIC.html") # open directly with Word
getwd()
# results 
summary(mod.l$P.T.WIN.m1)
library(boot)
inv.logit(coef(summary(mod.l$P.T.WIN.m1)))
results.T.Win <- data.frame(coef(summary(mod.l$T.Win)))
results.T.Win[, 1:4] <- round(results.T.Win[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$P.T.WIN.m1), digits = 3) #
#R2m   R2c
#theoretical 0.257 0.301
#delta       0.222 0.260
getwd()

#### RUN TRUE REPRO MODELS  ####---------------------

mod.l <- list()
mod.l$base <- glmer(true_repro ~ -1 + ageClass + MassAutumn_tm1 + (1|ID), # here write the model
                    data=df_fec, 
                    family="binomial",
                    control = glmerControl(optimizer="bobyqa", 
                                           optCtrl = list(maxfun = 2000000))) 


# Winter yr-1
mod.l$P.T.WIN.m1 <-glmer(true_repro ~ -1 + ageClass/P.WIN.m1 +ageClass/T.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000))) 
mod.l$TxP.WIN.m1 <- glmer(true_repro ~ -1 + ageClass/(T.WIN.m1*P.WIN.m1) +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 


mod.l$T.WIN.m1 <- glmer(true_repro ~ -1 + ageClass/T.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 2000000))) 

mod.l$P.WIN.m1 <- glmer(true_repro ~ -1 + ageClass/P.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                        data=df_fec, 
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 2000000))) 

# Spring yr-1

mod.l$P.T.SPRING.m1 <-glmer(true_repro ~ -1 + ageClass/P.SPRING.m1 +ageClass/T.SPRING.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                            data=df_fec, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000))) 
mod.l$TxP.SPRING.m1 <- glmer(true_repro ~ -1 + ageClass/(T.SPRING.m1*P.SPRING.m1) +  MassAutumn_tm1 + (1|ID), # here write the model
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000))) 

mod.l$T.SPRING.m1 <- glmer(true_repro ~ -1 + ageClass/T.SPRING.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 1000000))) 

mod.l$P.SPRING.m1 <- glmer(true_repro ~ -1 + ageClass/P.SPRING.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 1000000))) 

# Summer yr-1
mod.l$P.T.SUMMER.m1 <-glmer(true_repro ~ -1 + ageClass/P.SUMMER.m1 +ageClass/T.SUMMER.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                            data=df_fec, 
                            family="binomial",
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000))) 
mod.l$TxP.SUMMER.m1 <- glmer(true_repro ~ -1 + ageClass/(T.SUMMER.m1*P.SUMMER.m1) +  MassAutumn_tm1 + (1|ID), # here write the model
                             data=df_fec, 
                             family="binomial",
                             control = glmerControl(optimizer="bobyqa", 
                                                    optCtrl = list(maxfun = 2000000))) 

mod.l$T.SUMMER.m1 <- glmer(true_repro ~ -1 + ageClass/T.SUMMER.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 1000000))) 

mod.l$P.SUMMER.m1 <- glmer(true_repro ~ -1 + ageClass/P.SUMMER.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 1000000))) 

# Fall yr-1
mod.l$P.T.FALL.m1 <-glmer(true_repro ~ -1 + ageClass/P.FALL.m1 +ageClass/T.FALL.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                          data=df_fec, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000))) 
mod.l$TxP.FALL.m1 <- glmer(true_repro ~ -1 + ageClass/(T.FALL.m1*P.FALL.m1) +  MassAutumn_tm1 + (1|ID), # here write the model
                           data=df_fec, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000))) 

mod.l$T.FALL.m1 <- glmer(true_repro ~ -1 + ageClass/T.FALL.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 1000000))) 

mod.l$P.FALL.m1 <- glmer(true_repro ~ -1 + ageClass/P.FALL.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 1000000))) 

## exporting AIC table
x <- aictab(mod.l)
aictable <- xtable(x, caption = NULL, label = NULL, align = NULL,
                   digits = NULL, display = NULL, nice.names = TRUE,
                   include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)
print.xtable(aictable, type="html", 
             file="C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC/weather_truerepro_AIC.html") # open directly with Word
getwd()
# results 
summary(mod.l$P.T.WIN.m1)
library(boot)
inv.logit(coef(summary(mod.l$P.T.WIN.m1)))
results.T.Win <- data.frame(coef(summary(mod.l$T.Win)))
results.T.Win[, 1:4] <- round(results.T.Win[, 1:4], digits = 3)

round(MuMIn::r.squaredGLMM(mod.l$P.T.WIN.m1), digits = 3) #
# R2m   R2c
# theoretical 0.327 0.777
# delta       0.280 0.664

