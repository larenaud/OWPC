# Script for figures of final models made by Yanny

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
library(boot)
library(ggthemes)
library(pander)

# Accessing google drive
drive_find(n_max = 10)
# Select a pre-authorised account by entering the corresponding number in the console or enter '0' to obtain a new token.

# Set working directory DIFFERENT FOR EACH PERSON
setwd("")
#Ex: setwd("C:/Users/Proprietaire/Documents/uni poc/Phd/OWPC/Analyses/FinalModels")

# Cleaning R environment --------------------------------------------------------------------------------------------------
rm(list = ls())

# Importing data ----------------------------------------------------------------------------------------------------------

# Survival
# Download RData from drive
drive_download("OWPC/Analyses/cache/dataSurvivalModels.RData",overwrite=T)
# Import in R environment
load("dataSurvivalModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_surv<-droplevels(subset(dataSurvScld,!(yr %in% c("1999","2000","2016"))))
# Remove unnecessary objects for the environment
rm(clim_surv,pheno_surv,weather_surv,sheep_data,dataSurvScld,fullSurvDataScled)

# True repro
# Download RData from drive
drive_download("OWPC/Analyses/cache/dataFecundityModels.RData",overwrite=T)
# Import in R environment
load("dataFecundityModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_fec<-droplevels(subset(dataFecScld,!(yr %in% c("1999","2000","2001"))))
# Remove unnecessary objects for the environment
rm(clim_fec,pheno_fec,weather_fec,sheep_data,dataFecScld,fullFecDataScld)

# Final models ------------------------------------------------------------------------------------------------------------
finalSurv <- glm(alive_t1 ~ -1 + ageClass/WinSnowsurvT1 + pred, data=df_surv, family="binomial")

finalTrue <- glmer(true_repro ~ -1 + ageClass/(TWin*PWin) + MassAutumn_tm1 + (1|ID),
                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa",
                                                                          optCtrl = list(maxfun = 2000000)))


# Survival ---------------------------------------------------------------------------------------------------------------------------

# Age class : 0
newd<- data.frame()
newdata = expand.grid(WinSnowsurvT1  = seq(min(df_surv$WinSnowsurvT1 , na.rm = T),
                                           max(df_surv$WinSnowsurvT1 , na.rm = T), length = 200), # la variable d'intérêt
                      ageClass = "0", 
                      pred = "0")
newd.a0 <- rbind(newd, newdata)

# fit this dataframe true model estimates using predict
preds <- predict(finalSurv, newdata=newd.a0,se.fit = T, type = "link")

# get confidence intervals manually
critval <- 1.96
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

# now need to report on biological scale
fit2<- exp(fit)/(1+exp(fit))
upr2<- exp(upr)/(1+exp(upr))
lwr2<- exp(lwr)/(1+exp(lwr))

newd.a0$lwr <- lwr2 
newd.a0$upr <- upr2 
newd.a0$fit <- fit2

# Age class : 1
newd<- data.frame()
newdata = expand.grid(WinSnowsurvT1  = seq(min(df_surv$WinSnowsurvT1 , na.rm = T),
                                           max(df_surv$WinSnowsurvT1 , na.rm = T), length = 200), # la variable d'intérêt
                      ageClass = "1", 
                      pred = "0")
newd.a1 <- rbind(newd, newdata)

# fit this dataframe true model estimates using predict
preds <- predict(finalSurv, newdata=newd.a1,se.fit = T, type = "link")

# get confidence intervals manually
critval <- 1.96
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

# now need to report on biological scale 
fit2<- exp(fit)/(1+exp(fit))
upr2<- exp(upr)/(1+exp(upr))
lwr2<- exp(lwr)/(1+exp(lwr))

newd.a1$lwr <- lwr2 
newd.a1$upr <- upr2 
newd.a1$fit <- fit2


# Age class : 37
newd<- data.frame()
newdata = expand.grid(WinSnowsurvT1  = seq(min(df_surv$WinSnowsurvT1 , na.rm = T),
                                           max(df_surv$WinSnowsurvT1 , na.rm = T), length = 200), # la variable d'intérêt
                      ageClass = "37", 
                      pred = "0")
newd.a37 <- rbind(newd, newdata)

# fit this dataframe true model estimates using predict
preds <- predict(finalSurv, newdata=newd.a37,se.fit = T, type = "link")

# get confidence intervals manually
critval <- 1.96
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

fit2<- exp(fit)/(1+exp(fit))
upr2<- exp(upr)/(1+exp(upr))
lwr2<- exp(lwr)/(1+exp(lwr))

newd.a37$lwr <- lwr2 
newd.a37$upr <- upr2 
newd.a37$fit <- fit2

# Age class : 8
newd<- data.frame()
newdata = expand.grid(WinSnowsurvT1  = seq(min(df_surv$WinSnowsurvT1 , na.rm = T),
                                           max(df_surv$WinSnowsurvT1 , na.rm = T), length = 200), # la variable d'intérêt
                      ageClass = "8", 
                      pred = "0")
newd.a8 <- rbind(newd, newdata)

# fit this dataframe true model estimates using predict
preds <- predict(finalSurv, newdata=newd.a8,se.fit = T, type = "link")

# get confidence intervals manually
critval <- 1.96
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

fit2<- exp(fit)/(1+exp(fit))
upr2<- exp(upr)/(1+exp(upr))
lwr2<- exp(lwr)/(1+exp(lwr))

newd.a8$lwr <- lwr2 
newd.a8$upr <- upr2 
newd.a8$fit <- fit2



# Descaling data
surv.uns<-dataSurvUnscld
keep<-complete.cases(surv.uns[c("WinSnowsurvT1")])
surv.uns<-surv.uns[keep,]
q.SD<-sd(surv.uns$WinSnowsurvT1)
q.MEAN<-mean(surv.uns$WinSnowsurvT1)
newd.a0$WinSnowsurvT1.uns<-(newd.a0[,"WinSnowsurvT1"]*q.SD) + q.MEAN
newd.a1$WinSnowsurvT1.uns<-(newd.a1[,"WinSnowsurvT1"]*q.SD) + q.MEAN
newd.a37$WinSnowsurvT1.uns<-(newd.a37[,"WinSnowsurvT1"]*q.SD) + q.MEAN
newd.a8$WinSnowsurvT1.uns<-(newd.a8[,"WinSnowsurvT1"]*q.SD) + q.MEAN


# filter data by age class

df0<-filter(surv.uns, ageClass==0)
df1<-filter(surv.uns, ageClass==1)
df37<-filter(surv.uns, ageClass==37)
df8<-filter(surv.uns, ageClass==8)



# Figure
colors<-c("Age 0" = "#00FFD9", "Age 1" = "#FFB700", "Age 3-7" = "#FF0400", "Age 8+"= "#0015FF")

plot_surv <- 
  ggplot(newd.a1, aes(x=WinSnowsurvT1.uns)) +  
  geom_line(data=newd.a0, aes(y=fit, color="Age 0"), size=3) +
  geom_ribbon(data=newd.a0, aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = 'grey') +
  geom_jitter(data = df0, aes(x = WinSnowsurvT1, y = as.numeric(alive_t1)-1), width=2, height=0, alpha = 0.3, color="#00FFD9", size=2)+ # pour mettre la distribution des points brutes 
  
  geom_line(data=newd.a1, aes(y=fit, color="Age 1"), size=3) +
  geom_ribbon(data=newd.a1, aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = 'grey') +
  geom_jitter(data = df1, aes(x = WinSnowsurvT1, y = as.numeric(alive_t1)-1), width=2, height=0, alpha = 0.3, color="#FFB700", size=2)+ # pour mettre la distribution des points brutes 
  
  geom_line(data=newd.a37, aes(y=fit, color="Age 3-7"), size=3) +
  geom_ribbon(data=newd.a37, aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = 'grey') +
  geom_jitter(data = df37, aes(x = WinSnowsurvT1, y = as.numeric(alive_t1)-1), width=2, height=0, alpha = 0.3, color="#FF0400", size=2)+ # pour mettre la distribution des points brutes 
  
  geom_line(data=newd.a8, aes(y=fit, color="Age 8+"), size=3) +
  geom_ribbon(data=newd.a8, aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = 'grey') +
  geom_jitter(data = df8, aes(x = WinSnowsurvT1, y = as.numeric(alive_t1)-1), width=2, height=0, alpha = 0.3, color="#0015FF", size=2)+ # pour mettre la distribution des points brutes 
  
  
  theme(text = element_text(size = 20)) +
  
  theme(plot.background = element_blank(),panel.background = element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line=element_line(color="black")) +
  theme(legend.position = c(0.15, 0.8), legend.background = element_rect(), legend.title = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA)))  +
  scale_y_continuous(label=scales::percent)+
  labs(y="Probability to survive", x="Snow-covered days during Winter", color="Legend") +
  scale_color_manual(values=colors) +
  theme_pander() 

x11()
plot_surv

# True reproduction ~ PWin by ageclass---------------------------------------------------------------------

# Age class : 3
newd<- data.frame()
newdata = expand.grid(PWin  = seq(min(df_fec$PWin , na.rm = T),
                                  max(df_fec$PWin , na.rm = T), length = 50), # la variable d'intérêt
                      ageClass = "3", 
                      TWin = mean(df_fec$TWin),
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      ID = c("P1", "M25", "F10",  "Q13",  "E12"))
true.t3 <- rbind(newd, newdata)
#tmp = subset(df_fec, ageClass %in% 3) 
#sample(tmp$ID, 5)

# now in glmer, we can generate distribution of estimates and extract CI
# bootMer travaille en fonction - lui en spécifier une qui extrait nos coefficients de modèle avec predict 
myfun <- function(x) predict(x,newdata=true.t3,type="response",re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 100, verbose = T)

dboo <- data.frame(boo)
true.t3$predi <- apply(dboo, 2, mean)
true.t3$upr<- apply(dboo, 2, function(x) quantile(x, 0.975))
true.t3$lwr <- apply(dboo, 2, function(x) quantile(x, 0.025))

# Age Class : 4-8
newd<- data.frame()
newdata = expand.grid(PWin  = seq(min(df_fec$PWin , na.rm = T),
                                  max(df_fec$PWin , na.rm = T), length = 50), # la variable d'intérêt
                      ageClass = "48", 
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      TWin = mean(df_fec$TWin),
                      ID = c("M28", "I6", "A35",  "P11",  "A33"))
true.t48 <- rbind(newd, newdata)
#tmp = subset(df_fec, ageClass %in% 48) 
#sample(tmp$ID, 5)

myfun <- function(x) predict(x,newdata=true.t48,type="response",re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 100, verbose = T)

dboo <- data.frame(boo)
dboo<-na.omit(dboo)
true.t48$predi <- apply(dboo, 2, mean)
true.t48$upr<- apply(dboo, 2, function(x) quantile(x, 0.975))
true.t48$lwr <- apply(dboo, 2, function(x) quantile(x, 0.025))

# Age Class : 9+
newd<- data.frame()
newdata = expand.grid(PWin  = seq(min(df_fec$PWin , na.rm = T),
                                  max(df_fec$PWin , na.rm = T), length = 50), # la variable d'intérêt
                      ageClass = "9", 
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      TWin = mean(df_fec$TWin),
                      ID = c("G14", "A33", "15z",  "A44",  "J5"))
true.t9 <- rbind(newd, newdata)
#tmp = subset(df_fec, ageClass %in% 9) 
#sample(tmp$ID, 5)

myfun <- function(x) predict(x,newdata=true.t9,type="response",re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 100, verbose = T)

dboo <- data.frame(boo)
#dboo<-na.omit(dboo)
true.t9$predi <- apply(dboo, 2, mean)
true.t9$upr<- apply(dboo, 2, function(x) quantile(x, 0.975))
true.t9$lwr <- apply(dboo, 2, function(x) quantile(x, 0.025))

# Descaling Pwin and TWin
fec.uns<-dataFecUnscld
keep<-complete.cases(fec.uns[c("PWin")])
fec.uns<-fec.uns[keep,]
q.SD<-sd(fec.uns$PWin)
q.MEAN<-mean(fec.uns$PWin)
true.t3$PWin.uns<-(true.t3[,"PWin"]*q.SD) + q.MEAN
true.t48$PWin.uns<-(true.t48[,"PWin"]*q.SD) + q.MEAN
true.t9$PWin.uns<-(true.t9[,"PWin"]*q.SD) + q.MEAN

keep<-complete.cases(fec.uns[c("TWin", "PWin")])
fec.uns<-fec.uns[keep,]
q.SD<-sd(fec.uns$TWin)
q.MEAN<-mean(fec.uns$PWin)
true.t3$TWin.uns<-(true.t3[,"TWin"]*q.SD) + q.MEAN
true.t48$TWin.uns<-(true.t48[,"TWin"]*q.SD) + q.MEAN
true.t9$TWin.uns<-(true.t9[,"TWin"]*q.SD) + q.MEAN

df3<-filter(fec.uns, ageClass==3)
df48<-filter(fec.uns, ageClass==48)
df9<-filter(fec.uns, ageClass==9)

# Figure
colors<-c("3 years" = "#00FFD9", "4-8 years" = "#FFB700", "9 years and older" = "#FF0400")

true_PWin <-
  ggplot(true.t48, aes(x=PWin.uns)) + 
  
  geom_line(data=true.t3, aes(y=predi, color="3 years"), size=3) + # only have one age class
  geom_ribbon(data=true.t3, aes(ymin = lwr, ymax = upr), fill="grey", alpha=0.5) +  
  geom_jitter(data = df3, aes(x = PWin, y = as.numeric(true_repro)-1), width = 1, height = 0, color="#00FFD9", alpha=0.7, size=2) + # pour mettre la distribution des points brutes 
  
  geom_line(data=true.t48, aes(y=predi, color="4-8 years"), size=3) + # only have one age class
  geom_ribbon(data=true.t48, aes(ymin = lwr, ymax = upr), fill="grey", alpha=0.5) +  
  geom_jitter(data = df48, aes(x = PWin, y = as.numeric(true_repro)-1), width=1, height = 0, color="#FFB700", alpha=0.7, size=2) + # pour mettre la distribution des points brutes 
  
  geom_line(data=true.t9, aes(y=predi, color="9 years and older"), size=3) + # only have one age class
  geom_ribbon(data=true.t9, aes(ymin = lwr, ymax = upr), fill="#grey", alpha=0.5) +  
  geom_jitter(data = df9, aes(x = PWin, y = as.numeric(true_repro)-1), width = 1, height = 0, color="#FF0400", alpha=0.7, size=2) + # pour mettre la distribution des points brutes 
  
  theme(text = element_text(size = 20)) +
  theme(plot.background = element_blank(),panel.background = element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line=element_line(color="black")) +
  theme(legend.position = c(0.15, 0.8), legend.background = element_rect(), legend.title = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA)))  +
  scale_y_continuous(label=scales::percent)+
  labs(y="Probability to reproduce", x="Winter Precipitation (mm)", color="Legend") +
  scale_color_manual(values=colors) +
  theme_pander() 

x11()
true_PWin


# True reproduction ~ PWin*Twin for age class 4-8 yrs -------------------------------------------------------------------------------------------------------------------------------

# visreg sets it at 10, 50 90th percentile
T.Q<-quantile(df48$TWin, c(0.10, 0.50, 0.90))
T.low<-filter(df48, TWin<=T.Q[1])
T.med<-filter(df48, TWin>T.Q[1])
T.med<-filter(T.med, TWin<=T.Q[2])
T.high<-filter(df48, TWin>=T.Q[3])

# Colder temp (10th percentile)
newd<- data.frame()
newdata = expand.grid(PWin  = seq(min(df_fec$PWin , na.rm = T),
                                  max(df_fec$PWin , na.rm = T), length = 50), # la variable d'intérêt
                      ageClass = "48", 
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      TWin = T.Q[1],
                      ID = c("M28", "I6", "A35",  "P11",  "A33"))
true.txp48.low <- rbind(newd, newdata)
#tmp = subset(df_fec, ageClass %in% 48) 
#sample(tmp$ID, 5)

myfun <- function(x) predict(x,newdata=true.txp48.low,type="response",re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 100, verbose = T)

dboo <- data.frame(boo)
#dboo<-na.omit(dboo)
true.txp48.low$predi <- apply(dboo, 2, mean)
true.txp48.low$upr<- apply(dboo, 2, function(x) quantile(x, 0.975))
true.txp48.low$lwr <- apply(dboo, 2, function(x) quantile(x, 0.025))


# Median temp (50th percentile)
newd<- data.frame()
newdata = expand.grid(PWin  = seq(min(df_fec$PWin , na.rm = T),
                                  max(df_fec$PWin , na.rm = T), length = 50), # la variable d'intérêt
                      ageClass = "48", 
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      TWin = T.Q[2],
                      ID = c("M28", "I6", "A35",  "P11",  "A33"))
true.txp48.med <- rbind(newd, newdata)
#tmp = subset(df_fec, ageClass %in% 48) 
#sample(tmp$ID, 5)

myfun <- function(x) predict(x,newdata=true.txp48.med,type="response",re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 100, verbose = T)

dboo <- data.frame(boo)
#dboo<-na.omit(dboo)
true.txp48.med$predi <- apply(dboo, 2, mean)
true.txp48.med$upr<- apply(dboo, 2, function(x) quantile(x, 0.975))
true.txp48.med$lwr <- apply(dboo, 2, function(x) quantile(x, 0.025))


# Warmer temp (90th percentile)
newd<- data.frame()
newdata = expand.grid(PWin  = seq(min(df_fec$PWin , na.rm = T),
                                  max(df_fec$PWin , na.rm = T), length = 50), # la variable d'intérêt
                      ageClass = "48", 
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      TWin = T.Q[3],
                      ID = c("M28", "I6", "A35",  "P11",  "A33"))
true.txp48.high <- rbind(newd, newdata)
#tmp = subset(df_fec, ageClass %in% 48) 
#sample(tmp$ID, 5)

myfun <- function(x) predict(x,newdata=true.txp48.high,type="response",re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 100, verbose = T)

dboo <- data.frame(boo)
#dboo<-na.omit(dboo)
true.txp48.high$predi <- apply(dboo, 2, mean)
true.txp48.high$upr<- apply(dboo, 2, function(x) quantile(x, 0.975))
true.txp48.high$lwr <- apply(dboo, 2, function(x) quantile(x, 0.025))

# Descaling Pwin and TWin
fec.uns<-dataFecUnscld
keep<-complete.cases(fec.uns[c("PWin", "TWin")])
fec.uns<-fec.uns[keep,]
q.SD<-sd(fec.uns$PWin)
q.MEAN<-mean(fec.uns$PWin)
true.txp48.low$PWin.uns<-(true.txp48.low[,"PWin"]*q.SD) + q.MEAN
true.txp48.med$PWin.uns<-(true.txp48.med[,"PWin"]*q.SD) + q.MEAN
true.txp48.high$PWin.uns<-(true.txp48.high[,"PWin"]*q.SD) + q.MEAN

# Figure
colors<-c("-7.8C" = "#00FFD9", "-4.5C" = "#FFB700", "-1.9C" = "#FF0400")

true_PWinxTWin <-
  
  ggplot(true.txp48.low, aes(x=PWin.uns)) + 
  geom_line(data=true.txp48.low, aes(y=predi, color="-7.8C"), size=3) +
  geom_ribbon(data=true.txp48.low, aes(ymin = lwr, ymax = upr), fill="grey", alpha=0.5) +  
  geom_jitter(data = T.low, aes(x = PWin, y = as.numeric(true_repro)-1), width = 1, height = 0, color="#00FFD9", alpha=0.7, size=2) + # pour mettre la distribution des points brutes 
  
  geom_line(data=true.txp48.med, aes(y=predi, color="-4.5C"), size=3) + 
  geom_ribbon(data=true.txp48.med, aes(ymin = lwr, ymax = upr), fill="grey", alpha=0.5) +  
  geom_jitter(data = T.med, aes(x = PWin, y = as.numeric(true_repro)-1), width=1, height = 0, color="#FFB700", alpha=0.7, size=2) + # pour mettre la distribution des points brutes 
  
  geom_line(data=true.txp48.high, aes(y=predi, color="-1.9C"), size=3) +
  geom_ribbon(data=true.txp48.high, aes(ymin = lwr, ymax = upr), fill="grey", alpha=0.5) +  
  geom_jitter(data = T.high, aes(x = PWin, y = as.numeric(true_repro)-1), width = 1, height = 0, color="#FF0400", alpha=0.7, size=2) + # pour mettre la distribution des points brutes 
  
  theme(text = element_text(size = 20)) +
  
  theme(plot.background = element_blank(),panel.background = element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line=element_line(color="black")) +
  theme(legend.position = c(0.15, 0.8), legend.background = element_rect(), legend.title = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA)))  +
  scale_y_continuous(label=scales::percent)+
  labs(y="Probability to reproduce", x="Winter Precipitation (mm)", color="Legend") +
  scale_color_manual(values=colors) +
  theme_pander() 

x11()
true_PWinxTWin


#####
#save(plot_surv, true_PWin, true_PWinxTWin, file="Figures.Rdata")