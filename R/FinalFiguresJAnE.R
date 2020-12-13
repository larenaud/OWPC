# script for making final figures for J. Animal Ecology by L. Renaud Dec. 14 2020

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



# verifiation basic stat --------------------------------------------------

surv <- read.csv("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/raw/edited-2020-03-24.csv", sep = ",")
surv$sex[surv$sex == 'F'] <- "female"
colnames(surv)
surv <- surv[, c("yr","age","ID","sex","alive_t1","reproduced", "age.class", "code.sr", "pred")]

survF <- surv[surv$sex =="female",]
head(table(survF$yr, survF$code.sr))
survF$code.sr[survF$code.sr == '0œ'] <- "0"

t= data.frame(table(survF$ID)) %>%
  droplevels() %>%
  filter(Freq>0) # 581

survF$ID<- droplevels(survF$ID) 
View(t1<-table(survF$ID)) # 581 females (NA removed in environment)
# 
tmp <- t[t$Var1%in% names(t1)[t1>=1],] # 259 rows

tmp <- t[t$Var1%in% names(t1)[t1>1&t1<9],] # 259 rows
tmp$mother_id<- droplevels(tmp$mother_id) # 
View(t1<-table(tmp$mother_id))  # from 148 to 91 females in dataframe for plasticity analyses or 102 ??



sheep_data$ageClass <- ifelse(sheep_data$age %in% c48 , 48, sheep_data$ageClass)

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


# Final models survival ------------------------------------------------------------------------------------------------------------
finalSurv <- glm(alive_t1 ~ -1 + ageClass/WinSnowsurvT1 + pred, data=df_surv, family="binomial")
summary(finalSurv)


# figure 1A

# show effect of WinSnow
newd1 <- data.frame()
newdata <- expand.grid(WinSnowsurvT1 = seq(min(df_surv$WinSnowsurvT1, na.rm = T),
                                           max(df_surv$WinSnowsurvT1, na.rm = T), length = 200),
                       pred = "0", 
                       ageClass =c("1","8")) # generate df with observations # marginal effects on 2 age classes
newd1 <- rbind(newd1,newdata)

# fit this dataframe true model estimates using predict
preds <- predict(finalSurv, newdata=newd1,se.fit = T, type = "link")

# get confidence intervals manually
critval <- 1.96
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

# now need to report on biological scale 
fit2 <- finalSurv$family$linkinv(fit)
upr2 <- finalSurv$family$linkinv(upr)
lwr2 <- finalSurv$family$linkinv(lwr)

newd1$lwr <- lwr2 
newd1$upr <- upr2 
newd1$fit <- fit2


# Descaling data
surv.uns<-dataSurvUnscld
keep<-complete.cases(surv.uns[c("WinSnowsurvT1")])
surv.uns<-surv.uns[keep,]
q.SD<-sd(surv.uns$WinSnowsurvT1)
q.MEAN<-mean(surv.uns$WinSnowsurvT1)
newd1$WinSnowsurvT1.uns<-(newd1[,"WinSnowsurvT1"]*q.SD) + q.MEAN



plot_surv <- ggplot(newd1, aes(x=WinSnowsurvT1.uns, fit)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill=factor(ageClass)), alpha = 0.3) +
  geom_line(aes(y = fit, colour=factor(ageClass), linetype = factor(ageClass)), size=1.2) + 
  geom_jitter(data = surv.uns[surv.uns$ageClass %in% c("1","8"),], 
              aes(x = WinSnowsurvT1, y = as.numeric(alive_t1)-1, color = as.factor(ageClass)), 
              size=1.5, alpha = 0.5, height = 0.02, width = 0.05) +
  labs(x="Number of snow-covered days during winter", 
       y="Survival probability") 

plot_surv = plot_surv + 
  scale_colour_manual(values =c("#00BFC4", "#F8766D"),
                      labels=c("Yearlings", "8 years and older")) +
  scale_fill_manual(values =c("#00BFC4", "#F8766D"),
                    labels=c("Yearlings", "8 years and older")) + 
  scale_linetype_manual(values = c("solid", "dotted"),
                        labels=c("Yearlings", "8 years and older")) +
  guides(color = FALSE, fill= FALSE) + 
  guides(color=guide_legend(title="Age class"), linetype = guide_legend(title = 'Age class')) + 
  scale_y_continuous(label=scales::percent) +
 theme_cowplot(8) + 
 theme(legend.position = c(0.3, 0.3), legend.key.width = unit(1.5,"cm"))+ 
  theme(plot.margin = unit(c(0.5,0.1,0.5,0.5), "cm"))

plot_surv



# Figure 1B
newd<-data.frame(WinSnowsurvT1 = mean(df_surv$WinSnowsurvT1, na.rm = T), 
                 ageClass =c("1"))
newd2<-newd
for(i in 2:200) 
  newd2<-rbind(newd2,newd)
newd2$pred<-as.factor(c(rep(0,100),rep(1,100))) #factors Half T, half F

preds <- predict(finalSurv, newdata=newd2,se.fit = T, type = "link")
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

fit2 <- finalSurv$family$linkinv(fit)
upr2 <- finalSurv$family$linkinv(upr)
lwr2 <- finalSurv$family$linkinv(lwr)

newd2$lwr <- lwr2 
newd2$upr <- upr2 
newd2$fit <- fit2

x.labels <- c("Low predation", "High predation")
predation=ggplot(newd2,aes(x=pred,y=fit, ymin = lwr, ymax = upr))+
  geom_pointrange(size = 0.8, linetype = 1) +
  labs(x=expression('Predation')) + 
  labs(y="") +
  scale_x_discrete(labels= x.labels) +
  scale_y_continuous(label=scales::percent,limits = c(0,1)) + 
  theme_cowplot(8) +
  theme(plot.margin = unit(c(0.5,0.1,0.5,0.5), "cm"))

predation


# panel 1AB

p <- plot_grid(plot_surv, 
               predation,
               labels = c("(a)", "(b)"), 
               align = "vh", 
               ncol = 2)
ggsave("Figure_1Cussonetal.tiff", width=180, height = 80, units= "mm", dpi = 600, limitsize = TRUE)
#ggsave("Figure_1Cussonetal.eps", width=180, height = 80, units= "mm", dpi = 600, limitsize = TRUE)



# FIGURE 2- prediction figure for final true fecundity  ------------------------------------------------------
rm(list = ls())
getwd()
load("cache/dataFecundityModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_fec<-droplevels(subset(dataFecScld,!(yr %in% c("1999","2000","2001"))))
# Remove unnecessary objects for the environment
rm(clim_fec,pheno_fec,weather_fec,sheep_data,dataFecScld,fullFecDataScld)


# final model 
finalTrue <- glmer(true_repro ~ -1 + ageClass/(TWin * PWin) + MassAutumn_tm1 + (1 |ID), 
                   df_fec, 
                   family = "binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000)))
summary(finalTrue)



# figure 2A
# build dataframe
T.Q<-round(quantile(df_fec$TWin, c(0.10, 0.50, 0.90)),5)#  unscaled, like in model : -1.75020250  0.06182025  1.48370029 

newd<- data.frame()
newdata = expand.grid(PWin  = seq(min(df_fec$PWin , na.rm = T),
                                  max(df_fec$PWin, na.rm = T), length = 50), 
                      ageClass = "48", 
                      MassAutumn_tm1 = mean(df_fec[df_fec$ageClass %in% "48",]$MassAutumn_tm1, na.rm = T),
                      TWin  = c(-1.75020, 0.06182,  1.48370), # quartiles.. but scaled
                      ID = unique(df_fec$ID)) # assign random ID from real dataset
newd3 <- rbind(newd, newdata)

# generate distn of prediction and extract CI 
myfun <- function(x) predict(x,newdata=newd3,re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 500, verbose = T)

boo <- data.frame(boo)
str(boo)

# these are on a logit scale 
newd3$predi <- apply(boo, 2, mean) # removed boo$t because not in seq of x anymore
newd3$upr<- apply(boo, 2, function(x) quantile(x, 0.975))
newd3$lwr <- apply(boo, 2, function(x) quantile(x, 0.025))


# Descaling Pwin and TWin
fec.uns<-dataFecUnscld
keep<-complete.cases(fec.uns[c("PWin", "TWin")])
fec.uns<-fec.uns[keep,]
q.SD<-sd(fec.uns$PWin)
q.MEAN<-mean(fec.uns$PWin)
newd3$PWin.uns<-(newd3[,"PWin"]*q.SD) + q.MEAN

q.SD<-sd(fec.uns$TWin)
q.MEAN<-mean(fec.uns$TWin)
newd3$TWin.uns<-round((newd3[,"TWin"]*q.SD) + q.MEAN,5)

# raw values for points - splitted for colors 
T.Q
fec.uns$binTWin <- cut(fec.uns$TWin, 3, labels = c(-7.81708,  -4.48464, -1.86970))
table(df_fec$binPWin)
table(df_fec$true_repro)

plot_true <- ggplot(newd3, aes(PWin.uns, inv.logit(predi))) + 
  geom_ribbon(aes(ymin = inv.logit(lwr), ymax = inv.logit(upr), fill=factor(TWin.uns)), alpha=0.2) + 
  geom_line(aes(y = inv.logit(predi), linetype = factor(TWin.uns), colour=factor(TWin.uns))) + 
  geom_jitter(data = fec.uns[fec.uns$ageClass %in% "48",], 
              aes(x = PWin, y = as.numeric(true_repro)-1, color = as.factor(binTWin)), 
              size=1, alpha = 0.5, height = 0.01, width = 0.02) +
  labs(x="Winter precipitation (mm)", 
       y="Probability to reproduce") +
  theme_cowplot() 
 # theme(legend.position = c(0.2, 0.3)) + # coordonnées x-y de ton graph + 
 # theme(axis.text=element_text(size=10)) +
  #theme(axis.title = element_text(size =10))
plot_true = plot_true + 
  scale_colour_manual(values =c("#00BFC4", "#F8766D", "gray40"),
                      labels = c("-7.82°C",  "-4.48°C", "-1.87°C"))+
  scale_fill_manual(values =c("#00BFC4", "#F8766D", "gray40"),
                    labels = c("-7.82°C",  "-4.48°C", "-1.87°C"))+
  scale_linetype_manual(values = c("solid", "dotted", "dotdash"),
                        labels = c("-7.82°C",  "-4.48°C", "-1.87°C"))+
  guides(color = FALSE, fill= FALSE) + 
  guides(color=guide_legend(title="Temperature"), linetype = guide_legend(title = 'Temperature')) + 
  scale_y_continuous(label=scales::percent) +
  theme_cowplot(8) + 
  theme(legend.position = c(0.3, 0.3), legend.key.width = unit(1,"cm"))+ 
  theme(plot.margin = unit(c(0.5,0.1,0.5,0.5), "cm"))
plot_true

# Figure 2B
newd<- data.frame()
newdata = expand.grid(MassAutumn_tm1  = seq(min(df_fec[df_fec$ageClass %in% "48",]$MassAutumn_tm1 , na.rm = T),
                                            max(df_fec[df_fec$ageClass %in% "48",]$MassAutumn_tm1, na.rm = T), length = 50), 
                      ageClass = "48", 
                      TWin = mean(df_fec$TWin, na.rm = T),
                      PWin = mean(df_fec$PWin, na.rm = T), 
                      ID = unique(df_fec$ID)) # assign random ID from real dataset
newd4 <- rbind(newd, newdata)

# generate distn of prediction and extract CI 
myfun <- function(x) predict(x,newdata=newd4,type="link",re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 500, verbose = T)

boo <- data.frame(boo)
str(boo)

newd4$predi <- apply(boo, 2, mean) # removed boo$t because not in seq of x anymore
newd4$upr<- apply(boo, 2, function(x) quantile(x, 0.975))
newd4$lwr <- apply(boo, 2, function(x) quantile(x, 0.025))# prediction


# Descaling data
fec.uns<-dataFecUnscld
keep<-complete.cases(fec.uns[c("MassAutumn_tm1")]) # age class
fec.uns<-fec.uns[keep,]
q.SD<-sd(fec.uns$MassAutumn_tm1)
q.MEAN<-mean(fec.uns$MassAutumn_tm1)
newd4$MassAutumn_tm1.uns<-(newd4[,"MassAutumn_tm1"]*q.SD) + q.MEAN


mumMass<- ggplot(newd4, aes(y=inv.logit(predi), x = MassAutumn_tm1.uns)) +
  geom_ribbon(aes(ymin = inv.logit(lwr), ymax = inv.logit(upr)),  colour=NA, fill = '#00BFC4', alpha=0.2) + 
  geom_line(colour = '#00BFC4', size = 1.2) + 
  geom_jitter(data=fec.uns[fec.uns$ageClass %in% "48",], aes(y = as.numeric(true_repro)-1, x=MassAutumn_tm1), size = 1.5, alpha = .5, height = 0.02, colour = '#00BFC4') + 
  labs(x=expression('Autumn mass' [t-1])) + 
  labs(y=expression('')) +  
  scale_y_continuous(label=scales::percent)+
  theme_cowplot(8) +
  theme(plot.margin = unit(c(0.5,0.1,0.5,0.5), "cm"))
print(mumMass) 

# panel 2AB
q <- plot_grid (plot_true, 
                mumMass,
                labels = c("(a)", "(b)"), 
                align = "vh", 
                ncol = 2)

ggsave("graph/Figure_2Cussonetal.tiff", width=180, height = 80, units= "mm", dpi = 600, limitsize = TRUE)

# SAVE OBJECT FIGURES -----------------------------------------------------

save(newd, newd2, newd3,newd4, df_fec, df_surv,finalSurv, finalTrue, p,q,
     file = "graph/Figures1_2JAnE.RData")


