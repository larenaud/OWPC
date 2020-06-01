# this script is a combination of former 15_modelselection_final, 16_Final_models and 14_model_figures
# it is intended to select a final model combining all environmental variables for surv, raw repro and true reproduction. 
# modified by L. Renaud march 31 2020


# models with AIC < 2 were considered equivalent and included in the final selection 

# Cleaning R environment
rm(list = ls())

# Loading required libraries
library(lme4)
library(AICcmodavg)
library(xtable)
library(googledrive)
library(plyr)
library(dplyr)
library(MuMIn)
library(ggplot2)
library(cowplot)
library(xtable)
library(readxl)
library(boot)
library(ggthemes)

library(cowplot)

#install.packages("pander")
library(pander)

# Accessing google drive
drive_find(n_max = 10)
# Select a pre-authorised account by entering the corresponding number in the console or enter '0' to obtain a new token.

# Set working directory DIFFERENT FOR EACH PERSON
setwd("")
#Ex: setwd("C:/Users/Proprietaire/Documents/uni poc/Phd/OWPC/Analyses/FinalModels")

##### Survival ############################################################################################################
# Cleaning R environment -----------------------------------------------------------------------
rm(list = ls())
# Importing data -------------------------------------------------------------------------------

# Climate
drive_download("OWPC/Analyses/results/surv_clim.RData",overwrite = T)
load("surv_clim.RData")

# Pheno
drive_download("OWPC/Analyses/results/surv_pheno.RData",overwrite = T)
load("surv_pheno.RData")

# Weather
drive_download("OWPC/Analyses/results/surv_weather.RData",overwrite = T)
load("surv_weather.RData")

# Selecting models for each categories of environmental variable -------------------------------

# Climate
View(results.surv.clim[["aictable.surv"]])
# Best model is base model
# No model is selected

# Pheno
View(results.surv.pheno[["aictable.surv"]])
# Best model is Snow_present with a delta AICc >2 with base model.
# Other models "beat" the base model but none with a delta AICc >2.
# Snow_present is selected.

# Weather
View(results.surv.weat[["aictable.surv"]])
# Best model is Fall_T, but with delta AICc <2 with base model which is the second best model.

anova(mod.surv.weat[["Base"]], mod.surv.weat[["Fall_T"]], test = "LRT")

#  Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
#  1       442      375.8                       
#  2       437      364.6  5     11.2  0.04756 *

# Likelihood ratio test suggest our data significally "better" fit the Fall_T model.
# Fall_T model is selected.

# Final survival model -------------------------------------------------------------------------

# List of candidate models
mod.surv <- list()

# Base model
mod.surv$Base<- mod.surv.clim[["base"]]

# Climate
# no selected model

# Pheno
mod.surv$Snow_present<- mod.surv.pheno[["Snow_present"]]

# Weather
mod.surv$Fall_T<- mod.surv.weat[["Fall_T"]]

# Pheno + weather
mod.surv$Snowpres_FallT<- glm(alive_t1 ~ -1 + ageClass/WinSnowsurvT1 + ageClass/T.FALL + pred,
                              data=df_surv, family="binomial")

# Creating a list to store the results
results.surv<-list()

## Creating and exporting AIC table to results list
results.surv$aictable.surv <- xtable(aictab(mod.surv), caption = NULL, label = NULL, align = NULL,
                                          digits = NULL, display = NULL, nice.names = TRUE,
                                          include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.surv$aictable.surv[,3:6] <-round(results.surv[["aictable.surv"]][,3:6],digits=3)

# Results from survival best model -------------------------------------------------------------------------------------- 

# Snow_present
results.surv$coefs.surv.best <- data.frame(coef(summary(mod.surv[[as.character(results.surv[["aictable.surv"]][1,1])]])))
results.surv$coefs.surv.best[, 1:4] <- round(results.surv[["coefs.surv.best"]][, 1:4], digits = 3)
results.surv$r2.surv.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv[[as.character(results.surv[["aictable.surv"]][1,1])]]), digits = 3))

# Save results ---------------------------------------------------------------------------------------------------------- 

save(df_surv,mod.surv,results.surv,file = "final_surv.Rdata")


library(kableExtra)

kable(results.surv$aictable.surv) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "FinalAICSurv.html", self_contained = T) 

kable(results.surv$coefs.surv.best) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "FinalSurvCoef.html", self_contained = T) 

##### True repro ##########################################################################################################
# Cleaning R environment -----------------------------------------------------------------------
rm(list = ls())
# Importing data -------------------------------------------------------------------------------

# Climate
drive_download("OWPC/Analyses/results/true_repro_clim.RData",overwrite = T)
load("true_repro_clim.RData")

# Pheno
drive_download("OWPC/Analyses/results/true_repro_pheno.RData",overwrite = T)
load("true_repro_pheno.RData")

# Weather
drive_download("OWPC/Analyses/results/true_repro_weather.RData",overwrite = T)
load("true_repro_weather.RData")

# Selecting models for each categories of environmental variable -------------------------------

# Climate
View(results.true.repro.clim[["aictable.true.repro"]])
# Best model is base model
# No model is selected

# Pheno
View(results.true.repro.pheno[["aictable.true.repro"]])
# Best model is Date_PC2, but with a delta AICc <2 with base model which is the second best model.

anova(mod.true.repro.pheno[["base"]], mod.true.repro.pheno[["Date_PC2"]], test = "LRT")

#                                    Df    AIC    BIC     logLik  deviance  Chisq Chi Df Pr(>Chisq)  
#mod.true.repro.pheno[["base"]]      5   301.47  319.54  -145.74   291.47                           
#mod.true.repro.pheno[["Date_PC2"]]  8   300.02  328.93  -142.01   284.02     7.4456  3   0.05897 .

# Likelihood ratio test suggest our data marginally "better" fit the Date_PC2 model.
# Date_PC2 model is selected.

# Weather
View(results.true.repro.weat[["aictable.true.repro"]])
# Best model is Winter_PxT with delta AICc >2 with base model.
# Other models "beat" the base model, but Winter_PxT is cleary the best model with a delta AICc of 6.477 with the second best model.
# Winter_PxT is selected

# Final true repro model -------------------------------------------------------------------------

# List of candidate models
mod.true.repro <- list()

# Base model
mod.true.repro$Base<- mod.true.repro.clim[["Base"]]

# Climate
# no selected model

# Pheno
mod.true.repro$Date_PC2<- mod.true.repro.pheno[["Date_PC2"]]

# Weather
mod.true.repro$Winter_PxT<- mod.true.repro.weat[["Winter_PxT"]]

# Pheno + weather
mod.true.repro$DatePC2_WinterPxT<- glmer(true_repro ~ -1 + ageClass/PC2Date + ageClass/(TWin*PWin) + MassAutumn_tm1 + (1|ID), 
                                         data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa", 
                                         optCtrl = list(maxfun = 4000000)))
  
# Creating a list to store the results
results.true.repro<-list()

## Creating and exporting AIC table to results list
results.true.repro$aictable.true.repro <- xtable(aictab(mod.true.repro), caption = NULL, label = NULL, align = NULL,
                                     digits = NULL, display = NULL, nice.names = TRUE,
                                     include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.true.repro$aictable.true.repro[,3:6] <-round(results.true.repro[["aictable.true.repro"]][,3:6],digits=3)

# Results from true repro best model -------------------------------------------------------------------------------------- 

# Winter_PxT
results.true.repro$coefs.true.repro.best <- data.frame(coef(summary(mod.true.repro[[as.character(results.true.repro[["aictable.true.repro"]][1,1])]])))
results.true.repro$coefs.true.repro.best[, 1:4] <- round(results.true.repro[["coefs.true.repro.best"]][, 1:4], digits = 3)
results.true.repro$r2.true.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.true.repro[[as.character(results.true.repro[["aictable.true.repro"]][1,1])]]), digits = 3))


# Save results ---------------------------------------------------------------------------------------------------------- 
save(df_fec,mod.true.repro,results.true.repro,file = "final_true_models.Rdata")

kable(results.true.repro$aictable.true.repro) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "FinalAicTrue.html", self_contained = T) 

kable(results.true.repro$coefs.true.repro.best[, 1:4]) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "FinalTrueCoef.html", self_contained = T) 

#### Predictions ##########################################################################################################
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
rm(clim_surv,pheno_surv,weather_surv,sheep_data,dataSurvUnscld,dataSurvScld,fullSurvDataScled)

# True repro
# Download RData from drive
drive_download("OWPC/Analyses/cache/dataFecundityModels.RData",overwrite=T)
# Import in R environment
load("dataFecundityModels.RData")
# Rename scaled data frame and remove years with missing data for climate
df_fec<-droplevels(subset(dataFecScld,!(yr %in% c("1999","2000","2001"))))
# Remove unnecessary objects for the environment
rm(clim_fec,pheno_fec,weather_fec,sheep_data,dataFecUnscld,dataFecScld,fullFecDataScld)

# Final models ------------------------------------------------------------------------------------------------------------
finalSurv <- glm(alive_t1 ~ -1 + ageClass/WinSnowsurvT1 + pred, data=df_surv, family="binomial")

finalTrue <- glmer(true_repro ~ -1 + ageClass/(TWin*PWin) + MassAutumn_tm1 + (1|ID),
                   data=df_fec, family="binomial", control = glmerControl(optimizer="bobyqa",
                   optCtrl = list(maxfun = 2000000)))


# prediction figures survival  -----------------------------------------------------
load("final_surv.Rdata")
# alwways good to check model output before predicting 
summary(finalSurv) # choose affected age classes 

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

plot_surv <- ggplot(newd1, aes(WinSnowsurvT1, fit)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill=factor(ageClass)), alpha = 0.3) +
  geom_line(aes(y = fit, colour=factor(ageClass)), size=1.2) + 
 geom_jitter(data = df_surv[df_surv$ageClass %in% c("1","8"),], aes(x = WinSnowsurvT1, y = as.numeric(alive_t1)-1), 
             size=1, alpha = 0.5, height = 0.02, width = 0.05) +
  labs(x="Number of days with snow (std)", 
       y="Survival probability to one year") 
plot_surv = plot_surv + 
  scale_colour_manual(values =c("#00BFC4", "#F8766D"),
                      labels=c("Age class 1", "Age class 8")) +
  scale_fill_manual(values =c("#00BFC4", "#F8766D"),
                    labels=c("Age class 1", "Age class 8")) + 
  guides(color = FALSE, linetype = FALSE, fill= FALSE) + 
  guides(color=guide_legend(title="Age class")) + 
  theme_pander() +
  theme(legend.position = c(0.5, 0.3)) +# coordonnées x-y de ton graph + 
  theme(axis.text=element_text(size=10)) +
  theme(axis.title = element_text(size =10))

plot_surv

# control variable : pred 
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
  geom_pointrange(size = 1, linetype = 1) +
  labs(x=expression('Predation')) + 
  labs(y="Survival probability to one year") +  
  scale_x_discrete(labels= x.labels) +  theme_pander() +
  theme(legend.position = c(0.5, 0.3)) # coordonnées x-y de ton graph + 
theme(axis.text=element_text(size=10)) +
  theme(axis.title = element_text(size =10))
  #geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.05)
predation
# panel 

p <- plot_grid(plot_surv, 
               predation,
                labels = c("A", "B"), 
                align = "vh", 
                ncol = 2)
getwd()
save_plot("panelSurvModels.png", p,
          ncol = 2, # 
          nrow = 1, # 
          base_aspect_ratio = 1.2 # carré, rectangle ? 
)






# prediction figure for final true fecundity  ------------------------------------------------------

load("final_true_models.Rdata")

finalTrue <- glmer(true_repro ~ -1 + ageClass/(TWin * PWin) + MassAutumn_tm1 + (1 |ID), 
                   df_fec, 
                   family = "binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000)))
summary(finalTrue)

# inv.logit(0.78639) =0.6870557

# show interaction between TWin and PWin 
# choose value for 2nd term of interaction 
summary(df_fec$PWin) 
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1.79221 -0.74019  0.07058  0.06414  0.87347  2.26387 
summary(df_fec$TWin) 



# build dataframe
newd<- data.frame()
newdata = expand.grid(TWin  = seq(min(df_fec$TWin , na.rm = T),
                                      max(df_fec$TWin, na.rm = T), length = 50), 
                      ageClass = "48", 
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      PWin  = c(-0.74019, 0.07058, 0.87347), # quartiles.. 
                      ID = c("A50", "E13", "I6",  "L2",  "M7")) # assign random ID from real dataset
newd3 <- rbind(newd, newdata)

# generate distn of prediction and extract CI 
myfun <- function(x) predict(x,newdata=newd3,type="link",re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 500, verbose = T)

boo <- data.frame(boo)
str(boo)

newd3$predi <- apply(boo, 2, mean) # removed boo$t because not in seq of x anymore
newd3$upr<- apply(boo, 2, function(x) quantile(x, 0.975))
newd3$lwr <- apply(boo, 2, function(x) quantile(x, 0.025))


# make figure with interaction 
df_fec$binPWin <- cut(df_fec$PWin, 3, labels = c(-0.74019, 0.07058, 0.87347))
table(df_fec$binPWin)
table(df_fec$true_repro)


plot_true <- ggplot(newd3, aes(TWin, inv.logit(predi))) + 
  geom_ribbon(aes(ymin = inv.logit(lwr), ymax = inv.logit(upr),
                  fill=factor(PWin)), alpha=0.2) + 
  geom_line(aes(y = inv.logit(predi),  linetype = factor(PWin), colour=factor(PWin),
                fill=factor(PWin)), size=1.2 ) + 
 geom_jitter(data = df_fec[df_fec$ageClass %in% "48",], aes(x = TWin, y = as.numeric(true_repro)-1, color = factor(binPWin)), 
           size=1, alpha = 0.5, height = 0.02, width = 0.05) +
  labs(x="Winter temperature (std)", 
       y="Probability to have a viable lamb") +
  theme_pander() + 
  theme(legend.position = c(0.5, 0.3)) # coordonnées x-y de ton graph + 
theme(axis.text=element_text(size=10)) +
  theme(axis.title = element_text(size =10))

plot_true = plot_true + 
  scale_colour_manual(values =c("#00BFC4", "#F8766D", "gray40"),
                      labels=c(expression(paste(1^st, " quartile")), "Median", expression(paste(3^rd, " quartile")))) +
  scale_fill_manual(values =c("#00BFC4", "#F8766D", "gray40"),
                    labels=c(expression(paste(1^st, " quartile")), "Median", expression(paste(3^rd, " quartile")))) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed"),
                        labels=c(expression(paste(1^st, " quartile")), "Median", expression(paste(3^rd, " quartile")))) + 
  guides(color = FALSE, linetype = FALSE, fill= FALSE) + 
  guides(linetype=guide_legend(title="Precipitation"))
plot_true

# control variables : mass 
newd<- data.frame()
newdata = expand.grid(MassAutumn_tm1  = seq(min(df_fec$MassAutumn_tm1 , na.rm = T),
                                            max(df_fec$MassAutumn_tm1, na.rm = T), length = 50), 
                      ageClass = "48", 
                      TWin = mean(df_fec$TWin, na.rm = T),
                      PWin = mean(df_fec$PWin, na.rm = T), 
                      ID = c("A50", "E13", "I6",  "L2",  "M7")) # assign random ID from real dataset
newd4 <- rbind(newd, newdata)

# generate distn of prediction and extract CI 
myfun <- function(x) predict(x,newdata=newd4,type="link",re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 500, verbose = T)

boo <- data.frame(boo)
str(boo)

newd4$predi <- apply(boo, 2, mean) # removed boo$t because not in seq of x anymore
newd4$upr<- apply(boo, 2, function(x) quantile(x, 0.975))
newd4$lwr <- apply(boo, 2, function(x) quantile(x, 0.025))# prediction


mumMass<- ggplot(newd4, aes(y=inv.logit(predi), x = MassAutumn_tm1)) +
  geom_ribbon(aes(ymin = inv.logit(lwr), ymax = inv.logit(upr)),  colour=NA, fill = '#00BFC4', alpha=0.2) + 
  geom_line(colour = '#00BFC4') + 
 geom_jitter(data=df_fec, aes(y = as.numeric(true_repro)-1, x=MassAutumn_tm1), size = 1, alpha = .5, height = 0.02, colour = '#00BFC4') + 
  labs(x=expression('Autumn mass' [t-1])) + 
  labs(y=expression('Probability to have a viable lamb' [t])) +  
  theme(legend.title=element_blank(), legend.position = c(0.3, 0.4)) +
  theme(axis.text=element_text(size=10), 
        axis.title = element_text(size =10),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  theme_pander() 
print(mumMass) 



# panel
q <- plot_grid (plot_true, 
                mumMass,
                labels = c("A", "B"), 
                align = "vh", 
                ncol = 2)
getwd()
save_plot("panelTrueModels.png", q,
          ncol = 2, # 
          nrow = 1, # 
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.2 # carré, rectangle ? 
)



# SAVE OBJECT FIGURES -----------------------------------------------------

save(newd, newd2, newd3,newd4, df_fec, df_surv,finalSurv, finalTrue, p,q,
     file = "FIGS2-3.RData")
