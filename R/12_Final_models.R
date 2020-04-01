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
#install.packages("pander")
library(pander)


# climate models  ---------------------------------------------------------


# surv
load("surv_clim.Rdata")
aictab(mod.surv)

mod.surv$base
glm(formula = alive_t1 ~ -1 + ageClass + pred, family = "binomial", 
    data = df_surv)

mod.surv$winter.tm1.int
glm(formula = alive_t1 ~ -1 + ageClass/PDOSOI_winter_tm1 + pred, 
    family = "binomial", data = df_surv)





# raw
load("raw.repro_clim.Rdata")
aictab(mod.raw.repro)


mod.raw.repro$springPDOSOI
raw_repro ~ -1 + ageClass/PDOSpringFec + ageClass/SOISpringFec +      MassAutumn_tm1 + (1 | ID)



# true 
load("true.repro_clim.Rdata")
aictab(mod.true.repro)


mod.true.repro$base
true_repro ~ -1 + ageClass + MassAutumn_tm1 + (1 | ID)

mod.true.repro$winterPDOSOI
true_repro ~ -1 + ageClass/PDOWinterFec + ageClass/SOIWinterFec +      MassAutumn_tm1 + (1 | ID)
Data: df_fec




# pheno models ------------------------------------------------------------


# surv
load("surv_pheno.Rdata") 
aictab(mod.surv) 

# three equivalent models including base 
mod.surv$base
glm(formula = alive_t1 ~ -1 + ageClass + pred, family = "binomial", 
    data = df_surv)

mod.surv$pc2tim
Call:  glm(formula = alive_t1 ~ -1 + ageClass/PC2Tim + pred, family = "binomial", 
           data = df_surv)

mod.surv$pc2
Call:  glm(formula = alive_t1 ~ -1 + ageClass/PC2 + pred, family = "binomial", 
           data = df_surv)




# raw
load("raw.repro_pheno.Rdata")
aictab(mod.raw.repro)


mod.raw.repro$base
raw_repro ~ -1 + ageClass + MassAutumn_tm1 + (1 | ID)
Data: df_fec

mod.raw.repro$SummerNDVI
raw_repro ~ -1 + ageClass/SummerNDVIfec + MassAutumn_tm1 + (1 |      ID)
Data: df_fec




# true
load("true.repro_pheno.Rdata")
aictab(mod.true.repro)


mod.true.repro$pc2tim
Formula: true_repro ~ -1 + ageClass/PC2Tim + MassAutumn_tm1 + (1 | ID)
Data: df_fec




# weather models  ---------------------------------------------------------



# surv
load("surv_weather.Rdata")
aictab(mod.surv) 


# 2 similar models 
mod.surv$base 
glm(formula = alive_t1 ~ -1 + ageClass + pred, family = "binomial", 
    data = df_surv)


mod.surv$T.Fall
glm(formula = alive_t1 ~ -1 + ageClass/T.FALL + pred, family = "binomial", 
           data = df_surv)



# raw 
load("raw.repro_weather.Rdata")
aictab(mod.raw.repro)


mod.raw.repro$TWin
raw_repro ~ -1 + ageClass/TWin + MassAutumn_tm1 + (1 | ID)
Data: df_fec




# true 
load("true.repro_weather.Rdata")
aictab(mod.true.repro)



mod.true.repro$TxPWin
true_repro ~ -1 + ageClass/(TWin * PWin) + MassAutumn_tm1 + (1 |      ID)
Data: df_fec



# final model selection SURVIVAL ---------------------------------------------------
mod.surv <- list()

mod.surv$base <- glm(alive_t1 ~ -1 + ageClass + pred, 
                     family = "binomial",
                     df_surv)

mod.surv$winter.tm1.int <- glm(alive_t1 ~ -1 + ageClass/PDOSOI_winter_tm1 + pred, 
                               family = "binomial", 
                              df_surv)

mod.surv$pc2tim <- glm(alive_t1 ~ -1 + ageClass/PC2Tim + pred, 
                       family = "binomial",
                       df_surv)

mod.surv$pc2 <- glm(alive_t1 ~ -1 + ageClass/PC2 + pred,
                    family = "binomial", 
                    df_surv)

mod.surv$T.Fall <- glm(alive_t1 ~ -1 + ageClass/T.FALL + pred, 
                       family = "binomial", 
                       data = df_surv)

# does not converge 
mod.surv$combined <- glm(alive_t1 ~ -1 + ageClass + pred + ageClass/PDOSOI_winter_tm1 + 
                           ageClass/PC2Tim + ageClass/PC2 + ageClass/T.FALL, 
                         family = "binomial", 
                         df_surv)

# extract results 
results.surv<-list()
results.surv$aictable.surv <- xtable(aictab(mod.surv), caption = NULL, label = NULL, align = NULL,
                                     digits = NULL, display = NULL, nice.names = TRUE,
                                     include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.surv$aictable.surv[,3:6] <-round(results.surv[["aictable.surv"]][,3:6],digits=3)

results.surv$coefs.surv.best <- data.frame(coef(summary(mod.surv[[as.character(results.surv[["aictable.surv"]][1,1])]])))
results.surv$coefs.surv.best[, 1:4] <- round(results.surv[["coefs.surv.best"]][, 1:4], digits = 3)
results.surv$r2.surv.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.surv[[as.character(results.surv[["aictable.surv"]][1,1])]]), digits = 3))

# only one best 

save(df_surv,mod.surv,results.surv,file = "final_surv.Rdata")

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

# final model selection RAW -----------------------------------------------

mod.raw <- list()

mod.raw$base <- glmer(raw_repro ~ -1 + ageClass + MassAutumn_tm1 + (1 | ID), 
                      family = "binomial", 
                      df_fec, 
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000)))

mod.raw$springPDOSOI <- glmer(raw_repro ~ -1 + ageClass/PDOSpringFec + ageClass/SOISpringFec + MassAutumn_tm1 + (1 | ID),
                                    family = "binomial",
                                    df_fec,
                                    control = glmerControl(optimizer="bobyqa",
                                                           optCtrl = list(maxfun = 4000000)))

mod.raw$SummerNDVI <- glmer(raw_repro ~ -1 + ageClass/SummerNDVIfec + MassAutumn_tm1 + (1 |ID), 
                            family = "binomial", 
                            df_fec, 
                            control = glmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 2000000)))
mod.raw$TWin <- glmer(raw_repro ~ -1 + ageClass/TWin + MassAutumn_tm1 + (1 | ID), 
                      family = "binomial", 
                      df_fec, 
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000)))

# mod.raw$combined <- glmer(raw_repro ~ -1 + ageClass/PDOSpringFec + ageClass/SOISpringFec + 
#                           ageClass/SummerNDVIfec +
#                           ageClass/TWin + 
#                           MassAutumn_tm1 + (1 | ID),
#                           family = "binomial", 
#                           df_fec, 
#                           control = glmerControl(optimizer="bobyqa", 
#                                                  optCtrl = list(maxfun = 2000000)))

# Creating a list to store the results
results.raw.repro<-list()
results.raw.repro$aictable.raw.repro <- xtable(aictab(mod.raw), caption = NULL, label = NULL, align = NULL,
                                               digits = NULL, display = NULL, nice.names = TRUE,
                                               include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.raw.repro$aictable.raw.repro[,3:6] <-round(results.raw.repro[["aictable.raw.repro"]][,3:6],digits=3)

results.raw.repro$coefs.raw.repro.best <- data.frame(coef(summary(mod.raw[[as.character(results.raw.repro[["aictable.raw.repro"]][1,1])]])))
results.raw.repro$coefs.raw.repro.best[, 1:4] <- round(results.raw.repro[["coefs.raw.repro.best"]][, 1:4], digits = 3)
results.raw.repro$r2.raw.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.raw[[as.character(results.raw.repro[["aictable.raw.repro"]][1,1])]]), digits = 3))

# Option to create and save RData file with data, candidate models and results
save(df_fec,mod.raw,results.raw.repro,file = "final_raw.Rdata")


kable(results.raw.repro$aictable.raw.repro) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "FinalAicRaw.html", self_contained = T) 

kable(results.raw.repro$coefs.raw.repro.best[, 1:4]) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "FinalRawCoef.html", self_contained = T) 



# final model selection TRUE ----------------------------------------------
mod.true<- list()

mod.true$base <- glmer(true_repro ~ -1 + ageClass + MassAutumn_tm1 + (1 | ID), 
                       df_fec, 
                       family = "binomial",
                       control = glmerControl(optimizer="bobyqa", 
                                              optCtrl = list(maxfun = 2000000)))

mod.true$winterPDOSOI <- glmer(true_repro ~ -1 + ageClass/PDOWinterFec + ageClass/SOIWinterFec +MassAutumn_tm1 + (1 | ID), 
                               df_fec, 
                               family = "binomial",
                               control = glmerControl(optimizer="bobyqa", 
                                                              optCtrl = list(maxfun = 2000000)))

mod.true$pc2tim <- glmer( true_repro ~ -1 + ageClass/PC2Tim + MassAutumn_tm1 + (1 | ID), 
                          df_fec, 
                          family = "binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))

mod.true$TxPWin <- glmer(true_repro ~ -1 + ageClass/(TWin * PWin) + MassAutumn_tm1 + (1 |ID), 
                         df_fec, 
                         family = "binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000)))

mod.true$combined <- glmer(true_repro ~ -1 + ageClass + ageClass/PDOWinterFec + ageClass/SOIWinterFec +
                             ageClass/PC2Tim + 
                             ageClass/(TWin * PWin)+ MassAutumn_tm1 + (1 | ID),
                           family = "binomial",
                           df_fec, 
                           control = glmerControl(optimizer="bobyqa", 
                                               optCtrl = list(maxfun = 2000000)))


# Creating a list to store the results
results.true.repro<-list()
results.true.repro$aictable.true.repro <- xtable(aictab(mod.true), caption = NULL, label = NULL, align = NULL,
                                                 digits = NULL, display = NULL, nice.names = TRUE,
                                                 include.AICc = TRUE, include.LL = TRUE, include.Cum.Wt = FALSE)

results.true.repro$aictable.true.repro[,3:6] <-round(results.true.repro[["aictable.true.repro"]][,3:6],digits=3)

results.true.repro$coefs.true.repro.best <- data.frame(coef(summary(mod.true[[as.character(results.true.repro[["aictable.true.repro"]][1,1])]])))
results.true.repro$coefs.true.repro.best[, 1:4] <- round(results.true.repro[["coefs.true.repro.best"]][, 1:4], digits = 3)
results.true.repro$r2.true.repro.best<-data.frame(round(MuMIn::r.squaredGLMM(mod.true[[as.character(results.true.repro[["aictable.true.repro"]][1,1])]]), digits = 3))


# Option to create and save RData file with data, candidate models and results
save(df_fec,mod.true,results.true.repro,file = "final_true_models.Rdata")

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

# new final  models -----------------------------------------------

finalSurv <- glm(alive_t1 ~ -1 + ageClass/PC2Tim + pred, 
                       family = "binomial",
                       df_surv)# base equivalent to pc2tim

finalRaw <- glmer(raw_repro ~ -1 + ageClass/TWin + MassAutumn_tm1 + (1 | ID), 
                      family = "binomial", 
                      df_fec, 
                      control = glmerControl(optimizer="bobyqa", 
                                             optCtrl = list(maxfun = 2000000)))

finalTrue <- glmer(true_repro ~ -1 + ageClass/(TWin * PWin) + MassAutumn_tm1 + (1 |ID), 
                         df_fec, 
                         family = "binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000)))

# OLD 
# modfinal$combined <- glmer(raw_repro ~ -1 + ageClass/PDO.spring + ageClass/T.WIN.m1 + ageClass/P.WIN.m1 + MassAutumn_tm1 + (1|ID), 
#                           data=df, 
#                           family="binomial",
#                           control = glmerControl(optimizer="bobyqa", 
#                                                  optCtrl = list(maxfun = 2000000)))
# mod.final$clim.win <- glmer(true_repro ~ -1 + ageClass/PDO.winter + ageClass/SOI.winter + MassAutumn_tm1 + (1|ID), 
#                            data=df, 
#                            family="binomial",
#                            control = glmerControl(optimizer="bobyqa", 
#                                                   optCtrl = list(maxfun = 2000000))) 



# prediction figures survival  -----------------------------------------------------

load("final_surv.Rdata")
finalSurv <- glm(alive_t1 ~ -1 + ageClass/PC2Tim + pred, 
                 family = "binomial",
                 df_surv)# base equivalent to pc2tim


# alwways good to check model output before predicting 
summary(finalSurv) # choose affected age classes 

# show effect of PC2Tim
newd <- data.frame()
newdata <- expand.grid(PC2Tim = seq(min(df_surv$PC2Tim, na.rm = T),
                                      max(df_surv$PC2Tim, na.rm = T), length = 200),
                       pred = "0", 
                       ageClass = "1") # generate df with observations
newd <- rbind(newd,newdata)

# fit this dataframe true model estimates using predict
preds <- predict(finalSurv, newdata=newd,se.fit = T, type = "link")

# get confidence intervals manually
critval <- 1.96
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

# now need to report on biological scale 
fit2 <- finalSurv$family$linkinv(fit)
upr2 <- finalSurv$family$linkinv(upr)
lwr2 <- finalSurv$family$linkinv(lwr)

newd$lwr <- lwr2 
newd$upr <- upr2 
newd$fit <- fit2

plot_surv <- ggplot(newd, aes(PC2Tim, y=fit, group= ageClass)) +  
  geom_line(aes(linetype = ageClass)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, fill = 'navyblue') +
  geom_point(data = df_surv, aes(x = PC2Tim, y = as.numeric(alive_t1)-1)) + # pour mettre la distribution des points brutes 
  labs(x="Index of green-up (std)", 
       y="Survival probability to one year") +
  theme_pander() + 
  theme(legend.position = c(0.3, 0.4)) # coordonnées x-y de ton graph
plot_surv <- plot_surv  + guides(linetype=guide_legend(title="Age class")) # here linetype is what age class is called in aesthetics
print(plot_surv)

getwd()
ggsave("survivalPC2Tim.pdf", width = 110, height = 130, units = "mm", pointsize = 8)

# control variable : pred 















# prediction figures raw fecundity --------------------------------------------------------
load("final_raw.Rdata")
finalRaw <- glmer(raw_repro ~ -1 + ageClass/TWin + MassAutumn_tm1 + (1 | ID), 
                  family = "binomial", 
                  df_fec, 
                  control = glmerControl(optimizer="bobyqa", 
                                         optCtrl = list(maxfun = 2000000)))

summary(finalRaw)
newd<- data.frame()
newdata = expand.grid(TWin  = seq(min(df_fec$TWin , na.rm = T),
                                      max(df_fec$TWin , na.rm = T), length = 50), # la variable d'intérêt
                      ageClass = "9", 
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      ID = c("A50", "E13", "I6",  "L2",  "M7"))
newd <- rbind(newd, newdata)
# tmp = subset(df_fec, ageClass %in% 8) 
# sample(tmp$ID, 5)

# now in glmer, we can generate distribution of estimates and extract CI
# bootMer travaille en fonction - lui en spécifier une qui extrait nos coefficients de modèle avec predict 
myfun <- function(x) predict(x,newdata=newd,type="link",re.form=NA)
boo <- bootMer(finalRaw, myfun, nsim = 100, verbose = T) # increase nsim if necessary - could be more

# warnigns of problems of convergence so exclude NAs
boo <- na.omit(data.frame(boo))
str(boo)

newd$predi <- apply(boo, 2, mean) 
newd$upr<- apply(boo, 2, function(x) quantile(x, 0.975))
newd$lwr <- apply(boo, 2, function(x) quantile(x, 0.025))

# check if makes sense
summary(finalRaw)
#inv.logit()


# show effect for two different age classes by adding group
plot_raw<- ggplot(newd, aes(TWin, y=inv.logit(predi), group = ageClass)) + 
  geom_line(linetype = c("dotted", "solid")) + 
  geom_ribbon(aes(ymin = inv.logit(lwr), ymax = inv.logit(upr)), alpha = 0.3, fill = 'navyblue') +  
  geom_point(data = df_fec, aes(x = TWin, y = as.numeric(as.character(raw_repro))), alpha = 0.5) + # pour mettre la distribution des points brutes 
  labs(x=expression('Winter temperature (std)' [t-1]), 
       y="Probability to reproduce") +
  theme_pander() 

# only one age class
ggplot(newd, aes(TWin, y=inv.logit(predi), group = 1)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = inv.logit(lwr), ymax = inv.logit(upr)), alpha = 0.3, fill = 'navyblue') +  
  geom_point(data = df_fec, aes(x = TWin, y = as.numeric(as.character(raw_repro))), alpha = 0.5) + # pour mettre la distribution des points brutes 
  labs(x=expression('Winter temperature (std)' [t-1]), 
       y="Probability to reproduce") +
  theme_pander() 



p_spring <- ggplot(newd, aes(T.SPRING, y=fit, group= ageClass)) +  
  geom_line(aes(linetype = ageClass)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, fill = 'navyblue') +
  geom_point(data = df_surv, aes(x = T.FALL, y = as.numeric(alive_t1)-1)) + # pour mettre la distribution des points brutes 
  labs(x="Spring temperature (std)", 
       y="Survival probability to one year") +
  theme_pander() + 
  theme(legend.position = c(0.5, 0.4)) # coordonnées x-y de ton graph
p_spring <- p_spring  + guides(linetype=guide_legend(title="Age class")) # here linetype is what age class is called in aesthetics
print(p_spring)




ggsave("rawTWin.pdf", width = 110, height = 130, units = "mm", pointsize = 8)


# control variables 














# prediction figure for final true fecundity  ------------------------------------------------------

load("final_true_models.Rdata")

finalTrue <- glmer(true_repro ~ -1 + ageClass/(TWin * PWin) + MassAutumn_tm1 + (1 |ID), 
                   df_fec, 
                   family = "binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000)))

summary(finalTrue)

# inv.logit(0.761) = 0.6815708

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
                      PWin  = c(-0.74019, 0.07058, 0.87347), # lui choisir des valeurs 
                      ID = c("A50", "E13", "I6",  "L2",  "M7")) # assign random ID from real dataset
newd <- rbind(newd, newdata)



# now in glmer, we can generate distribution of estimates and extract CI
# bootMer travaille en fonction - lui specifier d'extraire nos coefficients de modèle avec predict 
myfun <- function(x) predict(x,newdata=newd,type="link",re.form=NA)
boo <- bootMer(finalTrue, myfun, nsim = 500, verbose = T)

boo <- data.frame(boo)
str(boo)

newd$predi <- apply(boo, 2, mean) # removed boo$t because not in seq of x anymore
newd$upr<- apply(boo, 2, function(x) quantile(x, 0.975))
newd$lwr <- apply(boo, 2, function(x) quantile(x, 0.025))


# make figure with interaction visible
plot_true <- ggplot(newd, aes(TWin, inv.logit(predi), colour=interaction(colour=factor(PWin), 
                                                               linetype = factor(PWin)), # change to factor?? 
                                 linetype=interaction(colour=factor(PWin), linetype = factor(PWin)), 
                                 fill=interaction(colour=factor(PWin), linetype = factor(PWin)))) + 
  geom_ribbon(aes(ymin = inv.logit(lwr), ymax = inv.logit(upr)), colour=NA, alpha=0.2) + 
  geom_line(aes(y = inv.logit(predi)), size=1.2) + 
  #geom_point(data = df_fec, aes(x = TWin, y = as.numeric(as.character(true_repro)))) + # pour mettre la distribution des points brutes 
  labs(x="Winter temperature (std)", 
       y="Probability to have a viable lamb") +
  theme_pander() 


plot_true = plot_true + 
  scale_colour_manual(values =c("blue", "orange", "gray40"),
                      labels=c(expression(paste(1^st, " quartile")), "Median", expression(paste(3^rd, " quartile")))) +
  scale_fill_manual(values =c("blue", "orange", "gray40"),
                    labels=c(expression(paste(1^st, " quartile")), "Median", expression(paste(3^rd, " quartile")))) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed"),
                        labels=c(expression(paste(1^st, " quartile")), "Median", expression(paste(3^rd, " quartile")))) + 
  guides(color = FALSE, linetype = FALSE, fill= FALSE) + 
  guides(linetype=guide_legend(title="Precipitation"))
plot_true

ggsave("trueTWin.pdf", width = 110, height = 130, units = "mm", pointsize = 8)










# control variables 










# plot grid 

# plot panel --------------------------------------------------------------
library(cowplot)

p <- plot_grid (plot_surv,
                plot_raw, 
                plot_true,
                labels = c("A", "B", "C"), 
                align = "vh")
getwd()
save_plot("graphs/panelFinalModels.pdf", p,
          ncol = 3, # 
          nrow = 1, # 
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)



# SAVE OBJECT FIGURES -----------------------------------------------------
#ALL BUT PREDATION ARE SAVED ; FUCKED UP 
save(newd, newd2, newd3, newd4, dt3, gpp_length_graph, snow_length, snowmelt_date, trend_neonatal, 
     file = "Graphs/Figures_1A-1C.RData")
