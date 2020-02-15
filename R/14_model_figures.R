
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
#install.packages("pander")
library(pander)

load("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/cache/df_weather.Rdata") # was on the drive with exact same name 

# Surv
mod.surv$base <- glm(alive_t1 ~ -1 + ageClass +  pred, 
                  data=df_surv, 
                  family="binomial") 

mod.surv$T.Spring <- glm(alive_t1 ~ -1 + ageClass/T.SPRING +  pred, 
                      data=df_surv, 
                      family="binomial")

mod.surv$T.Fall <- glm(alive_t1 ~ -1 + ageClass/T.FALL +  pred, 
                    data=df_surv, 
                    family="binomial")

# Raw repro
mod.raw$P.T.WIN.m1 <-glmer(raw_repro ~ -1 + ageClass/P.WIN.m1 +ageClass/T.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000)))

# True repro
mod.true$P.T.WIN.m1 <-glmer(true_repro ~ -1 + ageClass/P.WIN.m1 +ageClass/T.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000)))


# create survival figures -------------------------------------------------
df_surv$ID <- droplevels(df_surv$ID)

# alwways good to check model output before predicting 
summary(mod.surv$T.Spring) # choose affected age classes 

newd <- data.frame()
newdata <- expand.grid(T.SPRING = seq(min(df_surv$T.SPRING, na.rm = T),
                                     max(df_surv$T.SPRING, na.rm = T), length = 200),
                       pred = "0", 
                       ageClass = c("1", "8")) # generate df with 400 observations
newd <- rbind(newd,newdata)

# fit this dataframe true model estimates using predict
preds <- predict(mod.surv$T.Spring, newdata=newd,se.fit = T, type = "link")

# get confidence intervals manually
critval <- 1.96
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

# now need to report on biological scale 
fit2 <- mod.surv$T.Spring$family$linkinv(fit)
upr2 <- mod.surv$T.Spring$family$linkinv(upr)
lwr2 <- mod.surv$T.Spring$family$linkinv(lwr)

newd$lwr <- lwr2 
newd$upr <- upr2 
newd$fit <- fit2

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

## fall model
mod.surv$T.Fall <- glm(alive_t1 ~ -1 + ageClass/T.FALL +  pred, 
                       data=df_surv, 
                       family="binomial")
summary(mod.surv$T.Fall )
newd <- data.frame()
newdata <- expand.grid(T.FALL = seq(min(df_surv$T.FALL, na.rm = T),
                                      max(df_surv$T.FALL, na.rm = T), length = 200),
                       pred = "0", 
                       ageClass = "8") # generate df with 400 observations
newd <- rbind(newd,newdata)

# fit this dataframe true model estimates using predict
preds <- predict(mod.surv$T.Fall, newdata=newd,se.fit = T, type = "link") # fit le SE pour calculer le CI

# get confidence intervals manually
critval <- 1.96
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

# now need to report on biological scale 
fit2 <- mod.surv$T.Spring$family$linkinv(fit)
upr2 <- mod.surv$T.Spring$family$linkinv(upr)
lwr2 <- mod.surv$T.Spring$family$linkinv(lwr)

newd$lwr <- lwr2 
newd$upr <- upr2 
newd$fit <- fit2

p_surv_fall <- ggplot(newd, aes(T.FALL, y=fit, group = 1)) + 
  geom_line(linetype = "dotted") + # only have one age class
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, fill = 'navyblue') +  
  geom_point(data = df_surv, aes(x = T.FALL, y = as.numeric(alive_t1)-1)) + # pour mettre la distribution des points brutes 
  labs(x="Fall temperature (std)", 
       y="Survival probability to one year") +
  theme_pander() 
p_surv_fall



# fecundity I models --------------------------------------------------------
summary(mod.raw$P.T.WIN.m1)
newd<- data.frame()
newdata = expand.grid(T.WIN.m1  = seq(min(df_fec$T.WIN.m1 , na.rm = T),
                                      max(df_fec$T.WIN.m1 , na.rm = T), length = 50), # la variable d'intérêt
                      ageClass = "37", 
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      P.WIN.m1  = mean(df_fec$P.WIN.m1, na.rm = T), 
                      ID = c("A50", "E13", "I6",  "L2",  "M7"))
newd <- rbind(newd, newdata)
tmp = subset(df_fec, ageClass %in% 8) 
sample(tmp$ID, 5)

# now in glmer, we can generate distribution of estimates and extract CI
# bootMer travaille en fonction - lui en spécifier une qui extrait nos coefficients de modèle avec predict 
myfun <- function(x) predict(x,newdata=newd,type="link",re.form=NA)
boo <- bootMer(mod.raw$P.T.WIN.m1, myfun, nsim = 100, verbose = T) # increase if necessary - could be more

boo <- data.frame(boo)
str(boo)

newd$predi <- apply(boo, 2, mean) 
newd$upr<- apply(boo, 2, function(x) quantile(x, 0.975))
newd$lwr <- apply(boo, 2, function(x) quantile(x, 0.025))

# check if make sense
summary(mod.raw$P.T.WIN.m1)
#inv.logit()

raw_fec<- ggplot(newd, aes(T.WIN.m1, y=inv.logit(predi), group = 1)) + 
  geom_line(linetype = "dotted") + # only have one age class
  geom_ribbon(aes(ymin = inv.logit(lwr), ymax = inv.logit(upr)), alpha = 0.3, fill = 'navyblue') +  
  geom_point(data = df_surv, aes(x = T.WIN.m1, y = as.numeric(alive_t1)-1)) + # pour mettre la distribution des points brutes 
  labs(x=expression('Winter temperature (std)' [t-1]), 
       y="Probability to reproduce") +
  theme_pander() 
raw_fec

# fecundity II model ------------------------------------------------------
summary(mod.true$P.T.WIN.m1)
newd<- data.frame()
newdata = expand.grid(T.WIN.m1  = seq(min(df_fec$T.WIN.m1 , na.rm = T),
                                      max(df_fec$T.WIN.m1 , na.rm = T), length = 50), # la variable d'intérêt
                      ageClass = "37", 
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      P.WIN.m1  = mean(df_fec$P.WIN.m1, na.rm = T), 
                      ID = c("A50", "E13", "I6",  "L2",  "M7"))
newd <- rbind(newd, newdata)
tmp = subset(df_fec, ageClass %in% 8) 
sample(tmp$ID, 5)

# now in glmer, we can generate distribution of estimates and extract CI
# bootMer travaille en fonction - lui en spécifier une qui extrait nos coefficients de modèle avec predict 
myfun <- function(x) predict(x,newdata=newd,type="link",re.form=NA)
boo <- bootMer(mod.true$P.T.WIN.m1, myfun, nsim = 100, verbose = T)

boo <- data.frame(boo)
str(boo)

newd$predi <- apply(boo, 2, mean) # removed boo$t because not in seq of x anymore
newd$upr<- apply(boo, 2, function(x) quantile(x, 0.975))
newd$lwr <- apply(boo, 2, function(x) quantile(x, 0.025))

# check if make sense
summary(mod.true$P.T.WIN.m1)
#inv.logit()

surv_tmp <- ggplot(newd, aes(T.WIN.m1, y=inv.logit(predi), group = 1)) + 
  geom_line(linetype = "dotted") + # only have one age class
  geom_ribbon(aes(ymin = inv.logit(lwr), ymax = inv.logit(upr)), alpha = 0.3, fill = 'navyblue') +  
  geom_point(data = df_surv, aes(x = T.WIN.m1, y = as.numeric(alive_t1)-1)) + # pour mettre la distribution des points brutes 
  labs(x=expression('Winter temperature (std)' [t-1]), 
       y="Probability to have a viable lamb") +
  theme_pander() 
surv_tmp



