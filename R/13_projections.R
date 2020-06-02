# code to simulate demographic rates - lambda - under different scenarios of temperature and phenology 
# created by S. C. Beaumont 
# modified by L. Renaud # 2020-04-23
# modified by S. C. Beaumont  # 2020-06-01



library(plyr)
library(dplyr)
library(boot)
library(lme4)
library(popbio)
library(ggplot2)
library(pander)
library(cowplot)
rm(list = ls ())


# Load final databases 

load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Mai25/surv_clim.RData.Rdata")
rm(mod.surv.clim, results.surv.clim) # keep only df_surv

load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Mai25/true_repro_clim.RData.Rdata")
rm(mod.true.repro.clim,results.true.repro.clim) # keep only df_fec

# get best models from output
results.surv.clim
results.surv.pheno
results.surv.weat


results.true.repro.clim
results.true.repro.pheno
results.true.repro.weat


# recall best models structure and copy it
mod.surv.pheno$Snow_present

mod.true.repro.weat$Winter_PxT


# ===  FINAL MODELS  === # 

surv <- glm(alive_t1 ~ -1 + ageClass/WinSnowsurvT1 + pred,
            data=df_surv, family="binomial")


true <- glmer(true_repro ~ -1 + ageClass/(TWin*PWin) + MassAutumn_tm1 + (1|ID),data=df_fec,
              family="binomial", control = glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 200000)))


# Important variables : 
# FROM df_surv : WinSnowsurvT1
# FROM df_fec : TWin , PWin ,  MassAutumn_tm1



# ===  LESLIE FUNCTION  === # 

# extract model coefficients with uncertainty and generate NEW predicted leslie matrix 
# do matrix with true reproduction only ! 

newLeslie <- function(survm=surv, fec = true, env = newdata[1,]){ # here changed for fec = true
  # ceci sort les bêta des modèles 
  S = predict(survm, newdata = data.frame(ageClass = c("0","1","2","37","8"), env), type = "link", se.fit = T)
  myfun <- function(x) predict(x,newdata=data.frame(ageClass = c("3","48","9"), env),type="link",re.form=NA)
  F1 = as.numeric(bootMer(fec,myfun, nsim = 1)$t) # rajoute l'incetitude ; va juste chercher le chiffre qui nous intéresse dans t 
  S = rnorm(5, S$fit, S$se.fit)# rajoute de l'incertitude
  # ceci crée une matrice vide 
  L <- matrix(0, nrow=9, ncol=9)
  
  # survival is lamb overwinter survival * reproduction de la femelle /2 pour compter juste des bébés femelles 
  L[1,3] <- inv.logit(S[1]) * inv.logit(F1[1])*0.5 # doit fitter avec l'ordre des classes d'age ci-haut # la moitié des agneaux sont femelles 
  L[1,4] <- inv.logit(S[1]) * inv.logit(F1[2])*0.5 # same
  L[1,5] <- inv.logit(S[1]) * inv.logit(F1[2])*0.5
  L[1,6] <- inv.logit(S[1]) * inv.logit(F1[2])*0.5
  L[1,7] <- inv.logit(S[1]) * inv.logit(F1[2])*0.5
  L[1,8] <- inv.logit(S[1]) * inv.logit(F1[2])*0.5
  L[1,9] <- inv.logit(S[1]) * inv.logit(F1[3])*0.5
  
  
  L[2,1] <- inv.logit(S[2])
  L[3,2] <- inv.logit(S[3])    
  L[4,3] <- inv.logit(S[4]) 
  L[5,4] <- inv.logit(S[4])   
  L[6,5] <- inv.logit(S[4])    
  L[7,6] <- inv.logit(S[4])  
  L[8,7] <- inv.logit(S[4])   
  L[9,8] <- inv.logit(S[5])   
  L[9,9] <- inv.logit(S[5])  
  
  
  return(L)
}                                                                                                



# === PROJECTION DATAFRAMES (TEMPERATURE) === # 

# To construct future dataframe, need SD 

load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /dataProjectionsSURV.RData")
df_surv <-droplevels(subset(fullSurvDataScled,!(yr %in% c("1999","2000","2016"))))# n= 448

SD.surv <- data.SD


load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /dataProjectionsFEC.RData")
df_fec<-droplevels(subset(fullFecDataScld,!(yr %in% c("1999","2000","2001")))) # n= 274

SD.fec <- data.SD

# this is the data under climate change 

# ATTENTION use EACH VARIABLE'S SD nd add this SD to 1.5 so that everything is on the same SCALE



# this is the actual data 
# build fictive dataframes to predict lambda on 
# tricky : Temperature variable in df_fec isn't the same (time lag) than in df_surv. BE CAREFUL

# PRESENT DATA FRAME
env.now <- data.frame(TWin = mean(df_fec$TWin, na.rm = T), 
                      PWin = mean(df_fec$PWin, na.rm = T),
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T), # variables for reproduction model 
                      WinSnowsurvT1 = mean(df_surv$WinSnowsurvT1 , na.rm = T), # variables for survival model
                      pred = "0")                                                                                              

# FROM df_surv : WinSnowsurvT1
# FROM df_fec : TWin , PWin ,  MassAutumn_tm1                                                                                          


# Scaling is done like this :
# newd$snow_z<-(data.MEAN["SummerEVI"]- data.MEAN)/data.SD


# ex when you "unscale" a scaled variable you do this : 
# newd$snow<-(data.MEAN["SummerEVI"]*data.SD) + data.MEAN


# now we want to add 1.5 to TEMPERATURE ONLY on the scale of original variable, by dividing 1.5 by its SD (mean = 0)

# FUTURE DATAFRAME 

env.future <- data.frame(TWin = mean(df_fec$TWin, na.rm = T) + 1.5/SD.fec["TWin"], 
                         PWin = mean(df_fec$PWin, na.rm = T),
                         MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T), 
                         WinSnowsurvT1 = mean(df_surv$WinSnowsurvT1 , na.rm = T), # variables for survival model,
                         pred = "0")



# === PROJECTION (TEMPERATURE) === # 

# make TRUE fecondity 

lambda.true.now = 
  sapply(1:10000, function(it){
    bob = eigen.analysis(newLeslie(fec = true,env = env.now))
    return(bob$lambda1)
  })

hist(lambda.true.now)

lambda.true.future = 
  sapply(1:10000, function(it){ 
    bob = eigen.analysis(newLeslie(fec = true,env = env.future))
    return(bob$lambda1)
  })

hist(lambda.true.future)


lambda <- data.frame(lambda.true.now, 
                     lambda.true.future)                                                                                               


# projection figure FOR TEMPERATURE  -----------------------------------------------------------------

projectionTemp <- ggplot(lambda, aes(lambda.true.now, fill = "Current winter temperature")) + # le fill est transofrmé en facteur, donc le niveau devient en ordre alphab. 
  geom_density(alpha = 0.5) + 
  geom_density(aes(lambda.true.future, fill = "Warming of 1.5°C"), alpha = 0.5) + 
  labs(x=expression('Population growth rate'), 
       y="Density") +
  theme_cowplot(14) +
  theme(legend.title=element_blank(),
        legend.position = c(0.1, 0.9)) # removed legend title , give it coordinate
#guides(fill = guide_legend(override.aes = list(colour = NULL)),
#      color = guide_legend(override.aes = list(colour = NULL)))
projectionTemp






# projection figure FOR WINTER DURATION ----------------------------------------------------


# final models should be the same than previous projection (but updated)


# leslie function is the same 


# === PROJECTION DATAFRAMES (Winter duration) === # 

env.now <- data.frame(TWin = mean(df_fec$TWin, na.rm = T), 
                      PWin = mean(df_fec$PWin, na.rm = T),
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T), # variables for reproduction model 
                      WinSnowsurvT1 = mean(df_surv$WinSnowsurvT1 , na.rm = T), # variables for survival model
                      pred = "0")    

# 80  years (- 8.9 days per decade)

env.future <- data.frame(TWin = mean(df_fec$TWin, na.rm = T), 
                         PWin = mean(df_fec$PWin, na.rm = T),
                         MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T), # variables for reproduction model 
                         WinSnowsurvT1 = mean(df_surv$WinSnowsurvT1 , na.rm = T) - ( 71.1 / SD.surv["WinSnowsurvT1"]), # variables for survival model
                         pred = "0") 

# Sensitivity Test 1% 
env.future <- data.frame(TWin = mean(df_fec$TWin, na.rm = T), 
                         PWin = mean(df_fec$PWin, na.rm = T),
                         MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T), # variables for reproduction model 
                         WinSnowsurvT1 = mean(df_surv$WinSnowsurvT1 , na.rm = T) - 0.01 * mean(df_surv$WinSnowsurvT1 , na.rm = T) / SD.surv["WinSnowsurvT1"], # variables for survival model
                         pred = "0")  


# === PROJECTION (Winter duration) === # 

lambda.true.now = 
  sapply(1:10000, function(it){
    bob = eigen.analysis(newLeslie(fec = true,env = env.now))
    return(bob$lambda1)
  })
hist(lambda.true.now)

lambda.true.future = 
  sapply(1:10000, function(it){ 
    bob = eigen.analysis(newLeslie(fec = true,env = env.future))
    return(bob$lambda1)
  })
hist(lambda.true.future)


lambdaPheno <- data.frame(lambda.true.now, 
                          lambda.true.future)

# figure pheno -----------------------------------------------------------


library(scales)
show_col(hue_pal()(4))


projectionPheno <- ggplot(lambdaPheno, aes(lambda.true.now)) + 
  geom_density(alpha = 0.5, aes(fill = 'now'))+  # same as other fig 
  geom_density(aes(lambda.true.future, fill = "fut"), alpha = 0.5) + 
  labs(x=expression('Population growth rate'), 
       y="Density") +
  scale_fill_manual(breaks = c("now","fut"),values = c("#00BFC4",'#F8766D'),
                    labels=c("Current snow cover duration","71.1 days less snow cover"))+ # on force l'ordre de la key + couleur 
  theme_cowplot() +
  theme(legend.position = c(0.1, 0.9), 
        legend.title =element_blank())


# === Final Figure  === # 

p <- plot_grid (projectionTemp,
                projectionPheno, 
                labels = c("A", "B"), 
                align = "vh")
p

#save(lambdaPheno, file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Mai25/lambdaPheno.RData")
#save(projectionPheno,projectionTemp,file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Mai25/bothProjPlots.RData")
#save(lambda, file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Mai25/lambdaTemp.RData")

getwd()
save_plot("/Projections.pdf", p,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          base_aspect_ratio = 1.3
)


se <- function(x) sd(x)/sqrt(length(x))

mean(lambda[,1]) # lambda temperature present  = 1.088519
se(lambda[,1]) #  0.000233001
sd(lambda[,1]) # 0.0233001


mean(lambda[,2]) # lambda temperature future = 1.095101
se(lambda[,2]) # 0.0002327636
sd(lambda[,2]) # 0.02327636


mean(lambdaPheno[,1]) # lambda pheno present =  1.088665
se(lambdaPheno[,1]) # 0.0002369745
sd(lambdaPheno[,1]) # 0.02369745

mean(lambdaPheno[,2]) # lambda phene future = 0.9973135
se(lambdaPheno[,2]) # 0.000694024
sd(lambdaPheno[,2]) #0.0694024
