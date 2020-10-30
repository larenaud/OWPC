# code to simulate demographic rates - lambda - under different scenarios of temperature and phenology 
# created by S. C. Beaumont 
# modified by L. Renaud # 2020-04-23
# modified by S. C. Beaumont  # 2020-06-01
# modified by J Van de Walle # 2020-07-16
# modified by S. C. Beaumont  # 2020-07-20

####========================####
# Library
####========================####

library(plyr)
library(dplyr)
library(boot)
library(lme4)
library(popbio)
library(ggplot2)
library(pander)
library(cowplot)
library(boot)

rm(list = ls ())

####========================####
#Load Databases
####========================####

# Load final databases 

load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Mai25/surv_clim.RData.Rdata")
rm(mod.surv.clim, results.surv.clim) # keep only df_surv

load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Mai25/true_repro_clim.RData.Rdata")
rm(mod.true.repro.clim,results.true.repro.clim) # keep only df_fec


# Create dataframes of environmental conditions 
# To construct future dataframe, need SD 
load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /dataProjectionsSURV copie.RData")
df_surv <-droplevels(subset(fullSurvDataScled,!(yr %in% c("1999","2000","2016"))))# n= 448

meanSnow<- data.MEAN["WinSnowsurvT1"][[1]]

p5 <- 0.05*meanSnow
p10 <- 0.10*meanSnow
p20 <- 0.20*meanSnow
p30 <- 0.30*meanSnow


SD.surv <- data.SD


#load("C:/Users/joani/Documents/PhD/Labo/One Week Paper Challenge 2020/dataProjectionsFEC.RData")
load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /dataProjectionsFEC.RData")
df_fec<-droplevels(subset(fullFecDataScld,!(yr %in% c("1999","2000","2001")))) # n= 274

SD.fec <- data.SD

####========================####
# Env Databases
####========================####
# create dataframes of annual average environmental conditions
# For survival
env_annual_surv <- data.frame(
  year = as.numeric(as.character(unique(df_surv$yr))),
  WinSnowsurvT1 = c(1:length(unique(df_surv$yr)))
)

for(i in 1:length(unique(df_surv$yr))){
  df_surv$yr <- as.numeric(as.character(df_surv$yr)) 
  tmp <- df_surv[which(df_surv$yr == env_annual_surv[i, "year"]),]
  env_annual_surv$WinSnowsurvT1[i] <- unique(tmp$WinSnowsurvT1)
}

# For fecundity
env_annual_fec <- data.frame(
  year = as.numeric(as.character(unique(df_fec$yr))),
  TWin = c(1:length(unique(df_fec$yr))),
  PWin = c(1:length(unique(df_fec$yr)))
)

for(i in 1:length(unique(df_fec$yr))){
  df_fec$yr <- as.numeric(as.character(df_fec$yr)) 
  tmp <- df_fec[which(df_fec$yr == env_annual_fec[i, "year"]),]
  env_annual_fec$TWin[i] <- unique(tmp$TWin)
  env_annual_fec$PWin[i] <- unique(tmp$PWin)
}



env.now <- data.frame(TWin = mean(env_annual_fec$TWin), 
                      PWin = mean(env_annual_fec$PWin),
                      WinSnowsurvT1 = mean(env_annual_surv$WinSnowsurvT1) # variables for survival model,
)

#juste pour température
env.future <- data.frame(TWin = mean(env_annual_fec$TWin) + 1.5 / SD.fec["TWin"][[1]],  # Changer Temperature, influence juste repro
                         PWin = mean(env_annual_fec$PWin), #Changer TWin pour PWin
                         WinSnowsurvT1 = mean(env_annual_surv$WinSnowsurvT1)
                         # - (71.1/ SD.surv["WinSnowsurvT1"][[1]]) # variables for survival model,
)

env.future_jour <- data.frame(TWin = mean(env_annual_fec$TWin) + 1.5 / SD.fec["TWin"][[1]],  # Changer Temperature, influence juste repro
                              PWin = mean(env_annual_fec$PWin), #Changer TWin pour PWin
                              WinSnowsurvT1 = mean(env_annual_surv$WinSnowsurvT1) - (71.1/ SD.surv["WinSnowsurvT1"][[1]]) # variables for survival model,
)

# Pour Snow cover (5,10,20,30%)

env.future5 <- data.frame(TWin = mean(env_annual_fec$TWin) + 1.5 / SD.fec["TWin"][[1]],  # Changer Temperature, influence juste repro
                          PWin = mean(env_annual_fec$PWin), #Changer TWin pour PWin
                          WinSnowsurvT1  = mean(env_annual_surv$WinSnowsurvT1) - (p5/ SD.surv["WinSnowsurvT1"][[1]]) # variables for survival model,
)

#mean(env_annual_surv$WinSnowsurvT1)*SD.surv["WinSnowsurvT1"][[1]] + 


env.future10 <- data.frame(TWin = mean(env_annual_fec$TWin) + 1.5 / SD.fec["TWin"][[1]],  # Changer Temperature, influence juste repro
                           PWin = mean(env_annual_fec$PWin), #Changer TWin pour PWin
                           WinSnowsurvT1 =  mean(env_annual_surv$WinSnowsurvT1) - (p10/ SD.surv["WinSnowsurvT1"][[1]]) # variables for survival model,
)

env.future20 <- data.frame(TWin = mean(env_annual_fec$TWin) + 1.5 / SD.fec["TWin"][[1]],  # Changer Temperature, influence juste repro
                           PWin = mean(env_annual_fec$PWin), #Changer TWin pour PWin
                           WinSnowsurvT1 =  mean(env_annual_surv$WinSnowsurvT1) - (p20/ SD.surv["WinSnowsurvT1"][[1]]) # variables for survival model,
)

env.future30 <- data.frame(TWin = mean(env_annual_fec$TWin) + 1.5 / SD.fec["TWin"][[1]],  # Changer Temperature, influence juste repro
                           PWin = mean(env_annual_fec$PWin), #Changer TWin pour PWin
                           WinSnowsurvT1  = mean(env_annual_surv$WinSnowsurvT1) - (p30/ SD.surv["WinSnowsurvT1"][[1]]) # variables for survival model,
)


####========================####
# Final Models 
####========================####

## SURVIVAL
# I have removed predation from the models (JV)
surv <- glm(alive_t1 ~ -1 + ageClass/WinSnowsurvT1,
            data=df_surv, family="binomial")

## FECUNDITY
#Enlever masse JV
true <- glmer(true_repro ~ -1 + ageClass/(TWin*PWin) + (1|ID),data=df_fec,
              family="binomial", control = glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 200000)))



####========================####
# Bootstrap projections 
####========================####

niter <- 10000 # number of bootstraps


# +++++++ SURVIVAL ++++++++ # 

df_surv$rowNb <- 1:nrow(df_surv)

boot.surv <- function(df_surv, rowNb){   # Peut être mieux d'utiliser une colonne row number que d'utiliser ID
  df_surv <- df_surv[rowNb,] # select obs. in bootstrap sample
  mod <- glm(alive_t1 ~ -1 + ageClass/WinSnowsurvT1,
             data=df_surv, family="binomial")
  coefficients(mod) # return coefficient vector
}

surv.boot <- boot(data=df_surv, boot.surv, niter) 
head(surv.boot$t) # these are all the bootstraped coefficients

# organise output in a dataframe
# note here that 
# t1 = intercept age class 0, 
# t2 = intercept age class 1
# t3 = intercept age class 2
# t4 =  intercept age class 8+   ### Inversé 
# t5 = intercept age class 3-7  ### Inversé 
# t6 =  slope age class 0 with snow cover duration
# t7 =  slope age class 1 with snow cover duration
# t8 =  slope age class 2 with snow cover duration
# t9 =  slope age class 8 with snow cover duration ### Inversé 
# t10 =  slope age class 3-7 with snow cover duration ### Inversé 

data_pred_surv <- data.frame(
  ageClass = rep(c("0", "1", "2", "37", "8+"), each=niter),
  Intercept = c(surv.boot$t[,1], surv.boot$t[,2], surv.boot$t[,3], surv.boot$t[,5], surv.boot$t[,4]),
  Slope = c(surv.boot$t[,6], surv.boot$t[,7], surv.boot$t[,8], surv.boot$t[,10], surv.boot$t[,9])
)



# +++++++ FECUNDITY (I.E. REPRODUCTIVE RATE) ++++++++ # 

# now
myfun_now <- function(x) predict(x,newdata=data.frame(ageClass = c("3","48","9"),
                                                      env.now),
                                 type="link",re.form=NA)
Fec_now = bootMer(true,myfun_now, nsim = niter) 

head(Fec_now$t)

# future
myfun_future <- function(x) predict(x,newdata=data.frame(ageClass = c("3","48","9"),
                                                         env.future),       #### ENV FUTUR A CHANGER SI CHANGE CONDITION (JUSTE TEMPERATURE QUI INFLUENCE)
                                    type="link",re.form=NA)
Fec_future = bootMer(true,myfun_future, nsim = niter) 

# Save bootstrap iterations in RData
save(data_pred_surv,Fec_now,Fec_future, file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Juillet 7/bootstrapPred.RData")
load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Juillet 7/bootstrapPred.RData")


####========================####
# Population Models NOW
####========================####

# Create aposterior an empty dataframe which will later contain the estimated lambdas  
# For now
Lambda_now <- data.frame(
  iteration = c(1:niter),
  lambda = c(1:niter)
)

# Fill the dataframe
for(i in 1:niter){
  
  S0 <- data_pred_surv[which(data_pred_surv$ageClass=="0"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="0"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S1 <- data_pred_surv[which(data_pred_surv$ageClass=="1"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="1"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S2 <- data_pred_surv[which(data_pred_surv$ageClass=="2"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="2"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S37 <- data_pred_surv[which(data_pred_surv$ageClass=="37"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="37"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S8 <- data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  # Reproductive rates
  R3 <- Fec_now$t[i,1]
  R48 <- Fec_now$t[i,2]
  R9 <- Fec_now$t[i,3]
  
  # Create the matrix
  L <- matrix(0, nrow=9, ncol=9)
  
  
  L[1,3] <- inv.logit(S2)*inv.logit(R3)/2 # F2
  L[1,4] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,5] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,6] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,7] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,8] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,9] <- inv.logit(S8)*inv.logit(R9)/2 #F37
  
  
  L[2,1] <- inv.logit(S0)
  L[3,2] <- inv.logit(S1)  
  L[4,3] <- inv.logit(S2) 
  L[5,4] <- inv.logit(S37)   
  L[6,5] <- inv.logit(S37)    
  L[7,6] <- inv.logit(S37)  
  L[8,7] <- inv.logit(S37)   
  L[9,8] <- inv.logit(S37)   
  L[9,9] <- inv.logit(S8)
  
  Lambda_now$lambda[i] <- eigen.analysis(L)$lambda #extract lambdas
}


####========================####
# Projections TEMPERATURE
####========================####

  # For future conditions
  # Pour la température
  Lambda_future <- data.frame(
    iteration = c(1:niter),
    lambda = c(1:niter)
  )

for(i in 1:niter){
  
  S0 <- data_pred_surv[which(data_pred_surv$ageClass=="0"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="0"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S1 <- data_pred_surv[which(data_pred_surv$ageClass=="1"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="1"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S2 <- data_pred_surv[which(data_pred_surv$ageClass=="2"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="2"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S37 <- data_pred_surv[which(data_pred_surv$ageClass=="37"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="37"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S8 <- data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  # Reproductive rates
  R3 <- Fec_future$t[i,1]
  R48 <- Fec_future$t[i,2]
  R9 <- Fec_future$t[i,3]
  
  # Create the matrix
  L <- matrix(0, nrow=9, ncol=9)
  
  
  L[1,3] <- inv.logit(S2)*inv.logit(R3)/2 # F2
  L[1,4] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,5] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,6] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,7] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,8] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,9] <- inv.logit(S8)*inv.logit(R9)/2 #F37
  
  
  L[2,1] <- inv.logit(S0)
  L[3,2] <- inv.logit(S1)  
  L[4,3] <- inv.logit(S2) 
  L[5,4] <- inv.logit(S37)   
  L[6,5] <- inv.logit(S37)    
  L[7,6] <- inv.logit(S37)  
  L[8,7] <- inv.logit(S37)   
  L[9,8] <- inv.logit(S37)   
  L[9,9] <- inv.logit(S8)
  
  Lambda_future$lambda[i] <- eigen.analysis(L)$lambda
}



# +++++++ Figure  ++++++++ # 

# make density plots to compare the distributions
dens_x1 <- density(Lambda_now$lambda)
dens_x2 <- density(Lambda_future$lambda)

xlim <- c(0.8, 1.2)
ylim <- c(0,20)

col1 <- rgb(0.973,0.463,0.427,0.6) # Couleur original de Limoilou #f8766d, couleur de Joanie : rgb(0,0,0.3,0.6)
col2 <-rgb(0,0.749,0.769,0.6) # Couleur original de Limoilou #00bfc4, couleur de Joanie : rgb(0.3,0,0.2,0.6) # 4e terme = transparence 

par(mfrow=c(1,1))

tiff("PredTemp.tiff", res = 600, height=10, width=16, units="cm", pointsize=12)


plot(dens_x1, xlim = xlim, ylim = ylim, xlab = "Lambda", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)

#put our density plots in
polygon(dens_x1,  col = col1,  lty=2, lwd=2)
polygon(dens_x2,  col = col2, lty=1, lwd=2)

legend('topleft',legend=c('Actual temperature','Warming of 1.5 degree Celsius'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))

dev.off()








####========================####
# Projections SNOW DURATION
####========================####


# +++++++++++++++++++++++++++++++++++++++++++++++++
# Predictions 5% moins snow cover 
# Rien de fixé

Lambda_future5 <- data.frame(
  iteration = c(1:niter),
  lambda = c(1:niter)
)

for(i in 1:niter){
  
  S0 <- data_pred_surv[which(data_pred_surv$ageClass=="0"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="0"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  S1 <- data_pred_surv[which(data_pred_surv$ageClass=="1"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="1"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  S2 <- data_pred_surv[which(data_pred_surv$ageClass=="2"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="2"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  S37 <- data_pred_surv[which(data_pred_surv$ageClass=="37"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="37"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  S8 <- data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  # Reproductive rates
  R3 <- Fec_now$t[i,1]
  R48 <- Fec_now$t[i,2]
  R9 <- Fec_now$t[i,3]
  
  # Create the matrix
  L <- matrix(0, nrow=9, ncol=9)
  
  
  L[1,3] <- inv.logit(S2)*inv.logit(R3)/2 # F2
  L[1,4] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,5] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,6] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,7] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,8] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,9] <- inv.logit(S8)*inv.logit(R9)/2 #F37
  
  
  L[2,1] <- inv.logit(S0)
  L[3,2] <- inv.logit(S1)  
  L[4,3] <- inv.logit(S2) 
  L[5,4] <- inv.logit(S37)   
  L[6,5] <- inv.logit(S37)    
  L[7,6] <- inv.logit(S37)  
  L[8,7] <- inv.logit(S37)   
  L[9,8] <- inv.logit(S37)   
  L[9,9] <- inv.logit(S8)
  
  Lambda_future5$lambda[i] <- eigen.analysis(L)$lambda
}


# +++++++++++++++++++++++++++++++++++++++++++++++++
# Predictions 10% moins snow cover 
# Rien de fixé

Lambda_future10 <- data.frame(
  iteration = c(1:niter),
  lambda = c(1:niter)
)

for(i in 1:niter){
  
  S0 <- data_pred_surv[which(data_pred_surv$ageClass=="0"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="0"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  S1 <- data_pred_surv[which(data_pred_surv$ageClass=="1"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="1"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  S2 <- data_pred_surv[which(data_pred_surv$ageClass=="2"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="2"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  S37 <- data_pred_surv[which(data_pred_surv$ageClass=="37"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="37"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  S8 <- data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  # Reproductive rates
  R3 <- Fec_now$t[i,1]
  R48 <- Fec_now$t[i,2]
  R9 <- Fec_now$t[i,3]
  
  # Create the matrix
  L <- matrix(0, nrow=9, ncol=9)
  
  
  L[1,3] <- inv.logit(S2)*inv.logit(R3)/2 # F2
  L[1,4] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,5] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,6] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,7] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,8] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,9] <- inv.logit(S8)*inv.logit(R9)/2 #F37
  
  
  L[2,1] <- inv.logit(S0)
  L[3,2] <- inv.logit(S1)  
  L[4,3] <- inv.logit(S2) 
  L[5,4] <- inv.logit(S37)   
  L[6,5] <- inv.logit(S37)    
  L[7,6] <- inv.logit(S37)  
  L[8,7] <- inv.logit(S37)   
  L[9,8] <- inv.logit(S37)   
  L[9,9] <- inv.logit(S8)
  
  Lambda_future10$lambda[i] <- eigen.analysis(L)$lambda
}


# +++++++++++++++++++++++++++++++++++++++++++++++++
# Predictions 20% moins snow cover 
# Rien de fixé

Lambda_future20 <- data.frame(
  iteration = c(1:niter),
  lambda = c(1:niter)
)

for(i in 1:niter){
  
  S0 <- data_pred_surv[which(data_pred_surv$ageClass=="0"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="0"),"Slope"][i]*env.future20["WinSnowsurvT1"][[1]]
  
  S1 <- data_pred_surv[which(data_pred_surv$ageClass=="1"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="1"),"Slope"][i]*env.future20["WinSnowsurvT1"][[1]]
  
  S2 <- data_pred_surv[which(data_pred_surv$ageClass=="2"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="2"),"Slope"][i]*env.future20["WinSnowsurvT1"][[1]]
  
  S37 <- data_pred_surv[which(data_pred_surv$ageClass=="37"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="37"),"Slope"][i]*env.future20["WinSnowsurvT1"][[1]]
  
  S8 <- data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Slope"][i]*env.future20["WinSnowsurvT1"][[1]]
  
  # Reproductive rates
  R3 <- Fec_now$t[i,1]
  R48 <- Fec_now$t[i,2]
  R9 <- Fec_now$t[i,3]
  
  # Create the matrix
  L <- matrix(0, nrow=9, ncol=9)
  
  
  L[1,3] <- inv.logit(S2)*inv.logit(R3)/2 # F2
  L[1,4] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,5] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,6] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,7] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,8] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,9] <- inv.logit(S8)*inv.logit(R9)/2 #F37
  
  
  L[2,1] <- inv.logit(S0)
  L[3,2] <- inv.logit(S1)  
  L[4,3] <- inv.logit(S2) 
  L[5,4] <- inv.logit(S37)   
  L[6,5] <- inv.logit(S37)    
  L[7,6] <- inv.logit(S37)  
  L[8,7] <- inv.logit(S37)   
  L[9,8] <- inv.logit(S37)   
  L[9,9] <- inv.logit(S8)
  
  Lambda_future20$lambda[i] <- eigen.analysis(L)$lambda
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 30% less snow cover


Lambda_future30 <- data.frame(
  iteration = c(1:niter),
  lambda = c(1:niter)
)

for(i in 1:niter){
  
  S0 <- data_pred_surv[which(data_pred_surv$ageClass=="0"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="0"),"Slope"][i]*env.future30["WinSnowsurvT1"][[1]]
  
  S1 <- data_pred_surv[which(data_pred_surv$ageClass=="1"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="1"),"Slope"][i]*env.future30["WinSnowsurvT1"][[1]]
  
  S2 <- data_pred_surv[which(data_pred_surv$ageClass=="2"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="2"),"Slope"][i]*env.future30["WinSnowsurvT1"][[1]]
  
  S37 <- data_pred_surv[which(data_pred_surv$ageClass=="37"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="37"),"Slope"][i]*env.future30["WinSnowsurvT1"][[1]]
  
  S8 <- data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Slope"][i]*env.future30["WinSnowsurvT1"][[1]]
  
  # Reproductive rates
  R3 <- Fec_now$t[i,1]
  R48 <- Fec_now$t[i,2]
  R9 <- Fec_now$t[i,3]
  
  # Create the matrix
  L <- matrix(0, nrow=9, ncol=9)
  
  
  L[1,3] <- inv.logit(S2)*inv.logit(R3)/2 # F2
  L[1,4] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,5] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,6] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,7] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,8] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,9] <- inv.logit(S8)*inv.logit(R9)/2 #F37
  
  
  L[2,1] <- inv.logit(S0)
  L[3,2] <- inv.logit(S1)  
  L[4,3] <- inv.logit(S2) 
  L[5,4] <- inv.logit(S37)   
  L[6,5] <- inv.logit(S37)    
  L[7,6] <- inv.logit(S37)  
  L[8,7] <- inv.logit(S37)   
  L[9,8] <- inv.logit(S37)   
  L[9,9] <- inv.logit(S8)
  
  Lambda_future30$lambda[i] <- eigen.analysis(L)$lambda
}


#save(Lambda_now, Lambda_future,Lambda_future5,Lambda_future10,Lambda_future20,Lambda_future30, file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Juillet 7/LambdaIterations.RData")



#++++++++++++++++++++++++++++++ FIGURE ALL ++++++++++++++++++++++++++++++# 


# 5% 

# make density plots to compare the distributions

col1 <- rgb(0.973,0.463,0.427,0.6) # Couleur original de Limoilou #f8766d, couleur de Joanie : rgb(0,0,0.3,0.6)
col2 <-rgb(0,0.749,0.769,0.5) # Couleur original de Limoilou #00bfc4, couleur de Joanie : rgb(0.3,0,0.2,0.6) # 4e terme = transparence 

xlim <- c(0.8, 1.2)
ylim <- c(0,20)


dens_x1.a <- density(Lambda_now$lambda)
dens_x1.b <- density(Lambda_now$lambda)
dens_x1.c <- density(Lambda_now$lambda)
dens_x1.d <- density(Lambda_now$lambda)

dens_x2.a <- density(Lambda_future5$lambda) # Changer lambda future
dens_x2.b <- density(Lambda_future10$lambda)
dens_x2.c <- density(Lambda_future20$lambda)
dens_x2.d <- density(Lambda_future30$lambda)


tiff("Pred_SnowDuration.tiff", res = 600, height=10, width=16, units="cm", pointsize=10)

par(mfrow=c(2,2),mar=c(2,2,2,2)) #mar = c(bas, gauche, haut, droite)

plot(dens_x1.a, xlim = xlim, ylim = ylim, xlab = "Lambda", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)

#put our density plots in
polygon(dens_x1.a,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.a,  col = col2, lty=1, lwd=2)
legend('topleft',legend=c('Actual snow','5% less duration'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


# 10 % 

plot(dens_x1.b, xlim = xlim, ylim = ylim, xlab = "Lambda", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)

#put our density plots in
polygon(dens_x1.b,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.b,  col = col2, lty=1, lwd=2)

legend('topleft',legend=c('Actual snow','10% less duration'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


# 20% 
# make density plots to compare the distributions

plot(dens_x1.c, xlim = xlim, ylim = ylim, xlab = "Lambda", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)

#put our density plots in
polygon(dens_x1.c,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.c,  col = col2, lty=1, lwd=2)

legend('topleft',legend=c('Actual snow','20% less duration'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


# 30%

# make density plots to compare the distributions

plot(dens_x1.d, xlim = xlim, ylim = ylim, xlab = "Lambda", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)

#put our density plots in
polygon(dens_x1.d,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.d,  col = col2, lty=1, lwd=2)

legend('topleft',legend=c('Actual snow','30% less duration'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))

dev.off()


####============================================####
# Projections SNOW DURATION 5-10/fixed-not fixed 
####============================================####

load(file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Juillet 7/LambdaIterations.RData")
load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Juillet 7/bootstrapPred.RData")

Lambda_future5

Lambda_future10


# Do 5% and 10% with everything fixed except S1 

niter = 10000

# 5% FIXED 
Lambda_future5_fixed <- data.frame(
  iteration = c(1:niter),
  lambda = c(1:niter)
)

for(i in 1:niter){
  
  S0 <- data_pred_surv[which(data_pred_surv$ageClass=="0"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="0"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S1 <- data_pred_surv[which(data_pred_surv$ageClass=="1"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="1"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  S2 <- data_pred_surv[which(data_pred_surv$ageClass=="2"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="2"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S37 <- data_pred_surv[which(data_pred_surv$ageClass=="37"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="37"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S8 <- data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  # Reproductive rates
  R3 <- Fec_now$t[i,1]
  R48 <- Fec_now$t[i,2]
  R9 <- Fec_now$t[i,3]
  
  # Create the matrix
  L <- matrix(0, nrow=9, ncol=9)
  
  
  L[1,3] <- inv.logit(S2)*inv.logit(R3)/2 # F2
  L[1,4] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,5] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,6] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,7] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,8] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,9] <- inv.logit(S8)*inv.logit(R9)/2 #F37
  
  
  L[2,1] <- inv.logit(S0)
  L[3,2] <- inv.logit(S1)  
  L[4,3] <- inv.logit(S2) 
  L[5,4] <- inv.logit(S37)   
  L[6,5] <- inv.logit(S37)    
  L[7,6] <- inv.logit(S37)  
  L[8,7] <- inv.logit(S37)   
  L[9,8] <- inv.logit(S37)   
  L[9,9] <- inv.logit(S8)
  
  Lambda_future5_fixed$lambda[i] <- eigen.analysis(L)$lambda
}


# 10 % fixed 

Lambda_future10_fixed <- data.frame(
  iteration = c(1:niter),
  lambda = c(1:niter)
)

for(i in 1:niter){
  
  S0 <- data_pred_surv[which(data_pred_surv$ageClass=="0"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="0"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S1 <- data_pred_surv[which(data_pred_surv$ageClass=="1"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="1"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  S2 <- data_pred_surv[which(data_pred_surv$ageClass=="2"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="2"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S37 <- data_pred_surv[which(data_pred_surv$ageClass=="37"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="37"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  S8 <- data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Slope"][i]*env.now["WinSnowsurvT1"][[1]]
  
  # Reproductive rates
  R3 <- Fec_now$t[i,1]
  R48 <- Fec_now$t[i,2]
  R9 <- Fec_now$t[i,3]
  
  # Create the matrix
  L <- matrix(0, nrow=9, ncol=9)
  
  
  L[1,3] <- inv.logit(S2)*inv.logit(R3)/2 # F2
  L[1,4] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,5] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,6] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,7] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,8] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,9] <- inv.logit(S8)*inv.logit(R9)/2 #F37
  
  
  L[2,1] <- inv.logit(S0)
  L[3,2] <- inv.logit(S1)  
  L[4,3] <- inv.logit(S2) 
  L[5,4] <- inv.logit(S37)   
  L[6,5] <- inv.logit(S37)    
  L[7,6] <- inv.logit(S37)  
  L[8,7] <- inv.logit(S37)   
  L[9,8] <- inv.logit(S37)   
  L[9,9] <- inv.logit(S8)
  
  Lambda_future10_fixed$lambda[i] <- eigen.analysis(L)$lambda
}



# ++++++++ Figure ++++++++# 

col1 <- rgb(0.973,0.463,0.427,0.6) # Couleur original de Limoilou #f8766d, couleur de Joanie : rgb(0,0,0.3,0.6)
col2 <-rgb(0,0.749,0.769,0.5) # Couleur original de Limoilou #00bfc4, couleur de Joanie : rgb(0.3,0,0.2,0.6) # 4e terme = transparence 

xlim <- c(0.8, 1.2)
ylim <- c(0,20)


dens_x1.a <- density(Lambda_now$lambda)
dens_x1.b <- density(Lambda_now$lambda)
dens_x1.c <- density(Lambda_now$lambda)
dens_x1.d <- density(Lambda_now$lambda)

dens_x2.a <- density(Lambda_future5$lambda) # Changer lambda future
dens_x2.b <- density(Lambda_future10$lambda)
dens_x2.c <- density(Lambda_future5_fixed$lambda)
dens_x2.d <- density(Lambda_future10_fixed$lambda)


tiff("Pred_SnowDuration.tiff", res = 600, height=10, width=16, units="cm", pointsize=9)

par(mfrow=c(2,2),mar=c(2,2,2,2)) #mar = c(bas, gauche, haut, droite)

plot(dens_x1.a, xlim = xlim, ylim = ylim, xlab = "Lambda", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)

#put our density plots in
polygon(dens_x1.a,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.b,  col = col2, lty=1, lwd=2)
legend('topleft',legend=c('Actual snow','5% less duration'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


# 10 % 

plot(dens_x1.b, xlim = xlim, ylim = ylim, xlab = "Lambda", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)

#put our density plots in
polygon(dens_x1.b,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.b,  col = col2, lty=1, lwd=2)

legend('topleft',legend=c('Actual snow','10% less duration'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


# 5% fixed
# make density plots to compare the distributions

plot(dens_x1.c, xlim = xlim, ylim = ylim, xlab = "Lambda", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)

#put our density plots in
polygon(dens_x1.c,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.c,  col = col2, lty=1, lwd=2)

legend('topleft',legend=c('Actual snow','5% less duration, fixed'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


# 10% fixed

# make density plots to compare the distributions

plot(dens_x1.d, xlim = xlim, ylim = ylim, xlab = "Lambda", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)

#put our density plots in
polygon(dens_x1.d,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.d,  col = col2, lty=1, lwd=2)

legend('topleft',legend=c('Actual snow','10% less duration, fixed'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))

dev.off()

####============================================####
# Projections COMBINATION
####============================================####


#Predictions 5% moins snow cover   + TEMP 
# Rien de fixé

Lambda_future5_comb <- data.frame(
  iteration = c(1:niter),
  lambda = c(1:niter)
)

for(i in 1:niter){
  
  S0 <- data_pred_surv[which(data_pred_surv$ageClass=="0"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="0"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  S1 <- data_pred_surv[which(data_pred_surv$ageClass=="1"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="1"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  S2 <- data_pred_surv[which(data_pred_surv$ageClass=="2"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="2"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  S37 <- data_pred_surv[which(data_pred_surv$ageClass=="37"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="37"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  S8 <- data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Slope"][i]*env.future5["WinSnowsurvT1"][[1]]
  
  # Reproductive rates
  R3 <- Fec_future$t[i,1]
  R48 <- Fec_future$t[i,2]
  R9 <- Fec_future$t[i,3]
  
  # Create the matrix
  L <- matrix(0, nrow=9, ncol=9)
  
  
  L[1,3] <- inv.logit(S2)*inv.logit(R3)/2 # F2
  L[1,4] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,5] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,6] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,7] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,8] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,9] <- inv.logit(S8)*inv.logit(R9)/2 #F37
  
  
  L[2,1] <- inv.logit(S0)
  L[3,2] <- inv.logit(S1)  
  L[4,3] <- inv.logit(S2) 
  L[5,4] <- inv.logit(S37)   
  L[6,5] <- inv.logit(S37)    
  L[7,6] <- inv.logit(S37)  
  L[8,7] <- inv.logit(S37)   
  L[9,8] <- inv.logit(S37)   
  L[9,9] <- inv.logit(S8)
  
  Lambda_future5_comb$lambda[i] <- eigen.analysis(L)$lambda
}


# +++++++++++++++++++++++++++++++++++++++++++++++++
# Predictions 10% moins snow cover  + TEMP
# Rien de fixé

Lambda_future10_comb <- data.frame(
  iteration = c(1:niter),
  lambda = c(1:niter)
)

for(i in 1:niter){
  
  S0 <- data_pred_surv[which(data_pred_surv$ageClass=="0"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="0"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  S1 <- data_pred_surv[which(data_pred_surv$ageClass=="1"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="1"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  S2 <- data_pred_surv[which(data_pred_surv$ageClass=="2"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="2"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  S37 <- data_pred_surv[which(data_pred_surv$ageClass=="37"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="37"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  S8 <- data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Intercept"][i] + 
    data_pred_surv[which(data_pred_surv$ageClass=="8+"),"Slope"][i]*env.future10["WinSnowsurvT1"][[1]]
  
  # Reproductive rates
  R3 <- Fec_future$t[i,1]
  R48 <- Fec_future$t[i,2]
  R9 <- Fec_future$t[i,3]
  
  # Create the matrix
  L <- matrix(0, nrow=9, ncol=9)
  
  
  L[1,3] <- inv.logit(S2)*inv.logit(R3)/2 # F2
  L[1,4] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,5] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,6] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,7] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,8] <- inv.logit(S37)*inv.logit(R48)/2 #F37
  L[1,9] <- inv.logit(S8)*inv.logit(R9)/2 #F37
  
  
  L[2,1] <- inv.logit(S0)
  L[3,2] <- inv.logit(S1)  
  L[4,3] <- inv.logit(S2) 
  L[5,4] <- inv.logit(S37)   
  L[6,5] <- inv.logit(S37)    
  L[7,6] <- inv.logit(S37)  
  L[8,7] <- inv.logit(S37)   
  L[9,8] <- inv.logit(S37)   
  L[9,9] <- inv.logit(S8)
  
  Lambda_future10_comb$lambda[i] <- eigen.analysis(L)$lambda
}



####============================================####
# FIGURES FINALES
####============================================####


#save(Lambda_now,Lambda_future5,Lambda_future10,Lambda_future5_fixed,Lambda_future10_fixed,Lambda_future,Lambda_future5_comb,Lambda_future10_comb,file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Juillet 7/LambdaFigures.RData")

load("/Users/Sandrine/Documents/Sherbrooke/OWPC/Post_OWPC/Projections /Juillet 7/LambdaFigures.RData")

col1 <- rgb(0.973,0.463,0.427,0.6) # Couleur original de Limoilou #f8766d, couleur de Joanie : rgb(0,0,0.3,0.6)
col2 <-rgb(0,0.749,0.769,0.5) # Couleur original de Limoilou #00bfc4, couleur de Joanie : rgb(0.3,0,0.2,0.6) # 4e terme = transparence 

xlim <- c(0.8, 1.2)
ylim <- c(0,20)


dens_x1.a <- density(Lambda_now$lambda)
dens_x1.b <- density(Lambda_now$lambda)
dens_x1.c <- density(Lambda_now$lambda)
dens_x1.d <- density(Lambda_now$lambda)
dens_x1.e <- density(Lambda_now$lambda)
dens_x1.f <- density(Lambda_now$lambda)
dens_x1.g <- density(Lambda_now$lambda)

dens_x2.a <- density(Lambda_future5$lambda) 
dens_x2.b <- density(Lambda_future10$lambda)
dens_x2.c <- density(Lambda_future5_fixed$lambda)
dens_x2.d <- density(Lambda_future10_fixed$lambda)
dens_x2.e <- density(Lambda_future$lambda) # représente Temperature seulement
dens_x2.f <- density(Lambda_future5_comb$lambda)
dens_x2.g <- density(Lambda_future10_comb$lambda)




# Figure 5 panels
# 5%, 10%, +1.5, combination 5%, combination 10% 
# Projections with everything but S1 fixed, in supplementary 

tiff("Pojection_Options2.tiff", res = 600, height=12, width=16, units="cm", pointsize=9)


 
par(mfrow=c(3,2),mar=c(4,5,2,2)) #mar = c(bas, gauche, haut, droite)

# 5%
plot(dens_x1.a, xlim = xlim, ylim = ylim, xlab = "",ylab= "Density", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, expression(lambda), line=2.5,cex = 1.2)
#mtext(side=2, "Frequency", line=2.5)
#text(0.82, 18, labels="(a)", cex=1.5)
mtext(side = 3,"(a)", line = 0.6, at = 0.75)

#put our density plots in
polygon(dens_x1.a,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.a,  col = col2, lty=1, lwd=2)
legend(0.78,19,legend=c('Actual conditions','5% reduction'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1),x.intersp	= 0.2,cex = 1.2)


# 10 % 

plot(dens_x1.b, xlim = xlim, ylim = ylim, xlab = "" , ylab= "Density", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, expression(lambda), line=2.5,cex = 1.2)
#text(0.82, 18, labels="(b)", cex=1.5)
mtext(side = 3,"(b)",line = 0.6, at = 0.75)

#put our density plots in
polygon(dens_x1.b,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.b,  col = col2, lty=1, lwd=2)

legend(0.78,19,legend=c('Actual conditions','10% reduction'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1),x.intersp	= 0.2,cex = 1.2
       )

#Temp

plot(dens_x1.e, xlim = xlim, ylim = ylim, xlab = "",ylab= "Density", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, expression(lambda), line=2.5,cex = 1.2)
mtext(side = 3,"(c)", line = 0.6, at = 0.75)
#text(0.75, 21, labels="(c)", cex=1.5)

#put our density plots in
polygon(dens_x1.e,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.e,  col = col2, lty=1, lwd=2)

legend(0.78,19,legend=c('Actual conditions', expression("1.5 "^"o"*"C increase")),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1),x.intersp	= 0.2,cex = 1.2)


# 5% + Temp

plot(dens_x1.f, xlim = xlim, ylim = ylim, xlab = "", ylab= "Density",axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, expression(lambda), line=2.5,cex = 1.2)
#text(0.82, 18, labels="(d)", cex=1.5)
mtext(side = 3,"(d)", line = 0.6, at = 0.75)


#put our density plots in
polygon(dens_x1.f,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.f,  col = col2, lty=1, lwd=2)

legend(0.78,19,legend=c('Actual conditions','5% reduction'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1),x.intersp	= 0.2,cex = 1.2)
legend(0.80,11,legend= expression("1.5 "^"o"*"C increase"),
       bty = 'n',border = F, lty=c(2,1), col = "white",x.intersp	= 0.2,cex = 1.2)


# 10% + Temp

plot(dens_x1.g, xlim = xlim, ylim = ylim, xlab = "",ylab= "Density", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, expression(lambda), line=2.5,cex = 1.2)
#text(0.82, 18, labels="(e)", cex=1.5)
mtext(side = 3,"(e)", line = 0.6, at = 0.75)

#put our density plots in
polygon(dens_x1.g,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.g,  col = col2, lty=1, lwd=2)

legend(0.78,19,legend=c('Actual conditions','10% reduction'),
       fill = c(col1, col2,"white"), bty = 'n',
       border = T, lty=c(2,1),x.intersp	= 0.2,cex = 1.2)
legend(0.80,11,legend= expression("1.5 "^"o"*"C increase"),
       bty = 'n', border = F, lty=c(2,1), col = "white",x.intersp	= 0.2,cex = 1.2)




dev.off()





# Second Figure with everything fixed but S1 
# In supplementary

 tiff("Projection_Option2_Supplementary.tiff", res = 600, height=9, width=14, units="cm", pointsize=9)

# 5% fixed
# make density plots to compare the distributions

par(mfrow=c(2,1),mar=c(4,5,2,2)) #mar = c(bas, gauche, haut, droite)

plot(dens_x1.c, xlim = xlim, ylim = ylim, xlab = "",ylab= "Density", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, expression(lambda), line=2.5,cex = 1.2)
mtext(side = 3,"(a)", line = 0.6, at = 0.75, cex = 1.2)

#put our density plots in
polygon(dens_x1.c,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.c,  col = col2, lty=1, lwd=2)

legend(0.78,14,legend=c('Actual conditions','5% reduction, fixed'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1), cex = 0.9)


# 10% fixed

# make density plots to compare the distributions

plot(dens_x1.d, xlim = xlim, ylim = ylim, xlab = "", ylab= "Density",axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, expression(lambda), line=2.5,cex = 1.2)
mtext(side = 3,"(b)", line = 0.6, at = 0.75, cex = 1.2)

#put our density plots in
polygon(dens_x1.d,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.d,  col = col2, lty=1, lwd=2)

legend(0.78,14,legend=c('Actual conditions','10% reduction, fixed'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1), cex = 0.9)


dev.off()




# Figure 7 panels 
# Nothing in Supplementary 

col1 <- rgb(0.973,0.463,0.427,0.6) # Couleur original de Limoilou #f8766d, couleur de Joanie : rgb(0,0,0.3,0.6)
col2 <-rgb(0,0.749,0.769,0.5) # Couleur original de Limoilou #00bfc4, couleur de Joanie : rgb(0.3,0,0.2,0.6) # 4e terme = transparence 

xlim <- c(0.8, 1.2)
ylim <- c(0,20)


dens_x1.a <- density(Lambda_now$lambda)
dens_x1.b <- density(Lambda_now$lambda)
dens_x1.c <- density(Lambda_now$lambda)
dens_x1.d <- density(Lambda_now$lambda)
dens_x1.e <- density(Lambda_now$lambda)
dens_x1.f <- density(Lambda_now$lambda)
dens_x1.g <- density(Lambda_now$lambda)

dens_x2.a <- density(Lambda_future5$lambda) 
dens_x2.b <- density(Lambda_future10$lambda)
dens_x2.c <- density(Lambda_future5_fixed$lambda)
dens_x2.d <- density(Lambda_future10_fixed$lambda)
dens_x2.e <- density(Lambda_future$lambda) # représente Temperature seulement
dens_x2.f <- density(Lambda_future5_comb$lambda)
dens_x2.g <- density(Lambda_future10_comb$lambda)


tiff("Projection_Option1.tiff", res = 600, height=13, width=16.5, units="cm", pointsize=9)

par(mfrow=c(4,2),mar=c(4,5,2,2)) #mar = c(bas, gauche, haut, droite)

plot(dens_x1.a, xlim = xlim, ylim = ylim, xlab = "",ylab= "Density", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, "Lambda", line=2.5,cex = 0.8)
#mtext(side=2, "Frequency", line=2.5)
text(0.82, 18, labels="(a)", cex=1.5)

#put our density plots in
polygon(dens_x1.a,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.a,  col = col2, lty=1, lwd=2)
legend(0.78,14,legend=c('Actual snow','5% less duration'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


# 10 % 

plot(dens_x1.b, xlim = xlim, ylim = ylim, xlab = "" , ylab= "Density", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, "Lambda", line=2.5,cex = 0.8)
text(0.82, 18, labels="(b)", cex=1.5)

#put our density plots in
polygon(dens_x1.b,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.b,  col = col2, lty=1, lwd=2)

legend(0.78,14,legend=c('Actual snow','10% less duration'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


# 5% fixed
# make density plots to compare the distributions

plot(dens_x1.c, xlim = xlim, ylim = ylim, xlab = "",ylab= "Density", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, "Lambda", line=2.5,cex = 0.8)
text(0.82, 18, labels="(c)", cex=1.5)


#put our density plots in
polygon(dens_x1.c,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.c,  col = col2, lty=1, lwd=2)

legend(0.78,14,legend=c('Actual snow','5% less duration, fixed'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


# 10% fixed

# make density plots to compare the distributions

plot(dens_x1.d, xlim = xlim, ylim = ylim, xlab = "", ylab= "Density",axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, "Lambda", line=2.5,cex = 0.8)
text(0.82, 18, labels="(d)", cex=1.5)

#put our density plots in
polygon(dens_x1.d,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.d,  col = col2, lty=1, lwd=2)

legend(0.78,14,legend=c('Actual snow','10% less duration, fixed'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


#Temp

plot(dens_x1.e, xlim = xlim, ylim = ylim, xlab = "",ylab= "Density", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, "Lambda", line=2.5,cex = 0.8)
text(0.82, 18, labels="(e)", cex=1.5)

#put our density plots in
polygon(dens_x1.e,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.e,  col = col2, lty=1, lwd=2)

legend(0.78,14,legend=c('Actual snow','+1.5C warming'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))


# 5% + Temp

plot(dens_x1.f, xlim = xlim, ylim = ylim, xlab = "", ylab= "Density",axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, "Lambda", line=2.5,cex = 0.8)
text(0.82, 18, labels="(f)", cex=1.5)

#put our density plots in
polygon(dens_x1.f,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.f,  col = col2, lty=1, lwd=2)

legend(0.78,14,legend=c('Actual snow','5% less duration'),
       fill = c(col1, col2), bty = 'n',
       border = T, lty=c(2,1))
legend(0.80,8,legend=c('+1.5C warming'),
       bty = 'n',border = F, lty=c(2,1), col = "white")


# 10% + Temp

plot(dens_x1.g, xlim = xlim, ylim = ylim, xlab = "",ylab= "Density", axes=F,
     main = '', cex.axis=1.2, cex.lab=1.2, type="n")
axis(1, at=seq(0.8, 1.2, 0.01), cex.axis=1)
axis(2, at=seq(0, 20,1), cex.axis =1)
mtext(side=1, "Lambda", line=2.5,cex = 0.8)
text(0.82, 18, labels="(g)", cex=1.5)

#put our density plots in
polygon(dens_x1.g,  col = col1,  lty=2, lwd=2)
polygon(dens_x2.g,  col = col2, lty=1, lwd=2)

legend(0.78,14,legend=c('Actual snow','10% less duration'),
       fill = c(col1, col2,"white"), bty = 'n',
       border = T, lty=c(2,1))
legend(0.80,8,legend=c('+1.5C warming'),
       bty = 'n', border = F, lty=c(2,1), col = "white")


dev.off()

####============================================####
# Get the means and SD
####============================================####

# Lambda now 
mean(Lambda_now$lambda) # 1.066446
sd(Lambda_now$lambda) # 0.02252336

q <- quantile(Lambda_now$lambda, 0.975) # 1.109802 


# Lambda 5% 
mean(Lambda_future5$lambda) # 1.071571
sd(Lambda_future5$lambda) # 0.02433054
# Proportion lambda > 97.5 quartile of lambda now
length(which(Lambda_future5[,2] > q)) / (nrow(Lambda_future5)) # 0.0559


# Lambda 10% 
mean(Lambda_future10$lambda) # 1.073211
sd(Lambda_future10$lambda) # 0.02864516
length(which(Lambda_future10[,2] > q)) / (nrow(Lambda_future10)) #0.0975


# Lambda Temperature 
mean(Lambda_future$lambda) # 1.071984
sd(Lambda_future$lambda) # 0.02308012
length(which(Lambda_future[,2] > q)) / (nrow(Lambda_future)) # 0.049

# Combined 5% 
mean(Lambda_future5_comb$lambda) # 1.077196
sd(Lambda_future5_comb$lambda) # 0.02483625
length(which(Lambda_future5_comb[,2] > q)) / (nrow(Lambda_future5_comb)) # 0.0936

# Combined 10% 
mean(Lambda_future10_comb$lambda) # 1.078819
sd(Lambda_future10_comb$lambda) # 0.02909688
length(which(Lambda_future10_comb[,2] > q)) / (nrow(Lambda_future10_comb)) # 0.1455

#Fixed 5% 
mean(Lambda_future5_fixed$lambda) # 1.06988
sd(Lambda_future5_fixed$lambda) # 0.02226941
length(which(Lambda_future5_fixed[,2] > q)) / (nrow(Lambda_future5_fixed)) # 0.0342


#Fixed 10% 
mean(Lambda_future10_fixed$lambda) # 1.071331
sd(Lambda_future10_fixed$lambda) # 0.02219305
length(which(Lambda_future10_fixed[,2] > q)) / (nrow(Lambda_future10_fixed)) # 0.0391
