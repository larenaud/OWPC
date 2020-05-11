# code to simulate demographic rates - lambda - under different scenarios of temperature and phenology 
# created by S. C. Beaumont 
# modified by L. Renaud # 2020-04-23


library(plyr)
library(dplyr)
library(boot)
library(lme4)
library(popbio)
library(ggplot2)
library(pander)
library(cowplot)
rm(list = ls ())


# get matrices from goodle Drive and load R object
matrices <- load("data/Matrice_All_Years.RData")

# allYearRF = matrice faite avec RAW  (incluant agneaux fantômes)  
# allYearTF = matrice faite avec TRUE  (excluant agneaux fantômes)





# get final models to extract beta coefficients 
# survival 
load("graph/final_surv.Rdata")


surv <- glm(alive_t1 ~ -1 + ageClass/PC2Tim + pred, 
                 family = "binomial",
                 df_surv) 

load("graph/final_raw.Rdata")

raw <- glmer(raw_repro ~ -1 + ageClass/TWin + MassAutumn_tm1 + (1 | ID), 
                  family = "binomial", 
                  df_fec, 
                  control = glmerControl(optimizer="bobyqa", 
                                         optCtrl = list(maxfun = 2000000)))



load("graph/final_true_models.Rdata")
true <- glmer(true_repro ~ -1 + ageClass/(TWin * PWin) + MassAutumn_tm1 + (1 |ID), 
                   df_fec, 
                   family = "binomial",
                   control = glmerControl(optimizer="bobyqa", 
                                          optCtrl = list(maxfun = 2000000)))


plot(df_fec$TWin ~ df_fec$yr)



# extract model coefficients with uncertainty and generate NEW predicted leslie matrix 

newLeslie <- function(survm=surv, fec = raw, env = newdata[1,]){
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




# projections per se 
# this is the actual data # build fictive dataframes to predict on 

# use unscaled variables LATER

load("cache/dataSurvivalModels.Rdata")
load("cache/dataFecundityModels.RData")



q.SD<-sd(dt3$snow_log_up_jul)
q.MEAN<-mean(dt3$snow_log_up_jul)
newd$snow<-(newd[,"snow_z"]*q.SD) + q.MEAN



# no good

env.now <- data.frame(TWin = mean(df_fec$TWin, na.rm = T),
                      T.WIN = mean(df_surv$T.WIN , na.rm = T),
                      PC2Tim = mean(df_surv$PC2Tim, na.rm = T),
                      PWin = mean(df_fec$PWin, na.rm = T),
                      # dont if put PWin?? 
                      MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                      pred = "0")

# this is the data under climate change HOWEVER IT IS NOW SCALED
env.future <- data.frame(TWin = mean(df_fec$TWin, na.rm = T)+1.5,
                         T.WIN = mean(df_surv$T.WIN , na.rm = T)+1.5,
                         PC2Tim = mean(df_surv$PC2Tim, na.rm = T),
                         PWin = mean(df_fec$PWin, na.rm = T),
                         # dont if put PWin?? 
                         MassAutumn_tm1 = mean(df_fec$MassAutumn_tm1, na.rm = T),
                         pred = "0")


# raw fec # need 10 000
lambda.raw.now = 
sapply(1:10, function(it){
  bob = eigen.analysis(newLeslie(fec = raw,env = env.now))
  return(bob$lambda1)
})
hist(lambda.raw.now)



lambda.raw.future = 
  sapply(1:10, function(it){ # sapply fait des itérations 
    bob = eigen.analysis(newLeslie(fec = raw,env = env.future))
    return(bob$lambda1)
  })
hist(lambda.raw.future)


# make TRUE fecondity 

lambda.true.now = 
  sapply(1:10, function(it){
    bob = eigen.analysis(newLeslie(fec = true,env = env.now))
    return(bob$lambda1)
  })
hist(lambda.true.now)

lambda.true.future = 
  sapply(1:10, function(it){ 
    bob = eigen.analysis(newLeslie(fec = true,env = env.future))
    return(bob$lambda1)
  })
hist(lambda.true.future)


lambda <- data.frame(lambda.true.now, 
                     lambda.true.future)





# figure temp projection  -----------------------------------------------------------------

projectionTemp <- ggplot(lambda, aes(lambda.true.now, fill = "true.now")) + 
  geom_density(alpha = 0.5) + 
  geom_density(aes(lambda.true.future, fill = "true.future"), alpha = 0.5) + 
  labs(x=expression('Population growth rate based on true reproduction'), 
       y="Density") +
  theme_cowplot() 
projectionTemp






# green-up projections ----------------------------------------------------

# 19 day advance in 60 yrs. 



# get final models to extract beta coefficients # here try only 1 var of pc  
# survival 
load("graph/final_surv.Rdata")


surv <- glm(alive_t1 ~ -1 + ageClass/NDVIsurvT + pred, # will have to change pc2tim to other variable thus dataframe... 
            family = "binomial",
            df_surv) 

load("graph/final_raw.Rdata")

raw <- glmer(raw_repro ~ -1 + ageClass/TWin + MassAutumn_tm1 + (1 | ID), 
             family = "binomial", 
             df_fec, 
             control = glmerControl(optimizer="bobyqa", 
                                    optCtrl = list(maxfun = 2000000)))



load("graph/final_true_models.Rdata")
true <- glmer(true_repro ~ -1 + ageClass/(TWin * PWin) + MassAutumn_tm1 + (1 |ID), 
              df_fec, 
              family = "binomial",
              control = glmerControl(optimizer="bobyqa", 
                                     optCtrl = list(maxfun = 2000000)))


plot(df_fec$TWin ~ df_fec$yr)



# extract model coefficients with uncertainty and generate NEW predicted leslie matrix 

newLeslie <- function(survm=surv, fec = raw, env = newdata[1,]){
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


#unscale
# q.SD<-sd(dt3$snow_log_up_jul)
# q.MEAN<-mean(dt3$snow_log_up_jul)
# newd$snow<-(newd[,"snow_z"]*q.SD) + q.MEAN
# 


env.now <- data.frame(TWin = mean(dataFecUnscld$TWin, na.rm = T),
                      T.WIN = mean(dataSurvUnscld$T.WIN , na.rm = T),
                      NDVIsurvT = mean(dataSurvUnscld$NDVIsurvT, na.rm = T),
                      PWin = mean(dataFecUnscld$PWin, na.rm = T),
                      # dont if put PWin?? 
                      MassAutumn_tm1 = mean(dataFecUnscld$MassAutumn_tm1, na.rm = T),
                      pred = "0")

# this is the data under climate change HOWEVER IT IS NOW SCALED
env.future <- data.frame(TWin = mean(dataFecUnscld$TWin, na.rm = T),
                         T.WIN = mean(dataSurvUnscld$T.WIN , na.rm = T),
                         NDVIsurvT = mean(dataSurvUnscld$NDVIsurvT, na.rm = T) - 19, # to change 
                         PWin = mean(dataFecUnscld$PWin, na.rm = T),
                         # dont if put PWin?? 
                         MassAutumn_tm1 = mean(dataFecUnscld$MassAutumn_tm1, na.rm = T),
                         pred = "0")




# raw fec # need 10 000
lambda.raw.now = 
  sapply(1:100, function(it){
    bob = eigen.analysis(newLeslie(fec = raw,env = env.now))
    return(bob$lambda1)
  })
hist(lambda.raw.now)



lambda.raw.future = 
  sapply(1:100, function(it){ # sapply fait des itérations 
    bob = eigen.analysis(newLeslie(fec = raw,env = env.future))
    return(bob$lambda1)
  })
hist(lambda.raw.future)


# make TRUE fecondity 

lambda.true.now = 
  sapply(1:100, function(it){
    bob = eigen.analysis(newLeslie(fec = true,env = env.now))
    return(bob$lambda1)
  })
hist(lambda.true.now)

lambda.true.future = 
  sapply(1:100, function(it){ 
    bob = eigen.analysis(newLeslie(fec = true,env = env.future))
    return(bob$lambda1)
  })
hist(lambda.true.future)


lambdaPheno <- data.frame(lambda.true.now, 
                     lambda.true.future)

# figures pheno -----------------------------------------------------------



projectionPheno <- ggplot(lambdaPheno, aes(lambda.true.now, fill = "true.now")) + 
  geom_density(alpha = 0.5) + 
  geom_density(aes(lambda.true.future, fill = "true.future"), alpha = 0.5) + 
  labs(x=expression('Population growth rate based on reproduction'), 
       y="Density") +
  theme_cowplot() 
projectionPheno








# Old codes  --------------------------------------------------------------


##### fcts GetLowUp and GetLambda ####

GetLowUp <- function(x,se){
  
  lower <- x - 1.96*se
  upper <- x + 1.96*se
  
  resultVector <- c(lower,x,upper)
  return(resultVector)
}


#faire un dataframe test  
DoDataframe <- function(intercept,SEinter, b,SEb){
  
  d <- matrix(nrow= 3, ncol= 2)
  colnames(d) <- c("intercept", "beta")
  d <- as.data.frame(d)
  
  vecInter <- GetLowUp(intercept,SEinter)
  vecBeta <- GetLowUp(b,SEb)
  
  d$intercept <- vecInter
  d$beta <- vecBeta
  
  return(d)
  
}


test <- DoDataframe(0.90082,0.2065,-0.63192,0.20346)

demoRatesAll <- read_csv("data/demoRatesAll.csv")
demoRatesAll$X1 <- NULL
P = 3

GetLambda <- function(P,it){
  
  L <- matrix(nrow=9,ncol=9,data=0)
  
  L <- allYearRF 
  
  L[1,4] = inv.logit(as.matrix(test[it,]) %*% t(t(c(1,P))))* demoRatesAll[1,"RT48"]
  L[1,5] = inv.logit(as.matrix(test[it,]) %*% t(t(c(1,P))))*demoRatesAll[1,"RT48"]
  L[1,6] = inv.logit(as.matrix(test[it,]) %*% t(t(c(1,P))))*demoRatesAll[1,"RT48"]
  L[1,7] = inv.logit(as.matrix(test[it,]) %*% t(t(c(1,P))))*demoRatesAll[1,"RT48"]
  L[1,8] = inv.logit(as.matrix(test[it,]) %*% t(t(c(1,P))))*demoRatesAll[1,"RT48"]
  #L[1,9] = inv.logit(as.matrix(test1[it,]) %*% t(t(c(1,P))))*inv.logit(test_r[it,1])
  
  #L[2,1] = inv.logit(as.matrix(test0[it,]) %*% t(t(c(1,P))))
  #L[3,2] = inv.logit(as.matrix(test1[it,]) %*% t(t(c(1,P))))
  #L[4,3] = inv.logit(as.matrix(test1[it,]) %*% t(t(c(1,P))))
  #L[5,4] = inv.logit(as.matrix(test1[it,]) %*% t(t(c(1,P))))
  # L[6,5] = inv.logit(as.matrix(test1[it,]) %*% t(t(c(1,P))))
  # L[7,6] = inv.logit(as.matrix(test1[it,]) %*% t(t(c(1,P))))
  # L[8,7] = inv.logit(as.matrix(test1[it,]) %*% t(t(c(1,P))))
  # L[9,8] = inv.logit(as.matrix(test1[it,]) %*% t(t(c(1,P))))
  # L[9,9] = inv.logit(as.matrix(test1[it,]) %*% t(t(c(1,P))))
  
  return(c(eigen.analysis(L)$lambda1,eigen.analysis(L)$stable.stage))
  
}



Pmin= rangeVari[1,"P.Win"] ;  Pmax=rangeVari[3,"P.Win"]     # set hunting pressure prediction range 
BigTest <- llply(seq(Pmin,Pmax,length.out = 2), function(x) {
  lbz <- nrow(test)
  for(it in 1: nrow(test)){
    lbz[it] <- GetLambda(P = x,it)[1]
  }
  return(lbz)
})



lambdaz=data.frame(P=seq(Pmin,Pmax,length.out = 100),
                   lambda_short=laply(L_short,print))


##### Range variables ####

library(readr)
TP <- read_csv("data/Localweather_seasons.csv")
library(readxl)
PS <- read_excel("data/season_climate_ram.xlsx")


allVari <- merge(TP,pheno, by.x = "yr", by.y = "year") 
allVari <- merge(allVari, PS, by.x = "yr", by.y = "yr")
names(allVari)
allVari <- allVari[,-c(11, 13, 15, 17, 19, 21, 23)] # we'll now keep spring dates 

names(allVari)


names(allVari) <- c("yr","T.Win","P.Win","T.SPRING","P.SPRING","T.SUMMER","P.SUMMER", "T.AUT","P.AUT",
"ndviGU","eviGU", "laiGU", "gppGU", "snowGU","psnnetGU", "fparGU","SummerNDVI", "SummerEVI" , "SummerLAI" ,"SummerGPP",  "SummerSnow",
"SummerPSNNET", "SummerFPAR", "WinNDVI", "WinEVI", "WinLAI", "WinGPP", "WinSnow",
 "WinPSNNET",  "WinFPAR", "PDO.winter", "PDO.spring", "PDO.summer", "PDO.fall","SOI.winter",
"SOI.spring", "SOI.summer", "SOI.fall")

rangeVari <- matrix(nrow = 3, ncol = 38)
n <- names(allVari)
n[1] <- "type"
colnames(rangeVari) <-n
rangeVari <- as.data.frame(rangeVari)
rangeVari$type<- c("min","mean","max")

names(rangeVari)

for(i in 2:38){
  
  x <-  allVari[,i]
  
  x <- as.numeric(as.character(x))
  
  min <- min(x,na.rm = T)
  rangeVari[1,i] <- min
  
  mean <- mean(x, na.rm =T)
  rangeVari[2,i] <- mean
  mean
  
  max <- max(x, na.rm = T)
  rangeVari[3,i] <- max
  
}

#write.csv(rangeVari, file = "data/range_all_variables.csv")
