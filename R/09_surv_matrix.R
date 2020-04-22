#09_surv_matrix
# Modified by Sandrine Beaumont-Courteau 
# 22/04/2020

# This code create the Leslie matrix for all years combined
# Also create a table with the demographic rates used for the matrix
# For Raw and True reproduction




# Librairies 

library(popbio)
library(readr)
library(googledrive)
library(xtable)
library(readxl)



#drive_download("OWPC/Analyses/data/sheep_data.txt")
#pop <- read.table("/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/sheep_data.txt", header = T)


# Database from the R object in the google drive >Analyses>cache

pop <- sheep_data


# New column surv where survival is coded as 0 or 1 
pop$surv <- ifelse(pop$alive_t1 == FALSE, 0, ifelse(pop$alive_t1 == TRUE, 1, pop$alive_t1))


#ageClass from dataset not the same then the one I will be using 
#but still preserved the original class in a new column

pop$ageClassOld <- pop$ageClass


#New column ageClass (0,1,2,37,8)

pop$ageClass <- ifelse(pop$age >= 8, 8, pop$age)
c37 <- c(3:7)
pop$ageClass <- ifelse(pop$age %in% c37 , 37, pop$ageClass)


#Remove first year translocation
pop <- droplevels(pop[pop$first_yr_trans == 0, ])


#####  Bootstrap  #####

#To calculate SE on vital rates
binSE <- function(x){ sqrt((sum(x)/length(x))*(1-(sum(x)/length(x)))/length(x))} #For survival
se <- function(x) sd(x)/sqrt(length(x)) #For reproduction 
#From Joanie Van de Wall


# == Make Dataframe for sample size and demo rates == #


demoRatesAll <- matrix(nrow=1, ncol=30)
colnames(demoRatesAll) <- c("S0R","S0RSE","S0T","S0TSE","S1","S1SE","S2","S2SE","S37","S37SE","S8","S8SE",
                            "RR3","RR3SE","RR48","RR48SE","RR9","RR9SE",
                            "RT3","RT3SE","RT48","RT48SE","RT9","RT9SE",
                            "lambdaR","lambdaRUP","lambdaRDOWN","lambdaT","lambdaTUP","lambdaTDOWN")

demoRatesAll <- as.data.frame(demoRatesAll)

sampleSizeAll <- matrix(nrow = 1, ncol = 6)
colnames(sampleSizeAll) <- c("0R","0T","1","2","3to7","8Plus")
sampleSizeAll <- as.data.frame(sampleSizeAll)
#POUR REPRO SAMPLE 8+ est 129 bc A28 morte sans qu'on sache si elle s'était reproduit



# ================= CALCULATE DEMOGRAPHIC RATES ===================== # 

# In new dataset, the gost where already divided by 2 
#Also, in 1999, an adult had the ID 20R
#No other lamb had an ID starting by a number! 

Y0 <- pop[pop$age == 0, ]
Y0$substr <- substr(Y0$ID,1,2)
ghost <- Y0[Y0$substr == "20" | Y0$substr == "19",]
notGhost <- Y0[Y0$substr != "20" & Y0$substr != "19",]

newY0 <- Y0 # Because didnt have to divide the ghost lamb in 2 to account for sex

#randomGhost <- ghost[sample(nrow(ghost), length(ghost$ID)/2), ]
#newY0 <- rbind(notGhost,randomGhost)

s0R <- newY0$surv
sampleSizeAll[, "0R"] <- length(s0R)
S0R <- mean(newY0$surv, na.rm = TRUE)
demoRatesAll[, "S0R"] <- S0R
SES0R <- binSE(s0R)
demoRatesAll[, "S0RSE"] <- SES0R

s0T <- notGhost$surv
sampleSizeAll[, "0T"] <- length(s0T)
S0T <- mean(notGhost$surv,na.rm =T)
demoRatesAll[, "S0T"] <- S0T
SES0T <- binSE(s0T)
demoRatesAll[, "S0TSE"] <- SES0T


s1 <- na.omit(pop[pop$ageClass==1, "surv"])
sampleSizeAll[, "1"] <- length(s1)
S1 <- mean(s1)
demoRatesAll[, "S1"] <- S1
SES1 <- binSE(s1) 
demoRatesAll[, "S1SE"] <- SES1

s2 <- na.omit(pop[pop$ageClass==2, "surv"])
sampleSizeAll[, "2"] <- length(s2)
S2 <- mean(s2)
demoRatesAll[, "S2"] <- S2
SES2 <- binSE(s2) 
demoRatesAll[, "S2SE"] <- SES2

s37 <- na.omit(pop[pop$ageClass==37, "surv"])
sampleSizeAll[, "3to7"] <- length(s37)
S37 <- mean(s37)
demoRatesAll[, "S37"] <- S37
SES37 <- binSE(s37) 
demoRatesAll[, "S37SE"] <- SES37

s8 <- na.omit(pop[pop$ageClass==8, "surv"])
sampleSizeAll[, "8Plus"] <- length(s8)
S8 <- mean(s8)
demoRatesAll[, "S8"] <- S8
SES8 <- binSE(s8) 
demoRatesAll[, "S8SE"] <- SES8

#Raw repro

pop_repro <- pop[-is.na(pop$true_repro),]

rr3 <- pop_repro[which(pop_repro$age == 3), "raw_repro"]
RR3 <- mean(rr3)/2
demoRatesAll[, "RR3"] <- RR3
demoRatesAll[, "RR3SE"] <- se(rr3)


C48 <- c(4:8)
rr48 <- na.omit(pop_repro[pop_repro$age %in% C48, "raw_repro"])
RR48 <- mean(rr48)/2  
demoRatesAll[, "RR48"] <- RR48
demoRatesAll[, "RR48SE"] <- se(rr48)

rr9 <- pop_repro[which(pop_repro$ageClass >= 9), "raw_repro"]
RR9 <- mean(rr9)/2
demoRatesAll[, "RR9"] <- RR9
demoRatesAll[, "RR9SE"] <- se(rr9)


## true repro

rt3 <- pop_repro[which(pop_repro$age == 3), "true_repro"]
RT3 <- mean(rt3)/2
demoRatesAll[, "RT3"] <- RT3
demoRatesAll[, "RT3SE"] <- se(rt3)


C48 <- c(4:8)
rt48 <- na.omit(pop_repro[pop_repro$age %in% C48, "true_repro"])
RT48 <- mean(rt48)/2  
demoRatesAll[, "RT48"] <- RT48
demoRatesAll[, "RT48SE"] <- se(rt48)

rt9 <- pop_repro[which(pop_repro$ageClass >= 9), "true_repro"]
RT9 <- mean(rt9)/2
demoRatesAll[, "RT9"] <- RT9
demoRatesAll[, "RT9SE"] <- se(rt9)



# === Boot === # 

xs0R <- 1:length(s0R)
s0Rboot <- cbind(xs0R, s0R)

xs0T <- 1:length(s0T)
s0Tboot <- cbind(xs0T, s0T)

xs1 <- 1:length(s1)
s1boot <- cbind(xs1, s1)


xs2 <- 1:length(s2)
s2boot <- cbind(xs2, s2)
head(s2boot)

xs37 <- 1:length(s37)
s37boot <- cbind(xs37, s37)
head(s37boot)

xs8 <- 1:length(s8)
s8boot <- cbind(xs8, s8)
head(s8boot)

#raw
xrr3 <- 1:length(rr3)
RR3boot <- cbind(xrr3, rr3)
head(RR3boot)

xrr48 <- 1:length(rr48)
RR48boot <- cbind(xrr48,rr48)
head(RR48boot)

xrr9 <- 1:length(rr9)
RR9boot <- cbind(xrr9, rr9)
head(RR9boot)

#true
xrt3 <- 1:length(rt3)
RT3boot <- cbind(xrt3, rt3)
head(RT3boot)

xrt48 <- 1:length(rt48)
RT48boot <- cbind(xrt48,rt48)
head(RT48boot)

xrt9 <- 1:length(rt9)
RT9boot <- cbind(xrt9, rt9)
head(RT9boot)


# FECUNDITY RATES

#raw

FR2 <- mean(S2)*mean(RR3)

FR37 <- mean(S37)*mean(RR48)

FR8 <- mean(S8)*mean(RR9)

#true
FT2 <- mean(S2)*mean(RT3)

FT37 <- mean(S37)*mean(RT48)

FT8 <- mean(S8)*mean(RT9)


# Bootstrap 

#raw 

xRaw <- replicate(10000, {
  ibootcubssurvO <- sample(1:nrow(s0Rboot), replace = TRUE)
  bootS0 <- mean(s0Rboot[ibootcubssurvO,2])
  
  ibootc1surv <- sample(1:nrow(s1boot), replace = TRUE)
  bootS1 <- mean(s1boot[ibootc1surv,2])
  
  ibootc2surv <- sample(1:nrow(s2boot), replace = TRUE)
  bootS2 <- mean(s2boot[ibootc2surv,2])
  
  ibootc37surv <- sample(1:nrow(s37boot), replace = TRUE)
  bootS37 <- mean(s37boot[ibootc37surv,2])
  
  
  ibootc8surv <- sample(1:nrow(s8boot), replace = TRUE)
  bootS8 <- mean(s8boot[ibootc8surv,2])
  
  # Reproduction rates
  ibootc3repro <- sample(1:nrow(RR3boot), replace = TRUE)
  bootR3 <- mean(RR3boot[ibootc3repro,2])
  
  ibootc48repro <- sample(1:nrow(RR48boot), replace = TRUE)
  bootR48 <- mean(RR48boot[ibootc48repro,2])
  
  ibootc9repro <- sample(1:nrow(RR9boot), replace = TRUE)
  bootR9 <- mean(RR9boot[ibootc9repro,2],na.rm = T)
  
  # Fecundity rates
  bootF2= bootS2*bootR3/2 
  bootF37= bootS37*bootR48/2 
  bootF8= bootS8*bootR9/2 
  
  
  #matrix
  # MATRIX
  Mboot<-matrix(nrow=9,ncol=9,data=0)
  Mboot[1,3] = bootF2
  Mboot[1,4] = bootF37
  Mboot[1,5] = bootF37
  Mboot[1,6] = bootF37
  Mboot[1,7] = bootF37
  Mboot[1,8] = bootF37
  Mboot[1,9] = bootF8  # <--F8
  
  Mboot[2,1] = bootS0
  Mboot[3,2] = bootS1
  Mboot[4,3] = bootS2
  Mboot[5,4] = bootS37
  Mboot[6,5] = bootS37
  Mboot[7,6] = bootS37
  Mboot[8,7] = bootS37
  Mboot[9,8] = bootS37
  Mboot[9,9] = bootS8
  Mboot
  
  #lambda
  eigen.analysis(Mboot, zero=T)$lambda 
  
})

hist(xRaw)
mean(xRaw) 
demoRatesAll[, "lambdaRDOWN"] <- quantile(xRaw, 0.025)
demoRatesAll[, "lambdaRUP"] <- quantile(xRaw, 0.975)




xTrue <- replicate(10000, {
  ibootcubssurvO <- sample(1:nrow(s0Tboot), replace = TRUE)
  bootS0 <- mean(s0Tboot[ibootcubssurvO,2])
  
  ibootc1surv <- sample(1:nrow(s1boot), replace = TRUE)
  bootS1 <- mean(s1boot[ibootc1surv,2])
  
  ibootc2surv <- sample(1:nrow(s2boot), replace = TRUE)
  bootS2 <- mean(s2boot[ibootc2surv,2])
  
  ibootc37surv <- sample(1:nrow(s37boot), replace = TRUE)
  bootS37 <- mean(s37boot[ibootc37surv,2])
  
  
  ibootc8surv <- sample(1:nrow(s8boot), replace = TRUE)
  bootS8 <- mean(s8boot[ibootc8surv,2])
  
  # Reproduction rates
  ibootc3repro <- sample(1:nrow(RT3boot), replace = TRUE)
  bootR3 <- mean(RT3boot[ibootc3repro,2])
  
  ibootc48repro <- sample(1:nrow(RT48boot), replace = TRUE)
  bootR48 <- mean(RT48boot[ibootc48repro,2])
  
  ibootc9repro <- sample(1:nrow(RT9boot), replace = TRUE)
  bootR9 <- mean(RT9boot[ibootc9repro,2], na.rm = T)
  
  # Fecundity rates
  bootF2= bootS2*bootR3/2 
  bootF37= bootS37*bootR48/2 
  bootF8= bootS8*bootR9/2 
  
  
  #matrix
  # MATRIX
  Mboot<-matrix(nrow=9,ncol=9,data=0)
  Mboot[1,3] = bootF2
  Mboot[1,4] = bootF37
  Mboot[1,5] = bootF37
  Mboot[1,6] = bootF37
  Mboot[1,7] = bootF37
  Mboot[1,8] = bootF37
  Mboot[1,9] = bootF8  # <--F8
  
  Mboot[2,1] = bootS0
  Mboot[3,2] = bootS1
  Mboot[4,3] = bootS2
  Mboot[5,4] = bootS37
  Mboot[6,5] = bootS37
  Mboot[7,6] = bootS37
  Mboot[8,7] = bootS37
  Mboot[9,8] = bootS37
  Mboot[9,9] = bootS8
  Mboot
  
  #lambda
  eigen.analysis(Mboot, zero=T)$lambda 
  
})


hist(xTrue)
mean(xTrue) 
demoRatesAll[, "lambdaTDOWN"] <- quantile(xTrue, 0.025)
demoRatesAll[, "lambdaTUP"] <- quantile(xTrue, 0.975)



####### Mean lambda #######
#Calculate survival for all years 


Y0 <- pop[pop$age == 0, ]
Y0$substr <- substr(Y0$ID,1,2)
ghost <- Y0[Y0$substr == "20" | Y0$substr == "19",]
notGhost <- Y0[Y0$substr != "20" & Y0$substr != "19",]

newY0 <- Y0



S0R <- mean(newY0$surv, na.rm = TRUE)
S0T <- mean(notGhost$surv,na.rm =T)

S1 <- mean(na.omit(pop[which(pop$ageClass == 1), "surv"]))
S2 <- mean(pop[which(pop$ageClass == 2), "surv"], na.rm = TRUE)
S37 <- mean(pop[which(pop$ageClass == 37), "surv"], na.rm = TRUE)
S8 <- mean(pop[which(pop$ageClass == 8),"surv"],na.rm = TRUE)

#Calculate raw repro for all years 
R0 <- mean(pop[which(pop$ageClass==0), "raw_repro"], na.rm =TRUE)/2
R1 <- mean(pop[which(pop$ageClass == 1), "raw_repro"], na.rm = TRUE)/2
R2 <- mean(pop[which(pop$ageClass == 2), "raw_repro"], na.rm = TRUE)/2
R37 <- mean(pop[which(pop$ageClass == 37), "raw_repro"], na.rm = TRUE)/2
R8 <- mean(pop[which(pop$ageClass == 8), "raw_repro"], na.rm = TRUE)/2

#Calculate true repro for all year 
T0 <- mean(pop[which(pop$ageClass==0), "true_repro"], na.rm =TRUE)/2
T1 <- mean(pop[which(pop$ageClass == 1), "true_repro"], na.rm = TRUE)/2
T2 <- mean(pop[which(pop$ageClass == 2), "true_repro"], na.rm = TRUE)/2
T37 <- mean(pop[which(pop$ageClass == 37), "true_repro"], na.rm = TRUE)/2
T8 <- mean(pop[which(pop$ageClass == 8), "true_repro"], na.rm = TRUE)/2


#For fecundity, need repro 3,  4 to 8 and 9+
R3 <- mean(pop[which(pop$age == 3), "raw_repro"], na.rm = TRUE)/2
R48 <- mean(pop[which(pop$age >= 4 & pop$age <= 8), "raw_repro"], na.rm = TRUE)/2
R9 <- mean(pop[which(pop$ageClass >= 9), "raw_repro"], na.rm = TRUE)/2

T3 <- mean(pop[which(pop$age == 3), "true_repro"], na.rm = TRUE)/2
T48 <- mean(pop[which(pop$age >= 4 & pop$age <= 8), "true_repro"], na.rm = TRUE)/2
T9 <- mean(pop[which(pop$age >= 9), "true_repro"], na.rm = TRUE)/2

#Calculate raw fecundity all year
RF0 <- S0R*R1
RF1 <- S1*R2
RF2 <- S2*R3
RF37 <- S37*R48
RF8 <- S8*R9


#Calculate true fecundity all year 
TF0 <- S0T*T1
TF1 <- S1*T2
TF2 <- S2*T3
TF37 <- S37*T48
TF8 <- S8*T9


# Make a table

allYrTbl <- data.frame(Age_Class = c("0","1","2","3 à 7","8+"),
                       Survival_Raw = c(S0R,S1,S2,S37,S8),
                       Survival_True = c(S0T,S1,S2,S37,S8),
                       Raw_Repro = c(R0,R1,R2,R37,R8),
                       True_Repro = c(T0,T1,T2,T37,T8),
                       Raw_Fecundity = c(RF0,RF1,RF2,RF37,RF8),
                       True_Fecundity = c(TF0,TF1,TF2,TF37,TF8))

allYrTbl <- xtable(allYrTbl)


##RAW FECUNDITY

# Make empty squared matrix
m <- matrix(nrow=9,ncol=9,data=0)

##Add fecundity##

# Create a vector with the fecundity

f <- c(RF0,RF1,RF2,RF37,RF37,RF37,RF37,RF37,RF8)

for(i in 1:length(f)){
  m[1,i] <- f[i]
}

##Add survival##

s <- c(S0R,S1,S2,S37,S37,S37,S37,S37,S8)

for(i in 1:length(s)){
  if(i == length(s)){
    m[i,i] <- s[i]
    m[i,i-1] <- s[i-1]
    
  }else{
    m[i+1,i] <- s[i]
  }
}

allYearRF <- m
lambda_allYear_RF <- eigen.analysis(m)$lambda1
demoRatesAll[,"lambdaR"] <- lambda_allYear_RF

##TRUE FECUNDITY

# Make empty squared matrix
m <- matrix(nrow=9,ncol=9,data=0)

##Add fecundity##

# Create a vector with the fecundity

f <- c(TF0,TF1,TF2,TF37,TF37,TF37,TF37,TF37,TF8)

for(i in 1:length(f)){
  m[1,i] <- f[i]
}

##Add survival##

s <- c(S0T,S1,S2,S37,S37,S37,S37,S37,S8)

for(i in 1:length(s)){
  if(i == length(s)){
    m[i,i] <- s[i]
    m[i,i-1] <- s[i-1]
    
  }else{
    m[i+1,i] <- s[i]
  }
}

allYearTF <- m 
lambda_allYear_TF <- eigen.analysis(m)$lambda1
demoRatesAll[,"lambdaT"] <- lambda_allYear_TF


#### Elasticité ####

elasticity <- matrix(nrow=17,ncol=3,data=0)
colnames(elasticity) <- c("variables","elasticities_RAW","elasticities_TRUE")
elasticity <- as.data.frame(elasticity)
elasticity$variables <- c("S0","S1","S2","S3","S4","S5","S6","S7","S8Plus",
                          "F2","F3","F4","F5","F5","F6","F7","F8Plus")


# Raw #

ER <- eigen.analysis(allYearRF)$elasticities 

elasticity[which(elasticity$variables == "S0"), "elasticities_RAW"] <- ER[2,1]
elasticity[which(elasticity$variables == "S1"), "elasticities_RAW"]<- ER[3,2]
elasticity[which(elasticity$variables == "S2"), "elasticities_RAW"]<- ER[4,3]
elasticity[which(elasticity$variables == "S3"), "elasticities_RAW"]<- ER[5,4]
elasticity[which(elasticity$variables == "S4"), "elasticities_RAW"]<- ER[6,5]
elasticity[which(elasticity$variables == "S5"), "elasticities_RAW"]<- ER[7,6]
elasticity[which(elasticity$variables == "S6"), "elasticities_RAW"]<- ER[8,7]
elasticity[which(elasticity$variables == "S7"), "elasticities_RAW"]<- ER[9,8]
elasticity[which(elasticity$variables == "S8Plus"), "elasticities_RAW"]<- ER[9,9]

elasticity[which(elasticity$variables == "F2"), "elasticities_RAW"]<- ER[1,3]
elasticity[which(elasticity$variables == "F3"), "elasticities_RAW"]<- ER[1,4]
elasticity[which(elasticity$variables == "F4"), "elasticities_RAW"]<- ER[1,5]
elasticity[which(elasticity$variables == "F5"), "elasticities_RAW"]<- ER[1,6]
elasticity[which(elasticity$variables == "F6"), "elasticities_RAW"]<- ER[1,7]
elasticity[which(elasticity$variables == "F7"), "elasticities_RAW"]<- ER[1,8]
elasticity[which(elasticity$variables == "F8Plus"), "elasticities_RAW"]<- ER[1,9]


#True

ET <- eigen.analysis(allYearTF)$elasticities 

elasticity[which(elasticity$variables == "S0"), "elasticities_TRUE"] <- ET[2,1]
elasticity[which(elasticity$variables == "S1"), "elasticities_TRUE"]<- ET[3,2]
elasticity[which(elasticity$variables == "S2"), "elasticities_TRUE"]<- ET[4,3]
elasticity[which(elasticity$variables == "S3"), "elasticities_TRUE"]<- ET[5,4]
elasticity[which(elasticity$variables == "S4"), "elasticities_TRUE"]<- ET[6,5]
elasticity[which(elasticity$variables == "S5"), "elasticities_TRUE"]<- ET[7,6]
elasticity[which(elasticity$variables == "S6"), "elasticities_TRUE"]<- ET[8,7]
elasticity[which(elasticity$variables == "S7"), "elasticities_TRUE"]<- ET[9,8]
elasticity[which(elasticity$variables == "S8Plus"), "elasticities_TRUE"]<- ET[9,9]

elasticity[which(elasticity$variables == "F2"), "elasticities_TRUE"]<- ET[1,3]
elasticity[which(elasticity$variables == "F3"), "elasticities_TRUE"]<- ET[1,4]
elasticity[which(elasticity$variables == "F4"), "elasticities_TRUE"]<- ET[1,5]
elasticity[which(elasticity$variables == "F5"), "elasticities_TRUE"]<- ET[1,6]
elasticity[which(elasticity$variables == "F6"), "elasticities_TRUE"]<- ET[1,7]
elasticity[which(elasticity$variables == "F7"), "elasticities_TRUE"]<- ET[1,8]
elasticity[which(elasticity$variables == "F8Plus"), "elasticities_TRUE"]<- ET[1,9]



#write.csv(elasticity, file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/elasticity.csv" )


##### Stable age structure ####

# Raw #

stable_stage_Raw <- eigen.analysis(allYearRF)$stable.stage

# True #

stable_stage_True <- eigen.analysis(allYearTF)$stable.stage



##### Generation Time ####

# Raw #

generation_time_Raw <- generation.time(allYearRF)

# True #

generation_time_True <- generation.time(allYearTF)

#save(stable_stage_Raw,stable_stage_True,generation_time_Raw,generation_time_True,file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/stable_age_str_Gen_time.RData")



#####Sensitivity####
SR <- eigen.analysis(allYearRF)$sensitivities
ST <- eigen.analysis(allYearTF)$sensitivities




#INFO

# lambda all years grouped, using RAW fecundity = 0.9996975 (11/02/2020)
# lambda all years grouped, using TRUE fecundity = 1.010092 (11/02/2020)

#After adding 1999
# lambda all years grouped, using RAW fecundity = 0.9839261 (22/04/2020)
# lambda all years grouped, using TRUE fecundity = 0.9976678 (22/04/2020)


##############################################


write.csv(demoRatesAll,file="/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/demoRatesAll.csv")
write.csv(sampleSizeAll,file="/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/sampleSizeAll.csv")
#save(allYearRF,allYearTF, file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/Matrice_All_Years.RData")


                        
