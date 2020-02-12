# Matrice Dynamique de pop

library(popbio)
library(readr)
library(googledrive)
library(xtable)
library(readxl)



drive_download("OWPC/Analyses/data/sheep_data.txt")
pop <- read.table("/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/sheep_data.txt", header = T)

# New column surv where survival is coded as 0 or 1 
pop$surv <- ifelse(pop$alive_t1 == FALSE, 0, ifelse(pop$alive_t1 == TRUE, 1, pop$alive_t1))

#New column ageClass (0,1,2,37,8)

pop$ageClass <- ifelse(pop$age >= 8, 8, pop$age)
c37 <- c(3:7)
pop$ageClass <- ifelse(pop$age %in% c37 , 37, pop$ageClass)


#pop$ageClass <- as.numeric(pop$ageClass)


#Remove first year translocation
pop <- droplevels(pop[pop$first_yr_trans == 0, ])


##### Calculate Survival #####


#Calculate survival for all years 

# For survival of "ghost" lambs, divide their number in 2 to assume sex ratio of 1:1
#Create a subset of year 0

Y0 <- pop[pop$age == 0, ]
Y0$substr <- substr(Y0$ID,1,2)
ghost <- Y0[Y0$substr == "20",]
notGhost <- Y0[Y0$substr != "20",]
randomGhost <- ghost[sample(nrow(ghost), length(ghost$ID)/2), ]
newY0 <- rbind(notGhost,randomGhost)

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


##### Put in the matrix ####

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


######Create matrices for each year #####

# Empty list
YrVector <- c(2000:2016)
matricesRF <- vector("list", length(YrVector))
names(matricesRF) <- YrVector
matricesTF <- vector("list", length(YrVector))
names(matricesTF) <- YrVector

reproTbls <- vector("list", length(YrVector))
names(reproTbls) <- YrVector

#Create an empty table for year and lambda
#lambda is asymptotic growth rate
v <- (rep(NA,length(YrVector)))

AGR <- matrix(c(2000:2016,v,v), ncol=3)
colnames(AGR) <- c("year","lambdaRF","lambdaTF")
AGR <- as.data.frame(AGR)

#Create empty dataframe for reproductive output 
annualRepro <- matrix(nrow=17, ncol=22)
colnames(annualRepro) <- c("year","S0R","S0T","S1","S2","S37","S8",
                           "R1","R2","R3","R48","R9",
                           "RF0","RF1","RF2","RF37","RF8",
                           "TF0","TF1","TF2","TF37","TF8")
annualRepro <- as.data.frame(annualRepro)
annualRepro$year <- 2000:2016


#Loop to make the matrices

for(i in 1:length(YrVector)){
  
  yr <- YrVector[i]
  x <- pop[pop$yr == yr,]
  
  
  
  #Calculate survival 
  
  Y0 <- x[x$age == 0, ]
  
  Y0$substr <- substr(Y0$ID,1,2)
  
  ghost <- Y0[Y0$substr == "20",]
  
  notGhost <- Y0[Y0$substr != "20",]
  
  randomGhost <- ghost[sample(nrow(ghost), length(ghost$ID)/2), ]
  
  newY0 <- rbind(notGhost,randomGhost)
  
  S0R <- mean(newY0$surv, na.rm = TRUE)
  S0T <- mean(notGhost$surv, na.rm =T)
  
  S1 <- mean(x[which(x$ageClass == 1), "surv"], na.rm = TRUE)
  S2 <- mean(x[which(x$ageClass == 2), "surv"], na.rm = TRUE)
  S37 <- mean(x[which(x$ageClass == 37), "surv"], na.rm = TRUE)
  S8 <- mean(x[which(x$ageClass == 8),"surv"],na.rm = TRUE)


  #Calculate raw repro
  
  R0 <- mean(x[which(x$ageClass==0), "raw_repro"], na.rm =TRUE)/2
  R1 <- mean(x[which(x$ageClass == 1), "raw_repro"], na.rm = TRUE)/2
  R2 <- mean(x[which(x$ageClass == 2), "raw_repro"], na.rm = TRUE)/2
  R37 <- mean(x[which(x$ageClass == 37), "raw_repro"], na.rm = TRUE)/2
  R8 <- mean(x[which(x$ageClass == 8), "raw_repro"], na.rm = TRUE)/2
  
  #Calculate true repro 
  T0 <- mean(x[which(x$ageClass==0), "true_repro"], na.rm =TRUE)/2
  T1 <- mean(x[which(x$ageClass == 1), "true_repro"], na.rm = TRUE)/2
  T2 <- mean(x[which(x$ageClass == 2), "true_repro"], na.rm = TRUE)/2
  T37 <- mean(x[which(x$ageClass == 37), "true_repro"], na.rm = TRUE)/2
  T8 <- mean(x[which(x$ageClass == 8), "true_repro"], na.rm = TRUE)/2
  
  
  #For fecundity, need repro 3,  4 to 8 and 9+
  
  R3 <- mean(x[which(x$age == 3), "raw_repro"], na.rm = TRUE)/2
  R48 <- mean(x[which(x$age >= 4 & x$age <= 8), "raw_repro"], na.rm = TRUE)/2
  R9 <- mean(x[which(x$ageClass >= 9), "raw_repro"], na.rm = TRUE)/2
  
  T3 <- mean(x[which(x$age == 3), "true_repro"], na.rm = TRUE)/2
  T48 <- mean(x[which(x$age >= 4 & pop$age <= 8), "true_repro"], na.rm = TRUE)/2
  T9 <- mean(x[which(x$age >= 9), "true_repro"], na.rm = TRUE)/2
  
  #Calculate raw fecundity 
  
  RF0 <- S0R*R1
  RF1 <- S1*R2
  RF2 <- S2*R3
  RF37 <- S37*R48
  RF8 <- S8*R9
  
  
  #Calculate true fecundity 
  TF0 <- S0T*T1
  TF1 <- S1*T2
  TF2 <- S2*T3
  TF37 <- S37*T48
  TF8 <- S8*T9
  
  
  
  reproTbl <- data.frame(Age_Class = c("0","1","2","3 à 7","8+"),
                         Survival = c(S0,S1,S2,S37,S8),
                         Raw_Repro = c(R0,R1,R2,R37,R8),
                         True_Repro = c(T0,T1,T2,T37,T8),
                         Raw_Fecundity = c(RF0,RF1,RF2,RF37,RF8),
                         True_Fecundity = c(TF0,TF1,TF2,TF37,TF8))
  
  reproTbl <- xtable(reproTbl)
  
  reproTbls[[as.character(yr)]] <- reproTbl
  
  
  
  annualRepro[annualRepro$year == yr, "S0R"] <- S0R
  annualRepro[annualRepro$year == yr, "S0T"] <- S0R
  annualRepro[annualRepro$year == yr, "S1"] <- S1
  annualRepro[annualRepro$year == yr, "S2"] <- S2
  annualRepro[annualRepro$year == yr, "S37"] <- S37
  annualRepro[annualRepro$year == yr, "S8"] <- S8
  
  annualRepro[annualRepro$year == yr, "R1"] <- R1
  annualRepro[annualRepro$year == yr, "R2"] <- R2
  annualRepro[annualRepro$year == yr, "R3"] <- R3
  annualRepro[annualRepro$year == yr, "R48"] <- R48
  annualRepro[annualRepro$year == yr, "R9"] <- R9
 
  annualRepro[annualRepro$year == yr, "RF0"] <- RF0
  annualRepro[annualRepro$year == yr, "RF1"] <- RF1
  annualRepro[annualRepro$year == yr, "RF2"] <- RF2
  annualRepro[annualRepro$year == yr, "RF37"] <- RF37
  annualRepro[annualRepro$year == yr, "RF8"] <- RF8
  
  annualRepro[annualRepro$year == yr, "TF0"] <- TF0
  annualRepro[annualRepro$year == yr, "TF1"] <- TF1
  annualRepro[annualRepro$year == yr, "TF2"] <- TF2
  annualRepro[annualRepro$year == yr, "TF37"] <- TF37
  annualRepro[annualRepro$year == yr, "TF8"] <- TF8
  
  
  ##### Put in the matrix ####
  
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
  
  matricesRF[[as.character(yr)]] <- m
  
  if( RF1 != "NaN" & RF2 != "NaN" & RF37 != "NaN" & RF8 != "NaN" ){
    lam <- eigen.analysis(m)$lambda1
    AGR[AGR$year==yr,"lambdaRF"] <- lam
  } else {
    AGR[AGR$year==yr,"lambdaRF"] <- NA
  }
  
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
  
  matricesTF[[as.character(yr)]] <- m
  
  if( RF1 != "NaN" & RF2 != "NaN" & RF37 != "NaN" & RF8 != "NaN" ){
    lam <- eigen.analysis(m)$lambda1
    AGR[AGR$year==yr,"lambdaTF"] <- lam
  } else {
    AGR[AGR$year==yr,"lambdaTF"] <- NA
  }
}

#Ajuster 2007 et 2008 
M2007TF <- matricesTF[["2007"]]
TF2_2007 <- 0
(S2_2007TF <- allYearTF[4,3])
M2007TF[4,3] <- S2_2007TF
M2007TF[1,2:3] <- 0
lamTF_2007 <- eigen.analysis(M2007TF)$lambda1

M2007RF <- matricesRF[["2007"]]
RF2_2007 <- 0
(S2_2007RF <- allYearRF[4,3])
M2007RF[4,3] <- S2_2007RF
M2007RF[1,2:3] <- 0
lamRF_2007 <- eigen.analysis(M2007RF)$lambda1

AGR[AGR$year == 2007, "lambdaRF"] <- lamRF_2007
AGR[AGR$year == 2007, "lambdaTF"] <- lamTF_2007


#Pour 2008
M2008TF <- matricesTF[["2008"]]
M2008TF[1,3] <- 0
lamTF_2008 <- eigen.analysis(M2008TF)$lambda1

M2008RF <- matricesRF[["2008"]]
M2008RF[1,3] <- 0

lamRF_2008 <- eigen.analysis(M2008RF)$lambda1

AGR[AGR$year == 2008, "lambdaRF"] <- lamRF_2008
AGR[AGR$year == 2008, "lambdaTF"] <- lamTF_2008


hist(AGR$lambdaRF)
hist(AGR$lambdaTF)


plot(AGR$lambdaRF ~ AGR$year, col = "blue", pch = 1, ylim = c(0.7, 1.2))
points(AGR$lambdaTF ~ AGR$year, col = "orange", pch = 1)
legend("bottomright", legend = c("lambdaRF","lambdaTF"), 
       col = c("blue","orange"),
       lty = 1,
       pch =16)
