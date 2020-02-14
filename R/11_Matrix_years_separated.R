# Matrices chaque annnée 


library(popbio)

#load data and change it same as 06_surv_matrix
pop <- read.table("/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/sheep_data.txt", header = T)
pop$surv <- ifelse(pop$alive_t1 == FALSE, 0, ifelse(pop$alive_t1 == TRUE, 1, pop$alive_t1))
pop$ageClass <- ifelse(pop$age >= 8, 8, pop$age)
c37 <- c(3:7)
pop$ageClass <- ifelse(pop$age %in% c37 , 37, pop$ageClass)
pop <- droplevels(pop[pop$first_yr_trans == 0, ])

popFull <- pop



#####Bootstrap####
demoRates <- matrix(nrow=17, ncol=31)
colnames(demoRates) <- c("year","S0R","S0RSE","S0T","S0TSE","S1","S1SE","S2","S2SE","S37","S37SE","S8","S8SE",
                         "RR3","RR3SE","RR48","RR48SE","RR9","RR9SE",
                         "RT3","RT3SE","RT48","RT48SE","RT9","RT9SE",
                         "lambdaR","lambdaRUP","lambdaRDOWN","lambdaT","lambdaTUP","lambdaTDOWN")
demoRates <- as.data.frame(demoRates)

sampleSize <- matrix(nrow = 17, ncol = 7)
colnames(sampleSize) <- c("year","0R","0T","1","2","3to7","8Plus")
sampleSize<- as.data.frame(sampleSize)


YrVector <- c(2000:2016)


for(i in 1:length(YrVector)){
  
  yr <- YrVector[i]
  
  sampleSize[i,"year"] <- yr
  demoRates[i, "year"] <- yr
  
  d <- pop[pop$yr == yr,] 
  
  Y0 <- d[d$age == 0, ]
  Y0$substr <- substr(Y0$ID,1,2)
  ghost <- Y0[Y0$substr == "20",]
  notGhost <- Y0[Y0$substr != "20",]
  randomGhost <- ghost[sample(nrow(ghost), length(ghost$ID)/2), ]
  newY0 <- rbind(notGhost,randomGhost)
  
  s0R <- newY0$surv
  sampleSize[i, "0R"] <- length(s0R)
  S0R <- mean(newY0$surv, na.rm = TRUE)
  demoRates[i, "S0R"] <- S0R
  SES0R <- binSE(s0R)
  demoRates[i, "S0RSE"] <- SES0R
  

  s0T <- notGhost$surv
  sampleSize[i, "0T"] <- length(s0T)
  S0T <- mean(notGhost$surv,na.rm =T)
  demoRates[i, "S0T"] <- S0T
  SES0T <- binSE(s0T)
  demoRates[i, "S0TSE"] <- SES0T
  
  
  s1 <- na.omit(d[d$ageClass==1, "surv"])
  sampleSize[i, "1"] <- length(s1)
  S1 <- mean(s1)
  demoRates[i, "S1"] <- S1
  SES1 <- binSE(s1) 
  demoRates[i, "S1SE"] <- SES1
  
  s2 <- na.omit(d[d$ageClass==2, "surv"])
  sampleSize[i, "2"] <- length(s2)
  S2 <- mean(s2)
  demoRates[i, "S2"] <- S2
  SES2 <- binSE(s2) 
  demoRates[i, "S2SE"] <- SES2
  
  s37 <- na.omit(d[d$ageClass==37, "surv"])
  sampleSize[i, "3to7"] <- length(s37)
  S37 <- mean(s37)
  demoRates[i, "S37"] <- S37
  SES37 <- binSE(s37) 
  demoRates[i, "S37SE"] <- SES37
  
  s8 <- na.omit(d[d$ageClass==8, "surv"])
  sampleSize[i, "8Plus"] <- length(s8)
  S8 <- mean(s8)
  demoRates[i, "S8"] <- S8
  SES8 <- binSE(s8) 
  demoRates[i, "S8SE"] <- SES8
  
  
  
  #Raw repro
  
  pop_repro <- pop[-is.na(pop$true_repro),]
  pop_repro <- pop_repro[pop_repro$yr == yr,] 
  
  rr3 <- pop_repro[which(pop_repro$age == 3), "raw_repro"]
  RR3 <- mean(rr3)/2
  demoRates[i, "RR3"] <- RR3
  demoRates[i, "RR3SE"] <- se(rr3)
  
  
  C48 <- c(4:8)
  rr48 <- na.omit(pop_repro[pop_repro$age %in% C48, "raw_repro"])
  RR48 <- mean(rr48)/2  
  demoRates[i, "RR48"] <- RR48
  demoRates[i, "RR48SE"] <- se(rr48)
  
  rr9 <- pop_repro[which(pop_repro$ageClass >= 9), "raw_repro"]
  RR9 <- mean(rr9)/2
  demoRates[i, "RR9"] <- RR9
  demoRates[i, "RR9SE"] <- se(rr9)
  
  
  ## true repro
  
  rt3 <- pop_repro[which(pop_repro$age == 3), "true_repro"]
  RT3 <- mean(rt3)/2
  demoRates[i, "RT3"] <- RT3
  demoRates[i, "RT3SE"] <- se(rt3)
  
  
  C48 <- c(4:8)
  rt48 <- na.omit(pop_repro[pop_repro$age %in% C48, "true_repro"])
  RT48 <- mean(rt48)/2  
  demoRates[i, "RT48"] <- RT48
  demoRates[i, "RT48SE"] <- se(rt48)
  
  rt9 <- pop_repro[which(pop_repro$ageClass >= 9), "true_repro"]
  RT9 <- mean(rt9)/2
  demoRates[i, "RT9"] <- RT9
  demoRates[i, "RT9SE"] <- se(rt9)
  
  
  
  if(yr == 2007 | yr == 2008){
  next
  }
  
  
  
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
    bootR9 <- mean(RR9boot[ibootc9repro,2])
    
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
  
  #hist(xRaw)
  #mean(xRaw) 
  demoRates[i, "lambdaRDOWN"] <- quantile(xRaw, 0.025)
  demoRates[i, "lambdaRUP"] <- quantile(xRaw, 0.975)
  
  
  
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
    bootR9 <- mean(RT9boot[ibootc9repro,2])
    
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
  
  #hist(xTrue)
  #mean(xTrue) 
  demoRates[i, "lambdaTDOWN"] <- quantile(xTrue, 0.025)
  demoRates[i, "lambdaTUP"] <- quantile(xTrue, 0.975)
}


######Create matrices for each year #####

# Empty list
YrVector <- c(2000:2016)
matricesRF <- vector("list", length(YrVector))
names(matricesRF) <- YrVector
matricesTF <- vector("list", length(YrVector))
names(matricesTF) <- YrVector


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
  
  
  annualRepro[annualRepro$year == yr, "S0R"] <- S0R
  annualRepro[annualRepro$year == yr, "S0T"] <- S0T
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
    demoRates[demoRates$year==yr,"lambdaR"]<-lam
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
    demoRates[demoRates$year==yr,"lambdaT"]<-lam
  } else {
    AGR[AGR$year==yr,"lambdaTF"] <- NA
  }
}










######### À REVOIR #########
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


#CANNOT PUT IT IN THE DEMORATES DATAFRAME

#demoRates[demoRates$year == 2007,"lambdaT"]<-lamTF_2007
#demoRates[demoRates$year == 2007,"lambdaR"]<-lamRF_2007

#Pour 2008
M2008TF <- matricesTF[["2008"]]
M2008TF[1,3] <- 0
lamTF_2008 <- eigen.analysis(M2008TF)$lambda1

M2008RF <- matricesRF[["2008"]]
M2008RF[1,3] <- 0

lamRF_2008 <- eigen.analysis(M2008RF)$lambda1

AGR[AGR$year == 2008, "lambdaRF"] <- lamRF_2008
AGR[AGR$year == 2008, "lambdaTF"] <- lamTF_2008

#demoRates[demoRates$year == 2008,"lambdaT"]<-lamTF_2008
#demoRates[demoRates$year == 2008,"lambdaR"]<-lamRF_2008




histRF <- hist(AGR$lambdaRF)
histTF <- hist(AGR$lambdaTF)


col1 <- rgb(0, 80/255,153/255,0.85)
col2 <- rgb(210/255,115/255,0,0.85)

plot(AGR$lambdaRF ~ jitter(AGR$year, 1.5) , col = col1, pch = 16, ylim = c(0.7, 1.2), alpha = 0.05,
     xlab = "Year",
     ylab = "Lambda",
     cex = 1.5)
points(AGR$lambdaTF ~ AGR$year, col = col2, pch = 16, alpha= 0.05, cex = 1.5)
legend(x= 2008, y = 0.81, legend = c("lambda raw fecundity","lambda true fecundity"), 
       col = c(col1,col2),
       
       pch =16,
       cex =1.0)


#write.csv(AGR,file="/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/lambda.csv")
#write.csv(annualRepro,file="/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/VitalRates.csv")
#save(matricesRF,matricesTF, file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/Matrices.RData")



#write.csv(demoRates,file="/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/demoRates.csv")
#write.csv(sampleSize,file="/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/sampleSize.csv")


