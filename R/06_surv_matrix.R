# Ménage Données OWPC
library(popbio)

# this has been moved to 02_tiding_sheep_data.R

# surv <- read_csv("Documents/Sherbrooke/OWPC/OWPC/SURV20170328.csv")
# colnames(surv)
# surv <- surv[, c("yr","age","ID","sex","alive_t1","reproduced", "age.class")]
# survF <- surv[surv$sex =="female",]

library(readr)
pheno <- read_csv("Documents/Sherbrooke/OWPC/OWPC/pheno_ram.csv", 
                      col_types = cols(evi_log_do = col_skip(), 
                                       evi_log_up = col_skip(), fpar_log_do = col_skip(), 
                                       fpar_log_up = col_skip(), gpp_log_do = col_skip(), 
                                       gpp_log_up = col_skip(), lai_log_do = col_skip(), 
                                       lai_log_up = col_skip(), ndvi_log_do = col_skip(), 
                                       ndvi_log_up = col_skip(), psnnet_log_do = col_skip(), 
                                       psnnet_log_up = col_skip(), snow_log_do = col_skip(), 
                                       snow_log_up = col_skip()))


#pheno <- pheno[,1:11]

pheno_surv <- merge(survF, pheno, by.x = "yr", by.y ="year")

pheno_surv$reproduced <- ifelse(pheno_surv$age == 0 | pheno_surv$age == 1 & is.na(pheno_surv$reproduced),0,pheno_surv$reproduced)

#save(pheno_surv,pheno,survF,file="~/Desktop/OWPC/data_frame.RData")

#write.csv(pheno_surv,file="~/Desktop/OWPC/phenoSurv.csv")

#####Add columns length SUMMER!#####
x <- pheno_surv

#x$lengthNDVI <- x$ndvi_log_do_jul-x$ndvi_log_up_jul 
#x$lengthEVI <- x$evi_log_do_jul-x$evi_log_up_jul
#x$lengthLAI <- x$lai_log_do_jul-x$lai_log_up_jul
#x$lengthGPP <- x$gpp_log_do_jul-x$gpp_log_up_jul
#x$lengthSnow <- x$snow_log_do_jul-x$snow_log_up_jul
#x$lengthEVI <- x$evi_log_do_jul-x$evi_log_up_jul
#x$lengthPSNNET <- x$psnnet_log_do_jul-x$psnnet_log_up_jul
#x$lengthFPAR <- x$fpar_log_do_jul-x$fpar_log_up_jul

x$SummerNDVI <- x$ndvi_log_do_jul-x$ndvi_log_up_jul 
x$SummerEVI <- x$evi_log_do_jul-x$evi_log_up_jul
x$SummerLAI <- x$lai_log_do_jul-x$lai_log_up_jul
x$SummerGPP <- x$gpp_log_do_jul-x$gpp_log_up_jul
x$SummerSnow <- x$snow_log_do_jul-x$snow_log_up_jul
x$SummerEVI <- x$evi_log_do_jul-x$evi_log_up_jul
x$SummerPSNNET <- x$psnnet_log_do_jul-x$psnnet_log_up_jul
x$SummerFPAR <- x$fpar_log_do_jul-x$fpar_log_up_jul
 
pheno_surv <- x



#####Add columns length WINTER#####

yr <- (2000:2017)
bisYr <- c(2000,2004,2008,2012,2016)


P <- pheno

nomCol <- c("year","ndvi_log_up_jul","ndvi_log_do_jul","evi_log_up_jul","evi_log_do_jul",
             "lai_log_up_jul","lai_log_do_jul","gpp_log_up_jul","gpp_log_do_jul","snow_log_up_jul",  
            "snow_log_do_jul","psnnet_log_up_jul","psnnet_log_do_jul","fpar_log_up_jul","fpar_log_do_jul")

length(nomCol)

#Create empty data frame

Winter<- data.frame(matrix(ncol = 8, nrow = 17))
n <- c("Year", "WinNDVI", "WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET","WinFPAR")
colnames(Winter) <- n
Winter$Year <- 2000:2016



for (i in 1:16){
  
  yearD<- yr[i]
  yearU<- yr[i+1]

  
  nb <- 0
  
  if (yearD == 2000 |yearD == 2004 |yearD == 2008 |yearD == 2012 |yearD == 2016){
    
    nb <- 366
    
  } else{
    nb <- 365
  }
  
  
  ndvi <- nb -  P[P$year == yearD, "ndvi_log_do_jul"] + P[P$year == yearU, "ndvi_log_up_jul"]
  
  Winter[Winter$Year == yearU, "WinNDVI"] <- ndvi
  
  evi <- nb -  P[P$year == yearD, "evi_log_do_jul"] + P[P$year == yearU, "evi_log_up_jul"]
  
  Winter[Winter$Year == yearU, "WinEVI"] <- evi
  
  Winter[Winter$Year == yearU, "WinLAI"] <- nb -  P[P$year == yearD, "lai_log_do_jul"] + P[P$year == yearU, "lai_log_up_jul"]
  
  Winter[Winter$Year == yearU, "WinGPP"] <- nb -  P[P$year == yearD, "gpp_log_do_jul"] + P[P$year == yearU, "gpp_log_up_jul"]
  
  Winter[Winter$Year == yearU, "WinSnow"] <- nb -  P[P$year == yearD, "snow_log_do_jul"] + P[P$year == yearU, "snow_log_up_jul"]
  
  Winter[Winter$Year == yearU, "WinPSNNET"] <- nb -  P[P$year == yearD, "psnnet_log_do_jul"] + P[P$year == yearU, "psnnet_log_up_jul"]
  
  Winter[Winter$Year == yearU, "WinFPAR"] <- nb -  P[P$year == yearD, "fpar_log_do_jul"] + P[P$year == yearU, "fpar_log_up_jul"]
  
}


pheno_surv <- merge(pheno_surv, Winter, by.x = "yr", by.y ="Year")

#write.csv(pheno_surv,file="Documents/Sherbrooke/OWPC/OWPC/pheno_surv2.csv")

par(mfrow=c(2,1), margin( t =6, r=5, b=4,l=2))

 #####Plot variation of indices WINTER#####
plot(WinNDVI ~ yr, data = pheno_surv, type = "b", col = "green", ylim = c(125,350), xlab = "Year", ylab= "Length of up/down period WINTER")
points(WinEVI ~ yr, data = pheno_surv, type = "b", col = "pink")
points(WinLAI ~ yr, data = pheno_surv, type = "b", col = "red")
points(WinGPP ~ yr, data = pheno_surv, type = "b", col = "orange")
points(WinSnow ~ yr, data = pheno_surv, type = "b", col = "blue")
points(WinPSNNET ~ yr, data = pheno_surv, type = "b", col = "purple")
points(WinFPAR ~ yr, data = pheno_surv, type = "b", col = "black")
legend("topleft", legend = c("NDVI","EVI","LAI","GPP","Snow","PSNNET","FPAR"), 
       col = c("green","pink","red","orange","blue","purple","black"),
       lty = 1,
       horiz =TRUE,
       cex = 0.50)

#####Plot variation of indices SUMMER#####
plot(SummerNDVI ~ yr, data = pheno_surv, type = "b", col = "green", ylim = c(70,250), xlab = "Year", ylab= "Length of up/down period SUMMER")
points(SummerEVI ~ yr, data = pheno_surv, type = "b", col = "pink")
points(SummerLAI ~ yr, data = pheno_surv, type = "b", col = "red")
points(SummerGPP ~ yr, data = pheno_surv, type = "b", col = "orange")
points(SummerSnow ~ yr, data = pheno_surv, type = "b", col = "blue")
points(SummerPSNNET ~ yr, data = pheno_surv, type = "b", col = "purple")
points(SummerFPAR ~ yr, data = pheno_surv, type = "b", col = "black")
legend("topleft", legend = c("NDVI","EVI","LAI","GPP","Snow","PSNNET","FPAR"), 
       col = c("green","pink","red","orange","blue","purple","black"),
       lty = 1,
       horiz =TRUE,
       cex = 0.50)










pheno_surv[pheno_surv$ID == "M7",]




##### Calculate Survival #####

levels(as.factor(pheno_surv$age))
table(pheno_surv$age)

#Change survival from boolean to 0/1 on a dummy dataset
x <- pheno_surv
levels(as.factor(x$alive_t1))
x$surv <- ifelse(x$alive_t1 == FALSE, 0, ifelse(x$alive_t1 == TRUE, 1, NA))
#FAIRE  ÇA SUR NOUVEAU DATAFRAME AVEC TOUTES LES DONNÉES DE SURVIE 
pheno_surv <- x



survFSub <- survF[is.na(survF$alive_t1),]

survFR <- survF[is.na(survF$reproduced) & survF$yr>=2000,]
survFR <- droplevels(survFR)
survFR <- survFR[survF$age >= 1,]


?write.csv()
#write.csv(survFSub, file = "~/Desktop/OWPC/surFSub.csv")

#Calculate survival

S0 <- mean(x[which(x$age==0), "surv"], na.rm =TRUE)
S1 <- mean(x[which(x$age == 1), "surv"], na.rm = TRUE)
S2 <- mean(x[which(x$age == 2), "surv"], na.rm = TRUE)
S3 <- mean(x[which(x$age == 3), "surv"], na.rm = TRUE)
S4 <- mean(x[which(x$age == 4), "surv"], na.rm = TRUE)
S5 <- mean(x[which(x$age == 5),"surv"],na.rm = TRUE)
S6 <- mean(x[which(x$age == 6),"surv"],na.rm = TRUE)
S7 <- mean(x[which(x$age == 7),"surv"],na.rm = TRUE)
S8 <- mean(x[which(x$age == 8),"surv"],na.rm = TRUE)
S9 <- mean(x[which(x$age == 9),"surv"],na.rm = TRUE)
S10 <- mean(x[which(x$age == 10),"surv"],na.rm = TRUE)
S11 <- mean(x[which(x$age == 11),"surv"],na.rm = TRUE)
S12 <- mean(x[which(x$age == 12),"surv"],na.rm = TRUE)
S13 <- mean(x[which(x$age == 13),"surv"],na.rm = TRUE)
S14 <- mean(x[which(x$age == 14),"surv"],na.rm = TRUE)
S15 <- mean(x[which(x$age == 15),"surv"],na.rm = TRUE)
S16 <- mean(x[which(x$age == 16),"surv"],na.rm = TRUE)
S17 <- mean(x[which(x$age == 17),"surv"],na.rm = TRUE)

# If bin the 13 yo + 

S13P <- mean(x[which(x$age >= 13),"surv"],na.rm = TRUE)

# Too long bc we have 17 diff S
#Make a vector of names (S0,S1,etc)
#a <- paste0("S",0:17)
#for(i in 0:17){}


#Calculate Reproduction

R0 <- mean(x[which(x$age==0), "reproduced"], na.rm =TRUE)/2
R1 <- mean(x[which(x$age == 1), "reproduced"], na.rm = TRUE)/2
R2 <- mean(x[which(x$age == 2), "reproduced"], na.rm = TRUE)/2
R3 <- mean(x[which(x$age == 3), "reproduced"], na.rm = TRUE)/2
R4 <- mean(x[which(x$age == 4), "reproduced"], na.rm = TRUE)/2
R5 <- mean(x[which(x$age == 5),"reproduced"],na.rm = TRUE)/2
R6 <- mean(x[which(x$age == 6),"reproduced"],na.rm = TRUE)/2
R7 <- mean(x[which(x$age == 7),"reproduced"],na.rm = TRUE)/2
R8 <- mean(x[which(x$age == 8),"reproduced"],na.rm = TRUE)/2
R9 <- mean(x[which(x$age == 9),"reproduced"],na.rm = TRUE)/2
R10 <- mean(x[which(x$age == 10),"reproduced"],na.rm = TRUE)/2
R11 <- mean(x[which(x$age == 11),"reproduced"],na.rm = TRUE)/2
R12 <- mean(x[which(x$age == 12),"reproduced"],na.rm = TRUE)/2
R13 <- mean(x[which(x$age == 13),"reproduced"],na.rm = TRUE)/2
R14 <- mean(x[which(x$age == 14),"reproduced"],na.rm = TRUE)/2
R15 <- mean(x[which(x$age == 15),"reproduced"],na.rm = TRUE)/2
R16 <- mean(x[which(x$age == 16),"reproduced"],na.rm = TRUE)/2
R17 <- mean(x[which(x$age == 17),"reproduced"],na.rm = TRUE)/2

# If bin the 13 yo + 

R13P <- mean(x[which(x$age >= 13),"reproduced"],na.rm = TRUE)/2

#Calculate FECUNDITY

F0 <- S0*R1
F1 <- S1*R2
F2 <- S2*R3
F3 <- S3*R4
F4 <- S4*R5
F5 <- S5*R6
F6 <- S6*R7
F7 <- S7*R8
F8 <- S8*R9
F9 <- S9*R10
F10 <- S10*R11
F11 <- S11*R12
F12 <- S12*R13
F13 <- S13*R14
F14 <- S14*R15
F15 <- S15*R16
F16 <- S16*R17
F17 <- 0

# If bin 13yo + 

F13P <- S13P * R13P


##### Put in the matrix ####

# Make empty squared matrix
m <- matrix(nrow=18,ncol=18,data=0)

##Add fecundity##

# Create a vector with the fecundity

f <- c(F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17)

for(i in 1:length(f)){
  m[1,i] <- f[i]
}

##Add survival##

#Create a vector with survival W/O S17

s <- c(S0,S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16)

for(i in 1:length(s)){
  m[i+1,i] <- s[i]
}



##Same but for 13yo + 

mP <- matrix(nrow=15,ncol=15,data=0)


fP <- c(F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13P)

for(i in 1:length(fP)){
  mP[1,i] <- fP[i]
}

sP <- c(S0,S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13P)

long <- length(sP)

for(i in 1:length(sP)){
  mP[i+1,i] <- sP[i]
}
# Verify with Joanie and Limoilou, not 100% certain that my logic is good



#####Try some analysis #####

eigen.analysis(m)
eigen.analysis(m)$elasticities

eigen.analysis(m)$lambda1

######Create matrices for each year #####

# Empty list
matrices <- list()
#YrVector <- c("2000", "2001", "2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
YrVector <- c(2000:2016)


matrices <- vector("list", length(YrVector))
names(matrices) <- YrVector

#Create an empty table for year and lambda
#lambda is asymptotic growth rate

AsGrowthRates <- matrix(c(2000:2016,2000:2016), ncol=2)
colnames(AsGrowthRates) <- c("year","lambda")

#Loop to make the matrices

for(i in 1:length(YrVector)){
  
  yr <- YrVector[i]
  
  x <- pheno_surv[pheno_surv$yr == yr,]

  S0 <- mean(x[which(x$age==0), "surv"], na.rm =TRUE)
  S1 <- mean(x[which(x$age == 1), "surv"], na.rm = TRUE)
  S2 <- mean(x[which(x$age == 2), "surv"], na.rm = TRUE)
  S3 <- mean(x[which(x$age == 3), "surv"], na.rm = TRUE)
  S4 <- mean(x[which(x$age == 4), "surv"], na.rm = TRUE)
  S5 <- mean(x[which(x$age == 5),"surv"],na.rm = TRUE)
  S6 <- mean(x[which(x$age == 6),"surv"],na.rm = TRUE)
  S7 <- mean(x[which(x$age == 7),"surv"],na.rm = TRUE)
  S8 <- mean(x[which(x$age == 8),"surv"],na.rm = TRUE)
  S9 <- mean(x[which(x$age == 9),"surv"],na.rm = TRUE)
  S10 <- mean(x[which(x$age == 10),"surv"],na.rm = TRUE)
  S11 <- mean(x[which(x$age == 11),"surv"],na.rm = TRUE)
  S12 <- mean(x[which(x$age == 12),"surv"],na.rm = TRUE)
  S13 <- mean(x[which(x$age == 13),"surv"],na.rm = TRUE)
  S14 <- mean(x[which(x$age == 14),"surv"],na.rm = TRUE)
  S15 <- mean(x[which(x$age == 15),"surv"],na.rm = TRUE)
  S16 <- mean(x[which(x$age == 16),"surv"],na.rm = TRUE)
  S17 <- mean(x[which(x$age == 17),"surv"],na.rm = TRUE)
  
  S13P <- mean(x[which(x$age >= 13),"surv"],na.rm = TRUE)
  
  
  R0 <- mean(x[which(x$age==0), "reproduced"], na.rm =TRUE)/2
  R1 <- mean(x[which(x$age == 1), "reproduced"], na.rm = TRUE)/2
  R2 <- mean(x[which(x$age == 2), "reproduced"], na.rm = TRUE)/2
  R3 <- mean(x[which(x$age == 3), "reproduced"], na.rm = TRUE)/2
  R4 <- mean(x[which(x$age == 4), "reproduced"], na.rm = TRUE)/2
  R5 <- mean(x[which(x$age == 5),"reproduced"],na.rm = TRUE)/2
  R6 <- mean(x[which(x$age == 6),"reproduced"],na.rm = TRUE)/2
  R7 <- mean(x[which(x$age == 7),"reproduced"],na.rm = TRUE)/2
  R8 <- mean(x[which(x$age == 8),"reproduced"],na.rm = TRUE)/2
  R9 <- mean(x[which(x$age == 9),"reproduced"],na.rm = TRUE)/2
  R10 <- mean(x[which(x$age == 10),"reproduced"],na.rm = TRUE)/2
  R11 <- mean(x[which(x$age == 11),"reproduced"],na.rm = TRUE)/2
  R12 <- mean(x[which(x$age == 12),"reproduced"],na.rm = TRUE)/2
  R13 <- mean(x[which(x$age == 13),"reproduced"],na.rm = TRUE)/2
  R14 <- mean(x[which(x$age == 14),"reproduced"],na.rm = TRUE)/2
  R15 <- mean(x[which(x$age == 15),"reproduced"],na.rm = TRUE)/2
  R16 <- mean(x[which(x$age == 16),"reproduced"],na.rm = TRUE)/2
  R17 <- mean(x[which(x$age == 17),"reproduced"],na.rm = TRUE)/2
  
  R13P <- mean(x[which(x$age >= 13),"reproduced"],na.rm = TRUE)/2
  
  
  F0 <- S0*R1
  F1 <- S1*R2
  F2 <- S2*R3
  F3 <- S3*R4
  F4 <- S4*R5
  F5 <- S5*R6
  F6 <- S6*R7
  F7 <- S7*R8
  F8 <- S8*R9
  F9 <- S9*R10
  F10 <- S10*R11
  F11 <- S11*R12
  F12 <- S12*R13
  F13 <- S13*R14
  F14 <- S14*R15
  F15 <- S15*R16
  F16 <- S16*R17
  F17 <- 0
  
  F13P <- S13P * R13P
  
  mP <- matrix(nrow=15,ncol=15,data=0)
  
  
  fP <- c(F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13P)
  
  for(i in 1:length(fP)){
    mP[1,i] <- fP[i]
  }
  
  sP <- c(S0,S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13P)
  
  for(i in 1:length(sP)){
    mP[i+1,i] <- sP[i]
  }
  return(mP)
  
  matrices[[i]] <- mP
  AsGrowthRates[i,2] <- eigen.analysis(m)$lambda1
  
}

# Look at distribution of age in each year


test <- pheno_surv[pheno_surv$yr == 2000,]
table(test$age)

#Not all ages availaible each years!!
#What to do with that?



#####Bin ages ####


# 0 , 1 , 2-7, 8+
# In column class
# 2-7 noted as 2, 8+ noted as 8

d <- pheno_surv


d$class <- ifelse(d$age == 0, 0, 
                  ifelse(d$age == 1, 1,
                         ifelse(d$age >= 2 & d$age <= 7, 2,
                                ifelse(d$age >= 8, 8, NA))))

test <- cbind(d$age, d$class)


#it worked!


#####Test All year on class####

x<- d


S0 <- mean(x[which(x$class==0), "surv"], na.rm =TRUE)
S1 <- mean(x[which(x$class == 1), "surv"], na.rm = TRUE)
S2 <- mean(x[which(x$class == 2), "surv"], na.rm = TRUE)
S8 <- mean(x[which(x$class == 8),"surv"],na.rm = TRUE)

R0 <- mean(x[which(x$class==0), "reproduced"], na.rm =TRUE)/2
R1 <- mean(x[which(x$class == 1), "reproduced"], na.rm = TRUE)/2
R2 <- mean(x[which(x$class == 2), "reproduced"], na.rm = TRUE)/2
R8 <- mean(x[which(x$class == 8),"reproduced"],na.rm = TRUE)/2


F0 <- S0*R1
F1 <- S1*R2
F2 <- S2*R8
F8 <- S8*R8      


mP <- matrix(nrow=5,ncol=5,data=0)


fP <- c(F0,F1,F2,F8)

for(i in 1:length(fP)){
  mP[1,i] <- fP[i]
}

sP <- c(S0,S1,S2,S8)

for(i in 1:length(sP)){
  mP[i+1,i] <- sP[i]
}




eigen.analysis(mP)$lambda1


#Lambda is different for all years with class then for all year with age
#If make a stage structure (by binning), need to consider P : probability to survive and remain in stage 
#AND G: probability survive and grow to the next stage

#If not binning, need to find a way to compensate for missing ages in each year


#####Matrice each year with age class (Not Working) ####

matrices <- list()
YrVector <- c(2000:2016)

matrices <- vector("list", length(YrVector))
names(matrices) <- YrVector

#Create an empty table for year and lambda
#lambda is asymptotic growth rate

AsGrowthRates <- matrix(c(2000:2016,2000:2016), ncol=2)
colnames(AsGrowthRates) <- c("year","lambda")

#Loop to make the matrices

for(i in 1:length(YrVector)){
  
  yr <- YrVector[i]
  
  x <- d[d$yr == yr,]
  
  S0 <- mean(x[which(x$class==0), "surv"], na.rm =TRUE)
  S1 <- mean(x[which(x$class == 1), "surv"], na.rm = TRUE)
  S2 <- mean(x[which(x$class == 2), "surv"], na.rm = TRUE)
  S8 <- mean(x[which(x$class == 8),"surv"],na.rm = TRUE)
  
  R0 <- mean(x[which(x$class==0), "reproduced"], na.rm =TRUE)/2
  R1 <- mean(x[which(x$class == 1), "reproduced"], na.rm = TRUE)/2
  R2 <- mean(x[which(x$class == 2), "reproduced"], na.rm = TRUE)/2
  R8 <- mean(x[which(x$class == 8),"reproduced"],na.rm = TRUE)/2
  
  
  F0 <- S0*R1
  F1 <- S1*R2
  F2 <- S2*R8
  F8 <- S8*R8      
  
  
  mP <- matrix(nrow=5,ncol=5,data=0)
  
  
  fP <- c(F0,F1,F2,F8)
  
  for(i in 1:length(fP)){
    mP[1,i] <- fP[i]
  }
  
  sP <- c(S0,S1,S2,S8)
  
  for(i in 1:length(sP)){
    mP[i+1,i] <- sP[i]
  }

  
  matrices[[i]] <- mP
  AsGrowthRates[i,2] <- eigen.analysis(mP)$lambda1
}

















                        