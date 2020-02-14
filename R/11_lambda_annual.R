##Calcul lambda annuel


pop <- read.table("/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/sheep_data.txt", header = T)
pop$surv <- ifelse(pop$alive_t1 == FALSE, 0, ifelse(pop$alive_t1 == TRUE, 1, pop$alive_t1))
pop$ageClass <- ifelse(pop$age >= 8, 8, pop$age)
c37 <- c(3:7)
pop$ageClass <- ifelse(pop$age %in% c37 , 37, pop$ageClass)
pop <- droplevels(pop[pop$first_yr_trans == 0, ])

temp <- pop
temp$substr <- substr(temp$ID,1,2)
noGhost <- temp[temp$substr != "20",]



#Empty dataframe 

lambdaAnnual <- matrix(nrow=16, ncol=3)
colnames(lambdaAnnual) <- c("year","LARaw","LATrue")
lambdaAnnual <-as.data.frame(lambdaAnnual)

YrVec <- c(2000:2015)


for(i in 1:length(YrVec)){
  
  t <- YrVec[i]
  lambdaAnnual[i,"year"] <- t
  t1 <- YrVec[i] + 1
  
  
#RAW
  x <- length(pop[which(pop$yr == t), "ID"])
  y <- length(pop[which(pop$yr == t1), "ID"])
  
  lR <- x/y
  lambdaAnnual[i,"LARaw"] <- lR
  
  
  #True
  x <- length(noGhost[which(noGhost$yr == t), "ID"])
  y <- length(noGhost[which(noGhost$yr == t1), "ID"])
  
  lT <- x/y
  lambdaAnnual[i,"LATrue"] <- lT
  
}


#write.csv(lambdaAnnual,file="/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/lambdaAnnuel.csv")




