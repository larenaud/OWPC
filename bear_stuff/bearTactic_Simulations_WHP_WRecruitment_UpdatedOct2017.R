
### ANALYSIS TO COMPARE FITNESS OF SCANDINAVIAN BROWN BEAR FEMALES
# USING 2 DIFFERENT REPRODUCTIVE TACTICS

### LOAD PACKAGES  ---------------
library(car); library(ggplot2); library(boot)
library(AICcmodavg)
library(popbio)
library(plyr)
library(MCMCglmm)
library(ggplot2)
### LOAD DATA  ---------------
setwd("C:/Users/Joanie/Documents/PhD/Donn?es/Demography/Population Models/Simulations_HP_Dens")


# LOAD SURVIVAL TABLE WITH TACTICS
# THIS FILE WAS CREATED IN THE R SCRIPT DemographicAnalyses1993-2015_Tactics_UpdatedDecember2016
data <- read.table("C:/Users/Joanie/Documents/PhD/Donn?es/Demography/Population Models/Tactics/Survival1993-2015_Tactic_UpdatedFeb2017.txt", header=T)

# remove cubs
survival <- subset(data, data$age >0)

# remove NAs
survival <- na.omit(survival)

# Add a column age class
agec1 <- ifelse(survival$age == 1, "C1", survival$age)
agec2 <- ifelse(agec1 == 2, "C2", agec1)
agec3 <- ifelse(agec2 == 3, "C3", agec2)
c48 <- c(4:8)
agec48 <- ifelse(agec3 %in% c48, "C48", agec3)
c9plus <- c(9:max(survival$age))
agec9plus <- ifelse(agec48 %in% c9plus, "C9plus", agec48)
survival$ageclass <- agec9plus


# Make sure variables are in the right format prior to building models
survival$year <- as.factor(survival$year)
survival$id <- as.factor(survival$id)
survival$ageclass <- as.factor(survival$ageclass)
survival$tactic <- as.factor(survival$tactic)
survival$survival <- as.numeric(as.character(survival$survival))




############## RECRUITMENT
repro <- read.table("C:/Users/Joanie/Documents/PhD/Donn?es/Demography/Population Models/Tactics/Recruitment1993-2015_Tactic_UpdatedFeb2017.txt", header=T)
head(repro)
#only adults 
repro <- repro[which(repro$age>=5),]

# Add a column age class
C59 <- c(5:9)
agec59 <- ifelse(repro$age %in% C59, "C59", repro$age)
C10plus <- c(10:max(repro$age))
agec10plus <- ifelse(agec59 %in% C10plus, "C10plus", agec59)
repro$ageclass <- agec10plus

# create dataset with only the necessary columns for the models
repromod <- na.omit(repro)

# Make sure variables are in the right format prior to building models
repromod$year <- as.factor(repromod$year)
repromod$id <- as.factor(repromod$id)
repromod$ageclass <- as.factor(repromod$ageclass)
repromod$tactic <- as.factor(repromod$tactic)
repromod$recruit <- as.numeric(as.character(repromod$recruitment))


# HUNTING PRESSURE AND POPULATION DENSITY (LCOI)
hp <- read.table("C:/Users/Joanie/Documents/PhD/Donn?es/Dead Bears - SBBP/HuntingPressureByYear.txt", header=T)
hp <- hp[which(hp$year>=1993),] # we're only interested in the period 1993-2015
summary(hp)

lcoi <- read.table("C:/Users/Joanie/Documents/PhD/Donn?es/Demography/Density/LCOI_1993-2015_JV.txt", header=T)

hplcoi <- data.frame(hp, lcoi)
hplcoi <- hplcoi[,c(1,4,8)] # keep only LCOI_M, i.e. average between the two counties (G= G?vleborg, D=Dalarna)

# Are the two factors correlated?
cor.test(hplcoi$hp, hplcoi$LCOI_M)
# Both are correlated , cor=0.64, P = 0.001

# MAKE DATASETS FOR MODELS
# SURVIVAL
survival_2 <- join(survival, hplcoi, by="year", type="left")

# REPRODUCTION
repro_2 <- join(repromod, hplcoi, by="year", type="left")

#test <- repro_2[which(repro_2$tactic=="2.5" & repro_2$ageclass=="C10plus"),]
#mean(test$recruitment)

# ============= MODELS
prior1=list(R = list(V = 1, fix = 1))

# YEARLING SURVIVAL
# Tactic 1.5
s11.5 <- MCMCglmm(survival~ hp , family="categorical",prior = prior1,
                  nitt=2600005, thin=2500,burnin=100000, verbose=T,
                  data = na.omit(survival_2[survival_2$age == 1 & survival_2$tactic=="1.5",c("survival","hp")]))

#Tactic 2.5
s12.5 <- 1


# 2 y.o. SURVIVAL
s21.5 <- MCMCglmm(survival~ hp , family="categorical",prior = prior1,
                  nitt=2600005, thin=2500,burnin=100000, verbose=T,
                  data = na.omit(survival_2[survival_2$age == 2 & survival_2$tactic=="1.5",c("survival","hp")]))

#Tactic 2.5
s22.5 <- MCMCglmm(survival~ hp , family="categorical",prior = prior1,
                  nitt=2600005, thin=2500,burnin=100000, verbose=T,
                  data = na.omit(survival_2[survival_2$age == 2 & survival_2$tactic=="2.5",c("survival","hp")]))


# 3 y.o. SURVIVAL
# Tactic 1.5
s31.5 <- MCMCglmm(survival~ hp , family="categorical",prior = prior1,
                  nitt=2600005, thin=2500,burnin=100000, verbose=T,
                  data = na.omit(survival_2[survival_2$age == 3 & survival_2$tactic=="1.5",c("survival","hp")]))

#Tactic 2.5
s32.5 <- MCMCglmm(survival~ hp , family="categorical",prior = prior1,
                  nitt=2600005, thin=2500,burnin=100000, verbose=T,
                  data = na.omit(survival_2[survival_2$age == 3 & survival_2$tactic=="2.5",c("survival","hp")]))



# 4-8 y.o. SURVIVAL

# Tactic 1.5
s481.5 <- MCMCglmm(survival~ hp , family="categorical",prior = prior1,
                   nitt=2600005, thin=2500,burnin=100000, verbose=T,
                   data = na.omit(survival_2[survival_2$ageclass  == "C48" & survival_2$tactic=="1.5",c("survival","hp")]))

#Tactic 2.5
s482.5 <- MCMCglmm(survival~ hp , family="categorical",prior = prior1,
                   nitt=2600005, thin=2500,burnin=100000, verbose=T,
                   data = na.omit(survival_2[survival_2$ageclass  == "C48" & survival_2$tactic=="2.5",c("survival","hp")]))


# 9plus y.o. SURVIVAL

# Tactic 1.5
s9plus1.5 <- MCMCglmm(survival~ hp , family="categorical",prior = prior1,
                      nitt=2600005, thin=2500,burnin=100000, verbose=T,
                      data = na.omit(survival_2[survival_2$ageclass  == "C9plus" & survival_2$tactic=="1.5",c("survival","hp")]))

#Tactic 2.5
s9plus2.5 <- MCMCglmm(survival~ hp , family="categorical",prior = prior1,
                      nitt=2600005, thin=2500,burnin=100000, verbose=T,
                      data = na.omit(survival_2[survival_2$ageclass  == "C9plus" & survival_2$tactic=="2.5",c("survival","hp")]))



# Create dataframe of effect sizes
Effect_Surv <- data.frame(tactic=c(rep("1.5",5), rep("2.5", 5)), 
                          ageclass = rep(c("yearling", "2 y.o.", "3 y.o.", 
                                           "4-8 y.o.", "9+ y.o.")), 
                          mean = c(mean(s11.5$Sol[,"hp"]), mean(s21.5$Sol[,"hp"]), 
                                   mean(s31.5$Sol[,"hp"]),mean(s481.5$Sol[,"hp"]),
                                   mean(s9plus1.5$Sol[,"hp"]),1,
                                   mean(s22.5$Sol[,"hp"]),mean(s32.5$Sol[,"hp"]),
                                   mean(s482.5$Sol[,"hp"]),mean(s9plus2.5$Sol[,"hp"])),
                          lower95 = c(quantile(s11.5$Sol[,"hp"], prob=0.025),
                                      quantile(s21.5$Sol[,"hp"], prob=0.025),
                                      quantile(s31.5$Sol[,"hp"], prob=0.025),
                                      quantile(s481.5$Sol[,"hp"], prob=0.025),
                                      quantile(s9plus1.5$Sol[,"hp"], prob=0.025),
                                      1,
                                      quantile(s22.5$Sol[,"hp"], prob=0.025),
                                      quantile(s32.5$Sol[,"hp"], prob=0.025),
                                      quantile(s482.5$Sol[,"hp"], prob=0.025),
                                      quantile(s9plus2.5$Sol[,"hp"], prob=0.025)),
                          upper95 = c(quantile(s11.5$Sol[,"hp"], prob=0.975),
                                      quantile(s21.5$Sol[,"hp"], prob=0.975),
                                      quantile(s31.5$Sol[,"hp"], prob=0.975),
                                      quantile(s481.5$Sol[,"hp"], prob=0.975),
                                      quantile(s9plus1.5$Sol[,"hp"], prob=0.975),
                                      1,
                                      quantile(s22.5$Sol[,"hp"], prob=0.975),
                                      quantile(s32.5$Sol[,"hp"], prob=0.975),
                                      quantile(s482.5$Sol[,"hp"], prob=0.975),
                                      quantile(s9plus2.5$Sol[,"hp"], prob=0.975))) 


# REPRODUCTION 


# 5-9 yo

# Tactic 1.5
R591.5 <- MCMCglmm(recruit~ 1 , #family="poisson",
                   nitt=2600005, thin=2500,burnin=100000, verbose=T,
                   data = na.omit(repro_2[repro_2$ageclass == "C59" & repro_2$tactic=="1.5",c("recruit","id")]))

R592.5 <- MCMCglmm(recruit~ 1 , #family="poisson",
                   nitt=2600005, thin=2500,burnin=100000, verbose=T,
                   data = na.omit(repro_2[repro_2$ageclass == "C59" & repro_2$tactic=="2.5",c("recruit","id")]))


# 10plus yo
# Tactic 1.5
R10plus1.5 <- MCMCglmm(recruit~ 1 , #family="poisson",
                       nitt=2600005, thin=2500,burnin=100000, verbose=T,
                       data = na.omit(repro_2[repro_2$ageclass == "C10plus" & repro_2$tactic=="1.5",c("recruit","id")]))

R10plus2.5 <- MCMCglmm(recruit~ 1 , #family="poisson",
                       nitt=2600005, thin=2500,burnin=100000, verbose=T,
                       data = na.omit(repro_2[repro_2$ageclass == "C10plus" & repro_2$tactic=="2.5",c("recruit","id")]))


save(list=c("R10plus1.5", "R10plus2.5",
            "R591.5", "R592.5", 
            "s11.5", "s12.5",
            "s21.5","s22.5", 
            "s31.5", "s32.5",
            "s481.5","s482.5",
            "s9plus1.5","s9plus2.5"),
     file = "BearModRecruit5.Rdata")


# use mcmc model to predict lambda ----------------------------------------
load("C:/Users/Joanie/Documents/PhD/Donn?es/Demography/Population Models/Simulations_HP_Dens/BearModRecruit5.Rdata")

#################################################################
# Using predictions (even when non-significant effect of hunting)
##################################################################
l=list(s11.5$Sol, s21.5$Sol, s31.5$Sol,s481.5$Sol,s9plus1.5$Sol)
lapply(l, function(x) plot(x[,"hp"]))


Get.Lambda1.5 <- function(P,it){  # function to build matrix for hunt pressure = P
  L<-matrix(nrow=9,ncol=9,data=0)
  L[1,4] = inv.logit(s481.5$Sol[1,] %*% t(t(c(1,P))))*R591.5$Sol[it,]
  L[1,5] = inv.logit(s481.5$Sol[it,] %*% t(t(c(1,P))))*R591.5$Sol[it,]
  L[1,6] = inv.logit(s481.5$Sol[it,] %*% t(t(c(1,P))))*R591.5$Sol[it,]
  L[1,7] = inv.logit(s481.5$Sol[it,] %*% t(t(c(1,P))))*R591.5$Sol[it,]
  L[1,8] = inv.logit(s481.5$Sol[it,] %*% t(t(c(1,P))))*R591.5$Sol[it,]
  L[1,9] = inv.logit(s9plus1.5$Sol[it,] %*% t(t(c(1,P)))) *R10plus1.5$Sol[it,]
  
  L[2,1] = inv.logit(s11.5$Sol[1,] %*% t(t(c(1,0.3))))
  L[3,2] = inv.logit(s21.5$Sol[it,] %*% t(t(c(1,P)))) 
  L[4,3] = inv.logit(s31.5$Sol[it,] %*% t(t(c(1,P))))
  L[5,4] = inv.logit(s481.5$Sol[it,] %*% t(t(c(1,P))))
  L[6,5] = inv.logit(s481.5$Sol[it,] %*% t(t(c(1,P))))
  L[7,6] = inv.logit(s481.5$Sol[it,] %*% t(t(c(1,P))))
  L[8,7] = inv.logit(s481.5$Sol[it,] %*% t(t(c(1,P))))
  L[9,8] = inv.logit(s481.5$Sol[it,] %*% t(t(c(1,P))))
  L[9,9] = inv.logit(s9plus1.5$Sol[it,] %*% t(t(c(1,P))))
  return(c(eigen.analysis(L)$lambda1,eigen.analysis(L)$stable.stage))
}





# Tactic 2.5
Get.Lambda2.5 <-  function(P,it){  # function to build matrix for hunt pressure = P
  L<-matrix(nrow=9,ncol=9,data=0)
  L[1,4] = inv.logit(s482.5$Sol[it,] %*% t(t(c(1,P))))*R592.5$Sol[it,]
  L[1,5] = inv.logit(s482.5$Sol[it,] %*% t(t(c(1,P))))*R592.5$Sol[it,]
  L[1,6] = inv.logit(s482.5$Sol[it,] %*% t(t(c(1,P))))*R592.5$Sol[it,]
  L[1,7] = inv.logit(s482.5$Sol[it,] %*% t(t(c(1,P))))*R592.5$Sol[it,]
  L[1,8] = inv.logit(s482.5$Sol[it,] %*% t(t(c(1,P))))*R592.5$Sol[it,]
  L[1,9] = inv.logit(s9plus2.5$Sol[it,] %*% t(t(c(1,P)))) *R10plus2.5$Sol[it,]
  
  L[2,1] = 1
  L[3,2] = inv.logit(s22.5$Sol[it,] %*% t(t(c(1,P)))) 
  L[4,3] = inv.logit(s32.5$Sol[it,] %*% t(t(c(1,P))))
  L[5,4] = inv.logit(s482.5$Sol[it,] %*% t(t(c(1,P))))
  L[6,5] = inv.logit(s482.5$Sol[it,] %*% t(t(c(1,P))))
  L[7,6] = inv.logit(s482.5$Sol[it,] %*% t(t(c(1,P))))
  L[8,7] = inv.logit(s482.5$Sol[it,] %*% t(t(c(1,P))))
  L[9,8] = inv.logit(s482.5$Sol[it,] %*% t(t(c(1,P))))
  L[9,9] = inv.logit(s9plus2.5$Sol[it,] %*% t(t(c(1,P))))
  return(c(eigen.analysis(L)$lambda1,eigen.analysis(L)$stable.stage))
}

Xmin=0  ;  Xmax=0.3    # set hunting pressure prediction range 
L_short <- llply(seq(Xmin,Xmax,length.out = 100), function(x) {
  lbz <- nrow(s11.5$Sol)
  for(it in 1: nrow(s11.5$Sol)){
    lbz[it] <- Get.Lambda1.5(P = x,it)[1]
  }
  return(lbz)
})

L_long <- llply(seq(Xmin,Xmax,length.out = 2), function(x) {
  lbz <- nrow(s11.5$Sol)
  for(it in 1: nrow(s11.5$Sol)){
    lbz[it] <- Get.Lambda2.5(P = x,it)[1]
  }
  return(lbz)
})





lambdaz=data.frame(P=seq(Xmin,Xmax,length.out = 100),
                   lambda_short=laply(L_short,mean),
                   laply(L_short,function(x)  HPDinterval(mcmc(x))),
                   lambda_long=laply(L_long,mean),
                   laply(L_long,function(x)  HPDinterval(mcmc(x)))
)




save(lambdaz, file="lambdaz_Yearling_fem5.Rdata")
load("lambdaz_Yearling_fem5.Rdata")


means2 <- c(-0.367, -8.175, -11.065, -17.367, -6.307)
upseg2 <- c(4.610, 0.468, 2.243, -0.299, 0.139)
lowseg2 <- c(-5.895, -17.287,-26.454, -41.357, -11.735)

means <- c(0, -9.931, -11.622, 3.766, -2.892)
upseg <- c(0,11.576,11.636,33.762,7.834)
lowseg <- c(0,-28.900,-46.434, -24.757, -13.434)
par(mfrow=c(1,1), bty="n", mar=c(4,5,2,2))
plot(x=c(1:5), y=c(1:5), col="white", xlim=c(-50,50), xlab="beta", ylab="", axes=F)
par(new=T)
plot(means, y=c(1:5), pch=16, xlim=c(-50,50), xlab="", ylab="", axes=F)
segments(lowseg, c(1:5), upseg, c(1:5))
axis(2, at=seq(1,5,1), labels=c("9+ y.o.", "4-8 y.o.", "3 y.o.", "2 y.o.", "yearling"), las=2)
axis(1, at=seq(-50,50,10))
mtext(side=2, text="Age-class", line=4)
par(new=T)
plot(means2, y=c(1.2,2.2,3.2,4.2,5.2), pch=1, xlim=c(-50,50),ylim=c(1,5.1), xlab="", ylab="",axes=F)
segments(lowseg2, c(1.2,2.2,3.2,4.2,5.2), upseg2, c(1.2,2.2,3.2,4.2,5.2), lty=2)
abline(v=0)
legend("topright", pch=c(1,16), lty=c(2,1), bty="n", legend=c("1.5-year tactic", "2.5-year tactic"))





ggplot(data=lambdaz,aes(x=P))+
  geom_smooth(aes(y=lambda_short,color="short"),se=F)+  # plot the lambda
  geom_smooth(aes(y=lower,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=upper,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=lambda_long,color="long"),se=F)+
  geom_smooth(aes(y=lower.1,color="long"),linetype="dashed",se=F)+
  geom_smooth(aes(y=upper.1,color="long"),linetype="dashed",se=F)+
  labs(x="Hunting pressure",y=expression(lambda))










p<- ggplot(data=lambdaz,aes(x=P))+
  geom_smooth(aes(y=lambda_short,color="short"),se=F)+  # plot the lambda
  geom_smooth(aes(y=lower,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=upper,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=lambda_long,color="long"),se=F)+
  geom_smooth(aes(y=lower.1,color="long"),linetype="dashed",se=F)+
  geom_smooth(aes(y=upper.1,color="long"),linetype="dashed",se=F)+
  #  geom_ribbon(aes(ymin=lower.1, ymax=upper.1), alpha=0.5)
  labs(x="Hunting pressure",y=expression(lambda))
p+
  theme_bw() + 
  theme(axis.title=element_text(face="bold", size=14))

gg1 <- ggplot_build(p)
df2 <- data.frame(x=gg1$data[[2]]$x, 
                  ymin = gg1$data[[2]]$y,
                  ymax = gg1$data[[3]]$y,
                  ymin2 = gg1$data[[5]]$y,
                  ymax2 = gg1$data[[6]]$y)


p +
  geom_ribbon(data=df2, aes(x=x, ymin=ymin, ymax=ymax), fill="grey", alpha=0.8)+
  geom_ribbon(data=df2, aes(x=x, ymin=ymin2, ymax=ymax2, fill="grey", alpha=0.8))


q <-   ggplot(data=lambdaz,aes(x=P))+
  geom_ribbon(data=df2, aes(x=x, ymin=ymin2, ymax=ymax2), fill="grey80", alpha=0.8)+
  geom_ribbon(data=df2, aes(x=x, ymin=ymin, ymax=ymax), fill="grey30", alpha=0.8)+
  geom_smooth(aes(y=lambda_long),colour="gray60",se=F)+
  geom_smooth(aes(y=lambda_short),colour="grey10",se=F)+
  labs(x="Hunting pressure",y=expression(lambda))

q +
  theme_bw() + 
  theme(axis.title=element_text(face="bold", size=14))


#with mode
getmode <- function(v) {
  uniqv <- unique(round(v, digits=3))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


lambdazmode=data.frame(P=seq(Xmin,Xmax,length.out = 100),
                       lambda_short=laply(L_short,getmode),
                       laply(L_short,function(x)  HPDinterval(mcmc(x))),
                       lambda_long=laply(L_long,getmode),
                       laply(L_long,function(x)  HPDinterval(mcmc(x)))
)

ggplot(data=lambdazmode,aes(x=P))+
  geom_smooth(aes(y=lambda_short,color="short"),se=F)+  # plot the lambda
  geom_smooth(aes(y=lower,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=upper,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=lambda_long,color="long"),se=F)+
  geom_smooth(aes(y=lower.1,color="long"),linetype="dashed",se=F)+
  geom_smooth(aes(y=upper.1,color="long"),linetype="dashed",se=F)+
  labs(x="Hunting pressure",y=expression(lambda))





xlim <- c(0.90,1.30)
ylim <- c(0,20)
Col1.5 <- rgb(0,0,0,0.3)
Col2.5 <- rgb(0,0,0,0.6)

# hp =0
plot(density(unlist(L_short[1:100])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[1:100])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[1:100])), density = -1, col = Col2.5)


plot(density(unlist(L_short[1:100])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[1:100])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[1:100])), density = -1, col = Col2.5)



plot(density(unlist(L_short[1])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[1])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[1])), density = -1, col = Col2.5)


par(mfrow=c(4,1), mar=c(4,4,2,4))




##### test with different hp

#hp 0
plot(density(unlist(L_short[1:25])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[1:25])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[1:25])), density = -1, col = Col2.5)

#hp 10
plot(density(unlist(L_short[26:50])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[26:50])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[26:50])), density = -1, col = Col2.5)

#hp 20
plot(density(unlist(L_short[51:75])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[51:75])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[51:75])), density = -1, col = Col2.5)

#hp 30
plot(density(unlist(L_short[76:100])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[76:100])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[76:100])), density = -1, col = Col2.5)


#hp 40
plot(density(unlist(L_short[41:50])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[41:50])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[41:50])), density = -1, col = Col2.5)

#hp 50
plot(density(unlist(L_short[51:60])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[51:60])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[51:60])), density = -1, col = Col2.5)

#hp 60
plot(density(unlist(L_short[61:70])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[61:70])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[61:70])), density = -1, col = Col2.5)

#hp 70
plot(density(unlist(L_short[71:80])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[71:80])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[71:80])), density = -1, col = Col2.5)

#hp 80
plot(density(unlist(L_short[81:90])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[81:90])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[81:90])), density = -1, col = Col2.5)
#hp 90
plot(density(unlist(L_short[91:100])), xlim = xlim, ylim = ylim, xlab = (expression(lambda)), axes=F,
     main = '', cex.lab=1.2)
axis(1, at=seq(0.90,1.30, 0.05))
axis(2, at=seq(0, 20, 5))

polygon(density(unlist(L_short[91:100])), density = -1, col = Col1.5)
polygon(density(unlist(L_long[91:100])), density = -1, col = Col2.5)









#with median
lambdazmed=data.frame(P=seq(Xmin,Xmax,length.out = 100),
                      lambda_short=laply(L_short,median),
                      laply(L_short,function(x)  HPDinterval(mcmc(x))),
                      lambda_long=laply(L_long,median),
                      laply(L_long,function(x)  HPDinterval(mcmc(x)))
)

ggplot(data=lambdazmed,aes(x=P))+
  geom_smooth(aes(y=lambda_short,color="short"),se=F)+  # plot the lambda
  geom_smooth(aes(y=lower,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=upper,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=lambda_long,color="long"),se=F)+
  geom_smooth(aes(y=lower.1,color="long"),linetype="dashed",se=F)+
  geom_smooth(aes(y=upper.1,color="long"),linetype="dashed",se=F)+
  labs(x="Hunting pressure",y=expression(lambda))




################################################
#
#getmodes
lambdazmode2=data.frame(P=seq(Xmin,Xmax,length.out = 100),
                        lambda_short=laply(L_short,posterior.mode),
                        laply(L_short,function(x)  HPDinterval(mcmc(x))),
                        lambda_long=laply(L_long,posterior.mode),
                        laply(L_long,function(x)  HPDinterval(mcmc(x)))
)


ggplot(data=lambdazmode2,aes(x=P))+
  geom_smooth(aes(y=lambda_short,color="short"),se=F)+  # plot the lambda
  geom_smooth(aes(y=lower,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=upper,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=lambda_long,color="long"),se=F)+
  geom_smooth(aes(y=lower.1,color="long"),linetype="dashed",se=F)+
  geom_smooth(aes(y=upper.1,color="long"),linetype="dashed",se=F)+
  labs(x="Hunting pressure",y=expression(lambda))





library(vioplot)


















###  impact on stable age-structure

Xmin=0  ;  Xmax=max(survival_2$hp)     # set hunting pressure prediction range 
ageS_short <- llply(seq(Xmin,Xmax,length.out = 100), function(x) {
  lbz <- matrix(data = NA,nrow = nrow(s01.5a$Sol),ncol = 6)
  for(it in 1: nrow(s01.5a$Sol)){
    tmp=Get.Lambda1.5(P = x,it)[-1]
    tmp=c(tmp[1],tmp[2],tmp[3],tmp[4],tmp[5]+tmp[6]+tmp[7]+tmp[8]+tmp[9], tmp[10])
    lbz[it,] <-tmp
  }
  lbz=colMeans(lbz)
  return(lbz)
})

hp.vect=seq(Xmin,Xmax,length.out = 100)
res_short=data.frame()
for( i in 1:length(ageS_short)){
  tmp=as.data.frame(t(ageS_short[[i]]))
  colnames(tmp)=c("0","1","2","3","4-8","9+")
  res_short=rbind(res_short,  cbind(hp=hp.vect[i],tmp))
}
res_short=reshape2::melt(res_short,id.vars="hp")

ggplot(res_short,aes(x=hp,y=value,fill=variable))+
  geom_bar(stat = "identity",position="stack")+
  scale_fill_brewer(type = "qual",palette = 6)+
  labs(x="Hunting pressure",y="Proportion of pop",fill="Age\nclass",title="1.5 reproduction tactic")


ageS_long <- llply(seq(Xmin,Xmax,length.out = 100), function(x) {
  lbz <- matrix(data = NA,nrow = nrow(s01.5a$Sol),ncol = 6)
  for(it in 1: nrow(s01.5a$Sol)){
    tmp=Get.Lambda2.5(P = x,it)[-1]
    tmp=c(tmp[1],tmp[2],tmp[3],tmp[4],tmp[5]+tmp[6]+tmp[7]+tmp[8]+tmp[9], tmp[10])
    lbz[it,] <-tmp
  }
  lbz=colMeans(lbz)
  return(lbz)
},.parallel = T)


hp.vect=seq(Xmin,Xmax,length.out = 100)
res_long=data.frame()
for( i in 1:length(ageS_long)){
  tmp=as.data.frame(t(ageS_long[[i]]))
  colnames(tmp)=c("0","1","2","3","4-8","9+")
  res_long=rbind(res_long,  cbind(hp=hp.vect[i],tmp))
}
res_long=reshape2::melt(res_long,id.vars="hp")

ggplot(res_long,aes(x=hp,y=value,fill=variable))+
  geom_bar(stat = "identity",position="stack")+
  scale_fill_brewer(type = "qual",palette = 6)+
  labs(x="Hunting pressure",y="Proportion of pop",fill="Age\nclass",title="2.5 reproduction tactic")

























#NOW ALLOWING ONLY THE SIGNIFICANT EFFECTS (models including HP were better for S21.5, S481.5 and S9plus1.5)
#Tactic 1.5
Get.Lambda1.5b <- function(P,it){  # function to build matrix for hunt pressure = P
  L=matrix(0,nrow=10,ncol=10)
  L[2,1] <- inv.logit(s01.5b$Sol[it,])
  L[3,2] <- inv.logit(s11.5b$Sol[it,])
  L[4,3] <- inv.logit(s21.5a$Sol[it,] %*% t(t(c(1,P)))) 
  L[5,4] <- inv.logit(s31.5b$Sol[it,])
  L[6,5] <- inv.logit(s481.5a$Sol[it,] %*% t(t(c(1,P))))
  L[7,6] <- L[6,5]
  L[8,7] <- L[6,5]
  L[9,8] <- L[6,5]
  L[10,9] <- L[6,5]
  L[10,10] <- inv.logit(s9plus1.5a$Sol[it,] %*% t(t(c(1,P))))
  L[1,4] <- inv.logit(s31.5b$Sol[it,]) * R41.5b$Sol[it,]   /2
  L[1,5:9] <- inv.logit(s481.5a$Sol[it,] %*% t(t(c(1,P))))*R591.5b$Sol[it,]/2
  L[1,10] <- inv.logit(s9plus1.5a$Sol[it,] %*% t(t(c(1,P)))) *R10plus1.5b$Sol[it,]/2
  return(eigen.analysis(L)$lambda1)
}
# Tactic 2.5
Get.Lambda2.5b <-  function(P,it){  # function to build matrix for hunt pressure = P
  L=matrix(0,nrow=10,ncol=10)
  L[2,1] <- inv.logit(s02.5b$Sol[it,])
  L[3,2] <- 1#inv.logit(s12.5a$Sol[it,] %*% t(t(c(1,P))))
  L[4,3] <- inv.logit(s22.5b$Sol[it,]) 
  L[5,4] <- inv.logit(s32.5b$Sol[it,])
  L[6,5] <- inv.logit(s482.5b$Sol[it,])
  L[7,6] <- L[6,5]
  L[8,7] <- L[6,5]
  L[9,8] <- L[6,5]
  L[10,9] <- L[6,5]
  L[10,10] <- inv.logit(s9plus2.5b$Sol[it,])
  L[1,4] <- inv.logit(s32.5b$Sol[it,]) * R42.5b$Sol[it,]   /2
  L[1,5:9] <- inv.logit(s482.5b$Sol[it,])*R592.5b$Sol[it,]/2
  L[1,10] <- inv.logit(s9plus2.5b$Sol[it,]) *R10plus2.5b$Sol[it,]/2
  return(eigen.analysis(L)$lambda1)
}

L_short <- llply(seq(Xmin,Xmax,length.out = 100), function(x) {
  lbz <- nrow(s01.5a$Sol)
  for(it in 1: nrow(s01.5a$Sol)){
    lbz[it] <- Get.Lambda1.5b(P = x,it)
  }
  return(lbz)
},.parallel = T)  # build matrix for tactic=short


L_long <- llply(seq(Xmin,Xmax,length.out = 100), function(x) {
  lbz <- nrow(s01.5a$Sol)
  for(it in 1: nrow(s01.5a$Sol)){
    lbz[it] <- Get.Lambda2.5b(P = x,it)
  }
  return(lbz)
},.parallel = T)  # build matrix for tactic=short

lambdaz=data.frame(P=seq(Xmin,Xmax,length.out = 100),
                   lambda_short=laply(L_short,mean),
                   laply(L_short,function(x)  HPDinterval(mcmc(x))),
                   lambda_long=laply(L_long,mean),
                   laply(L_long,function(x)  HPDinterval(mcmc(x)))
)

ggplot(data=lambdaz,aes(x=P))+
  geom_smooth(aes(y=lambda_short,color="short"),se=F)+  # plot the lambda
  geom_smooth(aes(y=lower,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=upper,color="short"),linetype="dashed",se=F)+  # plot the lambda
  geom_smooth(aes(y=lambda_long,color="long"),se=F)+
  geom_smooth(aes(y=lower.1,color="long"),linetype="dashed",se=F)+
  geom_smooth(aes(y=upper.1,color="long"),linetype="dashed",se=F)+
  labs(x="Hunting pressure",y=expression(lambda))


#











L_long <- llply(seq(Xmin,Xmax,length.out = 100), function(x) {
  lbz <- nrow(s11.5$Sol)
  for(it in 1: nrow(s11.5$Sol)){
    lbz[it] <- Get.Lambda2.5(P = x,it)[1]
  }
  return(lbz)
})





lambdaz=data.frame(P=seq(Xmin,Xmax,length.out = 100),
                   lambda_short=laply(L_short,mean),
                   laply(L_short,function(x)  HPDinterval(mcmc(x))),
                   lambda_long=laply(L_long,mean),
                   laply(L_long,function(x)  HPDinterval(mcmc(x)))
)


