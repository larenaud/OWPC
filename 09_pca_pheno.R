# script for making pca for survival and fecundity data 

library(lme4)
library(plyr)
library(dplyr)
library(AICcmodavg)
library(xtable)
library(googledrive)
library(vegan)
library(knitr)

# pca survival ------------------------------------------------------------
df_surv <- read.csv("~/Documents/PhD/Analyses/OWPC/OWPC/data/surv_pheno_data.csv", sep=",")
str(df_surv)

# scale
colnames(df_surv)
lengths <- df_surv[c("SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR","WinNDVI_surv" ,
          "WinEVI_surv","WinLAI_surv","WinGPP_surv","WinSnow_surv","WinPSNNET_surv", "WinFPAR_surv")] 
hist(unlist(lengths))# 

# Standardisation
lengths <-scale(lengths)
lengths <- na.omit(lengths) # ça enlève toutes les lignes avec des NA

# faire la PCA
acp <- rda(lengths) # Option par défaut : scale=FALSE

#these are the loadings (correlations of each variable with each pca xis)
print(kable(round(scores(acp, choices = 1:4, display = "species", scaling = 0), 3)))

### Extraire les résultats
summary(acp)
summary(acp, scaling = 1)
quartz()
biplot(acp, scaling="sites") # relationships between years # the eigenvalues are expressed for sites, and species are left unscaled.
biplot(acp, scaling=1)

biplot(acp, scaling=2) # relationships between variables 
biplot(acp, scaling="species")

eigenvals(acp)
year <- summary(acp, scaling = 1)$sites
var <- summary(acp, scaling = 1)$species
summary(acp)$sp

getwd()

pdf("graph/biplot_pheno_scaling1.pdf")
plot(acp, scaling=1, main="Triplot RDA - scaling 1", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1,1))
points(scores(acp, display="sites", choices=c(1,2), scaling=1), pch=19, col=blues9, cex=1.2)
arrows(0,0,
       scores(acp, display="species", choices=c(1), scaling=1)*0.1,
       scores(acp, display="species", choices=c(2), scaling=1)*0.1,
       col="grey",length=0.2,angle=15)
dim(var)
var <- summary(acp, scaling = 1)$species

rownames(var)<- c("SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR",    
                  "WinNDVI_surv","WinEVI_surv","WinLAI_surv","WinGPP_surv","WinSnow_surv","WinPSNNET_surv" ,"WinFPAR_surv")

text(scores(acp, display="species", choices=c(1), scaling=1)*0.34,
     scores(acp, display="species", choices=c(2), scaling=1)*0.32,
     labels=rownames(var),
     col="blue", cex=1)
dev.off()

# merge 2 first pca to pheno_dataframe 
year <- summary(acp, scaling = 1)$sites

df_surv<- df_surv[!is.na(df_surv$SummerNDVI), ]
df_surv<- df_surv[!is.na(df_surv$SummerGPP), ]
df_surv<- df_surv[!is.na(df_surv$SummerGPP), ]
df_surv<- df_surv[!is.na(df_surv$WinNDVI), ] # now has equal number of lines

# need exact number of lines in two df 
df_surv<-cbind(df_surv, year[, 1:2])
getwd()
write.csv(df_surv, "data/df_surv_pca.csv")

# pca fecundity ------------------------------------------------------------
df_fec <- read.csv("~/Documents/PhD/Analyses/OWPC/OWPC/data/fecun_pheno_data.csv", sep=",")
str(df_fec)

# scale
colnames(df_fec)
lengths <- df_fec[c("WinNDVI_fec","WinEVI_fec","WinLAI_fec","WinGPP_fec","WinSnow_fec","WinPSNNET_fec","WinFPAR_fec",
                    "SummerNDVI_fec","SummerEVI_fec", "SummerLAI_fec","SummerGPP_fec","SummerSnow_fec","SummerPSNNET_fec", "SummerFPAR_fec")] 
hist(unlist(lengths))#,

# Standardisation
lengths <-scale(lengths)
lengths <- na.omit(lengths) # ça enlève toutes les lignes avec des NA

# faire la PCA
acp <- rda(lengths) # Option par défaut : scale=FALSE

#these are the loadings (correlations of each variable with each pca xis)
print(kable(round(scores(acp, choices = 1:4, display = "species", scaling = 0), 3)))

### Extraire les résultats
summary(acp)
summary(acp, scaling = 1)
quartz()
biplot(acp, scaling="sites") # relationships between years # the eigenvalues are expressed for sites, and species are left unscaled.
biplot(acp, scaling=1)

biplot(acp, scaling=2) # relationships between variables 
biplot(acp, scaling="species")

eigenvals(acp)
year <- summary(acp, scaling = 1)$sites
var <- summary(acp, scaling = 1)$species
summary(acp)$sp

getwd()

pdf("graph/biplot_pheno_fec_scaling1.pdf")
plot(acp, scaling=1, main="Triplot RDA - scaling 1", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1,1))
points(scores(acp, display="sites", choices=c(1,2), scaling=1), pch=19, col=blues9, cex=1.2)
arrows(0,0,
       scores(acp, display="species", choices=c(1), scaling=1)*0.1,
       scores(acp, display="species", choices=c(2), scaling=1)*0.1,
       col="grey",length=0.2,angle=15)
dim(var)
var <- summary(acp, scaling = 1)$species

rownames(var)<- c("WinNDVI_fec","WinEVI_fec","WinLAI_fec","WinGPP_fec","WinSnow_fec","WinPSNNET_fec","WinFPAR_fec",
                  "SummerNDVI_fec","SummerEVI_fec", "SummerLAI_fec","SummerGPP_fec","SummerSnow_fec","SummerPSNNET_fec", "SummerFPAR_fec")

text(scores(acp, display="species", choices=c(1), scaling=1)*0.34,
     scores(acp, display="species", choices=c(2), scaling=1)*0.32,
     labels=rownames(var),
     col="blue", cex=1)
dev.off()

# merge 2 first pca to pheno_dataframe 
year <- summary(acp, scaling = 1)$sites

df_fec<- df_fec[!is.na(df_fec$SummerNDVI), ]
df_fec<- df_fec[!is.na(df_fec$SummerGPP), ]
df_fec<- df_fec[!is.na(df_fec$WinNDVI), ] # now has equal number of lines

# need exact number of lines in two df 
df_fec<-cbind(df_fec, year[, 1:2])
getwd()
write.csv(df_fec, "data/df_fec_pca.csv")


