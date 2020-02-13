# script for making pca for survival and fecundity data 

library(lme4)
library(plyr)
library(dplyr)
library(AICcmodavg)
library(xtable)
library(googledrive)
library(vegan)
library(knitr)
rm(list = ls())
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
acp_surv <- rda(lengths) # Option par défaut : scale=FALSE

#these are the loadings (correlations of each variable with each pca xis)
print(kable(round(scores(acp_surv, choices = 1:4, display = "species", scaling = 0), 3)))

### Extraire les résultats
summary(acp_surv)
summary(acp_surv, scaling = 1)
#quartz()
biplot(acp_surv, scaling="sites") # relationships between years # the eigenvalues are expressed for sites, and species are left unscaled.
biplot(acp_surv, scaling=1)

biplot(acp_surv, scaling=2) # relationships between variables 
biplot(acp_surv, scaling="species")

eigenvals(acp_surv)
year <- summary(acp_surv, scaling = 1)$sites # this is the new scores for year year
var <- summary(acp_surv, scaling = 1)$species # this is the contribution of variables to each pc
summary(acp_surv)$sp

getwd()

#pdf("graph/biplot_pheno_scaling1.pdf")
plot(acp_surv, scaling=1, main="Triplot RDA - scaling 1", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1,1))
points(scores(acp_surv, display="sites", choices=c(1,2), scaling=1), pch=19, col=blues9, cex=1.2)
arrows(0,0,
       scores(acp_surv, display="species", choices=c(1), scaling=1)*0.1,
       scores(acp_surv, display="species", choices=c(2), scaling=1)*0.1,
       col="grey",length=0.2,angle=15)
dim(var)
var <- summary(acp_surv, scaling = 1)$species

rownames(var)<- c("SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR",    
                  "WinNDVI_surv","WinEVI_surv","WinLAI_surv","WinGPP_surv","WinSnow_surv","WinPSNNET_surv" ,"WinFPAR_surv")

text(scores(acp_surv, display="species", choices=c(1), scaling=1)*0.34,
     scores(acp_surv, display="species", choices=c(2), scaling=1)*0.32,
     labels=rownames(var),
     col="blue", cex=1)
dev.off()

# merge 2 first pca to pheno_dataframe 
year <- summary(acp_surv, scaling = 1)$sites

df_surv<- df_surv[!is.na(df_surv$SummerNDVI), ]
df_surv<- df_surv[!is.na(df_surv$SummerGPP), ]
df_surv<- df_surv[!is.na(df_surv$SummerGPP), ]
df_surv<- df_surv[!is.na(df_surv$WinNDVI), ] # now has equal number of lines

# need exact number of lines in two df 
df_surv<-cbind(df_surv, year[, 1:2])
getwd()
#write.csv(df_surv, "data/df_surv_pca.csv")

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
acp_fec <- rda(lengths) # Option par défaut : scale=FALSE

#these are the loadings (correlations of each variable with each pca xis)
print(kable(round(scores(acp_fec, choices = 1:4, display = "species", scaling = 0), 3)))

### Extraire les résultats
summary(acp_fec)
summary(acp_fec, scaling = 1)
quartz()
biplot(acp_fec, scaling="sites") # relationships between years # the eigenvalues are expressed for sites, and species are left unscaled.
biplot(acp_fec, scaling=1)

biplot(acp_fec, scaling=2) # relationships between variables 
biplot(acp_fec, scaling="species")

eigenvals(acp_fec)
year <- summary(acp_fec, scaling = 1)$sites
var <- summary(acp_fec, scaling = 1)$species
summary(acp_fec)$sp

getwd()

#pdf("graph/biplot_pheno_fec_scaling1.pdf")
plot(acp_fec, scaling=1, main="Triplot RDA - scaling 1", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1,1))
points(scores(acp_fec, display="sites", choices=c(1,2), scaling=1), pch=19, col=blues9, cex=1.2)
arrows(0,0,
       scores(acp_fec, display="species", choices=c(1), scaling=1)*0.1,
       scores(acp_fec, display="species", choices=c(2), scaling=1)*0.1,
       col="grey",length=0.2,angle=15)
dim(var)
var <- summary(acp_fec, scaling = 1)$species

rownames(var)<- c("WinNDVI_fec","WinEVI_fec","WinLAI_fec","WinGPP_fec","WinSnow_fec","WinPSNNET_fec","WinFPAR_fec",
                  "SummerNDVI_fec","SummerEVI_fec", "SummerLAI_fec","SummerGPP_fec","SummerSnow_fec","SummerPSNNET_fec", "SummerFPAR_fec")

text(scores(acp_fec, display="species", choices=c(1), scaling=1)*0.34,
     scores(acp_fec, display="species", choices=c(2), scaling=1)*0.32,
     labels=rownames(var),
     col="blue", cex=1)
dev.off()

# merge 2 first pca to pheno_dataframe 
year <- summary(acp_fec, scaling = 1)$sites

df_fec<- df_fec[!is.na(df_fec$SummerNDVI), ]
df_fec<- df_fec[!is.na(df_fec$SummerGPP), ]
df_fec<- df_fec[!is.na(df_fec$WinNDVI), ] # now has equal number of lines

# need exact number of lines in two df 
df_fec<-cbind(df_fec, year[, 1:2])
getwd()
#write.csv(df_fec, "data/df_fec_pca.csv")

# save as R object 
getwd()
save(acp_fec, acp_surv, df_fec, df_surv, file = "graph/acp_data.RData")
