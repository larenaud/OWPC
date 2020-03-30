# this script creates dataframes for model selection on REPRODUCTION 
# modified 24 march 2020 by L. Renaud

# results are 5 scripts : 
#sheep_data : yr, id and mass data of individual sheep
#pheno_surv : season lengths from 2000-2016 including PCA (2001-2015)
#clim_surv : seaonal PDO and SOI based on monthly data
#weather : seasonal Temp and Prec based on monthly data by F.Rousseu
#dataFecUnscld : the whole dataframe, unscaled
# dataFecScld : the whole dataframe scaled 



# these are left with NAs




# load libraries
library(googledrive)
library(plyr)
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(cowplot)
library(googledrive)
library(xtable)
library(AICcmodavg)
library(readxl)
library(vegan)
library(knitr)
library(kableExtra)

getwd()
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data") # where to download
# clean up 

rm(list = ls())

# 1 - Add pheno season lengths   ------------------------------------------------------

# see 02_tidying_survival.R for the early steps of adding season lengths based on (green-down - green-up) in Julian days

# skip and go to step 2


# 2- tidy pheno dataframe -----------------------------------------------------



pheno = read.csv2("pheno_by_yr.csv",
                  na.string = c("", "NA"),sep = ",")

# select needed only
colnames(pheno)
pheno <- unique(pheno)

pheno$year <- as.numeric(as.character(pheno$year))

colnames(pheno)

#add time lags to pheno --------------------------------------------------

# those that need time lag t-1 
tmp1 <- unique(pheno[, c("year", "SummerNDVI","SummerEVI","SummerLAI",
                         "SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR")])

# add time lag for summer lengths (real time lag t-1)
tmp1$year <- tmp1$year + 1
tmp1 <- tmp1 %>% 
  rename(SummerNDVIfec= SummerNDVI,
         SummerEVIfec=SummerEVI, 
         SummerLAIfec=SummerLAI,
         SummerGPPfec=SummerGPP,
         SummerSnowfec =SummerSnow,
         SummerPSNNETfec =SummerPSNNET,
         SummerFPARfec =SummerFPAR)

pheno_fec <- merge(pheno,
                   tmp1,
                   by.x = c("year"), 
                   by.y = c("year"), 
                   all.x = T)

colnames(pheno_fec)

pheno_fec<- pheno_fec%>%
  rename(WinNDVIfecT = WinNDVI,
         WinEVIfecT =WinEVI ,
         WinLAIfecT = WinLAI ,
         WinGPPfecT = WinGPP,
         WinSnowfecT = WinSnow,
         WinPSNNETfecT = WinPSNNET,
         WinFPARfecT = WinFPAR, 
         NDVIfecT = ndvi_log_up_jul,
         EVIfecT =evi_log_up_jul, 
         LAIfecT =lai_log_up_jul,
         GPPfecT =gpp_log_up_jul,
         SNOWfecT =snow_log_up_jul,
         PSNNETfecT =psnnet_log_up_jul,
         FPARfecT =fpar_log_up_jul
  )
colnames(pheno_fec)
pheno_fec <- pheno_fec[, c("year","NDVIfecT", "EVIfecT","LAIfecT", "GPPfecT",   "SNOWfecT" , "PSNNETfecT",  "FPARfecT",
                           "WinNDVIfecT","WinEVIfecT","WinLAIfecT", "WinGPPfecT","WinSnowfecT","WinPSNNETfecT","WinFPARfecT",
                           "SummerNDVIfec","SummerEVIfec","SummerLAIfec","SummerGPPfec","SummerSnowfec","SummerPSNNETfec", "SummerFPARfec")]

# 3 - make pca with appropriate time lags ---------------------------------------------------------------

colnames(pheno_fec)

pheno_fec<-pheno_fec %>% 
  filter(year > 2001)


lengths <- pheno_fec[c("WinNDVIfecT","WinEVIfecT","WinLAIfecT","WinGPPfecT","WinSnowfecT","WinPSNNETfecT","WinFPARfecT",
                    "SummerNDVIfec","SummerEVIfec", "SummerLAIfec","SummerGPPfec","SummerSnowfec","SummerPSNNETfec",
                    "SummerFPARfec")] 
hist(unlist(lengths))# MVN distribution

# Standardisation
lengths <-scale(lengths)
lengths <- na.omit(lengths) # ça enlève toutes les lignes avec des NA

# PCA
acp_fec <- rda(lengths) # Option par défaut : scale=FALSE

#loadings scaling 2
spScores = round(scores(acp_fec, choices = 1:4, display = "species", scaling = 2), 3) # used default scaling # could swith to 0

# see different results - keep scaling 2
summary(acp_fec)
summary(acp_fec, scaling = 1)

eigenvals(acp_fec)
year <- summary(acp_fec)$sites
var <- summary(acp_fec)$species # keep default scaling 
summary(acp_fec)$sp

kable(spScores) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1, 14)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "table2_pca_fec.html", self_contained = T) 


# pca for timing  ---------------------------------------------------------

colnames(pheno_fec)
lengths2<- pheno_fec[c("NDVIfecT","EVIfecT","LAIfecT","GPPfecT","SNOWfecT","PSNNETfecT","FPARfecT" )] 
hist(unlist(lengths2))# MVN distribution

# Standardisation
lengths2 <-scale(lengths2)
lengths2 <- na.omit(lengths2) # ça enlève toutes les lignes avec des NA

# PCA
acpTiming <- rda(lengths2) # Option par défaut : scale=FALSE

#loadings scaling 2
spScores = round(scores(acpTiming, choices = 1:4, display = "species", scaling = 2), 3) # used default scaling # could swith to 0

# see different results - keep scaling 2
summary(acpTiming)
summary(acpTiming, scaling = 1)

eigenvals(acpTiming)
yearTim <- data.frame(summary(acpTiming)$sites)

names(yearTim)[1:2] = c("PC1Tim", "PC2Tim")

varTim <- summary(acpTiming)$species # keep default scaling 
summary(acpTiming)$sp

kable(spScores) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "table2_pca_fec.html", self_contained = T) 


# need exact number of lines in two df 

pheno_fec<- pheno_fec[!is.na(pheno_fec$SummerNDVI), ]
pheno_fec<- pheno_fec[!is.na(pheno_fec$SummerGPP), ]
pheno_fec<- pheno_fec[!is.na(pheno_fec$WinNDVI), ] # now has equal number of lines

# need exact number of lines in two df 
pheno_fec<-cbind(pheno_fec, year[, 1:2])
pheno_fec<-cbind(pheno_fec, yearTim[, 1:2])


# show results 
biplot(acp_fec, scaling=1)
biplot(acp_fec, scaling=2) # relationships between variables 
biplot(acpTiming, scaling=1)
biplot(acpTiming, scaling=2) # relationships between variables 


# clean up 

pheno_fec <- pheno_fec[, c( "year","NDVIfecT",
                            "WinNDVIfecT",
                            "SummerNDVIfec",
                            "PC1","PC2",
                            "PC1Tim","PC2Tim" )]

# 4 - get raw climate data  ---------------------------------------------------
getwd()

# see 02_tidying_survival.R for early steps to create climate data from montly data 


# skip and go to step 5 






# 5 - tidy climate data and add time lags ---------------------------------

clim = read.csv2("season_climate_ram.csv",
                 na.string = c("", "NA"),sep = ",")


# add time lag # careful this is tricky
clim$yr <- as.numeric(as.character(clim$yr))

# PDO/ENSO for for survival to t1
colnames(clim)
tmp <- clim[, c("yr", "PDO.summer", "PDO.fall", "SOI.summer" ,"SOI.fall")]

#now add backward time lag for fall and summer seasons 
tmp$yr <- tmp$yr+1
dim(tmp)
tmp <- tmp %>% 
  rename(PDO.summer_fec= PDO.summer,
         PDO.fall_fec=PDO.fall, 
         SOI.summer_fec =SOI.summer,
         SOI.fall_fec =SOI.fall)
head(tmp)

colnames(clim)
clim_fec <- merge(clim,
                  tmp,
                  by.x = c("yr"), 
                  by.y = c("yr"), 
                  all.x=T)

clim_fec <- clim_fec %>% 
  rename(PDO.winter_fec = PDO.winter,
         PDO.spring_fec =PDO.spring, 
         SOI.winter_fec =SOI.winter,
         SOI.spring_fec =SOI.spring)
getwd()

colnames(clim_fec)
clim_fec <- clim_fec[, c("yr", "PDO.winter_fec", "PDO.spring_fec","PDO.summer_fec", "PDO.fall_fec",
                         "SOI.winter_fec", "SOI.spring_fec", "SOI.summer_fec", "SOI.fall_fec")]





# 6- get weather data   ---------------------------------------------------

# get data from François or skip and go to step 6

#drive_download("OWPC/Analyses/data/Raw/Climat/monthlyRam", type="xls", overwrite = T)
weather<-read.delim("monthlyRam", header=T, sep=",") # this is from François 

# create seasons

# ??? cannot find that code 






# 7 - add time lags to weather data --------------------------------------------------------


# originally by Y. Ritchot - modified by LR march 24 2020

#drive_download("OWPC/Analyses/data/Raw/Climat/Localweather_seasons",type="csv", overwrite=T)

weather<-read.delim("Localweather_seasons", header=T, sep=",")


# Add Summer(t-1) and Fall(t-1) for fecundity
weather$yr <- as.numeric(as.character(weather$yr))

tmp <- weather[, c("yr","T.SUMMER", "P.SUMMER", "T.AUT","P.AUT")]
names(weather)


tmp$yr <- tmp$yr+1

tmp <- tmp %>% 
  rename(TSummerFec= T.SUMMER, 
         PSummerFec= P.SUMMER, 
         TAutFec = T.AUT,
         PAutFec = P.AUT)

head(tmp)

colnames(weather)
weather_fec<-weather[, c("yr","T.Win","P.Win","T.SPRING", "P.SPRING")] # changed T.win.m1 to T.Win only 
head(weather_fec)
weather_fec <- merge(weather_fec,
                     tmp,
                     by.x = c("yr"), 
                     by.y = c("yr"))

# clean up 

rm(tmp, tmp1, acp_fec, clim, pheno, var, spScores, lengths)


# 8 - prepare sheep data ------------------------------------------------------

sheep_data <- read.csv2("repro_mass.csv", sep = ",")


# classes d'age 3,4-8,9
sheep_data$ageClass <- ifelse(sheep_data$age == 3, 3, sheep_data$age)
sheep_data$ageClass <- ifelse(sheep_data$age >= 9, 9, sheep_data$age)
c48 <- c(4:8)
sheep_data$ageClass <- ifelse(sheep_data$age %in% c48 , 48, sheep_data$ageClass)

sheep_data$ageClass <- as.factor(sheep_data$ageClass)


# add time lags to control variables 


tmp<-sheep_data[, c("yr", "ID", "MassAutumn", "pred", "true_repro")]
tmp$yr<-as.numeric(tmp$yr)
tmp$yr<-tmp$yr+1

tmp <- tmp %>% 
  rename(MassAutumn_tm1= MassAutumn, # previous mass in September y-1
         pred_tm1= pred, # previous predation y-1
         true_repro_tm1 = true_repro) # previous reproductive success y-1

tmp1= merge(tmp,
            sheep_data,
            by.x = c("yr", "ID"), 
            by.y =  c("yr", "ID"), 
            all.y=T)

# verif
tmp1 = tmp1 %>%
  arrange(ID) 

head(tmp1, 10)
tail(tmp1, 10)
# end verification 


sheep_data = tmp1


# clean up 

rm(tmp, tmp1)

# 9 - merge all dataframes to sheep data  -----------------------------------------

# merge # 1 # only keep ndvi

df1 <- merge(sheep_data[c("yr","ID", "raw_repro", "true_repro", "MassSpring","MassAutumn","age","ageClass", "pred", 
                          "first_yr_trans", "MassAutumn_tm1", "pred_tm1", "true_repro_tm1")],
                    pheno_fec,
                    by.x = "yr", 
                    by.y =  "year", 
                    all.x=T)


# merge # 2 add climate to previous file 

colnames(clim_fec)
colnames(clim_fec) <- c("yr","PDOWinterFec", "PDOSpringFec" ,"PDOSummerFec", "PDOFallFec","SOIWinterFec",
                        "SOISpringFec","SOISummerFec", "SOIFallFec")


colnames(df1)


# change file name each time 
df2 <- merge(df1, 
             clim_fec,
             by.x = "yr", 
             by.y =  "yr", 
             all.x=T)

# merge # 3 add weather 
colnames(weather_fec)
colnames(weather_fec) <- c("yr","TWin","PWin","TSpring","PSpring","TSummerFec", "PSummerFec", "TAutFec","PAutFec") 

colnames(df2)

df3 <- merge(df2, 
             weather_fec,
             by.x = "yr", 
             by.y =  "yr", 
             all.x=T)

dataFec <- df3



# tidy up for models : factors, scaling, structure

str(dataFec)

dataFec$yr<-as.factor(dataFec$yr)
dataFec$raw_repro<-as.factor(dataFec$raw_repro)
dataFec$true_repro<-as.factor(dataFec$true_repro)
dataFec$true_repro_tm1<-as.factor(dataFec$true_repro_tm1)

dataFec$pred<-as.factor(dataFec$pred)
dataFec$pred_tm1<-as.factor(dataFec$pred_tm1)

dataFec$MassSpring<-as.numeric(as.character(dataFec$MassSpring))
dataFec$MassAutumn<-as.numeric(as.character(dataFec$MassAutumn))
dataFec$MassAutumn_tm1<-as.numeric(as.character(dataFec$MassAutumn_tm1))

dataFec$PDOWinterFec<-as.numeric(as.character(dataFec$PDOWinterFec))
dataFec$PDOSummerFec<-as.numeric(as.character(dataFec$PDOSummerFec))
dataFec$PDOSpringFec<-as.numeric(as.character(dataFec$PDOSpringFec))
dataFec$PDOFallFec<-as.numeric(as.character(dataFec$PDOFallFec))
dataFec$SOIWinterFec<-as.numeric(as.character(dataFec$SOIWinterFec))
dataFec$SOISummerFec<-as.numeric(as.character(dataFec$SOISummerFec))
dataFec$SOISpringFec<-as.numeric(as.character(dataFec$SOISpringFec))
dataFec$SOIFallFec<-as.numeric(as.character(dataFec$SOIFallFec))


# Add new column for the combined effect of PDO and SOI. Combined effect = PDO - SOI
dataFec$PDOSOI_winter <- dataFec$PDOWinterFec - dataFec$SOIWinterFec
dataFec$PDOSOI_spring <- dataFec$PDOSpringFec - dataFec$SOISpringFec
dataFec$PDOSOI_summer <- dataFec$PDOSummerFec - dataFec$SOISummerFec
dataFec$PDOSOI_fall <- dataFec$PDOFallFec - dataFec$SOIFallFec


# select translocation = 0 and filter 

dataFec<-filter(dataFec, first_yr_trans==0)  # n = 685
dataFec<-filter(dataFec, age>=3) # n drops to 388


# control variables 
dataFec<- dataFec[!is.na(dataFec$MassSpring),] # n = 374 
dataFec<- dataFec[!is.na(dataFec$MassAutumn),]


# save unscaled 
dataFecUnscld = dataFec
str(dataFecUnscld)


# reorder things to scale and deep essential data 
colnames(dataFec)

colnames(pheno_fec)

dataFec <- dataFec[, c("yr","ID","raw_repro","true_repro", "age" ,"ageClass", "pred","first_yr_trans" ,"pred_tm1",
                       "true_repro_tm1","MassSpring","MassAutumn", "MassAutumn_tm1",
                       "NDVIfecT","WinNDVIfecT","SummerNDVIfec", "PC1","PC2","PC1Tim","PC2Tim", 
                       "PDOWinterFec","PDOSpringFec","PDOSummerFec","PDOFallFec","SOIWinterFec","SOISpringFec","SOISummerFec", "SOIFallFec",
                       "TWin","PWin","TSpring","PSpring","TSummerFec","PSummerFec","TAutFec","PAutFec", 
                       "PDOSOI_winter","PDOSOI_spring"  , "PDOSOI_summer","PDOSOI_fall")]

colnames(dataFec)

dataFec[, c(11:40)] <- scale(dataFec[, c(11:40)])
dataFecScld = dataFec


# cleaning 

rm(tmp,tmp1, tmp2, tmp3)
rm(df1, df2, df3)





# save as R objects  ------------------------------------------------------
# 
# save(sheep_data, pheno_fec, clim_fec, weather_fec, dataFecUnscld, dataFecScld,
#    file = "/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/cache/dataFecundityModels.RData")
# drive_upload("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/cache/dataFecundityModels.RData",
#              path = "OWPC/Analyses/cache/dataFecundityModels.RData", overwrite = T)
