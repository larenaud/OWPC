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
setwd("") 

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
                         "SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR", "snow_log_do_jul")])

# add time lag for summer lengths (real time lag t-1)
tmp1$year <- tmp1$year + 1
tmp1 <- tmp1 %>% 
  rename(SummerNDVIfec= SummerNDVI,
         SummerEVIfec=SummerEVI, 
         SummerLAIfec=SummerLAI,
         SummerGPPfec=SummerGPP,
         SummerSnowfec =SummerSnow,
         SummerPSNNETfec =SummerPSNNET,
         SummerFPARfec =SummerFPAR, 
         SNOWCOVERfec = snow_log_do_jul) #  this is NEW 

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
         SNOWMELTfecT =snow_log_up_jul,
         PSNNETfecT =psnnet_log_up_jul,
         FPARfecT =fpar_log_up_jul
  )

colnames(pheno_fec)
pheno_fec <- pheno_fec[, c("year","NDVIfecT", "EVIfecT","LAIfecT", "GPPfecT",   "SNOWMELTfecT" , "PSNNETfecT",  "FPARfecT","SNOWCOVERfec", # NEW
                           "WinNDVIfecT","WinEVIfecT","WinLAIfecT", "WinGPPfecT","WinSnowfecT","WinPSNNETfecT","WinFPARfecT",
                           "SummerNDVIfec","SummerEVIfec","SummerLAIfec","SummerGPPfec","SummerSnowfec","SummerPSNNETfec", "SummerFPARfec")]

# 3 - make pca with appropriate time lags ---------------------------------------------------------------
# 
# colnames(pheno_fec)
# 
# pheno_fec<-pheno_fec %>% 
#   filter(year > 2001)
# 
# 
# lengths <- pheno_fec[c("WinNDVIfecT","WinEVIfecT","WinLAIfecT","WinGPPfecT","WinSnowfecT","WinPSNNETfecT","WinFPARfecT",
#                     "SummerNDVIfec","SummerEVIfec", "SummerLAIfec","SummerGPPfec","SummerSnowfec","SummerPSNNETfec",
#                     "SummerFPARfec")] 
# hist(unlist(lengths))# MVN distribution
# 
# # Standardisation
# lengths <-scale(lengths)
# lengths <- na.omit(lengths) # ça enlève toutes les lignes avec des NA
# 
# # PCA
# acp_fec <- rda(lengths) # Option par défaut : scale=FALSE
# 
# #loadings scaling 2
# spScores = round(scores(acp_fec, choices = 1:4, display = "species", scaling = 2), 3) # used default scaling # could swith to 0
# 
# # see different results - keep scaling 2
# summary(acp_fec)
# summary(acp_fec, scaling = 1)
# 
# eigenvals(acp_fec)
# year <- summary(acp_fec)$sites
# var <- summary(acp_fec)$species # keep default scaling 
# summary(acp_fec)$sp
# 
# kable(spScores) %>%
#   kable_styling(font_size = 10) %>%
#   row_spec(c(0,1, 14)) %>%
#   kable_styling("bordered") %>%
#   save_kable(file = "table2_pca_fec.html", self_contained = T) 




# 4 - pca Summer only NO SNOW ---------------------------------------------------------


colnames(pheno_fec)

pheno_fec<-pheno_fec %>% 
  filter(year > 2001)


lengths <- pheno_fec[c("SummerNDVIfec","SummerEVIfec", "SummerLAIfec","SummerGPPfec","SummerPSNNETfec",
                       "SummerFPARfec")] 
hist(unlist(lengths))# MVN distribution

# Standardisation
lengths <-scale(lengths)
lengths <- na.omit(lengths) # ça enlève toutes les lignes avec des NA

# PCA
acp_fec <- rda(lengths) # Option par défaut : scale=FALSE


# see different results - keep scaling 2
summary(acp_fec)
summary(acp_fec, scaling = 1)
eigenvals(acp_fec)

#loadings - unscaled scores 
spScores = data.frame(round(scores(acp_fec, choices = 1:4, display = "species", scaling = 0), 3)) # used default scaling # could swith to 0
yearSummer <- data.frame(summary(acp_fec)$sites)
colnames(yearSummer)[1:2] <- c("PC1Summer", "PC2Summer")


kable(spScores) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1,6)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "tableS4PcaSummerFEC.html", self_contained = T) 



ggplot(yearSummer,aes(PC1Summer, PC2Summer)) +
  geom_point(data=yearSummer,aes(x=PC1Summer, y=PC2Summer, colour= NULL), size= 3, shape=16) +
  geom_hline(yintercept=0, size=.2) + geom_vline(xintercept=0, size=.2)+
  theme_minimal()+
  xlab("PC1Summer (64.7%)")+
  ylab("PC2Summer (18.6%)")+
  geom_segment(data=spScores, aes(x = 0, y = 0, xend = PC1*3, yend = PC2*3), arrow = arrow(length = unit(1/2, 'picas')), color = "grey30")+
  geom_text(data=spScores, aes(x=PC1*3.5,y=PC2*3.5,label=rownames(spScores)), size=3, colour=  "navyblue",position=position_jitter(width=0.15,height=0.1))
ggsave("BiplotSummerFEC.pdf", width = 130, height = 130, units = "mm", pointsize = 8)


# 5 - pca Winter only NO SNOW ---------------------------------------------------------

lengths <- pheno_fec[c("WinNDVIfecT","WinEVIfecT","WinLAIfecT","WinGPPfecT","WinPSNNETfecT","WinFPARfecT")] 
hist(unlist(lengths))# MVN distribution

# Standardisation
lengths <-scale(lengths)
lengths <- na.omit(lengths) # ça enlève toutes les lignes avec des NA

# PCA
acp_fec <- rda(lengths) # Option par défaut : scale=FALSE


# see different results - keep scaling 2
summary(acp_fec)
summary(acp_fec, scaling = 1)

eigenvals(acp_fec)

yearWinter <- data.frame(summary(acp_fec)$sites)
colnames(yearWinter)[1:2] <- c("PC1Winter", "PC2Winter")

#loadings scaling 2
spScores = data.frame(round(scores(acp_fec, choices = 1:4, display = "species", scaling = 0), 3)) # used default scaling # could swith to 0


kable(spScores) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1,6)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "tableS5PcaWinterFEC.html", self_contained = T) 


ggplot(yearWinter,aes(PC1Winter, PC2Winter)) +
  geom_point(data=yearWinter,aes(x=PC1Winter, y=PC2Winter, colour= NULL), size= 3, shape=16) +
  geom_hline(yintercept=0, size=.2) + geom_vline(xintercept=0, size=.2)+
  theme_minimal()+
  xlab("PC1Winter (80.7%)")+
  ylab("PC2Winter (10.8%)")+
  geom_segment(data=spScores, aes(x = 0, y = 0, xend = PC1*3, yend = PC2*3), arrow = arrow(length = unit(1/2, 'picas')), color = "grey30") +
  geom_text(data=spScores, aes(x=PC1*3.5,y=PC2*3.5,label=rownames(spScores)), size=3, colour=  "navyblue",position=position_jitter(width=0.15,height=0.1))
ggsave("BiplotWinterFEC.pdf", width = 130, height = 130, units = "mm", pointsize = 8)

# 6 - pca for timing NO SNOW ---------------------------------------------------------

colnames(pheno_fec)
lengths2<- pheno_fec[c("NDVIfecT","EVIfecT","LAIfecT","GPPfecT","PSNNETfecT","FPARfecT" )] 
hist(unlist(lengths2))# MVN distribution

# Standardisation
lengths2 <-scale(lengths2)
lengths2 <- na.omit(lengths2) # ça enlève toutes les lignes avec des NA

# PCA
acpTiming <- rda(lengths2) # Option par défaut : scale=FALSE

# see different results - keep scaling 2
summary(acpTiming)
summary(acpTiming, scaling = 1)

eigenvals(acpTiming)
yearTim <- data.frame(summary(acpTiming)$sites) # ONLY PC1 COULD BE TAKEN  ? 

names(yearTim)[1:2] = c("PC1Date", "PC2Date")

#loadings UNSCALED SCORES 
spScores = data.frame(round(scores(acpTiming, choices = 1:4, display = "species", scaling = 0),3)) # that gets you the loadings 


kable(spScores) %>%
  kable_styling(font_size = 10) %>%
  row_spec(c(0,1)) %>%
  kable_styling("bordered") %>%
  save_kable(file = "tableS6PcaDateFEC.html", self_contained = T) 

# show results 
biplot(acp_fec, scaling=2) # relationships between variables 
biplot(acpTiming, scaling=3) # this is equivalent to what ggplot does 


ggplot(yearTim,aes(PC1Date, PC2Date)) +
  geom_point(data=yearTim,aes(x=PC1Date, y=PC2Date, colour= NULL), size= 3, shape=16) +
  geom_hline(yintercept=0, size=.2) + geom_vline(xintercept=0, size=.2)+
  theme_minimal()+
  xlab("PC1Date (91.2%)")+
  ylab("PC2Date (5.4%)")+
  geom_segment(data=spScores, aes(x = 0, y = 0, xend = PC1*3, yend = PC2*3), arrow = arrow(length = unit(1/2, 'picas')), color = "grey30")+
  geom_text(data=spScores, aes(x=PC1*3.5,y=PC2*3.5,label=rownames(spScores)), size=3, colour=  "navyblue",position=position_jitter(width=0.15,height=0.1))
ggsave("BiplotTimingFec.pdf", width = 130, height = 130, units = "mm", pointsize = 8)



# bind pca data ---------------------------------------------------------------


# need exact number of lines in two df 

pheno_fec<- pheno_fec[!is.na(pheno_fec$SummerNDVI), ]
pheno_fec<- pheno_fec[!is.na(pheno_fec$SummerGPP), ]
pheno_fec<- pheno_fec[!is.na(pheno_fec$WinNDVI), ] # now has equal number of lines

pheno_fec<-cbind(pheno_fec, yearSummer[, 1:2])
pheno_fec<-cbind(pheno_fec, yearWinter[, 1:2])
pheno_fec<-cbind(pheno_fec, yearTim[, 1:2])


# 7 - get raw climate data  ---------------------------------------------------
getwd()

# see 02_tidying_survival.R for early steps to create climate data from montly data 


# skip and go to step 5 






# 8 - tidy climate data and add time lags ---------------------------------

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





# 9 - get weather data   ---------------------------------------------------

# get data from François or skip and go to step 10

#drive_download("OWPC/Analyses/data/Raw/Climat/monthlyRam", type="xls", overwrite = T)
weather<-read.delim("monthlyRam", header=T, sep=",") # this is from François 

# create seasons

# ??? cannot find that code 






# 10 - add time lags to weather data --------------------------------------------------------


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


# 11 - prepare sheep data ------------------------------------------------------

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

# 12 - merge all dataframes to sheep data  -----------------------------------------

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


# reorder 
colnames(dataFec)

dataFec <- dataFec[, c("yr","ID","raw_repro","true_repro","true_repro_tm1" , "age","ageClass","pred","first_yr_trans" ,"pred_tm1",
                       "MassSpring","MassAutumn","MassAutumn_tm1",
                       "NDVIfecT","EVIfecT","LAIfecT","GPPfecT","SNOWMELTfecT", "PSNNETfecT","FPARfecT","SNOWCOVERfec",
                       "WinNDVIfecT","WinEVIfecT","WinLAIfecT","WinGPPfecT","WinSnowfecT","WinPSNNETfecT", "WinFPARfecT",
                       "SummerNDVIfec","SummerEVIfec","SummerLAIfec","SummerGPPfec","SummerSnowfec","SummerPSNNETfec", "SummerFPARfec",
                       "PC1Summer" , "PC2Summer","PC1Winter","PC2Winter","PC1Date","PC2Date",
                       "PDOWinterFec","PDOSpringFec","PDOSummerFec","PDOFallFec", "SOIWinterFec","SOISpringFec","SOISummerFec","SOIFallFec",
                       "TWin","PWin","TSpring","PSpring","TSummerFec", "PSummerFec","TAutFec","PAutFec" )]

str(dataFec)

dataFec$yr<-as.factor(dataFec$yr)
dataFec$ID<-as.factor(dataFec$ID)
dataFec$raw_repro<-as.factor(dataFec$raw_repro)
dataFec$true_repro<-as.factor(dataFec$true_repro)
dataFec$true_repro_tm1<-as.factor(dataFec$true_repro_tm1)
dataFec$pred<-as.factor(dataFec$pred)
dataFec$pred_tm1<-as.factor(dataFec$pred_tm1)
dataFec$ageClass<-as.factor(dataFec$ageClass)


colnames(dataFec)

# dataFec[, c(11:57)]<-as.numeric(as.character(dataFec[, c(11:57)]))
# other are integer TO CHECK

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
dataFec$PDOSOI_fall <- dataFec$PDOFallFec - dataFec$SOIFallFec # n = 673


# select translocation = 0 and filter 

dataFec<-filter(dataFec, first_yr_trans==0)  # n = 685
dataFec<-filter(dataFec, age>=3) # n drops to 388



# control variables 
dataFec<- dataFec[!is.na(dataFec$MassSpring),] # n = 374 
dataFec<- dataFec[!is.na(dataFec$MassAutumn),]

# save unscaled 
dataFecUnscld = dataFec
str(dataFecUnscld)


# calculate sd for projections 
colnames(dataFecUnscld)
dim(dataFecUnscld)

data.SD<-apply(dataFecUnscld[, 11:61], 2,sd, na.rm = T) # 2 pour prendre colonnes
head(data.SD)
data.MEAN<-apply(dataFecUnscld[, 11:61], 2,mean, na.rm = T)

# get something and backtransform 
#newd$snow<-(data.MEAN["SummerEVI"]*data.SD) + data.MEAN

dataFec[, c(11:61)] <- scale(dataFec[, c(11:61)])
fullFecDataScld = dataFec


# this is for projections 
#save(data.SD, data.MEAN, dataFecUnscld, fullFecDataScld, file = "dataProjectionsR.RData")


# keep essential data 
colnames(dataFec)

colnames(pheno_fec)

dataFecScld <- dataFec[, c("yr","ID","raw_repro","true_repro", "true_repro_tm1", "age" ,"ageClass", "pred","pred_tm1",
                       "MassSpring","MassAutumn", "MassAutumn_tm1",
                       "NDVIfecT","WinNDVIfecT","SummerNDVIfec", "SNOWMELTfecT", "SNOWCOVERfec", "WinSnowfecT", "SummerSnowfec",
                       "PC1Summer","PC2Summer","PC1Winter","PC2Winter","PC1Date","PC2Date",
                       "PDOWinterFec","PDOSpringFec","PDOSummerFec","PDOFallFec","SOIWinterFec","SOISpringFec","SOISummerFec", "SOIFallFec",
                       "TWin","PWin","TSpring","PSpring","TSummerFec","PSummerFec","TAutFec","PAutFec", 
                       "PDOSOI_winter","PDOSOI_spring"  , "PDOSOI_summer","PDOSOI_fall")]


# cleaning 

rm(tmp,tmp1, tmp2, tmp3)
rm(df1, df2, df3)



# save as R objects  ------------------------------------------------------
# 
# save(sheep_data, pheno_fec, clim_fec, weather_fec, fullFecDataScld, dataFecUnscld, dataFecScld,
#    file = "dataFecundityModels.RData")
# drive_upload("dataFecundityModels.RData",
#              path = "OWPC/Analyses/cache/dataFecundityModels.RData", overwrite = T)
