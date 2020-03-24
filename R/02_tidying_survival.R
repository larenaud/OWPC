# Tidying data pheno #
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

# clean up 

rm(list = ls())

getwd()
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data") # where to download

View(pheno)

# 1- Add pheno season lengths ------------------------------------------------------

# skip and go to step 2  
drive_download("OWPC/Analyses/data/Raw/pheno_ram.csv", overwrite = T) # where to get file
pheno <- read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/raw/pheno_ram.csv", sep = ",")

head(pheno)

#Keep only julian dates 
pheno <- pheno[ ,c(1,16:29)]

#####Add columns length SUMMER!#####
x <- pheno

x$SummerNDVI <- x$ndvi_log_do_jul-x$ndvi_log_up_jul 
x$SummerEVI <- x$evi_log_do_jul-x$evi_log_up_jul
x$SummerLAI <- x$lai_log_do_jul-x$lai_log_up_jul
x$SummerGPP <- x$gpp_log_do_jul-x$gpp_log_up_jul
x$SummerSnow <- x$snow_log_do_jul-x$snow_log_up_jul
x$SummerEVI <- x$evi_log_do_jul-x$evi_log_up_jul
x$SummerPSNNET <- x$psnnet_log_do_jul-x$psnnet_log_up_jul
x$SummerFPAR <- x$fpar_log_do_jul-x$fpar_log_up_jul

pheno <- x


#####Add columns length WINTER#####

yr <- (2000:2017)
bisYr <- c(2000,2004,2008,2012,2016)


P <- pheno

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

pheno<- merge(pheno, Winter, by.x = "year", by.y ="Year")
#write.csv(pheno, "data/pheno_by_yr.csv", row.names = FALSE)
#drive_upload("data/pheno_by_yr.csv", path = "OWPC/Analyses/data/Raw/pheno_by_yr.csv", overwrite = T)

#####Plot variation of indices WINTER#####
plot(WinNDVI ~ year, data = pheno, type = "b", col = "green", ylim = c(125,350), xlab = "Year", ylab= "Length of up/down period WINTER")
points(WinEVI ~ year, data = pheno, type = "b", col = "pink")
points(WinLAI ~ year, data = pheno, type = "b", col = "red")
points(WinGPP ~ year, data = pheno, type = "b", col = "orange")
points(WinSnow ~ year, data = pheno, type = "b", col = "blue")
points(WinPSNNET ~ year, data = pheno, type = "b", col = "purple")
points(WinFPAR ~ year, data = pheno, type = "b", col = "black")
legend("topleft", legend = c("NDVI","EVI","LAI","GPP","Snow","PSNNET","FPAR"), 
       col = c("green","pink","red","orange","blue","purple","black"),
       lty = 1,
       horiz =TRUE,
       cex = 0.50)

#####Plot variation of indices SUMMER#####
plot(SummerNDVI ~ year, data = pheno, type = "b", col = "green", ylim = c(70,250), xlab = "Year", ylab= "Length of up/down period SUMMER")
points(SummerEVI ~ year, data = pheno, type = "b", col = "pink")
points(SummerLAI ~ year, data = pheno, type = "b", col = "red")
points(SummerGPP ~ year, data = pheno, type = "b", col = "orange")
points(SummerSnow ~ year, data = pheno, type = "b", col = "blue")
points(SummerPSNNET ~ year, data = pheno, type = "b", col = "purple")
points(SummerFPAR ~ year, data = pheno, type = "b", col = "black")
legend("topleft", legend = c("NDVI","EVI","LAI","GPP","Snow","PSNNET","FPAR"), 
       col = c("green","pink","red","orange","blue","purple","black"),
       lty = 1,
       horiz =TRUE,
       cex = 0.50)

getwd()


# 2 - survival & pheno dataframe ------------------------------------------------------
sheep_data <- read.csv2("repro_mass.csv", sep = ",")
pheno = read.csv2("pheno_by_yr.csv",
                  na.string = c("", "NA"),sep = ",")

# select needed only
colnames(pheno)
pheno <- unique(pheno[, c("year", "SummerNDVI","SummerEVI","SummerLAI",
                          "SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR","WinNDVI",
                          "WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET",
                          "WinFPAR")])

# survival - add time lag for winter 
pheno$year <- as.numeric(as.character(pheno$year))
colnames(pheno)

colnames(pheno) # here only winter season is a problem - summer is on same year than surv
tmp1 <- unique(pheno[, c("year", "WinNDVI","WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET","WinFPAR")])
tmp1$year <- tmp1$year - 1 # on leur met l'année de la survie 
dim(tmp1)
tmp1 <- tmp1 %>% 
  rename(WinNDVI_surv = WinNDVI,
         WinEVI_surv =WinEVI, 
         WinLAI_surv =WinLAI,
         WinGPP_surv =WinGPP,
         WinSnow_surv =WinSnow,
         WinPSNNET_surv =WinPSNNET,
         WinFPAR_surv =WinFPAR)
head(tmp1)

colnames(pheno)
pheno_surv <- merge(pheno[, c("year","SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET", "SummerFPAR")],
                    tmp1,
                    by.x = c("year"), 
                    by.y = c("year"), 
                    all.x= T)
# make pca  ---------------------------------------------------------------
# scale
colnames(pheno_surv)
lengths <- pheno_surv[c("SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR","WinNDVI_surv" ,
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

# #pdf("graph/biplot_pheno_scaling1.pdf")
# plot(acp_surv, scaling=1, main="Triplot RDA - scaling 1", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1,1))
# points(scores(acp_surv, display="sites", choices=c(1,2), scaling=1), pch=19, col=blues9, cex=1.2)
# arrows(0,0,
#        scores(acp_surv, display="species", choices=c(1), scaling=1)*0.1,
#        scores(acp_surv, display="species", choices=c(2), scaling=1)*0.1,
#        col="grey",length=0.2,angle=15)
# dim(var)
# var <- summary(acp_surv, scaling = 1)$species
# 
# rownames(var)<- c("SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR",    
#                   "WinNDVI_surv","WinEVI_surv","WinLAI_surv","WinGPP_surv","WinSnow_surv","WinPSNNET_surv" ,"WinFPAR_surv")
# 
# text(scores(acp_surv, display="species", choices=c(1), scaling=1)*0.34,
#      scores(acp_surv, display="species", choices=c(2), scaling=1)*0.32,
#      labels=rownames(var),
#      col="blue", cex=1)
# dev.off()

pheno_surv<- pheno_surv[!is.na(pheno_surv$SummerGPP), ] # missing data 
pheno_surv<- pheno_surv[!is.na(pheno_surv$WinNDVI_surv), ] # missing data 

# need exact number of lines in two df 
pheno_surv<-cbind(pheno_surv, year[, 1:2])
getwd()

# merge dataframes 
colnames(pheno_surv)
colnames(sheep_data)
df_pheno_surv= merge(sheep_data[c("yr","ID", "alive_t1", "MassSpring","MassAutumn","age","pred", "first_yr_trans")],
                     pheno_surv,
                     by.x = "yr", 
                     by.y =  "year", 
                     all.x=T) # keep all years even if NA

df_pheno_surv$yr<-as.factor(df_pheno_surv$yr)
df_pheno_surv$MassSpring<-as.numeric(as.character(df_pheno_surv$MassSpring))
df_pheno_surv$MassAutumn<-as.numeric(as.character(df_pheno_surv$MassAutumn))

df_pheno_surv$alive_t1<-as.factor(df_pheno_surv$alive_t1)
df_pheno_surv$pred<-as.factor(df_pheno_surv$pred)

rm(tmp1, var, year, pheno, lengths, acp_surv)

# 3 - create surv climate data ------------------------------------------------

# calculating seasonal PDO and SOI values from monthly data 

# Cleaning R
#rm(list=ls(all=TRUE))

# Set working directory
# Download database from google drive and save it in previously set working directory
drive_download("OWPC/Analyses/data/Raw/Climat/monthly_climate_ram",type = "csv",overwrite = T)

# get SOI and PDO raw for all years and combine into new montly climate
SOI <- read.delim("SOI.txt")
PDO <- read.delim("PDO_raw.txt", sep = "")
colnames(SOI) <- c("yr" , "SOI.JAN" ,"SOI.FEB", "SOI.MAR", "SOI.APR" ,"SOI.MAI", "SOI.JUN", "SOI.JUL" ,"SOI.AUG" ,"SOI.SEP", "SOI.OCT" ,"SOI.NOV" ,"SOI.DEC")
colnames(PDO) <- c("YEAR" , "PDO.JAN" ,"PDO.FEB", "PDO.MAR", "PDO.APR" ,"PDO.MAI", "PDO.JUN", "PDO.JUL" ,"PDO.AUG" ,"PDO.SEP", "PDO.OCT" ,"PDO.NOV" ,"PDO.DEC")

PDO = PDO[PDO$YEAR>1990 & PDO$YEAR < 2018, ]
SOI = SOI[SOI$yr>1990 & SOI$yr < 2018, ]

montly_climate = cbind(PDO, SOI)
montly_climate = montly_climate[, -14]

#write.csv(montly_climate, "montly_climate_ram.csv", row.names = FALSE)

# Load database in R environment
data<-read.csv("montly_climate_ram.csv")

# See values for December PDOs
data$PDO.DEC

# Create column with PDO values in december of the previous year # STARTING YR 1999 # ADDED ONE YR
tmp = subset(data, YEAR %in%c(1998:2017)) # to find previous values 
tmp$PDO.DEC
#[1] -0.44 -1.63  0.52 -0.93  2.10  0.33 -0.17  0.20  0.14 -0.58 -0.87  0.08 -1.21 -1.79 -0.48 -0.41  2.51  1.01  1.17 0.50
tmp$SOI.DEC
#[1]  1.32  1.37  0.77 -1.06 -1.30  0.92 -0.94  0.01 -0.39  1.49  1.43 -0.95  2.90  2.45 -0.77 -0.05 -0.66 -1.00  0.19 -0.27

data = subset(data, YEAR %in%c(1999:2017)) # to bind from previous years

data$PDO.DEC.PREV.YR<-c(-0.44, -1.63, 0.52, -0.93, 2.10, 0.33, -0.17, 0.20, 0.14, -0.58, -0.87, 0.08, -1.21, -1.79,-0.48, -0.41, 2.51, 1.01, 1.17)

# Calculate mean PDO in winter
data$PDO.winter<-(data$PDO.DEC.PREV.YR+data$PDO.JAN+data$PDO.FEB+data$PDO.MAR)/4
# Calculate mean PDO in spring
data$PDO.spring<-(data$PDO.APR+data$PDO.MAI)/2
# Calculate mean PDO in summer
data$PDO.summer<-(data$PDO.JUN+data$PDO.JUL+data$PDO.AUG+data$PDO.SEP)/4
# Calculate mean PDO in fall
data$PDO.fall<-(data$PDO.OCT+data$PDO.NOV)/2

# See values for December SOIs
data$SOI.DEC
# Create column with SOI values in december of the previous year
#tmp$SOI.DEC
#[1]  1.32  1.37  0.77 -1.06 -1.30  0.92 -0.94  0.01 -0.39  1.49  1.43 -0.95  2.90  2.45 -0.77 -0.05 -0.66 -1.00  0.19 -0.27
data$SOI.DEC.PREV.YR<-c(1.32,1.37, 0.77,-1.06,-1.30,0.92,-0.94,0.01,-0.39,1.49,1.43,-0.95,2.90,2.45,-0.77,-0.05,-0.66,-1.00, 0.19)
# Calculate mean SOI in winter
data$SOI.winter<-(data$SOI.DEC.PREV.YR+data$SOI.JAN+data$SOI.FEB+data$SOI.MAR)/4
# Calculate mean SOI in spring
data$SOI.spring<-(data$SOI.APR+data$SOI.MAI)/2
# Calculate mean SOI in summer
data$SOI.summer<-(data$SOI.JUN+data$SOI.JUL+data$SOI.AUG+data$SOI.SEP)/4
# Calculate mean SOI in fall
data$SOI.fall<-(data$SOI.OCT+data$SOI.NOV)/2

# Create new dataframe with seasonal values of PDO and SOI
season_climate_ram<-data.frame(data$YEAR,data$PDO.winter,data$PDO.spring,data$PDO.summer,data$PDO.fall,data$SOI.winter,data$SOI.spring,data$SOI.summer,data$SOI.fall)
colnames(season_climate_ram)<-c("yr","PDO.winter","PDO.spring","PDO.summer","PDO.fall","SOI.winter","SOI.spring","SOI.summer","SOI.fall")

# Save new dataframe in a csv file and upload it to google drive
getwd()


# HERE ADDED YEAR 1999 - MAKE SURE TO UPLOAD GOOD ONE 
#write.csv(season_climate_ram,"season_climate_ram.csv",row.names=FALSE)
#drive_upload("season_climate_ram.csv",path = "OWPC/Analyses/data/Raw/Climat",name = "season_climate_ram_1999")


# clean
rm(SOI, tmp, sheep_data, PDO, monthly_climate, data)

# merge with repro_mass
sheep_data <- read.csv2("repro_mass.csv", sep = ",")
clim = read.csv2("season_climate_ram.csv",
                 na.string = c("", "NA"),sep = ",")

# add time lag # careful this is tricky
clim$yr <- as.numeric(as.character(clim$yr))
colnames(clim)

# TIME LAG PDO/ENSO for survival to t1
tmp <- clim[, c("yr","PDO.winter", "PDO.spring", "SOI.winter","SOI.spring")]

tmp$yr <- tmp$yr - 1
dim(tmp)
tmp <- tmp %>% 
  rename(PDO.winter_surv = PDO.winter,
         PDO.spring_surv =PDO.spring, 
         SOI.winter_surv =SOI.winter,
         SOI.spring_surv =SOI.spring)
head(tmp)

colnames(clim)
clim_surv <- merge(clim[, c("yr","PDO.summer", "PDO.fall", "SOI.summer","SOI.fall", "PDO.winter", "SOI.winter")], # keep previous winter for survival analsyes
                   tmp,
                   by.x = c("yr"), 
                   by.y = c("yr"), 
                   all.x=T)

clim_surv <- clim_surv %>% 
  rename(PDO.summer_surv = PDO.summer,
         PDO.fall_surv=PDO.fall,
         SOI.summer_surv = SOI.summer,
         SOI.fall_surv = SOI.fall)

clim_surv <- clim_surv %>% 
  rename(PDO.winter_tm1 = PDO.winter,
         SOI.winter_tm1 =SOI.winter)
head(clim)

# merge dataframes 
colnames(clim_surv)
colnames(sheep_data)
clim_surv= merge(sheep_data[c("yr","ID", "alive_t1", "MassSpring","MassAutumn","age","pred", "first_yr_trans")],
               clim_surv,
               by.x = "yr", 
               by.y =  "yr", 
               all.x=T) # keep all years even if NA


clim_surv$yr<-as.factor(clim_surv$yr)
clim_surv$alive_t1<-as.factor(clim_surv$alive_t1)

clim_surv$MassSpring<-as.numeric(as.character(clim_surv$MassSpring))
clim_surv$MassAutumn<-as.numeric(as.character(clim_surv$MassAutumn))

clim_surv$PDO.winter_surv<-as.numeric(as.character(clim_surv$PDO.winter_surv))
clim_surv$PDO.summer_surv<-as.numeric(as.character(clim_surv$PDO.summer_surv))
clim_surv$PDO.spring_surv<-as.numeric(as.character(clim_surv$PDO.spring_surv))
clim_surv$PDO.fall_surv<-as.numeric(as.character(clim_surv$PDO.fall_surv))
clim_surv$SOI.winter_surv<-as.numeric(as.character(clim_surv$SOI.winter_surv))
clim_surv$SOI.summer_surv<-as.numeric(as.character(clim_surv$SOI.summer_surv))
clim_surv$SOI.spring_surv<-as.numeric(as.character(clim_surv$SOI.spring_surv))
clim_surv$SOI.fall_surv<-as.numeric(as.character(clim_surv$SOI.fall_surv))
clim_surv$PDO.winter_tm1<-as.numeric(as.character(clim_surv$PDO.winter_tm1))
clim_surv$SOI.winter_tm1<-as.numeric(as.character(clim_surv$SOI.winter_tm1))


# include weather data to surv data  ---------------------------------------------------





rm(list = ls())

setwd("C:/Users/Yanny/Documents/uSherbrooke/Hiver 2020/NDVI/OWPC/OWPC")

drive_download("OWPC/Analyses/data/Raw/sheep_data", type="csv", overwrite=T)
sheep<-read.csv("sheep_data.csv", header=T, sep=",")
names(sheep)

drive_download("OWPC/Analyses/data/Raw/Climat/Localweather_seasons",type="csv", overwrite=T)
weather<-read.csv("Localweather_seasons.csv", header=T, sep=",")

# add time lags

# No time lag for survival
# Add Summer(t-1) and Fall(t-1) for fecundity
weather$yr <- as.numeric(as.character(weather$yr))
names(weather)<-c("yr", "T.WIN.m1", "P.WIN.m1", "T.SPRING.m1", "P.SPRING.m1", "T.SUMMER", "P.SUMMER", "T.FALL", "P.FALL")

colnames(weather)

# Surv (win + spring t+1)
tmp <- weather[, c("yr","T.WIN.m1", "P.WIN.m1", "T.SPRING.m1","P.SPRING.m1")]

tmp$yr <- tmp$yr-1
names(tmp)<-c("yr", "T.WIN", "P.WIN", "T.SPRING", "P.SPRING")
head(tmp)

#weather_surv<-weather[, c("yr","T.Win","P.Win","T.SPRING","P.SPRING")]
head(weather_surv)
weather_surv <- merge(weather,
                      tmp,
                      by.x = c("yr"), 
                      by.y = c("yr"))
weather_surv<-filter(weather_surv, yr>=2000)





#New column ageClass (0,1,2,37,8)

weather_surv$ageClass <- ifelse(weather_surv$age >= 8, 8, weather_surv$age)
c37 <- c(3:7)
weather_surv$ageClass <- ifelse(weather_surv$age %in% c37 , 37, weather_surv$ageClass)


# prepare surv data for models --------------------------------------------
weather_surv$yr<-as.factor(weather_surv$yr)
weather_surv$pred<-as.factor(weather_surv$pred)
weather_surv$alive_t1<-as.factor(weather_surv$alive_t1)

weather_surv$MassSpring<-as.numeric(as.character(weather_surv$MassSpring))
weather_surv$MassAutumn<-as.numeric(as.character(weather_surv$MassAutumn))

str(weather_surv)
names(weather_surv)
colnames(weather_surv)

weather_surv[c("MassSpring","MassAutumn","T.WIN.m1","P.WIN.m1","T.SPRING.m1" ,"P.SPRING.m1","T.WIN","P.WIN","T.SPRING" ,"P.SPRING", "T.SUMMER","P.SUMMER",
          "T.FALL","P.FALL")] <- 
  scale(weather_surv[c("MassSpring","MassAutumn","T.WIN.m1","P.WIN.m1","T.SPRING.m1" ,"P.SPRING.m1","T.WIN","P.WIN","T.SPRING" ,"P.SPRING", "T.SUMMER","P.SUMMER",
                  "T.FALL","P.FALL")]) 
names(weather_surv)
#weather_surv<-na.omit(weather_surv)
# 
head(weather_surv)


























# scale all data frames and leave unscaled dataframe ??  

# save as objects 



# CAREFUL DIFFERENT VALUES FROM CALCULATED BY HAND
#write.csv(df_surv, "surv_climate_data.csv", row.names = FALSE)
#drive_upload("surv_climate_data.csv", path = "OWPC/Analyses/data/surv_climate_data.csv", overwrite = T)



#write.csv(df_pheno_surv, "data_pheno_surv.csv", row.names = FALSE)
#drive_upload("data/df_pheno_surv.csv", path = "OWPC/Analyses/data/df_surv_pca.csv", overwrite = T)




