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
library(readxl)

getwd()
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data") # where to download
drive_download("OWPC/Analyses/data/Raw/pheno_ram.csv") # where to get file

View(pheno)

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

# survival pheno dataframe ------------------------------------------------------
sheep_data <- read_excel("sheep_data.xlsx")
pheno = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/pheno_surv2.csv",
                  na.string = c("", "NA"),sep = ",")


# first part to this df is not good anymore - select needed only
colnames(pheno)
pheno <- unique(pheno[, c("yr", "SummerNDVI","SummerEVI","SummerLAI",
                          "SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR","WinNDVI",
                          "WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET",
                          "WinFPAR")])

# survival - add time lag for winter 
pheno$yr <- as.numeric(as.character(pheno$yr))
colnames(pheno)

colnames(pheno) # here only winter season is a problem - summer is on same year than surv
tmp1 <- unique(pheno[, c("yr", "WinNDVI","WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET","WinFPAR")])
tmp1$yr <- tmp1$yr - 1 # on leur met l'année de la survie 
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
pheno_surv <- merge(pheno[, c("yr","SummerNDVI","SummerEVI","SummerLAI","SummerGPP","SummerSnow","SummerPSNNET", "SummerFPAR")],
                    tmp1,
                    by.x = c("yr"), 
                    by.y = c("yr"), 
                    all.x= T)
# merge dataframes 
colnames(pheno_surv)
colnames(sheep_data)
df_pheno_surv= merge(sheep_data[c("yr","ID", "alive_t1", "MassSpring","MassAutumn","age","pred", "first_yr_trans")],
                     pheno_surv,
                     by.x = "yr", 
                     by.y =  "yr", 
                     all.x=T) # keep all years even if NA

df_pheno_surv$yr<-as.factor(df_pheno_surv$yr)
df_pheno_surv$MassSpring<-as.numeric(df_pheno_surv$MassSpring)
df_pheno_surv$MassAutumn<-as.numeric(df_pheno_surv$MassAutumn)

df_pheno_surv$alive_t1<-as.factor(df_pheno_surv$alive_t1)
df_pheno_surv$pred<-as.factor(df_pheno_surv$pred)

#write.csv(df_pheno_surv, "surv_pheno_data.csv", row.names = FALSE)
#drive_upload("surv_pheno_data.csv", path = "OWPC/Analyses/data/surv_pheno_data.csv", overwrite = T)

# here add pca source to R code



# fecundity pheno dataframe -----------------------------------------------------
sheep_data <- read_excel("sheep_data.xlsx")
pheno = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/pheno_surv2.csv",
                  na.string = c("", "NA"),sep = ",")

# select needed only
colnames(pheno)
pheno <- unique(pheno[, c("yr", "SummerNDVI","SummerEVI","SummerLAI",
                          "SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR","WinNDVI",
                          "WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET",
                          "WinFPAR")])
pheno$yr <- as.numeric(as.character(pheno$yr))
colnames(pheno)

tmp1 <- unique(pheno[, c("yr", "SummerNDVI","SummerEVI","SummerLAI",
                         "SummerGPP","SummerSnow","SummerPSNNET","SummerFPAR")])

# add time lag for summer lengths (real time lag t-1)
tmp1$yr <- tmp1$yr + 1
tmp1 <- tmp1 %>% 
  rename(SummerNDVI_fec= SummerNDVI,
         SummerEVI_fec=SummerEVI, 
         SummerLAI_fec=SummerLAI,
         SummerGPP_fec=SummerGPP,
         SummerSnow_fec =SummerSnow,
         SummerPSNNET_fec =SummerPSNNET,
         SummerFPAR_fec =SummerFPAR)

pheno_fec <- merge(unique(pheno[, c("yr","WinNDVI","WinEVI","WinLAI","WinGPP","WinSnow","WinPSNNET","WinFPAR")]), # no need for all duplicated data per ID
                   tmp1,
                   by.x = c("yr"), 
                   by.y = c("yr"), 
                   all.x = T)

pheno_fec<- pheno_fec%>%
  rename(WinNDVI_fec = WinNDVI,
         WinEVI_fec =WinEVI ,
         WinLAI_fec = WinLAI ,
         WinGPP_fec = WinGPP,
         WinSnow_fec = WinSnow,
         WinPSNNET_fec = WinPSNNET,
         WinFPAR_fec = WinFPAR)

# merge dataframes 
df= merge(sheep_data[c("yr","ID", "raw_repro", "true_repro", "MassSpring","MassAutumn","age","pred", "first_yr_trans")],
          pheno_fec,
          by.x = "yr", 
          by.y =  "yr", 
          all.x=T)
#write.csv(df, "fecun_pheno_data.csv", row.names = FALSE)
#drive_upload("fecun_pheno_data.csv", path = "OWPC/Analyses/data/fecun_pheno_data.csv", overwrite = T)


# here add pca source to R code
getwd()
source("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/R/09_pca_pheno.R")



##### Calculating seasonal PDO and SOI values from monthly data ##########################

# Cleaning R
#rm(list=ls(all=TRUE))

# Set working directory
#setwd("C:/Users/Proprietaire/Documents/uni poc/Phd/OWPC/Analyses")

library(googledrive)

# Download database from google drive and save it in previously set working directory
drive_download("OWPC/Analyses/data/Climat/monthly_climate_ram",type = "csv",overwrite = T)
# Load database in R environment
data<-read.csv("monthly_climate_ram.csv")

# See values for December PDOs
data$PDO.DEC
# Create column with PDO values in december of the previous year
data$PDO.DEC.PREV.YR<-c(-1.63, 0.52, -0.93, 2.10, 0.33, -0.17, 0.20, 0.14, -0.58, -0.87, 0.08, -1.21, -1.79,-0.48, -0.41, 2.51, 1.01)
# Calculate mean PDO in winter
data$PDO.winter<-(data$PDO.DEC.PREV.YR+data$PDO.JAN+data$PDO.FEB+data$PDO.MAR)/4
# Calculate mean PDO in spring
data$PDO.spring<-(data$PDO.APR+data$PDO.MAY)/2
# Calculate mean PDO in summer
data$PDO.summer<-(data$PDO.JUN+data$PDO.JUL+data$PDO.AUG+data$PDO.SEP)/4
# Calculate mean PDO in fall
data$PDO.fall<-(data$PDO.OCT+data$PDO.NOV)/2

# See values for December SOIs
data$SOI.DEC
# Create column with SOI values in december of the previous year
data$SOI.DEC.PREV.YR<-c(0.79,0.77,-1.06,-1.30,0.92,-0.94,0.01,-0.39,1.49,1.43,-0.95,2.90,2.45,-0.77,-0.05,-0.66,-1.00)
# Calculate mean SOI in winter
data$SOI.winter<-(data$SOI.DEC.PREV.YR+data$SOI.JAN+data$SOI.FEB+data$SOI.MAR)/4
# Calculate mean SOI in spring
data$SOI.spring<-(data$SOI.APR+data$SOI.MAY)/2
# Calculate mean SOI in summer
data$SOI.summer<-(data$SOI.JUN+data$SOI.JUL+data$SOI.AUG+data$SOI.SEP)/4
# Calculate mean SOI in fall
data$SOI.fall<-(data$SOI.OCT+data$SOI.NOV)/2

# Create new dataframe with seasonal values of PDO and SOI
season_climate_ram<-data.frame(data$yr,data$PDO.winter,data$PDO.spring,data$PDO.summer,data$PDO.fall,data$SOI.winter,data$SOI.spring,data$SOI.summer,data$SOI.fall)
colnames(season_climate_ram)<-c("yr","PDO.winter","PDO.spring","PDO.summer","PDO.fall","SOI.winter","SOI.spring","SOI.summer","SOI.fall")

# Save new dataframe in a csv file and upload it to google drive
#write.csv(season_climat_ram,"season_climate_ram.csv",row.names=FALSE)
#drive_upload("season_climate_ram.csv",path = "OWPC/Analyses/data/Climat",name = "season_climate_ram")


# survival - climate dataframe ------------------------------------------------------

#write.csv(season_climat_ram,"season_climate_ram.csv",row.names=FALSE)
rm(list = ls())
sheep_data <- read_excel("sheep_data.xlsx")

drive_download("OWPC/Analyses/data/Raw/sheep_data.txt") # where to get file
sheep_data <- read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/sheep_data.txt", sep = "")

clim = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/raw/season_climate_ram.csv",
                 na.string = c("", "NA"),sep = ",")

# add time lag # careful this is tricky
clim$yr <- as.numeric(as.character(clim$yr))
colnames(clim)

# PDO/ENSO for survival to t1
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
clim_surv <- merge(clim[, c("yr","PDO.summer", "PDO.fall", "SOI.summer","SOI.fall")],
                   tmp,
                   by.x = c("yr"), 
                   by.y = c("yr"), 
                   all.x=T)
clim_surv <- clim_surv %>% 
  rename(PDO.summer_surv = PDO.summer,
         PDO.fall_surv=PDO.fall,
         SOI.summer_surv = SOI.summer,
         SOI.fall_surv = SOI.fall)
getwd()

# here add time lags for 1999 for survival models 
tmp$yr <- tmp$yr - 1
dim(tmp)
tmp <- tmp %>% 
  rename(PDO.winter_surv = PDO.winter,
         PDO.spring_surv =PDO.spring, 
         SOI.winter_surv =SOI.winter,
         SOI.spring_surv =SOI.spring)
head(tmp)

# merge dataframes 
colnames(clim_surv)
colnames(sheep_data)
df_surv= merge(sheep_data[c("yr","ID", "alive_t1", "MassSpring","MassAutumn","age","pred", "first_yr_trans")],
               clim_surv,
               by.x = "yr", 
               by.y =  "yr", 
               all.x=T) # keep all years even if NA


df_surv$yr<-as.factor(df_surv$yr)
df_surv$alive_t1<-as.factor(df_surv$alive_t1)

df_surv$MassSpring<-as.numeric(as.character(df_surv$MassSpring))
df_surv$MassAutumn<-as.numeric(as.character(df_surv$MassAutumn))

df_surv$PDO.winter_surv<-as.numeric(as.character(df_surv$PDO.winter_surv))
df_surv$PDO.summer_surv<-as.numeric(as.character(df_surv$PDO.summer_surv))
df_surv$PDO.spring_surv<-as.numeric(as.character(df_surv$PDO.spring_surv))
df_surv$PDO.fall_surv<-as.numeric(as.character(df_surv$PDO.fall_surv))
df_surv$SOI.winter_surv<-as.numeric(as.character(df_surv$SOI.winter_surv))
df_surv$SOI.summer_surv<-as.numeric(as.character(df_surv$SOI.summer_surv))
df_surv$SOI.spring_surv<-as.numeric(as.character(df_surv$SOI.spring_surv))
df_surv$SOI.fall_surv<-as.numeric(as.character(df_surv$SOI.fall_surv))

#write.csv(df_surv, "surv_climate_data.csv", row.names = FALSE)
#drive_upload("surv_climate_data.csv", path = "OWPC/Analyses/data/surv_climate_data.csv", overwrite = T)

# fecundity - climate dataframe  ---------------------------------------------------
getwd()
sheep_data <- read.csv2("sheep_data.txt", sep = "")
clim = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/raw/season_climate_ram.csv",
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
# merge dataframes 
colnames(clim_fec)
colnames(sheep_data)
df_fec= merge(sheep_data[c("yr","ID", "raw_repro", "true_repro", "MassSpring","MassAutumn","age","pred", "first_yr_trans")],
              clim_fec,
              by.x = "yr", 
              by.y =  "yr", 
              all.x=T) # keep all years even if NA

# some values have been added manually 

#write.csv(df_fec, "fecun_climate_data.csv", row.names = FALSE)
#drive_upload("fecun_climate_data.csv", path = "OWPC/Analyses/data/fecun_climate_data.csv", overwrite = T)

df_fec$yr<-as.factor(df_fec$yr)

df_fec$raw_repro<-as.factor(df_fec$raw_repro)
df_fec$true_repro<-as.factor(df_fec$true_repro)

df_fec$MassSpring<-as.numeric(as.character(df_fec$MassSpring))
df_fec$MassAutumn<-as.numeric(as.character(df_fec$MassAutumn))

df_fec$PDO.winter_fec<-as.numeric(as.character(df_fec$PDO.winter_fec))
df_fec$PDO.summer_fec<-as.numeric(as.character(df_fec$PDO.summer_fec))
df_fec$PDO.spring_fec<-as.numeric(as.character(df_fec$PDO.spring_fec))
df_fec$PDO.fall_fec<-as.numeric(as.character(df_fec$PDO.fall_fec))
df_fec$SOI.winter_fec<-as.numeric(as.character(df_fec$SOI.winter_fec))
df_fec$SOI.summer_fec<-as.numeric(as.character(df_fec$SOI.summer_fec))
df_fec$SOI.spring_fec<-as.numeric(as.character(df_fec$SOI.spring_fec))
df_fec$SOI.fall_fec<-as.numeric(as.character(df_fec$SOI.fall_fec))

df_fec<- df_fec[!is.na(df_fec$PDO.winter_fec),]
df_fec<- df_fec[!is.na(df_fec$MassSpring),]
df_fec<- df_fec[!is.na(df_fec$MassAutumn),]

