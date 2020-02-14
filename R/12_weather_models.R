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
colnames(weather)

tmp <- weather[, c("yr","T.SUMMER", "P.SUMMER", "T.AUT","P.AUT")]

tmp$yr <- tmp$yr+1

head(tmp)

weather_fec<-weather[, c("yr","T.Win","P.Win","T.SPRING","P.SPRING")]
head(weather_fec)
weather_fec <- merge(weather_fec,
                  tmp,
                  by.x = c("yr"), 
                  by.y = c("yr"))
head(weather_fec)


  # Sheep surv data


# merge dataframes 
colnames(clim_fec)
colnames(sheep_data)
df_fec= merge(sheep,
              weather_fec,
              by.x = "yr", 
              by.y =  "yr", 
              all.x=T) # keep all years even if NA

df_surv<-merge(sheep,
               weather,
               by.x = "yr", 
               by.y =  "yr", 
               all.x=T) # keep all years even if NA