rm(list = ls())

library(googledrive)
setwd("~/uSherbrooke/Hiver 2020/NDVI/OWPC")


rm(list = ls())

drive_download("OWPC/Analyses/data/repro_mass.csv")
repro<-read.csv("repro_mass.csv", header=T, sep=",")


#get climate date 
drive_download("OWPC/Analyses/data/Climat/season_climate_ram")
  clim<-read.csv("season_climate_ram", header=T, sep=",")


# merge dataframes 
colnames(clim)
df= merge(unique(clim[, c("yr","PDO.winter", "PDO.spring", "PDO.summer", "PDO.fall","SOI.winter", "SOI.spring",
                          "SOI.summer","SOI.fall")]),
          repro,
          by.x = "yr", 
          by.y =  "yr")


df$yr<-as.factor(df$yr)
df$MassSpring<-as.numeric(as.character(df$MassSpring))
df$MassAutumn<-as.numeric(as.character(df$MassAutumn))
df$PDO.winter<-as.numeric(as.character(df$PDO.winter))
df$PDO.summer<-as.numeric(as.character(df$PDO.summer))
df$PDO.spring<-as.numeric(as.character(df$PDO.spring))
df$PDO.fall<-as.numeric(as.character(df$PDO.fall))
df$SOI.winter<-as.numeric(as.character(df$SOI.winter))
df$SOI.summer<-as.numeric(as.character(df$SOI.summer))
df$SOI.spring<-as.numeric(as.character(df$SOI.spring))
df$SOI.fall<-as.numeric(as.character(df$SOI.fall))
df$pred<-as.numeric(df$pred)
df$alive_t1<-as.numeric(df$alive_t1)
df$raw_repro<-as.numeric(df$raw_repro)
df$true_repro<-as.numeric(df$true_repro)

names(df)


# scale
df[c(2:9, 11:12)] <- scale(df[c(2:9, 11:12)])# CHANGE COLUMN NUMBER IF MODIFY DF!! 

dat<-df
  dat$yr<-NULL
  dat$ID<-NULL
  dat$age<-NULL
  #dat<-dat[, c("PDO.winter", "PDO.spring", "PDO.summer", "PDO.fall", "SOI.winter", "SOI.spring",
   #            "SOI.summer", "SOI.fall", "MassSpring", "MassAutumn")]


  for(j in 1: length(dat)) {

    for (i in 1:length(dat)) {
      a <- cor.test(dat[,j], dat[,i])
        print(paste(colnames(dat)[i], " est:", a$estimate, " p=value:", a$p.value))
    }
  }

install.packages("corrplot")



write.csv(mass, "Ram_mass.csv", row.names = FALSE)
drive_upload("Ram_mass.csv", path = "OWPC/Analyses/data/Ram_mass.csv", overwrite = T)


