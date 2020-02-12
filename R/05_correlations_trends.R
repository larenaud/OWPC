rm(list = ls())

library(googledrive)
setwd("~/uSherbrooke/Hiver 2020/NDVI/OWPC")

drive_download("OWPC/Analyses/data/Raw/sheep_data", type="csv", overwrite=T)
repro<-read.csv("sheep_data.csv", header=T, sep=",")


#get climate date 
drive_download("OWPC/Analyses/data/Raw/Climat/season_climate_ram", type="csv", overwrite = T)
  clim<-read.csv("season_climate_ram.csv", header=T, sep=",")


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

# scale
df[c(2:9, 11:12)] <- scale(df[c(2:9, 11:12)])# CHANGE COLUMN NUMBER IF MODIFY DF!! 

# Removing year, ID, age
dat<-df
  dat$yr<-NULL
  dat$ID<-NULL
  dat$age<-NULL

# Correlation test
  C<-cor(dat, method="pearson", use="complete.obs")
  
  write.table(C, file = "Correlations_Pheno.txt", sep = ",", quote = FALSE)
 
# Draw correlation matrix
  library(corrplot)
  Colour <- colorRampPalette(c("blue", "white", "orangered"))(200)
  corrplot(C, insig="n", method="color", col=Colour, addgrid.col = "darkgray", cl.pos="r",
          tl.col="black", tl.cex=1, cl.cex=1, type="full", tl.pos="tl", bg="white", diag=TRUE)

  # Precision on correlation values
  symnum(C)
  
  # moyennes correlations : SOI winter + PDO (0.6-0.8); Mass et repro (0.6-0.8)
  # Forte correlation : MS & MA (0.95)
  