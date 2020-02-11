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

# scale
df[c(2:9, 11:12)] <- scale(df[c(2:9, 11:12)])# CHANGE COLUMN NUMBER IF MODIFY DF!! 

# Removing year, ID, age
dat<-df
  dat$yr<-NULL
  dat$ID<-NULL
  dat$age<-NULL

# Correlation test
  C<-cor(dat, method="pearson", use="complete.obs")
  
 
# Draw correlation matrix
  library(corrplot)
  Colour <- colorRampPalette(c("blue", "white", "orangered"))(200)
  corrplot(C, insig="n", method="color", col=Colour, addgrid.col = "darkgray", cl.pos="r",
          tl.col="black", tl.cex=1, cl.cex=1, type="full", tl.pos="tl", bg="white", diag=TRUE)

  # Precision on correlation values
  symnum(C)
  
##### Seasonal climate trends ####################################################
library(googledrive)

#drive_download("OWPC/Analyses/data/Climat/season_climate_ram",type="csv",overwrite = T)
data<-read.csv("season_climate_ram.csv")
  
library(ggplot2)
library(cowplot)

# Theme object for ggplot graphs
fig <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), panel.background=element_blank()) + 
    theme(strip.background=element_blank(), strip.text.y = element_text()) + theme(legend.background=element_blank()) + 
    theme(legend.key=element_blank()) + theme(panel.border = element_blank())+ theme(legend.position = "none")+
    theme(axis.line = element_line(colour="black"))+theme(axis.text = element_text(size=12))+
    theme(axis.title = element_text(size=12))+theme(title = element_text(size=12))
  
# PDO trends
WinterPDO<-ggplot(data,aes(yr,PDO.winter)) + xlab("Year") + ylab("") + ggtitle("Winter PDO") + 
  scale_y_continuous(breaks=seq(-2,2,1),limits=c(-2,2.5)) + geom_line() + geom_point() + fig
SpringPDO<-ggplot(data,aes(yr,PDO.spring)) + xlab("Year") + ylab("") + ggtitle("Spring PDO") + 
  scale_y_continuous(breaks=seq(-2,2,1),limits=c(-2,2.5)) + geom_line() + geom_point() + fig
SummerPDO<-ggplot(data,aes(yr,PDO.summer)) + xlab("Year") + ylab("") + ggtitle("Summer PDO") + 
  scale_y_continuous(breaks=seq(-2,2,1),limits=c(-2,2.5)) + geom_line() + geom_point() + fig
FallPDO<-ggplot(data,aes(yr,PDO.fall)) + xlab("Year") + ylab("") + ggtitle("Fall PDO") + 
  scale_y_continuous(breaks=seq(-2,2,1),limits=c(-2,2.5)) + geom_line() + geom_point() + fig
PDO<-plot_grid(WinterPDO,SpringPDO,SummerPDO,FallPDO,ncol=1)
PDO
  
#SOI trends
WinterSOI<-ggplot(data,aes(yr,SOI.winter)) + xlab("Year") + ylab("") + ggtitle("Winter SOI") + 
  scale_y_continuous(breaks=seq(-2,2,1),limits=c(-2,2.5)) + geom_line() + geom_point() + fig
SpringSOI<-ggplot(data,aes(yr,SOI.spring)) + xlab("Year") + ylab("") + ggtitle("Spring SOI") + 
  scale_y_continuous(breaks=seq(-2,2,1),limits=c(-2,2.5)) + geom_line() + geom_point() + fig
SummerSOI<-ggplot(data,aes(yr,SOI.summer)) + xlab("Year") + ylab("") + ggtitle("Summer SOI") + 
  scale_y_continuous(breaks=seq(-2,2,1),limits=c(-2,2.5)) + geom_line() + geom_point() + fig
FallSOI<-ggplot(data,aes(yr,SOI.fall)) + xlab("Year") + ylab("") + ggtitle("Fall SOI") + 
  scale_y_continuous(breaks=seq(-2,2,1),limits=c(-2,2.5)) + geom_line() + geom_point() + fig
SOI<-plot_grid(WinterSOI,SpringSOI,SummerSOI,FallSOI,ncol=1)
SOI

# Combine PDO and SOI 
Clim<-plot_grid(PDO,SOI,ncol=2)
Clim

# Save ggplot to pdf
#ggsave2("C:/Users/Proprietaire/Documents/uni poc/Phd/OWPC/Analyses/Temp/Seasonal_climate_trends.pdf",
#        plot= Clim, device= cairo_pdf, scale = 0.8, width = 20, height = 30, units = "cm", dpi = 600)


##### 