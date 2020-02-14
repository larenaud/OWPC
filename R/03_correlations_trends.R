rm(list = ls())

library(googledrive)
setwd("~/uSherbrooke/Hiver 2020/NDVI/OWPC")

drive_download("OWPC/Analyses/data/Raw/sheep_data", type="csv", overwrite=T)
repro<-read.csv("sheep_data.csv", header=T, sep=",")



# Temperatures and precipitations
  drive_download("OWPC/Analyses/data/Raw/Climat/monthlyRam", type="csv", overwrite=TRUE)
    Temp<-read.csv("monthlyRam", header = TRUE, sep=",")
      Temp<-Temp[, c("year", "month", "mean_temp", "total_precip")]
        names(Temp)<-c("yr", "month", "mean_temp", "total_precip")
          Temp<-filter(Temp, yr>=1999)
            Temp<-filter(Temp, yr<=2017)
    

  JAN<-filter(Temp, month==1)
    JAN<-JAN[, c("yr", "mean_temp", "total_precip")]
    names(JAN)<-c("yr", "mean_temp.JAN", "total_precip.JAN")
  FEV<-filter(Temp, month==2)
    FEV<-FEV[, c("mean_temp", "total_precip")]
    names(FEV)<-c("mean_temp.FEV", "total_precip.FEV")
  MAR<-filter(Temp, month==3)
    MAR<-MAR[, c("mean_temp", "total_precip")]
    names(MAR)<-c("mean_temp.MAR", "total_precip.MAR")
  APR<-filter(Temp, month==4)
    APR<-APR[, c("mean_temp", "total_precip")]
    names(APR)<-c("mean_temp.APR", "total_precip.APR")
  MAI<-filter(Temp, month==5)
    MAI<-MAI[, c("mean_temp", "total_precip")]
    names(MAI)<-c("mean_temp.MAI", "total_precip.MAI")
  JUN<-filter(Temp, month==6)
    JUN<-JUN[, c("mean_temp", "total_precip")]
    names(JUN)<-c("mean_temp.JUN", "total_precip.JUN")
  JUL<-filter(Temp, month==7)
    JUL<-JUL[, c("mean_temp", "total_precip")]
    names(JUL)<-c("mean_temp.JUL", "total_precip.JUL")
  AUG<-filter(Temp, month==8)
    AUG<-AUG[, c("mean_temp", "total_precip")]
    names(AUG)<-c("mean_temp.AUG", "total_precip.AUG")
  SEP<-filter(Temp, month==9)
    SEP<-SEP[, c("mean_temp", "total_precip")]
    names(SEP)<-c("mean_temp.SEP", "total_precip.SEP")
  OCT<-filter(Temp, month==10)
    OCT<-OCT[, c("mean_temp", "total_precip")]
    names(OCT)<-c("mean_temp.OCT", "total_precip.OCT")
  NOV<-filter(Temp, month==11)
    NOV<-NOV[, c("mean_temp", "total_precip")]
    names(NOV)<-c("mean_temp.NOV", "total_precip.NOV")
  DEC<-filter(Temp, month==12)
    DEC<-DEC[, c("mean_temp", "total_precip")]
    names(DEC)<-c("mean_temp.DEC", "total_precip.DEC")
  
  T<-cbind(JAN,FEV,MAR,APR,MAI,JUN,JUL,AUG,SEP,OCT,NOV,DEC)
  # Toujours le 15e jour du mois

  # T&P/season
      # Moving december to y-1 for winter
      D<-filter(Temp, month==12)
        D$yr<-as.numeric(D$yr)
          D$yr<-D$yr+1
      
      notd<-filter(Temp, month!=12)
        Met<-bind_rows(notd, D)

      
        
        # Winter : Dec - Mar
        Met
          Win<-Met[c(1:57,191:210),]
            Win<-Win[order(Win$yr),]
          
          Mean.W<-aggregate(Win[,3:4], list(Win$yr),mean)
          names(Mean.W)<-c("yr", "T.Win", "P.Win")
          Win
        # Spring : April - May
          
          Spring<-Met[c(58:95),]
            Spring<-Spring[order(Spring$yr),]
          
            Mean.S<-aggregate(Spring[,3:4], list(Spring$yr),mean)
            Mean.S<-Mean.S[, c("mean_temp", "total_precip")]
            names(Mean.S)<-c("T.SPRING", "P.SPRING")
            
        # Summer : Jun - Sep
            #Met # 86 153
            Summer<-Met[c(96:171),]
            Summer<-Summer[order(Summer$yr),]
            
            Mean.Sum<-aggregate(Summer[,3:4], list(Summer$yr),mean)
            Mean.Sum<-Mean.Sum[, c("mean_temp", "total_precip")]
            names(Mean.Sum)<-c("T.SUMMER", "P.SUMMER")
            
        # Autumn : Oct - Nov
            Met # 86 153
            Aut<-Met[c(172:190),]
            Aut<-Aut[order(Aut$yr),]
            
            Mean.Aut<-aggregate(Aut[,3:4], list(Aut$yr),mean)
            Mean.Aut<-Mean.Aut[, c("mean_temp", "total_precip")]
            names(Mean.Aut)<-c("T.AUT", "P.AUT")
            
        # Merge
            Met.tot<-cbind(Mean.W, Mean.S, Mean.Sum, Mean.Aut)
            
            write.csv(Met.tot, "Localweather_seasons.csv", row.names=F)
            drive_upload("Localweather_seasons.csv",path = "OWPC/Analyses/data/Raw/Climat",name = "Localweather_seasons", overwrite=T)
            
          

# merge dataframes for climat

clim$yr<-as.factor(clim$yr)
  repro$yr<-as.factor(repro$yr)
    df= merge(unique(clim[, c("yr","PDO.winter", "PDO.spring", "PDO.summer", "PDO.fall","SOI.winter", "SOI.spring",
                          "SOI.summer","SOI.fall")]), repro, by.x = "yr", by.y =  "yr")
   # T$yr<-as.factor(T$yr)
    #  df<-merge(df, T, by="yr", all.x=TRUE) For monthly weather
    Met.tot$yr<-as.factor(Met.tot$yr)
    data.clim<-merge(clim, Met.tot, by="yr")
    data.clim$PDO.SOI.WIN<-data.clim$PDO.winter+data.clim$SOI.winter
    data.clim$PDO.SOI.SPRING<-data.clim$PDO.spring+data.clim$SOI.spring
    data.clim$PDO.SOI.SUMMER<-data.clim$PDO.summer+data.clim$SOI.summer
    data.clim$PDO.SOI.AUT<-data.clim$PDO.fall+data.clim$SOI.fall


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
#df[c(2:9, 11:12)] <- scale(df[c(2:9, 11:12)])# CHANGE COLUMN NUMBER IF MODIFY DF!! 

# Removing year, ID, age
dat<-df
  dat$yr<-NULL
  dat$ID<-NULL
  dat$age<-NULL
  dat$alive_t1<-NULL
  dat$pred<-NULL
  dat$true_repro<-NULL
  dat$first_yr_trans<-NULL

  names(df)
  

#### Correlation tests for climat ####
  
    C<-cor(dat, method="pearson", use="complete.obs")
  write.table(C, file = "Correlations_Pheno.txt", sep = ",", quote = FALSE)
 
# Draw correlation matrix
  library(corrplot)
  par(mfrow=c(1,1))
  x11()
  Colour <- colorRampPalette(c("blue", "white", "orangered"))(200)
  corrplot(C, insig="n", method="color", col=Colour, addgrid.col = "darkgray", cl.pos="r",
          tl.col="black", tl.cex=1, cl.cex=1, type="full", tl.pos="tl", bg="white", diag=TRUE)

  # Precision on correlation values
  symnum(C)
  
  # Climate with weather by seasons
  
  data.clim$yr<-NULL
  C.season<-cor(data.clim, method="pearson", use="complete.obs")
  par(mfrow=c(1,1))
  x11()
  Colour <- colorRampPalette(c("blue", "white", "orangered"))(200)
  corrplot(C.season, insig="n", method="color", col=Colour, addgrid.col = "darkgray", cl.pos="r",
           tl.col="black", tl.cex=1, cl.cex=1, type="full", tl.pos="tl", bg="white", diag=TRUE)
  
  
<<<<<<< HEAD
  # moyennes correlations : SOI winter + PDO (0.6-0.8); Mass et repro (0.6-0.8)
  # Forte correlation : MS & MA (0.95)
  
=======
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

#### Correlations for pheno ####


drive_download("OWPC/Analyses/data/Raw/pheno_surv2.csv", overwrite=TRUE)
pheno<-read.csv("pheno_surv2.csv", header=TRUE, sep=",")



pheno<-pheno[,c("SummerNDVI", "SummerEVI", "SummerLAI", "SummerGPP", "SummerSnow",
                "SummerPSNNET", "SummerFPAR", "WinNDVI", "WinEVI", "WinLAI", "WinGPP",
                "WinSnow", "WinPSNNET", "WinFPAR")]
  Cpheno<-cor(pheno, method = "pearson", use="complete.obs")
 
  par(mfrow=c(1,1))
  x11()
  Colour <- colorRampPalette(c("blue", "white", "orangered"))(200)
  corrplot(Cpheno, insig="n", method="color", col=Colour, addgrid.col = "darkgray", cl.pos="r",
           tl.col="black", tl.cex=1, cl.cex=1, type="full", tl.pos="tl", bg="white", diag=TRUE)
  
  
  
##### 
>>>>>>> fc0f155dd47abdd01e1d1ec9b00a923fab2eca98
