<<<<<<< HEAD

pheno<-drive_get("~/OWPC/Analyses/data/pheno_ram.csv")
View(pheno)
=======
# Tidying data pheno #
library(googledrive)


pheno <- drive_find("pheno_ram.csv")
drive_reveal("pheno_ram","path")
drive_download("OWPC/Analyses/data/pheno_ram.csv")
pheno <- read.csv2("/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/pheno_ram.csv", sep = ",")


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






>>>>>>> 5855572c1b719f96e9bf0a7eac0ba8a4bb789b43
