##Prediction for matrices 



##### fct GetLambda ####


GetLambda <- function(){
  
  L <- matrix(nrow=9,ncol=9,data=0)
  
  L[1,4] = 
  L[1,5] = 
  L[1,6] = 
  L[1,7] = 
  L[1,8] = 
  L[1,9] = 
  
  L[2,1] =
  L[3,2] = 
  L[4,3] = 
  L[5,4] = 
  L[6,5] = 
  L[7,6] = 
  L[8,7] =
  L[9,8] = 
  L[9,9] = 
    
  return(c(eigen.analysis(L)$lambda1,eigen.analysis(L)$stable.stage))
  
}


##### Range variables ####

library(readr)
TP <- read_csv("data/Localweather_seasons.csv")
pheno <- read_csv("data/pheno_by_yr.csv")
library(readxl)
PS <- read_excel("data/season_climate_ram.xlsx")



allVari <- merge(TP,pheno, by.x = "yr", by.y = "year") 
allVari <- merge(allVari, PS, by.x = "yr", by.y = "yr")
names(allVari)
allVari <- allVari[,-c(10:23)]

names(allVari)

rangeVari <- matrix(nrow = 3, ncol = 31)
n <- names(allVari)
n[1] <- "type"
colnames(rangeVari) <-n
rangeVari <- as.data.frame(rangeVari)
rangeVari$type<- c("min","mean","max")

names(rangeVari)

for(i in 2:31){
  
  x <-  allVari[,i]
  
  x <- as.numeric(as.character(x))
  
  min <- min(x,na.rm = T)
  rangeVari[1,i] <- min
  
  mean <- mean(x, na.rm =T)
  rangeVari[2,i] <- mean
  mean
  
  max <- max(x, na.rm = T)
  rangeVari[3,i] <- max
  
}

#write.csv(rangeVari, file = "/Users/Sandrine/Documents/Sherbrooke/OWPC/Analyse/OWPC/data/range_all_variables.csv")


surv_tmp <- ggplot(test, aes(T.WIN.m1, y=lambda, group = 1)) + 
  geom_line(linetype = "solid") + # only have one age class
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, fill = 'navyblue') +  
  #geom_point(data = df_surv, aes(x = T.WIN.m1, y = as.numeric(alive_t1)-1)) + # pour mettre la distribution des points brutes 
  labs(x=expression('Winter temperature (std)' [t-1]), 
       y="Population growth rate based on true reproduction") +
  theme_pander() 
surv_tmp

