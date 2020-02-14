
#load("C:/Users/joani/Documents/PhD/Labo/One Week Paper Challenge 2020/test_for_loop.Rdata")

# add AIC
# tidy up initial dataframe 

#surv_climate_data
#fecun_climate_data
#df_fec_pca
#df_surv_pca
library(lme4)

head(df_surv)
colnames(df_surv)
variables <- c("PDO.summer_surv", "PDO.fall_surv","SOI.summer_surv", "SOI.fall_surv",
               "PDO.winter_surv","PDO.spring_surv", "SOI.winter_surv" ,"SOI.spring_surv", 
               "PDOSOI_winter",   "PDOSOI_spring" ,  "PDOSOI_summer" ,  "PDOSOI_fall" )

desired_length <- 5 # or whatever length you want
empty_list <- vector(mode = "list", length = desired_length)


for(j in 1:length(levels(df_surv$ageClass))){
 tmp <- subset(df_surv, df_surv$ageClass == levels(df_surv$ageClass)[j]) 

coefs <- data.frame(
  var = variables,
  coef = c(1:length(variables)),
  ageClass = rep(df_surv$ageClass[j],length(variables)),
  p_value = c(1:length(variables))
)


for(i in 1:length(variables)){
mod1 <- glm(alive_t1 ~ MassAutumn + pred + tmp[,variables[i]], # here write the model
              data=tmp, 
              family="binomial")#,
              #control = glmerControl(optimizer="bobyqa", 
                                     #optCtrl = list(maxfun = 100000))) 

summary(mod1)
summary(mod1)$coef[2,"Estimate"]
coefs[i,"coef"] <-  summary(mod1)$coef[2,"Estimate"]
coefs[i,"p-value"] <-  summary(mod1)$coef[2,"Pr(>|z|)"]
### ajouter des trucs
empty_list[[j]] <- coefs
}
}

data_final <- rbind(empty_list[[1]], empty_list[[2]], empty_list[[3]], empty_list[[4]], empty_list[[5]])
head(data_final)

data_final$ageClass <- rep(unique(df_surv$ageClass), each=12)
data_final

