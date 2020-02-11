<<<<<<< HEAD
library(googledrive)
# Set working directory
setwd("~/uSherbrooke/Hiver 2020/NDVI/OWPC")

=======
setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC")
library(googledrive)
library(plyr)
library(dplyr)
rm(list = ls())
# drive_find(pattern = "OWPC")
# drive_ls()
# drive_reveal("pheno_ram")
# drive_download("~/OWPC/Analyses/data/pheno_ram.csv")
# pheno=drive_get("~/OWPC/Analyses/data/pheno_ram.csv")

# reproductive success code
#0 – no evidence of lactation; 
#1 – neonatal mortality (evidence of lactation but no lamb seen); 
#2 – summer mortality (between birth and late September);
#3 – survival to weaning but overwinter mortality; 
#4 – survival to weaning, overwinter survival unknown;
#5 – survival to 1 year
#8- contraceptive implant (1996 for example)
#9- lamb never seen after capture, potential cause= capture

# RAW reproduction 
# if RS was 0 : got new code 0
# if RS was 1 and + : got new code 1

# VIABLE REPRODUCTION
# if RS was 0 or 1 (lamb not seen) : got new code 0 
# if RS was 2 and + : got new code 1 

# my surv file with reproductive success codes s
surv <- read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/SURV20170328.csv", sep = ",")

colnames(surv)
surv <- surv[, c("yr","age","ID","sex","alive_t1","reproduced", "age.class", "code.sr", "pred")]

survF <- surv[surv$sex =="female",]

# RAW reproduction 
# if RS was 0 : got new code 0 (failed to reproduce)
# if RS was 1 and + : got new code 1 (tried to reproduce)
survF$raw_repro <- 0
survF$raw_repro <- ifelse(!survF$code.sr == 0 & !survF$code.sr == 8,1,survF$raw_repro)

# VIABLE REPRODUCTION
# if RS was 0 or 1 (lamb not seen) : got new code 0 
# if RS was 2 and + : got new code 1 
survF$true_repro <- 0
survF$true_repro <- ifelse(!survF$code.sr == 0 & !survF$code.sr == 1 & !survF$code.sr == 8,1,survF$true_repro)

# lambs do NOT reproduce
survF$raw_repro<- ifelse(survF$age == 0 | survF$age == 1 & is.na(survF$reproduced),0,survF$raw_repro)
survF$true_repro<- ifelse(survF$age == 0 | survF$age == 1 & is.na(survF$reproduced),0,survF$true_repro)

# females = females[females$code.sr == "1",]
# females$lamb_id = paste(females$ID, females$yr, sep = "-")
colnames(survF)
survF<- survF[, c("yr","age","ID","alive_t1","raw_repro","true_repro", "pred")]

survF$alive_t1 <- as.factor(survF$alive_t1)
survF$raw_repro <- as.factor(survF$raw_repro)
survF$true_repro <- as.factor(survF$true_repro)
survF$pred <- as.factor(survF$pred)

survF<-survF %>%
  group_by(yr, ID, age) 

survF<-survF[survF$yr>=2000,] # n = 581

# add ghost lambs 
>>>>>>> fef7ba03d31368a574633c2fa7e7e52be9d2afed
getwd()
load("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/cache/surv_data_pop.RData")
names(survF)
names(females)

ghost <- females[, c("yr","age","ID", "neonatal", "pred")]
ghost <- ghost[ghost$neonatal == 0,]
ghost$age =0
ghost$ID = paste(ghost$yr, ghost$ID, sep = "-")

#"alive_t1"   "raw_repro"  "true_repro" "pred" 
ghost$alive_t1 = "FALSE"
ghost$raw_repro = 0
ghost$true_repro = 0

ghost$raw_repro = as.factor(ghost$raw_repro )
ghost$true_repro = as.factor(ghost$true_repro )
ghost$pred = as.factor(ghost$pred)
ghost$alive_t1 = as.factor(ghost$alive_t1)
ghost$ID = as.factor(ghost$ID)
colnames(ghost)
str(ghost)

ghost <- ghost[, c("yr","age","ID","alive_t1","raw_repro","true_repro","pred")]
ghost = as.data.frame(ghost)
ghost$yr = as.integer(as.character(ghost$yr))
str(ghost)

ghost<-ghost %>% 
  filter(yr > 1999)
ghost = as.data.frame(ghost)

colnames(survF)
colnames(ghost)

str(survF)

survF$yr = as.factor(survF$yr)
survF <- as.data.frame(survF)

# add lamb ghosts to current females 
survF<-rbind(survF, ghost) # 639
str(survF)
survF$yr = as.factor(survF$yr)

survF <- survF %>% 
  group_by(yr, ID) %>% 
  droplevels()

#### Sheep mass ####

# day 12=June 5; Day 22=June 15; Day 144= Sept 15

drive_download("OWPC/Analyses/data/adjWT2018.csv", overwrite=T)
getwd()
mass<-read.csv("adjWT2018.csv", header=T, sep=";")
mass<-mass[,c("yr","ID", "JJ", "age", "adjwt", "sex")]

mass<-filter(mass, yr>=2000)
mass<-filter(mass, yr<=2016)
mass<-filter(mass, sex=="female")
mass<-mass[,c("yr","ID", "JJ", "age", "adjwt")]

mjune<-filter(mass, JJ!=114)
mjune<-mjune[,c("yr","ID", "adjwt")]
names(mjune)<-c("yr", "ID", "MassSpring")

msep<-filter(mass, JJ==114)
msep<-msep[,c("yr","ID", "adjwt")]
names(msep)<-c("yr", "ID", "MassAutumn")

mass<-merge(mjune, msep, by=c("yr", "ID"), all.x=TRUE)

#write.csv(mass, "Ram_mass.csv", row.names = FALSE)
#drive_upload("Ram_mass.csv", path = "OWPC/Analyses/data/Ram_mass.csv", overwrite = T)

setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data")
drive_download("OWPC/Analyses/data/Ram_mass.csv", overwrite=T)

mass <- read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/Ram_mass.csv", sep=",")
#mass <- read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/pheno_surv2.csv", sep=",")

df1 <- merge(mass, 
               survF, 
               by.x = c("yr", "ID"),
               by.y = c("yr", "ID"), 
               all.y=T) # garder juste le nombre de lignes de mass
survF = df1

write.csv(survF, "repro_mass.csv", row.names = FALSE)
drive_upload("repro_mass.csv", path = "OWPC/Analyses/data/repro_mass.csv", overwrite = T)




<<<<<<< HEAD
# merging to existing dataframe # would be nice to have a RData and not tons of .csv

pheno_surv = read.csv2("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/data/phenoSurv.csv",
                       na.string = c("", "NA"),sep = ",")
# do we consider they reproduced ? no

#perharps a rbind is better AT THE very beginning 
tmp= merge(pheno_surv,
           females[, c("ID", "yr", "lamb_id")],
           by.x = c("ID", "yr"), 
           by.y = c("ID", "yr"), 
           all.y = T) # keep all ghosts
#=======

#### Sheep mass ####

# day 12=June 5; Day 22=June 15; Day 144= Sept 15

library(dplyr)

drive_download("OWPC/Analyses/data/adjWT2018.csv", overwrite=T)

  mass<-read.csv("adjWT2018.csv", header=T, sep=";")
    mass<-mass[,c("yr","ID", "JJ", "age", "adjwt", "sex")]

    mass<-filter(mass, yr>=2000)
      mass<-filter(mass, yr<=2016)
        mass<-filter(mass, sex=="female")
          mass<-mass[,c("yr","ID", "JJ", "age", "adjwt")]

    mjune<-filter(mass, JJ!=114)
      mjune<-mjune[,c("yr","ID", "adjwt")]
        names(mjune)<-c("yr", "ID", "MassSpring")

    msep<-filter(mass, JJ==114)
      msep<-msep[,c("yr","ID", "adjwt")]
        names(msep)<-c("yr", "ID", "MassAutumn")

    mass<-merge(mjune, msep, by=c("yr", "ID"), all.x=TRUE)
    
  write.csv(mass, "Ram_mass.csv", row.names = FALSE)
  drive_upload("Ram_mass.csv", path = "OWPC/Analyses/data/Ram_mass.csv", overwrite = T)
=======

>>>>>>> fef7ba03d31368a574633c2fa7e7e52be9d2afed
