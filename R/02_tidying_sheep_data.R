setwd("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC")
library(googledrive)
drive_find(pattern = "OWPC")
drive_ls()
drive_reveal("pheno_ram")
drive_download("~/OWPC/Analyses/data/pheno_ram.csv")
pheno=drive_get("~/OWPC/Analyses/data/pheno_ram.csv")
phene=


load("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/cache/surv_data_pop.RData")

head(females)

females = females[females$code.sr == "1",]
females$lamb_id = paste(females$ID, females$yr, sep = "-")

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

#write.csv(mass, "Ram_mass.csv", row.names = FALSE)
#drive_upload("Ram_mass.csv", path = "OWPC/Analyses/data/Ram_mass.csv", overwrite = T)
