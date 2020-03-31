# script for creating repro_mass.csv dataframe 1999-2018.
# Created by L. Renaud.
# translocated added manually by PO Cusson
# Updated March 5 2020. 


#=======
library(googledrive)
library(plyr)
library(dplyr)



# Set working directory
setwd("") # 
getwd()

# clean 
rm(list = ls())


# definition code.sr ------------------------------------------------------


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


# get necessary data ------------------------------------------------------

# my surv file with reproductive success codes 
surv <- read.csv2("SURV20170328.csv", sep = ",")

# THIS FILE MISSES 6 U FEMALES IN 2015 (TRANSLOCATED)
# added manually based on marco files "yrlgs - all" and "ewes - all"


surv <- read.csv("edited-2020-03-24.csv")
surv$sex[surv$sex == 'F'] <- "female"
colnames(surv)
surv <- surv[, c("yr","age","ID","sex","alive_t1","reproduced", "age.class", "code.sr", "pred")]

survF <- surv[surv$sex =="female",]
head(table(survF$yr, survF$code.sr))
survF<-survF[survF$yr>=1999,] # n = 641

survF$code.sr[survF$code.sr == '0œ'] <- "0"


# add missing or manual data  ---------------------------------------------

# get first year trans from old data 

trans <- read.delim("old/sheep_data.txt", sep = "")
# add status for 2015 - 2016 from old sheep data # unite two columns each having birthdates 

library(tidyr)
names(survF)
names(trans)
str(survF)
str(trans)

tmpTrans <-trans %>%
  filter(yr >=2015)
tmpSurv <-survF %>%
  filter(yr >=2015)

tmpSurv$ID <- droplevels(tmpSurv$ID)
tmpTrans$ID <- droplevels(tmpTrans$ID)
which(!tmpSurv$yr==tmpTrans$yr)

tmpTrans<- tmpTrans[-c(45, 46, 92:94),]
trans$ID <- droplevels(trans$ID)
tmpSurv<- tmpSurv[-35,]
tmpSurv$ID <- droplevels(tmpSurv$ID)


which(!tmpSurv$yr==tmpTrans$yr)

tmpTrans <- tmpTrans %>% 
  arrange(yr,ID)

tmpSurv <- tmpSurv %>% 
  arrange(yr, ID)

which(!tmpSurv$yr==tmpTrans$yr)

tmpSurv$alive_t1_2 <- coalesce(tmpSurv$alive_t1, tmpTrans$alive_t1)
tmpTrans$alive_t1_2 <- coalesce(tmpSurv$alive_t1, tmpTrans$alive_t1)
which(!tmpTrans$alive_t1==tmpTrans$alive_t1_2)

colnames(tmpSurv)
survF<-merge(survF, 
             tmpSurv[, c("yr", "ID", "alive_t1_2")], 
             by.x = c("yr", "ID"), 
             by.y= c("yr", "ID"),
             all.x =T)

survF$ID <- droplevels(survF$ID)
survF$alive_t1 <- coalesce(survF$alive_t1, survF$alive_t1_2)
which(!survF$alive_t1==survF$alive_t1_2)

survF = survF[, -10]

View(tmp)

survF<-merge(survF, 
             trans[, c("yr", "ID", "first_yr_trans")], 
             by.x = c("yr", "ID"), 
             by.y= c("yr", "ID"),
             all.x =T)

survF$first_yr_trans <- ifelse(is.na(survF$first_yr_trans),0,survF$first_yr_trans)

table(survF$yr, survF$first_yr_trans)# added missing u females
table(trans$yr, trans$first_yr_trans)


# add reproduction  -------------------------------------------------------

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


# add NEONATAL
# survF[survF$code.sr == 1, "neonatal"] <- 0 # neonatal: 0 = died
# survF[!survF$code.sr == 1, "neonatal"] <- 1#


# survF$neonatal <- NA
# survF$neonatal <- ifelse(survF$code.sr == 1, 0,survF$neonatal)
# 
# survF$neonatal <- ifelse(survF$code.sr %in% c(2:5, 9), 1, survF$neonatal)
# levels(survF$neonatal)


# OR 

survF  <- survF %>% 
  mutate(neonatal = case_when(
  code.sr == 1 ~ "0", 
  code.sr %in% c(2:5, 9) ~ "1"
#  TRUE ~ "NA"
)
) %>% 
  mutate(neonatal= as.factor(neonatal),
         yr = as.factor(yr), 
         alive_t1 = as.factor(alive_t1), 
         raw_repro = as.factor(raw_repro),
         true_repro = as.factor(true_repro),
         pred = as.factor(pred))


head(table(survF$yr, survF$neonatal))

survF$code.sr<-droplevels(survF$code.sr) 
survF$ID<-droplevels(survF$ID) 
summary(survF$code.sr)

#   0    1    2    3    4    5    8    9  NA's 
#  132   64   46   86    1  112   10    2  188 


colnames(survF)
survF<- survF[, c("yr","age","ID","alive_t1","raw_repro","true_repro", "pred", "neonatal", "first_yr_trans")]


#verif
head(table(survF$yr, survF$raw_repro))
head(table(survF$yr, survF$true_repro))
#ends verif



# add ghost lambs  --------------------------------------------------------

ghost <- survF[, c("yr","age","ID", "neonatal", "pred")]
ghost <- ghost[ghost$neonatal == "0",] # le statut de leur mère 

ghost$age = 0 # these are lambs 
ghost$ID = paste(ghost$yr, ghost$ID, sep = "-")


#"alive_t1"   "raw_repro"  "true_repro" "pred" 
ghost$alive_t1 = "FALSE"
ghost$raw_repro = 0
ghost$true_repro = 0
ghost$neonatal= NA
ghost$first_yr_trans= 0

ghost$raw_repro = as.factor(ghost$raw_repro )
ghost$true_repro = as.factor(ghost$true_repro )
ghost$pred = as.factor(ghost$pred)
ghost$alive_t1 = as.factor(ghost$alive_t1)
ghost$first_yr_trans = as.factor(ghost$first_yr_trans)

ghost$ID = as.factor(ghost$ID)
colnames(ghost)
str(ghost)

ghost <- ghost[, c("yr","age","ID","alive_t1","raw_repro","true_repro","pred", "neonatal", 'first_yr_trans')]
ghost = as.data.frame(ghost)
ghost$yr = as.integer(as.character(ghost$yr))
str(ghost)

ghost<-ghost %>% 
  filter(yr > 1998) # keep as many years as possible 
ghost = as.data.frame(ghost)

randomGhost <- ghost[sample(nrow(ghost), length(ghost$ID)/2), ]

survF <- rbind(survF,randomGhost)


survF$yr = as.factor(survF$yr)

survF <- survF %>% 
  group_by(yr, ID) %>% 
  droplevels() # n = 673
survF <- as.data.frame(survF)



# add sheep mass ----------------------------------------------------------

# day 12=June 5; Day 22=June 15; Day 144= Sept 15

#drive_download("OWPC/Analyses/data/adjWT2018.csv", overwrite=T)


getwd()
mass<-read.csv("adjWT2018.csv", header=T, sep=";")
mass<-mass[,c("yr","ID", "JJ", "age", "adjwt", "sex")]

mass<-filter(mass, yr>=1999) # added more years march 5 2020 LAR 
mass<-filter(mass, yr<=2018) # n = 726  to update ! 
mass<-filter(mass, sex=="female")
mass<-mass[,c("yr","ID", "JJ", "age", "adjwt")]

mjune<-filter(mass, JJ!=114)
mjune<-mjune[,c("yr","ID", "adjwt")]
names(mjune)<-c("yr", "ID", "MassSpring")

msep<-filter(mass, JJ==114)
msep<-msep[,c("yr","ID", "adjwt")]
names(msep)<-c("yr", "ID", "MassAutumn")

mass<-merge(mjune, msep, by=c("yr", "ID"), all.x=TRUE)

df1 <- merge(mass, 
               survF, 
               by.x = c("yr", "ID"),
               by.y = c("yr", "ID"), 
               all.y=T) # useless to keep up to 2018
survF = df1 # n  = 699


survF$ID <- droplevels(survF$ID) # 188 females 
survF$yr <- as.factor(survF$yr)

rm(ghost, df1, mass, mjune, msep, surv)



# verification with database ----------------------------------------------


# compare with ram mtn pop
library(readxl)
RamMtnpop <- read_excel("RamMtnpop.xls")

survF%>% 
  group_by(yr) %>% 
  filter(age == 0) %>%
  summarise(n())

tmp = RamMtnpop %>% 
  group_by(yr) %>%
  filter(yr >1998) %>%
  summarise(fem)

tmp1 = RamMtnpop %>% 
  group_by(yr) %>%
  filter(yr >1998) %>%
  summarise(`yrl f`)
# ends verification with ram pop





# overwrite  --------------------------------------------------------------


# write.csv(survF, "repro_mass.csv", row.names = FALSE)
# drive_upload("repro_mass.csv", path = "OWPC/Analyses/data/repro_mass.csv", overwrite = T)
