library(googledrive)
setwd("~/uSherbrooke/Hiver 2020/NDVI/OWPC")
drive_download("OWPC/Analyses/data/pheno_ram2.csv", overwrite=T)


pheno<-read.csv("pheno_ram2.csv", header=T, sep=",")
mass<-mass[,c("yr","ID", "JJ", "age", "adjwt", "sex")]

write.csv(mass, "Ram_mass.csv", row.names = FALSE)
drive_upload("Ram_mass.csv", path = "OWPC/Analyses/data/Ram_mass.csv", overwrite = T)


