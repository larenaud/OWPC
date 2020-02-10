getwd()
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
