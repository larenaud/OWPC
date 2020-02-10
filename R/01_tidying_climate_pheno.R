
getwd()
load("/Users/LimoilouARenaud/Documents/PhD/Analyses/OWPC/OWPC/cache/surv_data_pop.RData")

head(females)

library(plyr)
library(dplyr)

females = females[females$code.sr == "1",]
females$lamb_id = paste(females$ID, females$yr, sep = "-")

x=2
