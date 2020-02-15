# FINAL SELECTED MODELS
mod.final <- list()

# RAW REPRODUCTION

modfinal$combined <- glmer(raw_repro ~ -1 + ageClass/PDO.spring + ageClass/T.WIN.m1 + ageClass/P.WIN.m1 + MassAutumn_tm1 + (1|ID), 
                          data=df, 
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 2000000)))

# TRUE REPRODUCTION

mod.final$clim.win <- glmer(true_repro ~ -1 + ageClass/PDO.winter + ageClass/SOI.winter + MassAutumn_tm1 + (1|ID), 
                           data=df, 
                           family="binomial",
                           control = glmerControl(optimizer="bobyqa", 
                                                  optCtrl = list(maxfun = 2000000))) 