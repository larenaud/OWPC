# Load dfweather.Rdata

# Surv

mod.surv <- list()

mod.surv$base <- glm(alive_t1 ~ -1 + ageClass +  pred, 
                  data=df_surv, 
                  family="binomial") 

mod.surv$T.Spring <- glm(alive_t1 ~ -1 + ageClass/T.SPRING +  pred, 
                      data=df_surv, 
                      family="binomial")

mod.surv$T.Fall <- glm(alive_t1 ~ -1 + ageClass/T.FALL +  pred, 
                    data=df_surv, 
                    family="binomial")

# Raw repro
mod.raw <- list()
mod.raw$P.T.WIN.m1 <-glmer(raw_repro ~ -1 + ageClass/P.WIN.m1 +ageClass/T.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000)))

# True repro
mod.true <- list()
mod.true$P.T.WIN.m1 <-glmer(true_repro ~ -1 + ageClass/P.WIN.m1 +ageClass/T.WIN.m1 +  MassAutumn_tm1 + (1|ID), # here write the model
                         data=df_fec, 
                         family="binomial",
                         control = glmerControl(optimizer="bobyqa", 
                                                optCtrl = list(maxfun = 2000000)))