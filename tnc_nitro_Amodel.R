#this script is for sourcing the Amax vs TNC and N plots into master
source("functions and packages/startscripts.R")
library(lme4)
library(lmerTest)
library(effects)


photo_chem <- read.csv("calculated data/Amax_chem.csv")
#run volume format func
photo_chem<- vollab_func(photo_chem)
#get better units for plotting
photo_chem$starch_perc <- photo_chem$starch*100
photo_chem$nitro_mg <- photo_chem$Nmass_notnc *1000


#model and stats
Afit_full <- lmer(A_mass ~ nitro_mg+starch_perc+nitro_mg:starch_perc + (1|ID), data=photo_chem)
#####model parameters for plotting
f <- fixef(Afit_full)
coef(Afit_full)

anova(Afit_full)
summary(Afit_full)


#nitro only
Afit_nitro <- lmer(A_mass ~ nitro_mg + (1|ID), data=photo_chem)
coef(Afit_nitro)
anova(Afit_nitro)
summary(Afit_nitro)

Afit_tnc <- lmer(A_mass ~ starch_perc + (1|ID), data=photo_chem)
coef(Afit_tnc)
anova(Afit_tnc)
summary(Afit_tnc)

##compare R2 values to test contribution of each vs whole model


