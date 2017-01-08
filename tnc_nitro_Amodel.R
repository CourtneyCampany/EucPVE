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

#plot predicted and observed in bins------------------------------------------------------

#make bin levels and mid points for both starch and nitrogen
#model and stats
Afit_full <- lmer(A_mass ~ nitro_mg+starch_perc+nitro_mg:starch_perc + (1|ID), data=photo_chem)
#####model parameters for plotting
f <- fixef(Afit_full)
coef(Afit_full)

anova(Afit_full)
