#THis script has the code to calculate predictions from simple and full models
#but plots only from the full models now
#when testing just free plants the two models are the same

#simple model plots moved to end

#source functions and packages
source("functions and packages/startscripts.R")
require(lme4)
require(lmerTest)
library(effects)
source("functions and packages/rsquared_glmm.R")
#runthe new r2 function to get r2 and p values for the model


photo_chem <- read.csv("calculated data/Amax_chem.csv")
#run volume format func
photo_chem<- vollab_func(photo_chem)

#run stats on pots only

photo_chem_nofree <- subset(photo_chem, volume != "1000")
  photo_chem_nofree <- droplevels(photo_chem_nofree)

#create models and extract coefs 


#model and stats
Afit_full <- lmer(A_mass ~ Nmass_notnc+starch+Nmass_notnc:starch + (1|ID), data=photo_chem_nofree)
summary(Afit_full)
  Afit_N <- lmer(A_mass ~ Nmass_notnc + (1|ID), data=photo_chem_nofree)
  Afit_TNC <- lmer(A_mass ~ starch + (1|ID), data=photo_chem_nofree)


rsquared.glmm(list(Afit_full, Afit_N, Afit_TNC))

plot(effect("Nmass_notnc:starch", Afit_full), multiline=TRUE)

require(car)
Anova(Afit_full)
visreg(Afit_full, "starch", by="Nmass_notnc", overlay=TRUE)

#model parameters
f <- fixef(Afit_full)


#plot predicted and observed in bins------------------------------------------------------

#make bin levels and mid points for both starch and nitrogen
starchbin <- c(0, 0.04, 0.08, 0.12, 0.16, 100)
starchmid <- c(0.02, 0.06, 0.1, 0.14, 0.2)

nitrobin <- c(0, 0.0025, 0.005, 0.0075, 0.01, 100)
nitromid <- c(0.00125, 0.00375, 0.00625, 0.00875, 0.0125)

#set bins in raw dfr
photo_chem_nofree$starchbin <- cut(photo_chem_nofree$starch, breaks = starchbin)
photo_chem_nofree$nitrobin <- cut(photo_chem_nofree$Nmass_notnc, breaks = nitrobin)

tnc_predict<- expand.grid(starch = c(0.02, 0.06, 0.1, 0.14, 0.2),
                    Nmass_notnc = seq(0.0007432, 0.0175502, length=25))
tnc_predict$Amass_pred <- predict(Afit_full, tnc_predict, re.form=NA)




#plot and predict (2 graphs)---------------------------------------------------------------------
cols <- c("green3", "cyan4", "#0081FFFF", "darkviolet", "red")
#cols <- c("forestgreen", "#80FE1AFF","#ECFD08FF","#FF7500FF","#FF3300FF")
#coln <- c("#FF3300FF", "#FF7500FF","#ECFD08FF",  "#80FE1AFF", "forestgreen" )
coln <- c("red", "darkviolet", "#0081FFFF", "cyan4", "green3")
binlab <- c("0-4", "4-8", "8-12", "12-16", ">16")
binlab2 <- c(" 0    -.001", ".001-.003", ".003-.006", ".006-.008", ".008-.012")

pchbin <- c(1,2,3,4,5)
legpch <- c(5, 10, 15, 20, 25, 35, "free")


# ###using predict instead of eqautions####




