#source functions and packages
source("functions and packages/functions.R")
source("functions and packages/stats packages.R")
source("functions and packages/plot objects.R")

require(car)
require(lme4)
library(lmerTest)
library(lattice)
require(effects)
library(LMERConvenienceFunctions)

#lme with repeated dates (nlme package), lmer with nonlinear distrutions

photo_chem <- read.csv("calculated data/Amax_chem.csv")
#run volume format func
photo_chem<- vollab_func(photo_chem)
photo_chem$plot <- as.factor(gsub("-[0-9]", "", photo_chem$ID))


Amass_full <- lmer(A_mass ~ starch+Nmass_notnc+starch:Nmass_notnc + (1|ID), data=photo_chem)
anova(Amass_full)
mcp.fnc(Amass_full)
alleff <- allEffects(Amass_full)
plot(alleff[[1]])
plot(alleff[[2]])

AmassN1 <- lmer(A_mass~ Nmass_notnc + (1|ID), data=photo_chem) #N as a fixed effect, random intercept only
AmassN2 <- lmer(A_mass~ Nmass_notnc + (Nmass_notnc|ID), data=photo_chem) #random slope effect
AmassN3 <- lmer(A_mass~ Nmass_notnc + (Nmass_notnc|plot/ID), data=photo_chem) #plot random effect
anova(AmassN1,AmassN2)

dotplot(ranef(AmassN2))

#simple models first, #visulaise with visreg/effects pacakges
#model3 (adds campaign as fixed effect)
Amass_simple <- lm(A_mass~ starch*Nmass_notnc, data=photo_chem)
anova(Amass_simple)
summary(Amass_simple)
visreg(Amass_simple)


AmassN <- lm(A_mass~ Nmass_notnc, data=photo_chem)
anova(AmassN)
summary(AmassN)
visreg(AmassN)
AmassN2 <- lm(A_mass~ Nmass_notnc*volume, data=photo_chem)
visreg(AmassN2, xvar="Nmass_notnc", by="volume", overlay=TRUE)

AmassS <- lm(A_mass~ starch, data=photo_chem)
anova(AmassS)
summary(AmassS)
visreg(AmassS)
AmassS2 <- lm(A_mass~ starch*volume, data=photo_chem)
visreg(AmassS2, xvar="starch", by="volume", overlay=TRUE)