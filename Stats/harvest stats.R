source("functions and packages/load packages.R")
source("functions and packages/functions.R")

#read data
seedlingmass <- read.csv("calculated data/seedling mass.csv")
seedlingmass$block <- as.factor(gsub("-[1-9]", "", seedlingmass$ID))

#treatment means for total mass and ratios
finalmass <- subset(seedlingmass, select = c("ID", "volume", "block", "totalmass", "rootshoot", "frootleaf"))
mass_agg <- summaryBy( .~ volume , data = finalmass,  FUN=c(mean,se))

##Stats for harvest mass--------------------------------------------------------------------------------------
require(nlme)
require(visreg)
library(multcomp)

seedlingmass$volume <- as.factor(seedlingmass$volume)
seedlingmass$volume <-  relevel(seedlingmass$volume, ref="1000")

#srl (not different)
mass_container <- lme(totalmass ~ volume, random= ~1|ID, data=seedlingmass)
anova(mass_container)
summary(mass_container)
plot(mass_container)
visreg(mass_container)

###with block effect (use this)
mass_container2 <- lme(totalmass ~ volume, random= ~1|block/ID, data=seedlingmass)
anova(mass_container2)
summary(mass_container2)
visreg(mass_container2)

anova(mass_container, mass_container2)

tukey_mass<- glht(mass_container2, linfct = mcp(volume = "Tukey"))
mass_siglets <-cld(tukey_mass)
mass_siglets2 <- mass_siglets$mcletters$Letters
write.csv(mass_siglets2, "master_scripts/sigletters/sigletts_plant/sl_mass.csv", row.names=FALSE)  



# #general Stats
# totalmasslm <- lm(totalmass ~ as.factor(volume), data=finalmass)
# rootshootlm <- lm(rootshoot ~ as.factor(volume), data=finalmass)
# frootleaflm <- lm(frootleaf ~ as.factor(volume), data=finalmass)
# 
# library(visreg)
# visreg(totalmasslm)
# 
# extract_func(totalmasslm)
# extract_func(rootshootlm)
# extract_func(frootleaflm)
# 
# 
# # Pot size effect.
# totalmass_potsize <- lm(totalmass ~ as.factor(volume), data=finalmass, subset=volume != "1000")
# rootshoot_potsize <- lm(rootshoot ~ as.factor(volume), data=finalmass, subset=volume != "1000")
# frootleaf_potsize <- lm(frootleaf ~ as.factor(volume), data=finalmass, subset=volume != "1000")
# 
# extract_func(totalmass_potsize)
# extract_func(rootshoot_potsize)
# extract_func(frootleaf_potsize)


