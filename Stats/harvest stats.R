source("functions and packages/load packages.R")
source("functions and packages/functions.R")

#read data
seedlingmass <- read.csv("calculated data/seedling mass.csv")

#treatment means for total mass and ratios
finalmass <- subset(seedlingmass, select = c("ID", "volume", "totalmass", 
                                             "rootshoot", "frootleaf"))
mass_agg <- summaryBy( .~ volume , data = finalmass,  FUN=c(mean,se))

#Stats
totalmasslm <- lm(totalmass ~ as.factor(volume), data=finalmass)
rootshootlm <- lm(rootshoot ~ as.factor(volume), data=finalmass)
frootleaflm <- lm(frootleaf ~ as.factor(volume), data=finalmass)

library(visreg)
visreg(totalmasslm)

extract_func(totalmasslm)
extract_func(rootshootlm)
extract_func(frootleaflm)


# Pot size effect.
totalmass_potsize <- lm(totalmass ~ as.factor(volume), data=finalmass, subset=volume != "1000")
rootshoot_potsize <- lm(rootshoot ~ as.factor(volume), data=finalmass, subset=volume != "1000")
frootleaf_potsize <- lm(frootleaf ~ as.factor(volume), data=finalmass, subset=volume != "1000")

extract_func(totalmass_potsize)
extract_func(rootshoot_potsize)
extract_func(frootleaf_potsize)


