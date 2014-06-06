source("functions and packages/load packages.R")

#read data
source("calculated data/seedling mass.csv")


#treatment means for total mass and ratios
finalmass <- subset(seedlingmass, select = c("ID", "volume", "totalmass", 
                                             "rootshoot", "frootleaf"))
mass_agg <- summaryBy( .~ volume , data = finalmass,  FUN=c(mean,sd,length))

mass_agg$massSE <- with(mass_agg, totalmass.sd/sqrt(totalmass.length))
mass_agg$rootshootSE <- with(mass_agg, rootshoot.sd/sqrt(rootshoot.length))
mass_agg$frootleafSE <- with(mass_agg, frootleaf.sd/sqrt(frootleaf.length))
mass_agg$finerootSE <- with(mass_agg, frootleaf.sd/sqrt(frootleaf.length))
mass_agg$shootSE <- with(mass_agg, frootleaf.sd/sqrt(frootleaf.length))

#Stats
totalmasslm <- lm(totalmass ~ as.factor(volume), data=finalmass)
rootshootlm <- lm(rootshoot ~ as.factor(volume), data=finalmass)
frootleaflm <- lm(frootleaf ~ as.factor(volume), data=finalmass)

getP <- function(x)anova(x)[[5]][1]
getP(totalmasslm)
getP(rootshootlm)
getP(frootleaflm)


# Pot size effect.
totalmass_potsize <- lm(totalmass ~ as.factor(volume), data=finalmass, subset=volume != "1000")
rootshoot_potsize <- lm(rootshoot ~ as.factor(volume), data=finalmass, subset=volume != "1000")
frootleaf_potsize <- lm(frootleaf ~ as.factor(volume), data=finalmass, subset=volume != "1000")

getP(totalmass_potsize)
getP(rootshoot_potsize)
getP(frootleaf_potsize)

#Plotting

with(finalmass, bargraph.CI(as.factor(volume), totalmass))
with(finalmass, bargraph.CI(as.factor(volume), rootshoot))
with(finalmass, bargraph.CI(as.factor(volume), frootleaf))
