source("functions and packages/startscripts.R")

#1: seedling mass
harvestmass <- read.csv("calculated data/seedling mass.csv") 


#2. amax

#Read in spot A measurements and merge plot design

plotsumm <- read.csv("raw data/plot_summary.csv")
  plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#read in gas exchange master file, all dates
gasexchange <- read.csv("raw data/AsatAmax_master.csv")
  gasexchange$Date <- as.Date(gasexchange$Date)
  gasexchange$ID <- paste(gasexchange$plot, gasexchange$pot, sep = "-")

PS <- merge(plotsumm, subset(gasexchange, select = c("campaign", "ID", "CO2", "Photo")))
PS$type <- factor(ifelse (PS$CO2 == "400", "Asat", "Amax"))

#run function to add campaign date
PS <- add_campaign_date(PS)


#Amax_means for Data table-----------------------------------------------------------------------
PSmax <- subset(PS, type=="Amax")

###merge Amax with biomass
  
massphoto <- merge(harvestmass[, c(1:2,11)], PSmax_ID)
  
###stats with amax and growth--------------------------------------------------------------------------

massphoto_mod <- lme(totalmass ~ Photo, random= ~1|ID, data=massphoto)

anova(massphoto_mod)
summary(massphoto_mod)
library(visreg)
visreg(massphoto_mod)
r.squaredGLMM(massphoto_mod)

potmassphoto <- lme(totalmass ~ Photo, random= ~1|ID, data=massphoto[massphoto$volume != 1000,])
anova(potmassphoto)
summary(potmassphoto)
library(visreg)
visreg(potmassphoto)

library(MuMIn)
r.squaredGLMM(potmassphoto)

###effect size of jmax and vc max from free to container

biochem <- read.csv("calculated data/jmax_vcmax.csv")

jfree <- mean(biochem[biochem$volume == 1000, "Jmax.mean"])
jpot <-  mean(biochem[biochem$volume != 1000, "Jmax.mean"])

vfree <- mean(biochem[biochem$volume == 1000, "Vcmax.mean"])
vpot <-  mean(biochem[biochem$volume != 1000, "Vcmax.mean"])


Jeffect <- (jfree-jpot)/jfree
veffect <- (vfree-vpot)/vfree


###effect size of totalmass
mfree <- mean(massphoto[massphoto$volume == 1000, "totalmass"])
mpot <-  mean(massphoto[massphoto$volume != 1000, "totalmass"])

meffect <- (mfree-mpot)/mfree



# harvest leaf area -------------------------------------------------------
leaves <- read.csv("calculated data/LA_harvest.csv")
  
plotsumm <- read.csv("raw data/plot_summary.csv")
  plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")
  
leafarea <- merge(plotsumm, leaves)
  leafarea$block <- as.factor(gsub("-[1-9]", "", leafarea$ID))
  leafarea$volume <- as.factor(leafarea$volume)
  
library(multcomp)
library(MuMIn)

leafvol <-  lme(totalarea ~ volume, random= ~1|ID, data=leafarea)
library(car)
Anova(leafvol)
anova(leafvol)
summary(leafvol)
library(visreg)
visreg(leafvol)
r.squaredGLMM(leafvol)

leafvol2 <- lme(totalarea ~ volume, random= ~1|block/ID, data=leafarea)
Anova(leafvol2)
anova(leafvol2)
summary(leafvol2)
visreg(leafvol2)
r.squaredGLMM(leafvol2)

tukey_leaf<- glht(leafvol2, linfct = mcp(volume = "Tukey"))
leaf_siglets <-cld(tukey_leaf)
leaf_siglets2 <- leaf_siglets$mcletters$Letters
write.csv(leaf_siglets2, "master_scripts/sigletters/sigletts_plant/sl_leafarea.csv", row.names=FALSE)



#no free
la_pots <- leaves[leaves$volume != 1000,]
la_pots <- droplevels(la_pots)
potleaf <- lme(totalarea ~ volume, random= ~1|ID, data=la_pots[la_pots$volume != 1000,])
anova(potleaf)
Anova(potleaf)
summary(potleaf)
visreg(potleaf)

   

