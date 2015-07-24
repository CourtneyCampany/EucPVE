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

#mean of 5 logs per plant
PSmax_spot<- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSmax)
  PSmax_spot$volume <- as.factor(PSmax_spot$volume)

#mean by plant over all dates, then treatment means
PSmax_ID <- summaryBy(Photo~volume + ID, FUN=mean, keep.names=TRUE, data=PSmax_spot)
  PSmax_ID$volume <- as.factor(PSmax_ID$volume)

###merge Amax with biomass
  
massphoto <- merge(harvestmass[, c(1:2,11)], PSmax_ID)
  
###stats with amax and growth--------------------------------------------------------------------------

#asat

massphoto_mod <- lme(totalmass ~ Photo, random= ~1|ID, data=massphoto)

anova(massphoto_mod)
summary(massphoto_mod)
library(visreg)
visreg(massphoto_mod)



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
