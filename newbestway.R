source("functions and packages/plot objects.R")
library(doBy)

###this replaces the model scenarios
###calulate total net daily carbon gain (modelled)
### combine that with estimated leaf area (from data)
### compare to harvest mass, the difference will be what fraction goes to unmeasured pools (respiration, turnover, exudation)

##read data
finalmass <- read.csv("calculated data/harvest_mass_means.csv")

la_pred <- read.csv("Calculated data/LApred_volume.csv")
  names(la_pred)[3] <- "LA"
Cday_leaf <- read.csv("calculated data/model_runs/cday_120_clean.csv")

##Cday with leaf area needs self shading (use slope intercept, from 5-1000 vol)
sigma <- read.csv("gC_day_model/M_leafarea_model.csv")


#1. Calculate total net daily carbon day per tree with Cday and real leaf area
  dailyC <- merge(la_pred,Cday_leaf)
  ##this needs to include self shadeing (M as a linear function of leaf area)
  dailyC <- merge(dailyC, sigma[, c(2,3,5)], by="volume")
    dailyC$M <- with(dailyC, b*LA+intercept)
    #calculate total daily C gain with self shading
    dailyC$tdcg <- with(dailyC, LA * carbon_day * M)

#2. Calculate total seedling C gain over experiment (120d)
  plantC <- summaryBy(tdcg ~ volume, FUN=sum, data=dailyC)

#3. Compare estimated Daily C gain with final mass from harvest
  harvestC <- merge(finalmass, plantC)
  harvestC$massC <- harvestC$mass*.5

  
###PLOT------------------------------------------------------------------------------------------------------------------------

##uses harvest mass carbon, see how far off they are
with(harvestC, plot(tdcg.sum, mass, xlim=c(0,250), ylim=c(0,250), col=as.factor(volume), pch=c(rep(16, 6), 17),
                    ylab = "Seedling Mass (g C)", xlab = "Total Seedling Carbon Assimilation (g C)"))
abline(0,1)

##compare total carbon gain to the fraction that ends up in biomass (35% pots, 41% free)
with(harvestC, plot(tdcg.sum, mass/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
      xlab = "Total Seedling Carbon Assimilation (g C)", ylim=c(.5, .9), xlim=c(0, 250)))


#mean Cday
aleafvol <- summaryBy(. ~ volume, data=Cday_leaf, FUN=mean)
harvestC2 <- merge(harvestC, aleafvol)


###Fraction of Daily Carbon gain to mass
with(harvestC2, plot(carbon_day.mean, mass/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
              ylim=c(.5,.85),xlim=c(5,8),  xlab = cdaylab))


###Fraction of Daily Carbon gain to not mass
with(harvestC2, plot(carbon_day.mean, 1- mass/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
                    xlim=c(5,8),  ylim=c(.1, .5),xlab = cdaylab))


###conversion efficiency = turns production into mass (in model)

