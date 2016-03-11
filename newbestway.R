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
##Cday with leaf area needs self shading
sigma <- read.csv("gC_day_model/M_leafarea_model.csv")

#self shading
M <- M_slope*leafarea[i-1] + M_intercept


#1. Calculate total net daily carbon day per tree with Cday and real leaf area
  dailyC <- merge(la_pred,Cday_leaf)
    dailyC$tdcg <- with(dailyC, LA * carbon_day)

  # need to worry about sigma!

#2. Calculate total seedling C gain over experiment (120d)
  plantC <- summaryBy(tdcg ~ volume, FUN=sum, data=dailyC)

#3. Compare estimated Daily C gain with final mass from harvest
  harvestC <- merge(finalmass, plantC)


###PLOT------------------------------------------------------------------------------------------------------------------------
with(harvestC, plot(tdcg.sum, mass, xlim=c(0,250), ylim=c(0,250), col=as.factor(volume), pch=c(rep(16, 6), 17),
                    ylab = treelab, xlab = "Total Seedling Carbon Assimilation (g C)"))
abline(0,1)



with(harvestC, plot(tdcg.sum, mass/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
     xlim=c(0, 275), ylim=c(.4,.85), xlab = "Total Seedling Carbon Assimilation (g C)"))



aleafvol <- summaryBy(. ~ volume, data=Cday_leaf, FUN=mean)
dat <- merge(harvestC, aleafvol)

with(dat, plot(carbon_day.mean, (mass*.5)/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
    xlab = cdaylab))

with(dat, plot(carbon_day.mean, mass/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
              ylim=c(.4,.85),xlim=c(5,8),  xlab = cdaylab))
