source("functions and packages/plot objects.R")
library(doBy)

### this replaces the model scenarios
### calulate total net daily carbon gain (modelled)
### combine with emprical estimates of daiil leaf area
### compare to harvest mass, the difference represents fraction to unmeasured pools

##read data
finalmass <- read.csv("calculated data/harvest_mass_means.csv")

la_pred <- read.csv("Calculated data/LApred_volume.csv")
  names(la_pred)[3] <- "LA"
  
  
Cday_net <- read.csv("calculated data/Aleaf_model/cday_120_clean.csv")
Cday_gross <- read.csv("calculated data/Aleaf_model/cday_120_clean_gross.csv")

##Cday with leaf area needs self shading (use slope intercept, from 5-1000 vol)
sigma <- read.csv("calculated data/M_leafarea_model.csv")


#Calculate total net daily carbon day per tree with Cday and real leaf area------------------------------------------------
  dailyC <- merge(la_pred,Cday_net)
  ##this needs to include self shadeing (M as a linear function of leaf area)
  dailyC <- merge(dailyC, sigma[, c(2,3,5)], by="volume")
    dailyC$M <- with(dailyC, b*LA+intercept)
    #calculate total daily C gain with self shading
    dailyC$tdcg <- with(dailyC, LA * carbon_day * M)

#Calculate total seedling C gain over experiment (120d)
  plantC <- summaryBy(tdcg ~ volume, FUN=sum, data=dailyC)

#Compare estimated Daily C gain with final mass from harvest
  harvestC <- merge(finalmass, plantC)
  harvestC$massC <- harvestC$mass*.5
  
  
##repeat with gross Cday-----------------------------------------------------------------------------------------------------

dailyCgross <- merge(la_pred,Cday_gross)
  ##this needs to include self shadeing (M as a linear function of leaf area)
  dailyCgross <- merge(dailyCgross, sigma[, c(2,3,5)], by="volume")
  dailyCgross$M <- with(dailyCgross, b*LA+intercept)
  #calculate total daily C gain with self shading
  dailyCgross$tdcg <- with(dailyCgross, LA * carbon_day * M)
  
  #Calculate total seedling C gain over experiment (120d)
  plantC_gross <- summaryBy(tdcg ~ volume, FUN=sum, data=dailyC)
  
  #Compare estimated Daily C gain with final mass from harvest
  harvestC_gross <- merge(finalmass, plantC_gross)
  harvestC_gross$massC <- harvestC$mass*.5
  

  
#mean Cday-------------------------------------------------------------------------------------------------------------------
  anet_vol <- summaryBy(. ~ volume, data=Cday_net, FUN=mean)
  agross_vol <- summaryBy(. ~ volume, data=Cday_gross, FUN=mean)
  
  #merge with tdcg dfr
  harvestC2 <- merge(harvestC, anet_vol)
  harvestC2_gross <- merge(harvestC_gross, agross_vol) 
  

###PLOT------------------------------------------------------------------------------------------------------------------------

#1. how far off are TDCG and havest mass C
windows(7,7)
with(harvestC, plot(tdcg.sum, massC, xlim=c(0,250), ylim=c(0,250), col=as.factor(volume), pch=c(rep(16, 6), 17),
                    ylab = "Seedling Carbon Mass (g)", xlab = "Total Seedling Carbon Assimilation (g C)", cex=1.5))
abline(0,1, lty=2)


#2. compare total carbon gain to the fraction that ends up in biomass (35% pots, 41% free)
windows(7,7)
with(harvestC, plot(tdcg.sum, massC/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
      xlab = "Total Seedling Carbon Assimilation (g C)", xlim=c(0, 250), ylim=c(.25,.45)))


#3. Fraction of net Daily Carbon gain to C mass
with(harvestC2, plot(carbon_day.mean, massC/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
               xlab = cdaylab, xlim=c(5, 9), ylim=c(.25,.45)))


#4. Fraction of gross Daily Carbon gain to not mass (CUE)
with(harvestC2_gross, plot(carbon_day.mean, massC/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
                     xlab = cdaylab,xlim=c(5, 9), ylim=c(.25,.45)))


#5. leftover (really high, how does this compare with mass model?)
with(harvestC2, plot(carbon_day.mean, 1- massC/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
                    xlim=c(5,9),  ylim=c(.5,.8), xlab = cdaylab))



