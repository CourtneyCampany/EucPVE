source("functions and packages/startscripts.R")

##modelled seedlings C gain (120 days) vs observed final mass

#harvest mass (treatment means)
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
mass_actual$Date <- as.Date("2013-05-21")
mass_actual$mass_adj <- with(mass_actual, mass/mass[7])

#modelled C gain
cue <- read.csv("calculated data/CUEdaily.csv") 

harvestC <- merge(mass_actual[,1:2], cue)
  harvestC$massC <- harvestC$mass*.5  
  harvestC$volume <- gsub(1000, 40, harvestC$volume)

#plot-----------------------------------------------------------------------------------------
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
plot(massC/tdc_net.sum ~ volume, data=harvestC, ylim=c(.25, .45), xaxt='n', cex=1.5, pch=pchs, col=cols, 
       ylab="Seedling Mass (g C) / \nModelled Total Net C Gain (g C)", 
       xlab="Soil Volume (l)")
    axis(1, at=c(5,10,15,20,25,35,40), labels=c(5,10,15,20,25,35,"Free"))

  
##results section----------------------------------------------------------------------------------------
mean(harvestC$tdc_net.sum[1:6])
se(harvestC$tdc_net.sum[1:6])

harvestC$tdc_net.sum[7]
  
leftoverC <- with(harvestC, (tdc_net.sum-massC)/tdc_net.sum)
mean(leftoverC[1:6])
se(leftoverC[1:6])
leftoverC[7]
  
cue_dat <- with(harvestC, massC/tdc_net.sum)
mean(cue_dat[1:6])
se(cue_dat[1:6])
cue_dat[7]
