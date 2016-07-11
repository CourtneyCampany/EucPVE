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
pch3 <- c(rep(1,6), 6)
  
# free_cue <- read.csv("calculated data/free_cue.csv")

windows(7,7)
par(cex.axis=.96, cex.lab=1.2,mfrow=c(2,1),oma=c(0.1,0.1,0.1,0.1), las=1)   
  
par(mar=c(4,5.5,2,2), cex.axis=0.8, las=1)
#plot(a)
plot(massC ~ tdc_net.sum, data=harvestC,pch=pchs,col=palette(),cex=1.6, xlim=c(0, 225),
       ylim=c(0, 225), ylab="Seedling Mass (g C)", xlab="Net Total Leaf Carbon Gain (g C)")
  segments(x0=0, x1=max(harvestC$tdc_net.sum),y0=0, y1=max(harvestC$massC), 
            lwd=3, lty=3, col="darkblue")
  abline(0,1, lwd=2, lty=2, col="grey35")
  text(225,210,"(a)", cex=1.2)
  
  legend("topleft", leglab, pch=pchs,text.font=1, title=vollab, col=palette(), bty='n',cex=1.0)
  
#plot(b)
par(mar=c(4,5.5,1,2))
plot(massC/tdc_net.sum ~ volume, data=harvestC, ylim=c(.25, .45), xaxt='n', cex=1.5, pch=pchs, col=cols, 
       ylab="Seedling Mass (g C) / \nModelled Total Net C Gain (g C)", 
       xlab="Soil Volume (l)")
  axis(1, at=c(5,10,15,20,25,35,40), labels=c(5,10,15,20,25,35,"Free"))
  text(40,.44,"(b)", cex=1.2)
  
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
