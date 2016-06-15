source("functions and packages/startscripts.R")
###CUE bar plot

##use modelled C net and gross to determine respiration
##use harvest mass + respiration + unaccounted as fractions of total gross

##read data-----------------------------------------------------------------------------------------------------------------
finalmass <- read.csv("calculated data/harvest_mass_means.csv")

la_pred <- read.csv("Calculated data/LApred_volume.csv")
  names(la_pred)[3] <- "LA"
  la_pred$Date <- as.Date(la_pred$Date)

Cday_net <- read.csv("calculated data/Aleaf_model/cday_120_clean.csv")
  Cday_net$Date <- as.Date(Cday_net$Date)
  
Cday_gross <- read.csv("calculated data/Aleaf_model/cday_120_clean_gross.csv")
  Cday_gross$Date <- as.Date(Cday_gross$Date)

##Cday with leaf area needs self shading (use slope intercept, from 5-1000 vol)
sigma <- read.csv("calculated data/M_leafarea_model.csv")

##function to generate total plant daily C gain
modelledC_func <- function(leafarea, shading, Cday){ #leafarea dfr, self shading dfr, and modelled C gain (gm2)
  dailyCnet <- merge(leafarea,Cday)
  ##this needs to include self shadeing (M as a linear function of leaf area)
  dailyCnet <- merge(dailyCnet, shading[, c(2,3,5)], by="volume")
    dailyCnet$M <- with(dailyCnet, b*LA+intercept)
    #calculate total daily C gain with self shading
    dailyCnet$tdcg <- with(dailyCnet, LA * carbon_day * M)
  print("successfully calculated total daily carbon with modelled Cgain, leaf area and self shading")
  
    dailyCnet$volume <- as.factor(dailyCnet$volume)
  return(dailyCnet)
}

##calculate net and gross C gain, then quantify leaf respiration--------------------------------------------------------------
dailyCnet <- modelledC_func(la_pred, sigma, Cday_net)
  names(dailyCnet)[8] <- "tdc_net"
  names(dailyCnet)[4] <- "cday_net"
dailyCgross <- modelledC_func(la_pred, sigma, Cday_gross)
  names(dailyCgross)[8] <- "tdc_gross"
  names(dailyCgross)[4] <- "cday_gross"


dailyC <- merge(dailyCgross[, c(1:2, 4, 8)], dailyCnet[,c(1:2,4,8)], by=c("Date", "volume"))
  dailyC$Rleaf <- with(dailyC, tdc_gross-tdc_net)


#Calculate total seedling C gain over experiment (120d) and compare to final harevst mass C----------------------------------
plantCnet <- doBy::summaryBy(tdc_net+Rleaf+tdc_gross ~ volume, FUN=sum, data=dailyC)
  
write.csv(plantCnet, "calculated data/CUEdaily.csv", row.names = FALSE) 
  
#Compare estimated Daily C gain with final mass from harvest
harvestC <- merge(finalmass, plantCnet)
  harvestC$massC <- harvestC$mass*.5
  
##dataframe for stacked bar plot
cue_fractions <- data.frame(volume=as.factor(harvestC$volume), 
                          mass_perc = with(harvestC, massC/tdc_gross.sum),
                          leafresp_perc = with(harvestC, Rleaf.sum/tdc_gross.sum),
                          leftover_perc = with(harvestC, (tdc_gross.sum-(massC+Rleaf.sum))/tdc_gross.sum))

cue_fractions2 <- cue_fractions[,2:4]
cue_fractions3 <- cue_fractions[,2:3]
i <- c(1,2,3) #correct order of variables
i2 <- c(1,2)
treecols <- c("black", "grey50","grey95")
treecols2 <- c("grey95","grey50", "black")
treefraclab <- c("Biomass", "Leaf Respiration", "Woody Respiration + Unaccounted")
leglab <- c(5, 10, 15, 20, 25, 35, "Free")
  
windows(7,7)  
par(mar = c(5, 5, 5, 2), xpd = TRUE, mgp=c(2.5,1,0))
barplot(t(as.matrix(cue_fractions2))[i,], names.arg=leglab, col=treecols, width=2, xlab= "Carbon Use Efficency", 
          ylab="Total Modelled Photosynthetic Uptake", ylim=c(0, 1.1))
  legend("topright", inset = c(0.47, -.175), fill = treecols2, legend=treefraclab, cex=1)
box()

##mass and leaf resp
windows(7,7)  
par(mar = c(5, 5, 5, 2), xpd = TRUE, mgp=c(2.5,1,0))
barplot(t(as.matrix(cue_fractions3))[i2,], names.arg=leglab, col=treecols[1:2], width=2, xlab= "Soil Volume", 
        ylab="Fraction of Total Modelled Photosynthetic Uptake", ylim=c(0, .5))
legend("topright", inset = c(0.7, -.15), fill = treecols2[1:2], legend=treefraclab[1:2], cex=1)
box()

##mass only
windows(7,7)  
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
bar(mass_perc, volume, cue_fractions,half.errbar=FALSE, xlab="Soil Volume  (l)",ylab="", 
    ylim=c(0,.5), names.arg = leglab,
    col="grey", legend=FALSE, bg="white")
title(ylab="Fraction of Total Modelled Photosynthesis", mgp=c(2.5,1,0))


plot(massC/tdc_net.sum ~ volume, data=harvestC[!harvestC$volume == "1000",], ylim=c(.25, .45))

#colors
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
cols <- palette(gradient(7))

###CUE plot as points with free
test <- harvestC
test$volume <- gsub(1000, 40, test$volume)
plot(massC/tdc_net.sum ~ volume, data=test, ylim=c(.25, .45), xaxt='n', cex=1.5, pch=16, col=cols, ylab="CUE", xlab="Volume (l)")
axis(1, at=c(5,10,15,20,25,30,35,40), labels=c(5,10,15,20,25,30,35,"Free"))

