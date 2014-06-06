source("functions and packages/load packages.R")

#read data
wp <- read.csv("raw data/waterpotential.csv")
wp$Date <- as.Date(wp$Date, format = "%Y/%m/%d")

gasexchange <- read.csv("raw data/PS_waterpotential.csv")
gasexchange$Date <- as.Date(gasexchange$Date, format = "%m/%d/%Y")

plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")


#WATER POTENTIAL 
wp <- merge(wp, plotsumm)
wp <- subset(wp, !is.na(bar_pre))
#convert to millipascals
wp$predawn <- with(wp, ((bar_pre/10)*-1))
wp$peak <- with(wp, ((bar_peak/10)*-1))

wp_pre <- wp[, c(3, 6:8)]
wp_pre$type <- "predawn"
names(wp_pre)[4] <- "waterpotential"

wp_peak <- wp[, c(3, 6:7, 9)]
wp_peak$type <- "peak"
names(wp_peak)[4] <- "waterpotential"

waterpotential <- rbind(wp_pre, wp_peak)


#what to graph (predaawn and peak for each volume? mean of two dates?)
#plot bits
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
#cl <- palette(gradient(7))

#PLOT----------------------------------------------------------------------------------

wp_agg <- summaryBy(waterpotential ~ Date+volume+type, data=waterpotential, FUN=mean, keep.names=TRUE)

bargraph.CI(as.factor(volume), waterpotential, group=type,data=wp_agg,
            ylim = c(-2,0), ylab = "Water Potential (mPa)", 
            xlab = "Pot Volume (l)", legend=TRUE, col=c("grey40", "grey60"))

box()

bargraph.CI(as.factor(volume), waterpotential, group=type,data=waterpotential,
            ylim = c(-2,0), ylab = "Water Potential (mPa)", 
            xlab = "Pot Volume (l)", legend=TRUE, col=c("grey40", "grey60"), x.leg=.1, y.leg=-1.5)

box()
dev.copy2pdf(file= "output/waterpotential.pdf")
dev.off()

#-----------------------------------------------------------------------------------------------------------
bargraph.CI(as.factor(volume), waterpotential, group=type,data=subset(waterpotential, Date=="2013-04-23"),
            ylim = c(-3,0), ylab = "Water Potential (mPa)", 
            xlab = "Pot Volume (l)", legend=TRUE, args.legend = "bottomright")

box()

bargraph.CI(as.factor(volume), waterpotential, group=type,data=subset(waterpotential, Date=="2013-05-20"),
            ylim = c(-2,0), ylab = "Water Potential (mPa)", 
            xlab = "Pot Volume (l)", legend=TRUE, col=c("grey40", "grey60"))

box()

#--------------------------------------------------------------------------------------------------------




##PHOTOSYNTHESIS
#gasexchange <-  subset(gasexchange, select = c("Date", "plot", "pot","Photo", "Cond", "Ci", "Tleaf", "Trmmol"))
#gasexchange$ID <- paste(gasexchange$plot, gasexchange$pot, sep = "-")

#gasexchange_agg <-summaryBy(Photo+Cond+Ci+Tleaf+Trmmol ~ ID+Date, data=gasexchange, FUN=mean, keep.names=TRUE)


#new dfr with water potential and gasexchange

#waterpotential <- merge(subset(wp, select=c("volume","ID", "Date",	"predawn", "peak")), 
                        #gasexchange_agg, by=c("ID", "Date"))

#treatment means
wp_agg <- summaryBy(predawn+peak+Photo+Cond+Ci+Tleaf ~ Date+volume , data = waterpotential, 
                    FUN = mean, keep.names=TRUE)
wp_agg$volume <- as.factor(wp_agg$volume)



with(waterpotential, bargraph.CI(as.factor(volume), predawn, as.factor(Date),
                                 border="blue",
                                 ylim = c(-.8,0), ylab = "Predawn Water Potential (mPa)", 
                                 xlab = "Pot Volume (l)"),
     col=c("gray60", "gray20"))
box()

dev.copy2pdf(file= "output/predawn.pdf")





with(waterpotential, bargraph.CI(as.factor(volume), peak, as.factor(Date),
                  border="blue",
                  ylim = c(-2.5,0),ylab = "Peak Water Potential (mPa)",
                  xlab = "Pot Volume (l)"),
                  col=c("gray60", "gray20"))
box()

dev.copy2pdf(file= "output/midday.pdf")


with(waterpotential, bargraph.CI(as.factor(volume), Cond, as.factor(Date),
                  border="blue",
                  ylim = c(0,.5),ylab = expression(Conductance~~(mol~m^-2~s^-1)),
                  xlab = "Pot Volume (l)"),
                  col=c("gray60", "gray20")) 
box()
                                     
dev.copy2pdf(file= "output/midday_conductance.pdf")


with(waterpotential, bargraph.CI(as.factor(volume), Photo, as.factor(Date),
                                 border="blue",
                                 ylim = c(0,25),ylab = expression(italic(A)[sat]~~(mu*mol~m^-2~s^-1)),
                                 xlab = "Pot Volume (l)"),
     col=c("gray60", "gray20")) 
box()

dev.copy2pdf(file= "output/midday_Photo.pdf")


with(waterpotential, bargraph.CI(as.factor(volume), Ci, as.factor(Date),
                                 border="blue",
                                 ylim = c(0,350),ylab = "Ci",
                                 xlab = "Pot Volume (l)"),
     col=c("gray60", "gray20")) 
box()

dev.copy2pdf(file= "output/midday_Ci.pdf")

------------------------------------------------------------------------------------
windows(8,12)
par(cex.axis=0.9,  # axis label size
    mfrow=c(3,1),  # rows and columns of plots
    omi=c(1,1,0.2,0.2),  # outer margin (inches)
    mar=c(0,0,0,0))  # margin around plots (they are tight together) 

with(waterpotential, bargraph.CI(as.factor(volume), predawn,as.factor(Date),                                    
                      ylim = c(-.8,0), 
                      ylab=expression(Predawn~Water~Potential~~("mPa"))))
axes=FALSE 
xlab=""     
axis(1, labels=FALSE) 
     axis(2, labels=TRUE)  
                               
box()

with(waterpotential, bargraph.CI(as.factor(volume), peak, as.factor(Date),     
                        ylim = c(-2.5,0),
                        ylab=expression(Peak~Water~Potential~~("mPa"))))
axes=FALSE 
xlab=""
axis(1, labels=FALSE) 
axis(2, labels=TRUE) 
                          
box()

with(waterpotential, bargraph.CI(as.factor(volume), Cond, as.factor(Date),
                         ylim = c(0,.5),
                         ylab = expression(Conductance~~(mol~m^-2~s^-1)),
                         xlab = "Pot Volume (l)")) 
axis(1, labels=TRUE) 
axis(2, labels=TRUE)
                   
box()
# Axis titles
mtext("Volume(l)", side=1, outer=TRUE, line=3, cex=1.0)


---------------------------------------------------------------------------------------------------------
windows(8,12)
    par(cex.axis=0.9,  # axis label size
    mfrow=c(2,1),  # rows and columns of plots
    omi=c(1,1,0.2,0.2),  # outer margin (inches)
    mar=c(0,0,0,0))  # margin around plots (they are tight together) 

# First Panel
 barplot(countpre,
     type='n',
     ylim=c(-.8,0),
     axes=FALSE, 
     ann=FALSE)  # suppresses axes; add customized ones

axis(1, labels=FALSE) #axis needs no labels
axis(2, labels=TRUE)  # Y axis
mtext("Predawn Water Potential (mb)", side=2, outer=TRUE, line=3, cex=1.0, at=.85)


points(with(waterpotential, bargraph.CI(as.factor(volume), predawn,  as.factor(Date),                                    
                                        ylim = c(-.8,0), #ylab = "Predawn Water Potential (mb)", 
                                        xlab = "Pot Volume (l)")))  
box()

#2nd panel
barplot(countpeak,
      type='n',
      ylim=c(-.8,0),
      axes=FALSE, ann=FALSE)  # suppresses axes; add customized ones

axis(1, labels=FALSE) 
axis(2, labels=TRUE)
mtext("Peak Water Potential (mb)", side=2, outer=TRUE, line=3, cex=1.0)

points(with(waterpotential, bargraph.CI(as.factor(volume), peak,  as.factor(Date),                                    
                                        ylim = c(-2.5,0), #ylab = "Predawn Water Potential (mb)", 
                                        xlab = "Pot Volume (l)")))  
box()
