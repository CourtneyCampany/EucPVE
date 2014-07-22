#source("functions and packages/startscripts.R")

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

wp_agg <- summaryBy(waterpotential ~ Date+volume+type, data=waterpotential, FUN=mean, keep.names=TRUE)

#PLOT----------------------------------------------------------------------------------
bargraph.CI(as.factor(volume), waterpotential, group=type,data=waterpotential,
            ylim = c(-2,0), ylab = "Water Potential (mPa)", 
            xlab = "Pot Volume (l)", legend=TRUE, col=c("grey40", "grey60"), x.leg=.1, y.leg=-1.5)

box()
#dev.copy2pdf(file= "output/waterpotential.pdf")
#dev.off()
