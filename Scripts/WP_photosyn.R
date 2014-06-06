setwd("C:/Users/90919620/Google Drive/EucPVE/R Database")

library(sciplot)

#Read in spot measurements 
WPgasexchange <- read.csv("WP_gasexchange.csv")
WPgasexchange$ID <- paste(WPgasexchange$plot, WPgasexchange$pot, sep = "-")
WP_photo <-  aggregate(cbind(Photo, Cond, Ci, Trmmol) ~ ID, data=WPgasexchange, FUN=mean)

#water potential
WP <- read.csv("waterpotential.csv")
WP$ID <- paste(WP$plot, WP$pot, sep = "-")
WP$predawn <- with(WP, ((bar_pre/10)*-1))
WP$peak <- with(WP, ((bar_peak/10)*-1))
WP <- subset(WP, !is.na(WP$bar_pre))

WP_PS <- merge(WP, WP_photo)

#plot design
plotsumm <- read.csv("plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

WP_PS <- merge(WP_PS, plotsumm)

#factor designations
WP_PS$volume <- as.factor(WP_PS$volume)

WP_PS$WPdiff <- with(WP_PS, predawn - peak)
WP_PS$K <- with(WP_PS,Trmmol/WPdiff)

WP_trt <- aggregate(cbind(K, Trmmol, Photo, Ci, Cond, predawn, peak) ~ volume, data=WP_PS, FUN=mean)


#plot
leglab <- c(5, 10, 15, 20, 25, 35, "free")
palette <- c("red", "orange", "yellow", "green", "blue", "purple", "black")

                        
with(WP_PS, bargraph.CI(volume, K, ylim=c(-0.5,0), border="blue"))
box()
    
with(WP_PS, bargraph.CI(volume, Photo, border="blue"))
box()

with(WP_PS, plot(Trmmol, peak, 
                 ylim=c(-3,0)))


plot(peak~Trmmol, ylim=c(-3,0), xlim=c(0, 7.5),pch=16, col=volume, data=WP_PS)

legend("topright", leglab, lty = 1, lwd = 2, col = palette(), 
       title = expression(Pot ~ volume ~ (l)), inset = 0.01, bty = "n")




