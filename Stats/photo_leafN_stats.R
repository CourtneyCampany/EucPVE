#Examine the relationship between A and leaf N

source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

#read in Asat and leaf Ndata, merge
Asat <- read.csv("calculated data/Asat.csv")
Amax <- read.csv("calculated data/Amax.csv")
leafN <- read.csv("calculated data/leaf N content.csv")

N_Asat <- merge(Asat, leafN[c(1:4,12:13)])
N_Amax <- merge(Amax, leafN[c(1:4,12:13)])

#merge with plot summary
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

N_Asat <- merge(N_Asat, plotsumm[3:4], all=TRUE)
N_Asat$volume <- as.factor(N_Asat$volume)

N_Amax <- merge(N_Amax, plotsumm[3:4], all=TRUE)
N_Amax$volume <- as.factor(N_Amax$volume)

#-----------------------------------------------------------------------------------------
#calculate overall means and means for each date

#mean
Nsat_agg <- summaryBy(Photo+Nmass ~ volume, FUN=c(mean, se), 
                             keep.names=TRUE, data=N_Asat)

Nmax_agg <- summaryBy(Photo+Nmass ~ volume, FUN=c(mean, se), 
                      keep.names=TRUE, data=N_Amax)

#means(campaign)
Nsat_campaign <- summaryBy(Photo+Nmass ~ campaign + volume, FUN=c(mean, se), 
                                  keep.names=TRUE, data=N_Asat)

Nmax_campaign <- summaryBy(Photo+Nmass ~ campaign + volume, FUN=c(mean, se), 
                           keep.names=TRUE, data=N_Amax)

#----------------------------------------------------------------------------------------------------
#plot stuff
manX <- seq(0, 0.015, 0.005)
meanX <- seq(0, 0.01, 0.005)

#-----------------------------------------------------------------------------------------------
#row panel function
rowfunction <- function(df1, df2, df3, ylabel, ...){
par(mfrow=c(1,3),omi=c(1,1,0.2,0.2), mar=c(0,0,0,0), cex=1.3)

#raw
plot(Photo ~ Nmass, data = df1, col=volume, xlim=c(0, 0.02),pch=pchs[volume], 
     axes=FALSE, xlab="", ylab="", ...)
box()
axis(1, at = manX, labels = manX)
axis(2, labels=TRUE)
mtext(ylabel, side=2, line=2.5, cex=1.3)
title(main=raw, line=-1.5, font.main=1, adj=.1)
legend("bottomright", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n')
d_ply(df1, .(volume), function(x) add_trend_line("Nmass", "Photo", x, ))

#volume means
plot(Photo.mean ~ Nmass.mean, data = df2, col=volume, xlim=c(0, 0.015),pch=pchs[volume], 
     ylab="", axes=FALSE,cex=2, ...)
box()
axis(1, at = meanX, labels = meanX)
mtext(nitro, side=1, line=2.5, cex=1.3)
title(main=volmean, line=-1.5, font.main=1, adj=.1)

#means by date
plot(Photo.mean ~ Nmass.mean, data = df3, col=volume, xlim=c(0, 0.015),
     pch=pch2[volume], cex=2, ylab="", axes=FALSE, ...)
box()
axis(1, at = meanX, labels = meanX)
title(main=datemean, line=-1.5, font.main=1, adj=.1)
d_ply(df3, .(volume), function(x) c(add_trend_line("Nmass.mean", "Photo.mean", x),
                                              text(x$Photo.mean~x$Nmass.mean, labels=unique(x$campaign), 
                                                   font=2,cex=.75)))
dev.off()
}


#make plots
#Asat
windows(width = 16, height = 6)
pdf(file="output/stats_plots/asat_leafN.pdf", onefile = TRUE, width = 16, height = 6)
rowfunction(N_Asat,Nsat_agg,Nsat_campaign, satlab, ylim=c(0, 30))
#Amax
windows(width = 16, height = 6)
pdf(file="output/stats_plots/amax_leafN.pdf", onefile = TRUE, width = 16, height = 6)
rowfunction(N_Amax,Nmax_agg,Nmax_campaign, maxlab, ylim=c(0, 45))




