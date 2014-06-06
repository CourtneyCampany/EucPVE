#Dark Respiration and Sugars

source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

#read in Asat and leaf Ndata

#dark respiration (two dates)
resp_dark <- read.csv("raw data/Rdark_cleanCi.csv")
resp_dark$Date <- as.Date(resp_dark$Date)
resp_dark$ID <- paste(resp_dark$plot, resp_dark$pot, sep = "-")

#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

rd <- merge(resp_dark[,c(1,10,62)], plotsumm[,3:4])

#mean Rd by volume
rd_agg <- ddply(rd, .(volume), function(x) data.frame(rd_mean=mean(x$Photo)))

#merge Rd with TNC-sugars, R per leaf mass?
leaf_tnc <- read.csv("calculated data/leaf_tnc.csv")
leaf_sugars <- leaf_tnc[,c(1:6, 8,11)]

rd_sugar<- merge(leaf_tnc[,c(1:6, 8,11)], rd_agg)
rd_sugar$volume <- as.factor(rd_sugar$volume)

#new variables
rd_sugar$resppermass <- with(rd_sugar, rd_mean/mass)

#-----------------------------------------------------------------------------------------
#calculate overall means and means for each date

rdsugar_agg <- summaryBy(resppermass+leafsugar ~ volume, FUN=c(mean, se), 
                      keep.names=TRUE, data=rd_sugar)

#means(campaign)
rdsugar_campaign <- summaryBy(resppermass+leafsugar ~ campaign + volume, FUN=c(mean, se), 
                           keep.names=TRUE, data=rd_sugar)

#----------------------------------------------------------------------------------------------------
#plot stuff
manX <- seq(0, 0.06, 0.01)
meanX <- seq(0, 0.025, 0.005)

#row plot-------------------------------------------------------------------

windows(width = 16, height = 6)
pdf(file="output/stats_plots/rd_sugar.pdf", onefile = TRUE, width = 16, height = 6)
par(mfrow=c(1,3),omi=c(1,1,0.2,0.2), mar=c(0,0,0,0), cex=1.3)
  
#raw
plot(resppermass ~ leafsugar, data = rd_sugar, col=volume, xlim=c(0, .07), 
     ylim=c(-10,2),pch=pchs[volume], axes=FALSE, xlab="", ylab="")
  box()
  axis(1, at = manX, labels = manX)
  axis(2, labels=TRUE)
  mtext(rdlab, side=2, line=2.5, cex=1.3)
  title(main=raw, line=-1.5, font.main=1, adj=.1)
  legend("bottomright", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n')
#d_ply(rd_sugar, .(volume), function(x) add_trend_line("leafsugar", "resppermass", x, ))
  
  #volume means
plot(resppermass.mean ~ leafsugar.mean, data = rdsugar_agg, col=volume, ylim=c(-5,0), xlim=c(0, 0.03),pch=pchs[volume], 
       ylab="", axes=FALSE,cex=2)
  box()
  axis(1, at = meanX, labels = meanX)
  mtext(suglab, side=1, line=2.5, cex=1.3)
  title(main=volmean, line=-1.5, font.main=1, adj=.1)
  
  #means by date
plot(resppermass.mean ~ leafsugar.mean, data = rdsugar_campaign, col=volume, ylim=c(-5,0), xlim=c(0, 0.03),
       pch=pch2[volume], cex=2, ylab="", axes=FALSE)
  box()
  axis(1, at = meanX, labels = meanX)
  title(main=datemean, line=-1.5, font.main=1, adj=.1)
d_ply(rdsugar_campaign, .(volume), function(x) c(add_trend_line("leafsugar.mean", "resppermass.mean", x),
                                      text(x$resppermass.mean~x$leafsugar.mean, labels=unique(x$campaign), 
                                           font=2,cex=.75)))
  


dev.off()




