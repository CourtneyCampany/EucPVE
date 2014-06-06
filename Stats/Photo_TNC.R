source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

#read in Asat and leaf Ndata, merge
Amax <- read.csv("calculated data/Amax.csv")
Asat <- read.csv("calculated data/Asat.csv")
leaf_tnc <- read.csv("calculated data/leaf_tnc.csv")

Asat_starch <- merge(Asat[, c(1:2,5:6,9)], leaf_tnc)
Asat_starch$volume <- as.factor(Asat_starch$volume)

Amax_starch <- merge(Amax[, c(1:2,5:6,9)], leaf_tnc)
Amax_starch$volume <- as.factor(Amax_starch$volume)
#--------------------------------------------------------------------------------------
#calculate overall means and means for each date

#means(all dates)
Amax_starch_agg <- summaryBy(Photo+leafstarch+percstarch ~ volume, FUN=c(mean, se), 
                             keep.names=TRUE, data=Amax_starch)

Asat_starch_agg <- summaryBy(Photo+leafstarch+percstarch ~ volume, FUN=c(mean, se), 
                             keep.names=TRUE, data=Asat_starch)


#means(campaign)
Asat_starch_campaign <- summaryBy(Photo+leafstarch+percstarch ~ campaign + volume, FUN=c(mean, se), 
                                  keep.names=TRUE, data=Asat_starch)

Amax_starch_campaign <- summaryBy(Photo+leafstarch+percstarch ~ campaign + volume, FUN=c(mean, se), 
                                  keep.names=TRUE, data=Amax_starch)

#---------------------------------------------------------------------------------------
#plot stuff
manX <- seq(0, 18, 3)
meanX <- seq(0, 18, 3)
#---------------------------------------------------------------------------------------
#panel graph for each relationship with raw, overall mean, and mean by date


#Amax and starch%
windows(10,10)
par(mfrow=c(2,2))

#raw
plot(Photo ~ percstarch, data = Amax_starch, col=volume, ylim=c(0,50), xlim=c(0, 20), pch=pchs[volume], ylab="", 
     xlab=starchlab) 
title(ylab=maxlab, mgp=ypos)
title(main=raw, line=-1.5, font.main=1)
d_ply(Amax_starch, .(volume), function(x) add_trend_line("percstarch", "Photo", x, ))
#overall mean
plot(Photo.mean ~ percstarch.mean, data = Amax_starch_agg,col=volume, ylim=c(10, 40), xlim=c(0, 15),
     pch=pchs[volume], cex=2, ylab="", xlab=starchlab) 
legend("bottomleft", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n')
title(ylab=maxlab, mgp=ypos)
title(main=volmean, line=-1.5, font.main=1)
#means by date
plot(Photo.mean ~ percstarch.mean, data = Amax_starch_campaign,col=volume, ylim=c(10, 40), xlim=c(0, 20),
     pch=pch2[volume], xlab=starchlab, ylab="",cex=2.5,  lwd=lwds) 
title(ylab=maxlab, mgp=ypos)
title(main=datemean, line=-1.5, font.main=1)
d_ply(Amax_starch_campaign, .(volume), function(x) c(add_trend_line("percstarch.mean", "Photo.mean", x), 
                                                     text(x$Photo.mean~x$percstarch.mean, labels=unique(x$campaign), 
                                                      font=2,cex=.75)))

dev.copy2pdf(file="output/stats_plots/_____.pdf")
dev.off()

#Asat and starch%
windows(10,10)
par(mfrow=c(2,2))

#raw
plot(Photo ~ percstarch, data = Asat_starch, col=volume, ylim=c(0,30), xlim=c(0, 20), pch=pchs[volume], ylab="", 
     xlab=starchlab) 
title(ylab=maxlab, mgp=ypos)
title(main=raw, line=-1.5, font.main=1)
d_ply(Asat_starch, .(volume), function(x) add_trend_line("percstarch", "Photo", x, ))

#overall mean
plot(Photo.mean ~ percstarch.mean, data = Asat_starch_agg,col=volume, ylim=c(10, 30), xlim=c(0, 15),
     pch=pchs[volume], cex=2, ylab="", xlab=starchlab) 
legend("bottomleft", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n')
title(ylab=maxlab, mgp=ypos)
title(main=volmean, line=-1.5, font.main=1)
#means by date
plot(Photo.mean ~ percstarch.mean, data = Asat_starch_campaign,col=volume, ylim=c(10, 30), xlim=c(0, 20),
     pch=pch2[volume], xlab=starchlab, ylab="",cex=2.5,  lwd=lwds) 
title(ylab=satlab, mgp=ypos)
title(main=datemean, line=-1.5, font.main=1)
d_ply(Asat_starch_campaign, .(volume), function(x) c(add_trend_line("percstarch.mean", "Photo.mean", x), 
                                                     text(x$Photo.mean~x$percstarch.mean, labels=unique(x$campaign), 
                                                     font=2,cex=.75)))

dev.copy2pdf(file="output/stats_plots/____.pdf")
dev.off()

#---------------------------------------------------------------------------------------------------------------------
#row panel function
rowfunction <- function(df1, df2, df3, ylabel, ...){
  par(mfrow=c(1,3),omi=c(1,1,0.2,0.2), mar=c(0,0,0,0), cex=1.3)
  
  #raw
  plot(Photo ~ percstarch, data = df1, col=volume, xlim=c(0, 20),pch=pchs[volume], 
       axes=FALSE, xlab="", ylab="", ...)
  box()
  axis(1, at = manX, labels = manX)
  axis(2, labels=TRUE)
  mtext(ylabel, side=2, line=2.5, cex=1.3)
  title(main=raw, line=-1.5, font.main=1, adj=.1)

  d_ply(df1, .(volume), function(x) add_trend_line("percstarch", "Photo", x, ))
  
  #volume means
  plot(Photo.mean ~ percstarch.mean, data = df2, col=volume, xlim=c(0, 20),pch=pchs[volume], 
       ylab="", axes=FALSE,cex=2, ...)
  box()
  axis(1, at = meanX, labels = meanX)
  mtext(tnclab, side=1, line=2.5, cex=1.3)
  title(main=volmean, line=-1.5, font.main=1, adj=.1)
  legend("bottomleft", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n')
  #means by date
  plot(Photo.mean ~ percstarch.mean, data = df3, col=volume, xlim=c(0, 20),
       pch=pch2[volume], cex=2, ylab="", axes=FALSE, ...)
  box()
  axis(1, at = meanX, labels = meanX)
  title(main=datemean, line=-1.5, font.main=1, adj=.1)
  d_ply(df3, .(volume), function(x) c(add_trend_line("percstarch.mean", "Photo.mean", x),
                                      text(x$Photo.mean~x$percstarch.mean, labels=unique(x$campaign), 
                                           font=2,cex=.75)))
  dev.off()
}


#make plots---------------------------------------------------------------------------------

#Asat-TNC
windows(width = 16, height = 6)
pdf(file="output/stats_plots/asat_tnc.pdf", onefile = TRUE, width = 16, height = 6)
rowfunction(Asat_starch,Asat_starch_agg,Asat_starch_campaign, satlab, ylim=c(0, 30))

#Amax_TNC
windows(width = 16, height = 6)
pdf(file="output/stats_plots/amax_tnc.pdf", onefile = TRUE, width = 16, height = 6)
rowfunction(Amax_starch,Amax_starch_agg,Amax_starch_campaign, maxlab, ylim=c(0, 45))


