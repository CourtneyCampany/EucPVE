#this script is for sourcing the allometry plots into master
source("functions and packages/startscripts.R")

height <- read.csv("calculated data/height.csv")
diam <- read.csv("calculated data/diameter.csv")

#height treatment means------------------------------------------------------------------------
height_agg <- summaryBy(height ~ Date + volume , data = height, FUN = c(mean,se))
height_agg<- datevol_func(height_agg)

#diameter treatment means-------------------------------------------------------------------------
diam_agg <- summaryBy(diameter ~ Date + volume , data = diam, FUN = c(mean,se))
diam_agg<- datevol_func(diam_agg)


#PLOT
#3 panel plot with allometry mean data (with and without "free" seedling)
startday <- as.Date(strptime("01-01-2013", format = "%m-%d-%Y", tz=""))
xlim1 <- as.Date(strptime("01-05-2013", format = "%m-%d-%Y", tz=""))
xlim2 <- as.Date(strptime("06-01-2013", format = "%m-%d-%Y", tz=""))
xAT <- seq.Date(startday, by="month", length=6,format = "%m-%d-%Y")
xlimdays <- c(xlim1, xlim2)

ltys <- c(rep(1,6),2)

windows(7,10)
#png(filename = "ngia_plots/heightdiam.png", width = 7, height = 10, units = "in", res= 600)

par(cex.axis=1.25, cex.lab=1.25,las=1,mgp=c(3,1,0),mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))   

# First Panel
par(mar=c(0,7,2,2))
plot(height.mean ~ Date, data=height_agg,type='n',ylab=expression(Height~~(cm)),  
     ylim=c(20,125), axes=FALSE, xlab="", xlim=xlimdays)  
  axis.Date(1, at=xAT, labels=FALSE) #axis needs no labels
  axis(2, labels=TRUE, at=c(0,20,40,60,80,100,120))  

d_ply(height_agg, .(volume), function(x) points(x$height.mean ~ x$Date,  col=x$volume, lty = ltys[x$volume],
                                              cex=2, type='l', lwd=2))    
box()

legend("topleft", leglab, lty=ltys, lwd=2, title=vollab, cex=1, col=palette(), bty='n', inset=.02)
mtext(expression(paste(italic("Eucalpytus tereticornis"), " seedlings", sep=" ")), side=3,line=.5, cex=1.25)

# Second panel   
par(mar=c(0,7,0,2))
plot(diameter.mean ~ Date, data=diam_agg, type='n',ylab=expression(Diameter~~(mm)),ylim=c(0,15), 
     axes = FALSE, xlab="",xlim=xlimdays)
  axis.Date(1, at=xAT, label=TRUE, format="%b") 
  axis(2)     

d_ply(diam_agg, .(volume), function(x) points(x$diameter.mean ~ x$Date,  col=x$volume, lty = ltys[x$volume],
                                              cex=2, type='l', lwd=2))
box()

dev.off()

# lo <- loess(test$diameter.mean~test$Date)
# 
# test <- diam_agg[diam_agg$volume==25,]
# 
# smoothingSpline = smooth.spline(test$Date, test$diameter.mean, spar=0.5)
# plot(test$Date, test$diameter.mean)
# lines(smoothingSpline)


###can we use this to test H*D ?????

sizeindex <- merge(height_agg, diam_agg)

sizeindex$SI <- with(sizeindex, height.mean * diameter.mean)
sizeindex$logSI <- with(sizeindex, log10(SI))

windows(7,7)
png(filename = "ngia_plots/sizeindex.png", width = 7, height = 7, units = "in", res= 600)
par(mar=c(4,6,2,2),cex.axis=1.25, cex.lab=1.25,las=1,mgp=c(3.75,1,0))
plot(SI~ Date, data=sizeindex, type='n',ylab=expression(Size~Index~~(calliper~x~height)),
     axes = FALSE, xlab="",xlim=xlimdays)
axis.Date(1, at=xAT, label=TRUE, format="%b") 
axis(2)     

d_ply(sizeindex, .(volume), function(x) points(x$SI ~ x$Date,  col=x$volume, lty = ltys[x$volume],
                                              cex=2, type='l', lwd=2))
box()
legend("topleft", leglab, lty=ltys, lwd=2, title=vollab, cex=1, col=palette(), bty='n', inset=.02)
(expression(paste(italic("Eucalpytus tereticornis"), " seedlings", sep=" ")), side=3,line=.5, cex=1.25)

dev.off()

##same logged
library(magicaxis)

windows(7,7)
png(filename = "ngia_plots/logsizeindex.png", width = 7, height = 7, units = "in", res= 600)
par(mar=c(4,6,2,2),cex.axis=1.25, cex.lab=1.25,las=1,mgp=c(3.75,1,0))
plot(logSI~ Date, data=sizeindex, type='n',ylab=expression(Size~Index~~(calliper~x~height)),
     axes = FALSE, xlab="",xlim=xlimdays, ylim=c(2, 3.3))
axis.Date(1, at=xAT, label=TRUE, format="%b") 
magaxis(side=2, unlog=2, frame.plot=TRUE)
    

d_ply(sizeindex, .(volume), function(x) points(x$logSI ~ x$Date,  col=x$volume, lty = ltys[x$volume],
                                               cex=2, type='l', lwd=2))
box()
legend("topleft", leglab, lty=ltys, lwd=2, title=vollab, cex=1, col=palette(), bty='n', inset=.04)
mtext(expression(paste(italic("Eucalpytus tereticornis"), " seedlings", sep=" ")), side=3,line=.5, cex=1.25)

dev.off()

###SI vs volume (only containers)
pots <- sizeindex[sizeindex$volume != "1000",]
pots$volume <- as.numeric(as.character(pots$volume))


pots_final <- pots[pots$Date == max(pots$Date), ]
pots_final <- pots_final[c(6,1:5),]

potcols <- c("red"  ,"#D4002A" ,"#AA0055" ,"#7F007F", "#5500AA" ,"#2A00D4")
unordercols <- c("#D4002A" ,"#AA0055" ,"#7F007F", "#5500AA" ,"#2A00D4", "red")

windows(7,7)
png(filename = "ngia_plots/eucSI.png", width =8, height = 7, units = "in", res= 600)
par(mar=c(6,7,2,2),cex.axis=1.4, cex.lab=1.75,las=0,mgp=c(4.5,1,0))
plot(logSI~ log10(volume), data=pots, xlab="Container volume (L)", ylab="",axes=FALSE,
     ylim=c(1, 3.5), xlim=c(.68,3.4), cex=1.25, col=unordercols)
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
points(logSI~ log10(volume), data=pots_final, col=potcols, pch=16, cex=1.6)
box(bty="l", lwd=2)
title(expression(paste(italic("Eucalpytus tereticornis"), " seedlings", sep=" ")),  line=-2,cex.main=1.75)
dev.off()



