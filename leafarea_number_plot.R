#this script is for sourcing the allometry plots into master
source("functions and packages/startscripts.R")

leafno <- read.csv("calculated data/leaf_number.csv")

#leafno treatment means
leafno_agg <- summaryBy(count ~ Date + volume , data = leafno, FUN = c(mean,se))
leafno_agg<- datevol_func(leafno_agg)
#leafno treatment means no free plant
leafno_allpot <- subset(leafno, volume != "1000")
leafno_allpot_agg <- summaryBy(count ~ Date + volume, data = leafno_allpot, FUN = c(mean,se))
leafno_allpot_agg<- datevol_func(leafno_allpot_agg)


xAT <- seq.Date(from=as.Date("2013-1-1"), length=20, by="month")

#leaf area
leafarea_time <- read.csv("calculated data/cumulative leaf area.csv")
leafarea_time <-datevol_func (leafarea_time)


####two panel with heigh and diameter

#third panel
plot(count.mean ~ Date, data=leafno_agg, 
     type='n',ylab=expression(Leaf~Number~~("#")),ylim=c(0,250), axes = FALSE, xlab="")
axis.Date(1, at=xAT, labels=TRUE)  #axis needs no labels
axis(2)     

with(leafno_agg, arrows(Date, count.mean, Date, count.mean+count.se, angle=90, col=palette(),length=0.03))
with(leafno_agg, arrows(Date, count.mean, Date, count.mean-count.se, angle=90, col=palette(),length=0.03))
points(count.mean ~ Date, data=leafno_agg,  pch=pchs[volume], cex=PTcex,col = volume)
box()


png(filename = "output/presentations/LA_Leafnum.png", width = 10.5, height = 9.5, units = "in", res= 400)

#windows()
par(cex.axis=1.5,  cex.lab=1.5, mfrow=c(2,1),  omi=c(1,0,0.1,0.1),  mar=c(0,7,0,0))  

# First Panel

plot(count.mean ~ Date, data=leafno_agg, 
     type='n',ylab=expression(Leaf~Number~~("#")), ylim=c(0,250), axes = FALSE, xlab="")
axis.Date(1, at=xAT, labels=FALSE) 
axis(2, labels=TRUE)  
with(leafno_agg, arrows(Date, count.mean, Date, count.mean+count.se, angle=90, col=volume,length=0.03))
with(leafno_agg, arrows(Date, count.mean, Date, count.mean-count.se, angle=90, col=volume,length=0.03))
points(count.mean ~ Date, data=leafno_agg, pch=pchs[volume], cex=PTcex, col = volume)

legend("topleft", leglab, pch=pchs,text.font=3, inset=0.02, title=expression(Pot~volume~(l)), 
       col=palette(), bty='n')
box()

#2nd panel
plot(canopysqm.mean ~ Date, data=leafarea_time, axes=FALSE,xlab="", ylab="",
     type='n', ylim=c(0,.6))
box()
axis.Date(1, at=xAT, labels=TRUE) 
axis(2, labels=TRUE)  
title(ylab=LAm2, mgp=ypos)
with(leafarea_time, arrows(Date, canopysqm.mean, Date, canopysqm.mean+canopysqm.se, angle=90, 
                           col=volume,length=0.03))
with(leafarea_time, arrows(Date, canopysqm.mean, Date, canopysqm.mean-canopysqm.se, angle=90, 
                           col=volume,length=0.03))
d_ply(leafarea_time, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                                                   col=x$volume, type="b", pch = pchs[x$volume],cex=PTcex,))


dev.off()
