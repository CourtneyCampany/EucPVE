#this script is for sourcing the allometry plots into master
source("functions and packages/startscripts.R")

height <- read.csv("calculated data/height.csv")
diam <- read.csv("calculated data/diameter.csv")
leafno <- read.csv("calculated data/leaf_number.csv")

#height treatment means------------------------------------------------------------------------
height_agg <- summaryBy(height ~ Date + volume , data = height, FUN = c(mean,se))
  height_agg<- datevol_func(height_agg)

#diameter treatment means-------------------------------------------------------------------------
diam_agg <- summaryBy(diameter ~ Date + volume , data = diam, FUN = c(mean,se))
  diam_agg<- datevol_func(diam_agg)

#leafno treatment means---------------------------------------------------------------------------
leafno_agg <- summaryBy(count ~ Date + volume , data = leafno, FUN = c(mean,se))
  leafno_agg<- datevol_func(leafno_agg)

#leaf area interpolated--------------------------------------------------------------------------
leafarea_time <- read.csv("calculated data/cumulative leaf area.csv")
  leafarea_time <-datevol_func (leafarea_time)


#PLOT
#3 panel plot with allometry mean data (with and without "free" seedling)
xAT <- seq.Date(from=as.Date("2013-1-1"), length=20, by="month")

#all plants
windows(14,12)
#png(filename = "output/presentations/growth.png", width = 10.5, height = 9.5, units = "in", res= 400)
par(cex.axis=1.5, cex.lab=1.5,
    las=1,
    mgp=c(3.5,1,0),
    mfrow=c(3,1),  # rows and columns of plots
    omi=c(.5,0,0.1,0.1),  # outer margin (inches)
    mar=c(0,7,0,0))   # margin around plots (they are tight together)   
# First Panel
plot(height.mean ~ Date, data=height_agg,type='n',ylab=expression(Height~~(cm)),  
     ylim=c(0,135), axes=FALSE, xlab="")  
axis.Date(1, at=xAT, labels=FALSE) #axis needs no labels
axis(2, labels=TRUE, at=c(0,20,40,60,80,100,120))  
legend("topleft", leglab, pch=c(rep(16,6),17),text.font=3, inset=0.02, title=expression(Pot~volume~(l)), 
       cex=1.5, col=palette(), bty='n')

with(height_agg, arrows(Date, height.mean, Date, height.mean+height.se, angle=90, col=palette(),length=0.03, cex=2))
with(height_agg, arrows(Date, height.mean, Date, height.mean-height.se, angle=90, col=palette(),length=0.03, cex=2))
points(height.mean ~ Date, data=height_agg,pch=pchs[volume], cex=2, col = volume)      
box()
text(x=as.Date("2013-05-21"), 128, "(a)", cex=2)

# Second panel         
plot(diameter.mean ~ Date, data=diam_agg, type='n',ylab=expression(Diameter~~(mm)),ylim=c(0,17), axes = FALSE, xlab="")
axis.Date(1, at=xAT, labels=FALSE)  
axis(2)     

with(diam_agg, arrows(Date, diameter.mean, Date, diameter.mean+diameter.se, angle=90, col=palette(),length=0.03, cex=2))
with(diam_agg, arrows(Date, diameter.mean, Date, diameter.mean-diameter.se, angle=90, col=palette(),length=0.03, cex=2))
points(diameter.mean ~ Date, data=diam_agg, pch=pchs[volume], cex=2,col = volume)
box()
text(x=as.Date("2013-05-21"), 16, "(b)", cex=2)

#third panel

plot(canopysqm.mean ~ Date, data=leafarea_time, axes=FALSE,xlab="", ylab="",
     type='n', ylim=c(0,.7))
box()
axis(2, labels=TRUE, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))  
axis.Date(1, at=xAT, labels=TRUE)  
title(ylab=LAm2, mgp=ypos)
with(leafarea_time, arrows(Date, canopysqm.mean, Date, canopysqm.mean+canopysqm.se, angle=90, 
                           col=volume,length=0.03, cex=2))
with(leafarea_time, arrows(Date, canopysqm.mean, Date, canopysqm.mean-canopysqm.se, angle=90, 
                           col=volume,length=0.03, cex=2))
d_ply(leafarea_time, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                                                   col=x$volume, pch = pchs[x$volume],cex=2))
text(x=as.Date("2013-05-21"), .65, "(c)", cex=2)

dev.copy2pdf(file= "output/allometry.pdf")
dev.off()


# plot(count.mean ~ Date, data=leafno_agg, 
#      type='n',ylab=expression(Leaf~Number~~("#")),ylim=c(0,250), axes = FALSE, xlab="")
# axis.Date(1, at=xAT, labels=TRUE)  #axis needs no labels
# axis(2)     
# 
# with(leafno_agg, arrows(Date, count.mean, Date, count.mean+count.se, angle=90, col=palette(),length=0.03))
# with(leafno_agg, arrows(Date, count.mean, Date, count.mean-count.se, angle=90, col=palette(),length=0.03))
# points(count.mean ~ Date, data=leafno_agg,  pch=pchs[volume], cex=PTcex,col = volume)
# box()



