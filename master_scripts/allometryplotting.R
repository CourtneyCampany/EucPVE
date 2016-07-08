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

#leaf area interpolated--------------------------------------------------------------------------
leafarea_time <- read.csv("calculated data/cumulative_leaf_area.csv")
  #leafarea_time <- read.csv("calculated data/LApred_volume.csv")
  leafarea_time <-datevol_func (leafarea_time)


#PLOT
#3 panel plot with allometry mean data (with and without "free" seedling)
startday <- as.Date(strptime("01-01-2013", format = "%m-%d-%Y", tz=""))
xlim1 <- as.Date(strptime("01-05-2013", format = "%m-%d-%Y", tz=""))
xlim2 <- as.Date(strptime("06-01-2013", format = "%m-%d-%Y", tz=""))
xAT <- seq.Date(startday, by="month", length=6,format = "%m-%d-%Y")
xlimdays <- c(xlim1, xlim2)

# windows(7,10)

par(cex.axis=1.21, cex.lab=1.51,las=1,mgp=c(3.5,1,0),mfrow=c(3,1),  
    omi=c(.5,0,0.1,0.1))   

# First Panel
par(mar=c(0,7,2,2))
plot(height.mean ~ Date, data=height_agg,type='n',ylab=expression(Height~~(cm)),  
     ylim=c(20,135), axes=FALSE, xlab="", xlim=xlimdays)  
axis.Date(1, at=xAT, labels=FALSE) #axis needs no labels
axis(2, labels=TRUE, at=c(0,20,40,60,80,100,120))  


with(height_agg, arrows(Date, height.mean, Date, height.mean+height.se, angle=90, col=palette(),length=0.03, cex=2))
with(height_agg, arrows(Date, height.mean, Date, height.mean-height.se, angle=90, col=palette(),length=0.03, cex=2))
points(height.mean ~ Date, data=height_agg,pch=pchs[volume], cex=2, col = volume)      
box()
# text(x=15710, 129, "(a)", cex=1.51)
text(x=15855, 129, "(a)", cex=1.51)
legend("topleft", leglab, pch=c(rep(16,6),17),text.font=3,  title=vollab, cex=1.21, col=palette(), bty='n', inset=.01)

# Second panel   
par(mar=c(0,7,0,2))
plot(diameter.mean ~ Date, data=diam_agg, type='n',ylab=expression(Diameter~~(mm)),ylim=c(0,17), 
     axes = FALSE, xlab="",xlim=xlimdays)
axis.Date(1, at=xAT, labels=FALSE)  
axis(2)     

with(diam_agg, arrows(Date, diameter.mean, Date, diameter.mean+diameter.se, angle=90, col=palette(),length=0.03, cex=2))
with(diam_agg, arrows(Date, diameter.mean, Date, diameter.mean-diameter.se, angle=90, col=palette(),length=0.03, cex=2))
points(diameter.mean ~ Date, data=diam_agg, pch=pchs[volume], cex=2,col = volume)
box()
text(x=15855, 16, "(b)", cex=1.51)

#third panel
par(mar=c(2,7,0,2))
plot(canopysqm.mean ~ Date, data=leafarea_time, axes=FALSE,xlab="", ylab=LAm2,
     type='n', ylim=c(0,.7),xlim=xlimdays)
box()
axis(2, labels=TRUE, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))  
axis.Date(1, at=xAT, label=TRUE, format="%b") 
#title(ylab=LAm2, mgp=ypos)
with(leafarea_time, arrows(Date, canopysqm.mean, Date, canopysqm.mean+canopysqm.se, angle=90, 
                           col=volume,length=0.03, cex=2))
with(leafarea_time, arrows(Date, canopysqm.mean, Date, canopysqm.mean-canopysqm.se, angle=90, 
                           col=volume,length=0.03, cex=2))
d_ply(leafarea_time, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                                                   col=x$volume, pch = pchs[x$volume],cex=2))
text(x=15855, .65, "(c)", cex=1.51)
