#this script is for sourcing the allometry plots into master
source("functions and packages/startscripts.R")

height <- read.csv("calculated data/height.csv")
diam <- read.csv("calculated data/diameter.csv")
leafno <- read.csv("calculated data/leaf_number.csv")

#height treatment means
height_agg <- summaryBy(height ~ Date + volume , data = height, FUN = c(mean,se))
  height_agg<- datevol_func(height_agg)
#height treatment means no free plant, and treatment means
height_allpot <- subset(height, volume != "1000")
  height_allpot <- droplevels(height_allpot)
height_allpot_agg <- summaryBy(height ~ Date + volume, data = height_allpot, FUN = c(mean,se))
  height_allpot_agg<- datevol_func(height_allpot_agg)

#diameter treatment means
diam_agg <- summaryBy(diameter ~ Date + volume , data = diam, FUN = c(mean,se))
  diam_agg<- datevol_func(diam_agg)
#treatment no free plant
diam_allpot <- subset(diam, volume != "1000")
diam_allpot_agg <- summaryBy(diameter ~ Date + volume, data = diam_allpot, FUN = c(mean,se))
  diam_allpot_agg<- datevol_func(diam_allpot_agg)

#leafno treatment means
leafno_agg <- summaryBy(count ~ Date + volume , data = leafno, FUN = c(mean,se))
  leafno_agg<- datevol_func(leafno_agg)
#leafno treatment means no free plant
leafno_allpot <- subset(leafno, volume != "1000")
leafno_allpot_agg <- summaryBy(count ~ Date + volume, data = leafno_allpot, FUN = c(mean,se))
  leafno_allpot_agg<- datevol_func(leafno_allpot_agg)

#PLOT
#3 panel plot with allometry mean data (with and without "free" seedling)

xAT <- seq.Date(from=as.Date("2013-1-1"), length=20, by="month")

#all plants
windows()
#png(filename = "output/presentations/growth.png", width = 8.5, height = 9.5, units = "in", res= 400)
par(cex.axis=1.5, cex.lab=1.5,
    mfrow=c(3,1),  # rows and columns of plots
    omi=c(1,0,0.1,0.1),  # outer margin (inches)
    mar=c(0,7,0,0))   # margin around plots (they are tight together)   
# First Panel
plot(height.mean ~ Date, data=height_agg,type='n',ylab=expression(Height~~(cm)),  
     ylim=c(20,125), axes=FALSE, xlab="")  
axis.Date(1, at=xAT, labels=FALSE) #axis needs no labels
axis(2, labels=TRUE)  # Y axis
legend("topleft", leglab, pch=c(rep(16,6),17),text.font=3, inset=0.02, title=expression(Pot~volume~(l)), 
       cex=1.5, col=palette(), bty='n')

with(height_agg, arrows(Date, height.mean, Date, height.mean+height.se, angle=90, col=palette(),length=0.03))
with(height_agg, arrows(Date, height.mean, Date, height.mean-height.se, angle=90, col=palette(),length=0.03))
points(height.mean ~ Date, data=height_agg,pch=pchs[volume], cex=PTcex, col = volume)      
box()

# Second panel         
plot(diameter.mean ~ Date, data=diam_agg, type='n',ylab=expression(Diameter~~(mm)),ylim=c(0,15), axes = FALSE, xlab="")
axis.Date(1, at=xAT, labels=FALSE)  
axis(2)     

with(diam_agg, arrows(Date, diameter.mean, Date, diameter.mean+diameter.se, angle=90, col=palette(),length=0.03))
with(diam_agg, arrows(Date, diameter.mean, Date, diameter.mean-diameter.se, angle=90, col=palette(),length=0.03))
points(diameter.mean ~ Date, data=diam_agg, pch=pchs[volume],cex=PTcex,col = volume)
box()

#third panel
plot(count.mean ~ Date, data=leafno_agg, 
     type='n',ylab=expression(Leaf~Number~~("#")),ylim=c(0,250), axes = FALSE, xlab="")
axis.Date(1, at=xAT, labels=TRUE)  #axis needs no labels
axis(2)     

with(leafno_agg, arrows(Date, count.mean, Date, count.mean+count.se, angle=90, col=palette(),length=0.03))
with(leafno_agg, arrows(Date, count.mean, Date, count.mean-count.se, angle=90, col=palette(),length=0.03))
points(count.mean ~ Date, data=leafno_agg,  pch=pchs[volume], cex=PTcex,col = volume)
box()

#dev.copy2pdf(file= "output/allometry.pdf")
#dev.off()

#no free--------------------------------------------------------------------------------
#PLOT with no FREE plant
windows()
par(cex.axis=1.5,  cex.lab=1.5,mfrow=c(3,1),  omi=c(1,0,0.1,0.1),  mar=c(0,7,0,0))  

# First Panel
plot(height.mean ~ Date, data=height_allpot_agg,
     type='n',ylab=expression(Height~~(cm)),  ylim=c(25,100), axes=FALSE, xlab="") 

axis.Date(1, at=xAT, labels=FALSE) 
axis(2, labels=TRUE)  
legend("topleft", leglab2, pch=16, text.font=3, inset=0.02, title=expression(Pot~volume~(l)), 
       cex=1.5,col=palette(), bty='n')
with(height_allpot_agg, arrows(Date, height.mean, Date, height.mean+height.se, angle=90, col=volume,length=0.03))
with(height_allpot_agg, arrows(Date, height.mean, Date, height.mean-height.se, angle=90, col=volume,length=0.03))
points(height.mean ~ Date, data=height_allpot_agg,pch=16, cex=PTcex,col = volume)
box()

# Second panel         
plot(diameter.mean ~ Date, data=diam_allpot_agg, type='n',ylab=expression(Diameter~~(mm)),ylim=c(0,10), 
     axes = FALSE, xlab="")
axis.Date(1, at=xAT, labels=FALSE)  
axis(2)  
with(diam_allpot_agg, arrows(Date, diameter.mean, Date, diameter.mean+diameter.se, angle=90, col=volume,length=0.03))
with(diam_allpot_agg, arrows(Date, diameter.mean, Date, diameter.mean-diameter.se, angle=90, col=volume,length=0.03))
points(diameter.mean ~ Date, data=diam_allpot_agg, pch=16, cex=PTcex, col = volume)
box()

#third panel
plot(count.mean ~ Date, data=leafno_allpot_agg, 
     type='n',ylab=expression(Leaf~Number~~("#")), ylim=c(0,120), axes = FALSE, xlab="")
axis.Date(1, at=xAT, labels=TRUE)  
axis(2)    
with(leafno_allpot_agg, arrows(Date, count.mean, Date, count.mean+count.se, angle=90, col=volume,length=0.03))
with(leafno_allpot_agg, arrows(Date, count.mean, Date, count.mean-count.se, angle=90, col=volume,length=0.03))
points(count.mean ~ Date, data=leafno_allpot_agg, pch=16, cex=PTcex, col = volume)
box()

#dev.copy2pdf(file= "output/allometrypots.pdf")
#dev.off()