source("functions and packages/startscripts.R")

#Read in spot A measurements and merge plot design
source("read data scripts/physiology read data.R")

PS <- merge(plotsumm, subset(gasexchange, select = c("campaign", "plot", "pot", "CO2", "CO2R",
                                                     "Photo", "Cond", "Ci", "Tleaf", "VpdL", "Area", "Trmmol")))

#run function to add campaign date
PS <- add_campaign_date(PS)

#factor designations
PS$type <- factor(ifelse (PS$CO2 == "400", "Asat", "Amax"))

#pot volume means
PS_agg <- aggregate(cbind(Photo, Cond, Ci, Trmmol) ~ campaign + type + volume, data=PS, FUN=mean)

#subset Amax
PSmax <- subset(PS, type=="Amax")
PSmax <- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSmax)
  PSmax$volume <- as.factor(PSmax$volume)

PSagg <- summaryBy(Photo ~ Date + volume, data=PSmax, 
                   FUN=c(mean,sd,length))
  PSagg$SE <- with(PSagg, Photo.sd/sqrt(Photo.length))
  PSagg$volume <- as.factor(PSagg$volume)

#subset Asat
PSsat <- subset(PS, type=="Asat")
PSsat <- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSsat)
  PSsat$volume <- as.factor(PSsat$volume)

PSsat_agg <- summaryBy(Photo ~ Date + volume, data=PSsat, 
                       FUN=c(mean,sd,length))

PSsat_agg$SE <- with(PSsat_agg, Photo.sd/sqrt(Photo.length))
  PSsat_agg$volume <- as.factor(PSsat_agg$volume)
  
# asat ----------------------------------------------------------------------------------- 
  
 windows(7,7) 
  
  plot(Photo.mean ~ Date, data=PSsat_agg, xlab="", ylab=satlab,
       ylim=c(10,27))
  with(PSsat_agg, arrows(Date, Photo.mean, Date, Photo.mean+SE, angle=90, col=palette(),length=0.05))
  with(PSsat_agg, arrows(Date, Photo.mean, Date, Photo.mean-SE, angle=90, col=palette(),length=0.05))
  d_ply(PSsat_agg, .(volume), function(x) points(x$Photo.mean ~ x$Date,  
                                                 col=x$volume, type="b", pch = pchs[x$volume], cex=1.6))
  legend("topright", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, col=palette(), bty='n')
  

#----------------------------------------------------------------------------------------------------
#two panel graph of A
par(mfrow=c(2,1), omi=c(1,0,0.5,0.5),mar=c(0,5,0,0),mgp = c(2.5, 1, 0))
#Amax
plot(Photo.mean ~ Date, data=PSagg, axes=FALSE,xlab="", ylab="",
     type='n', ylim=c(0,40))
box()
axis(2, labels=TRUE)  
title(ylab=maxlab)
with(PSagg, arrows(Date, Photo.mean, Date, Photo.mean+SE, angle=90, col=palette(),length=0.05))
with(PSagg, arrows(Date, Photo.mean, Date, Photo.mean-SE, angle=90, col=palette(),length=0.05))
d_ply(PSagg, .(volume), function(x) points(x$Photo.mean ~ x$Date,  
                                           col=x$volume, type="b", pch = pchs[x$volume],cex=1.6))
legend("bottomright", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, col=palette(), bty='n')


#Asat
plot(Photo.mean ~ Date, data=PSsat_agg, xlab="", ylab=satlab,
     ylim=c(0,40))
with(PSsat_agg, arrows(Date, Photo.mean, Date, Photo.mean+SE, angle=90, col=palette(),length=0.05))
with(PSsat_agg, arrows(Date, Photo.mean, Date, Photo.mean-SE, angle=90, col=palette(),length=0.05))
d_ply(PSsat_agg, .(volume), function(x) points(x$Photo.mean ~ x$Date,  
                                               col=x$volume, type="b", pch = pchs[x$volume], cex=1.6))
