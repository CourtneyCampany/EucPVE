
airvars <- downloadTOA5(c("FACE","airvars"), maxnfiles=200)


library(plantecophys)


airvars <- within(airvars, {
  
  Tair <- (AirTC_1_Avg + AirTC_2_Avg)/2
  RH <- (RH_1_Avg + RH_2_Avg)/2
  VPD <- RHtoVPD(RH,Tair)
  Date <- as.Date(DateTime)
})


clim <- summarize(group_by(airvars, Date),
                  Tmax = max(Tair, na.rm=TRUE),
                  Tmin = min(Tair, na.rm=TRUE),
                  Tmean = mean(Tair, na.rm=TRUE),
                  VPDmin = mean(VPD, na.rm=TRUE),
                  VPDmean = mean(VPD, na.rm=TRUE),
                  VPDmax = max(VPD, na.rm=TRUE))
clim$Tmin[clim$Tmin < -10] <- 10


addpoly <- function(x,y1,y2,col=alpha("lightgrey",0.8),...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=NA,...)
}


windows(7,5)
par(mar=c(5,5,2,2), cex.axis=0.8)
with(clim, {
  plot(Date, Tmax, type='l', ylim=c(-1,45), col="dimgrey",
       xlab="",axes=FALSE,ylab=expression(Daily~T[min]~and~T[max]~(degree*C)),
       panel.first={
         abline(h=seq(0,40,by=10),lty=5)
         addpoly(Date, Tmin, Tmax,col="lightgrey")
        })
  lines(Date, Tmin, col="dimgrey")
  
})
axis(2)
xAT <- seq.Date(as.Date("2012-7-1"), by="2 months", length=50)
xATminor <- seq.Date(as.Date("2012-7-1"), by="1 month", length=100)
axis.Date(1, at=xAT, format="%b-'%y" )
axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
axis(2)
box()
dev.copy2pdf(file="output/figures/Eucface_tmintmax.pdf")




windows(7,5)
par(mar=c(5,5,2,2), cex.axis=0.8)
with(clim, {
  plot(Date, VPDmax, type='l', ylim=c(0,8), col="dimgrey",
       xlab="",axes=FALSE,ylab=expression(Daily~VPD[min]~and~VPD[max]~(kPa)),
       panel.first={
          abline(h=seq(0,8,by=1),lty=5)
         addpoly(Date, VPDmin, VPDmax,col="lightgrey")
       })
  lines(Date, VPDmin, col="dimgrey")
  
})
axis(2)
xAT <- seq.Date(as.Date("2012-7-1"), by="2 months", length=50)
xATminor <- seq.Date(as.Date("2012-7-1"), by="1 month", length=100)
axis.Date(1, at=xAT, format="%b-'%y" )
axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
box()
dev.copy2pdf(file="output/figures/Eucface_vpd.pdf")


