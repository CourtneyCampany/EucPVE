source("functions and packages/startscripts.R")
source("functions and packages/gamplotfunctions.R")

require(lubridate)
require(doBy)
library(plantecophys)
library(scales)

eucpve_met<- read.csv("calculated data/eucpve_met.csv")
  eucpve_met$Time15<- ymd_hms(eucpve_met$DateTime15)
  eucpve_met$month <- month(eucpve_met$DateTime15)
  names(eucpve_met)[3:5]<-c("Par", "Temperature", "RH")
  eucpve_met$VPD <- RHtoVPD(eucpve_met$RH, eucpve_met$Temperature)
  eucpve_met$Date <- as.Date(eucpve_met$DateTime15, "%Y-%m-%d")

###Figure with Temp, VPD, and PAR

#first get exact experimente dates
met_120 <- subset(eucpve_met[3:9], Date >= "2013-01-21" & Date <= "2013-05-21")

#new dfr with max min
airvars <- summaryBy(Par+Temperature+VPD~Date, met_120, FUN =c(min, max, mean))


##plot---------------------------------------------------------------------
xAT <- seq.Date(as.Date("2012-1-21"), by="month", length=50)
tminlab <- expression(T[min])
tmaxlab <- expression(T[max])
vpdlab <- expression(Daily~VPD[min]~and~VPD[max]~(kPa))


windows(12,8)
####multipanel plot of 
par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3.5,1,0),mfrow=c(3,1),  
    omi=c(.5,0,0.1,0.1))
    
#1=temp
par(mar=c(0,7,2,2))
with(airvars, {
  plot(Date, Temperature.max, type='l', col="red",ylim=c(0,40),lwd=2,
       xlab="",axes=FALSE,ylab=expression(Daily~T[min]~and~T[max]~(degree*C)),
       panel.first={
         addpoly(Date, Temperature.min, Temperature.max,col="grey98")
       })
  lines(Date, Temperature.min, col="blue", lwd=2)
  
})
axis(2)
axis.Date(1, at=xAT, labels=FALSE)
legend("topright",col=c("red","blue"),lty=1,lwd=2,legend=c(tmaxlab,tminlab), inset=.01, cex=1.51)
box()

#2=PPFD
par(mar=c(0,7,0,2))
plot(Par.max~Date,type="l",col="orange",data=airvars, xlab="", lwd=2,ylim=c(0, 2750),
     ylab=expression(Daily~PPFD[max]~~(mol~m^-2~s^-1)),axes=FALSE)
axis(2)
axis.Date(1, at=xAT, labels=FALSE)
box()

#3= VPD
par(mar=c(2,7,0,2))
plot(VPD.max~Date, type="l",col="forestgreen",xlab="",lwd=2,ylim=c(0,5.5),
    ylab=expression(Daily~VPD[max]~~(kPa)),data=airvars, axes=FALSE)
axis(2)
axis.Date(1, at=xAT)
box()

#dev.copy2pdf(file= "output/.pdf")
#dev.off()




# #vpd
# par(mar=c(5,5,2,2), cex.axis=0.8)
# with(airvars, {
#   plot(Date, VPD.max, type='l', col="forestgreen",ylim=c(0,5),
#        xlab="",axes=FALSE,ylab=expression(Daily~VPD[min]~and~VPD[max]~(kPa)),
#        panel.first={
#          addpoly(Date, VPD.min, VPD.max,col="darkseagreen1")
#        })
#   lines(Date, VPD.min, col="forestgreen")
#   
# })
# axis(2)
# 
# axis.Date(1, at=xAT )
# box()




