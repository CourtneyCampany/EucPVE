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

#need total daily ppfd
ppfd<- met_120[, c(1, 4:5, 7)]
  ppfd$par15_mol <- ppfd$Par/1000000
  ppfd$par15_mol_s <- ppfd$par15_mol*15*60

daypar <- summaryBy(par15_mol_s ~ Date, data=ppfd,FUN=sum, keeo.names=TRUE)
names(daypar)[2] <- "PPFD_day"



##plot---------------------------------------------------------------------
xAT <- seq.Date(as.Date("2013-1-21"), by="month", length=50)
tminlab <- expression(T[min])
tmaxlab <- expression(T[max])
vpdlab <- expression(VPD[min]~and~VPD[max]~(kPa))


windows(7,7)
####multipanel plot of 
par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3.5,1,0),mfrow=c(3,1),  
    omi=c(.5,0,0.1,0.1))
    
#1=temp
par(mar=c(0,7,2,2))
with(airvars, {
  plot(Date, Temperature.max, type='l', col="red",ylim=c(0,40),lwd=2,
       xlab="",axes=FALSE,ylab=expression(T[min]~and~T[max]~(degree*C)),
       panel.first={
         addpoly(Date, Temperature.min, Temperature.max,col="grey95")
       })
  lines(Date, Temperature.min, col="blue", lwd=2)
  
})
axis(2)
axis.Date(1, at=xAT, labels=FALSE)
legend("topright",col=c("red","blue"),lty=1,lwd=2,legend=c(tmaxlab,tminlab), inset=.01, cex=1.51, bty='n')
box()
text(x=15725, 39, "(a)", cex=1.51)

#2=PPFD
par(mar=c(0,7,0,2))
plot(PPFD_day~Date,type="l",col="orange",data=daypar, xlab="", lwd=2,ylim=c(0, 65),
     ylab=expression(PPFD[day]~~(mols~m^-2~d^-1)),axes=FALSE)
axis(2)
axis.Date(1, at=xAT, labels=FALSE)
box()
text(x=15725, 62, "(b)", cex=1.51)

#3= VPD
par(mar=c(2,7,0,2))
test <- plot(VPD.max~Date, type="l",col="forestgreen",xlab="",lwd=2,ylim=c(0,5.2),
    ylab=expression(VPD[max]~~(kPa)),data=airvars, axes=FALSE)
axis(2)
axis.Date(1, at=xAT)
box()
text(x=15725, 4.9, "(c)", cex=1.51)

dev.copy2pdf(file= "master_scripts/manuscript_figs/airvars.pdf")
dev.off()







