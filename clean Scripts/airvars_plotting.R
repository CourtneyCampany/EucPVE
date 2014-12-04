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
windows(7,5)

par(mar=c(5,5,2,2), cex.axis=0.8)
with(airvars, {
  plot(Date, Temperature.max, type='l', col="blue",ylim=c(0,40),
       xlab="",axes=FALSE,ylab=expression(Daily~T[min]~and~T[max]~(degree*C)),
       panel.first={
         #abline(h=seq(0,40,by=10),lty=5)
         addpoly(Date, Temperature.min, Temperature.max,col="lightblue1")
       })
  lines(Date, Temperature.min, col="blue")
  
})
axis(2)
xAT <- seq.Date(as.Date("2012-1-21"), by="month", length=50)
#xATminor <- seq.Date(as.Date("2012-7-1"), by="1 month", length=100)
axis.Date(1, at=xAT, format="%b-'%y" )
box()

#par

parlab <- expression(mol~m^-2~s^-1)

par(mar=c(5,5,2,2), cex.axis=0.8)
with(airvars, {
  plot(Date, Par.max, type='l', col="red",ylim=c(0,2200),
       xlab="",axes=FALSE,ylab=expression(Daily~PPFD[min]~and~PPFD[max]~(mol~m^-2~s^-1)),
       panel.first={
         #abline(h=seq(0,40,by=10),lty=5)
         addpoly(Date, Par.min, Par.max,col="pink")
       })
  lines(Date, Par.min, col="red")
  
})
axis(2)
xAT <- seq.Date(as.Date("2012-1-21"), by="month", length=50)
axis.Date(1, at=xAT, format="%b-'%y" )
box()

#vpd
par(mar=c(5,5,2,2), cex.axis=0.8)
with(airvars, {
  plot(Date, VPD.max, type='l', col="forestgreen",ylim=c(0,5),
       xlab="",axes=FALSE,ylab=expression(Daily~T[min]~and~T[max]~(degree*C)),
       panel.first={
         #abline(h=seq(0,40,by=10),lty=5)
         addpoly(Date, VPD.min, VPD.max,col="darkseagreen1")
       })
  lines(Date, VPD.min, col="forestgreen")
  
})
axis(2)
xAT <- seq.Date(as.Date("2012-1-21"), by="month", length=50)
#xATminor <- seq.Date(as.Date("2012-7-1"), by="1 month", length=100)
axis.Date(1, at=xAT, format="%b-'%y" )
box()

plot(VPD.max~Date, data=met_maxmin, type="l", col="forestgreen")

plot(Temperature.max~Date, data=met_maxmin,  type="l", col="red")




#simple example of met data table
require(data.table)
library(gridExtra)
# met_agg <- summaryBy(Par+Temperature+VPD ~ month, data=eucpve_met, FUN=mean, keep.names=TRUE)
# met_agg$month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
# met_agg<- met_agg[1:5,]
# 
# met_table <- as.table <- met_agg
# pdf(file = "output/temp1.pdf")
# capture.output(met_table, file= "output/temp1.txt")
# write.csv(met_table, "calculated data/met data.csv", row.names=FALSE)


EmptyLine <- data.frame(month = "",Par = "",Temperature = "", RH = "")
Eqmonthdf <- as.data.frame(met_agg[1,])

pdf(file = "output/temp.pdf")

for (i in 2:nrow(met_agg)) 
{
  if (as.vector(met_agg$month[i])  ==  as.vector(met_agg$month[i-1])) 
  {Eqmonthdf <- rbind(Eqmonthdf, met_agg[i,])}
  
  else {
    Eqmonthdf <- rbind(Eqmonthdf, EmptyLine)
    Eqmonthdf <- rbind(Eqmonthdf, met_agg[i,]) 
  }
}

grid.table(Eqmonthdf, show.rownames = FALSE)
dev.off()
