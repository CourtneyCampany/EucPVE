#calculate Aplant for the entire day and then multiply by the leaf area
#convert to C gain

library(doBy)
library(lubridate)

source("functions and packages/functions.R")

#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")


#use A from farquhar model or observed, M from yplant, and leaf area to calculate carbon gain for seedlings

#read in predicted Aleaf form model (15min rate umols m2s)
Aleaf <- read.csv("calculated data/Aleaf_pred_15min.csv")

#M from yplant sim Aplant/Aleaf
M <- read.csv("calculated data/M_eucs.csv")
M_agg <- summaryBy(M ~ volume, data=M, FUN=mean, keep.names=TRUE)
M_agg$volume <- gsub("free","1000", M_agg$volume)
M_agg$volume <- as.factor(M_agg$volume)

#interpolated leaf area in m2 by pot
leafpred <- read.csv("calculated data/LApredbypot.csv")
leafpred$Date <- as.Date(ymd(leafpred$Date))
leafpred <- merge(leafpred, plotsumm[, 3:4])
leafpred$volume <- as.factor(leafpred$volume)


##MODELED A---------------------------------------------------------------------
#calculate leaf A with self shading
Aadj <- merge(Aleaf, M_agg)
Aadj$Aleaf_ss <- with(Aadj, ALEAF*M)
Aadj$Anet_ss <- with(Aadj, Anet*M)
#multiple the adjusted rate by the leaf area, covert to g C and sum to day
Aplant <- Aadj[, c(1, 10:11, 13:14)]
Aplant$Date <- as.Date(Aplant$Date)

Amodel_pve <- merge(leafpred, Aplant, by=c("volume", "Date"))

#umols CO2 to mols CO2 to g C
Amodel_pve$Cpred <- with(Amodel_pve, (Aleaf_ss/1000000)*12)
#turns rate of A (umols m2s)into units of every 15 min
Amodel_pve$Cpred_15 <- with(Amodel_pve, Cpred*15*60)

#calculate Anet and do the same
Amodel_pve$Cpred_net <- with(Amodel_pve, (Anet_ss/1000000)*12)
Amodel_pve$Cpred_net_15 <- with(Amodel_pve, Cpred_net*15*60)

#carbon gain from leaf area and Cnet
Amodel_pve$Cgain_15 <- with(Amodel_pve, Cpred_net_15*LA_pred)

A15 <- subset(Amodel_pve, volume=="15")

#day total
Amodel_day <- summaryBy(Cgain_15 ~ ID + Date, data= Amodel_pve, FUN=sum, keep.names=TRUE)
Amodel_day <- merge(Amodel_day, plotsumm[,3:4])
Amodel_day$volume <- as.factor(Amodel_day$volume)
names(Amodel_day)[3] <- "C_day"

#volume means
Amodel_agg <- summaryBy(C_day~ volume+Date, data=Amodel_day, FUN=mean, keep.names=TRUE)

A15 <- subset(Amodel_day, volume=="15")
A15agg <- subset(Amodel_agg, volume=="15")


#colors
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
pchs = c(rep(16,6),17)

plot(C_day~Date, data=Amodel_day, col=volume, ylim=c(0, 6),pch=pchs[volume])
plot(C_day~Date, data=Amodel_agg, col=volume, ylim=c(0, 6),pch=pchs[volume])



