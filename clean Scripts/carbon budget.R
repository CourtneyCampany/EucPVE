#calculate Aplant for the entire day and then multiply by the leaf area
#convert to C gain

source("functions and packages/load packages.R")
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#use A from farquhar model and met data, M from yplant, and leaf area to calculate carbon gain for seedlings

#read in predicted Aleaf form model (15min rate umols m2s)
Aleaf <- read.csv("calculated data/Aleaf_pred_15min.csv")

####write now there are big dips in C day on certain days resulting from Aleaf model

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

#ploting of C day
plot(C_day~Date, data=Amodel_day, col=volume, ylim=c(0, 6),pch=pchs[volume])

windows()
plot(C_day~Date, data=Amodel_agg, col=volume, ylim=c(0, 4),pch=pchs[volume], ylab="", xlab="")
  title(ylab=cdaylab, mgp=ypos, cex=1.2)
  legend("topleft", leglab,    
       bty="n", text.font=1,pch=pchs, col=palette(), title=vollab)
dev.copy2pdf(file= "output/Cgain_pve.pdf")
dev.off()

#read in met data and graph PAR to look at dips--------------------------------------------------
met <- read.csv("calculated data/eucpve_met.csv")
met$DateTime15 <- ymd_hms(met$DateTime15)
met$Date <- as.Date(met$DateTime15)
#subset for experiemtn date range
pve_met <- subset(met, Date  >= "2013-01-21" & Date  <= "2013-05-21")
#daily total par
par_day <- summaryBy(PPFD_Avg.mean~Date, data=pve_met, FUN=sum, keep.names=TRUE)

windows()
plot(PPFD_Avg.mean~Date, data=par_day, type="l", xlab="", ylab="PPFD daily sum")
dev.copy2pdf(file= "output/PPFD_pve.pdf")
dev.off()
