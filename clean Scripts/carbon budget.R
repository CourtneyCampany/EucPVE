#calculate Aplant for the entire day and then multiply by the leaf area
#use A from farquhar model and met data, M from yplant, and leaf area to calculate carbon gain for seedlings
#convert to C gain

source("functions and packages/load packages.R")
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#read M model slope and intercept
Mmodel <- read.csv("stats output/M_leaf#_model.csv")
Mmodel$volume <-gsub("free", 1000, Mmodel$volume )

#interpolated leaf count by pot
leafcount <- read.csv("calculated data/L#pred.csv")
leafcount$Date <- as.Date(ymd(leafcount$Date))
leafcount <- merge(leafcount, plotsumm[, 3:4])
leafcount$volume <- as.factor(leafcount$volume)
plot(count_pred~Date, data=leafcount, pch=pchs[volume],col=volume)

M_predict <- merge(leafcount, Mmodel[,c(1, 3:4)], by="volume")
M_predict$M <- with(M_predict, (b*count_pred)+intercept)
plot(M~Date, data=M_predict, pch=pchs[volume],col=volume)

#read in predicted Aleaf form model (15min rate umols m2s)
Aleaf <- read.csv("calculated data/Aleaf_pred_15min.csv")

#interpolated leaf area in m2 by pot
leafarea <- read.csv("calculated data/LApred.csv")
leafarea$Date <- as.Date(ymd(leafarea$Date))
leafarea <- merge(leafarea, plotsumm[, 3:4])
leafarea$volume <- as.factor(leafarea$volume)

##MODELED A---------------------------------------------------------------------
#assume that totA and totAo are total mols of CO2day?
#convert Aleaf to day sum then apply M 

#convert aleaf and anet to mols co2 day
Aleaf$Aleaf_mols <- Aleaf$ALEAF/1000000 #mols co2 s for every 15 min
Aleaf$Anet_mols <- Aleaf$Anet/1000000

#turns rate of A (mols m2s)into units of every 15 min
Aleaf$Aleaf_15 <- with(Aleaf, Aleaf_mols*15*60)
Aleaf$Ane_15 <- with(Aleaf, Anet_mols*15*60)

A_day <- summaryBy(Aleaf_15+Ane_15~Date+volume, data=Aleaf, FUN=sum, keep.names=TRUE )
names(A_day)[3:4] <- c("Aleaf_day", "Anet_day")

#calculate leaf A with self shading
Aadj <- merge(M_predict[,c(1:4,7)],A_day, by=c("volume", "Date"))

#adjusted total photo with M and multiplt by 12 for gC
Aadj$Aleaf_adj <- with(Aadj, (Aleaf_day*M)*12)
Aadj$Anet_adj <- with(Aadj, (Anet_day*M)*12)

#merge with leaf area
Aplant <- merge(Aadj, leafarea)
#calculate total C by mulipling by leaf area
Aplant$netC_day <- with(Aplant, Aleaf_day*canopysqm_pred)
Aplant$C_day<-  with(Aplant, Anet_day*canopysqm_pred)

#volume means
Aplant_agg <- summaryBy(netC_day+C_day~ volume+Date, data=Aplant, FUN=mean, keep.names=TRUE)

#ploting of C day
plot(C_day~Date, data=Aplant, col=volume, ylim=c(0, 1),pch=pchs[volume])
plot(netC_day~Date, data=Aplant_agg, col=volume, ylim=c(0, 1),pch=pchs[volume])

# #multiple the adjusted rate by the leaf area, covert to g C and sum to day
# Aplant <- Aadj[, c(1, 10:11, 13:14)]
# Aplant$Date <- as.Date(Aplant$Date)
# 
# Amodel_pve <- merge(leafpred, Aplant, by=c("volume", "Date"))

# #umols CO2 to mols CO2 to g C
# Amodel_pve$Cpred <- with(Amodel_pve, (Aleaf_ss/1000000)*12)
# #turns rate of A (umols m2s)into units of every 15 min
# Amodel_pve$Cpred_15 <- with(Amodel_pve, Cpred*15*60)

# #calculate Anet and do the same
# Amodel_pve$Cpred_net <- with(Amodel_pve, (Anet_ss/1000000)*12)
# Amodel_pve$Cpred_net_15 <- with(Amodel_pve, Cpred_net*15*60)

# #carbon gain from leaf area and Cnet
# Amodel_pve$Cgain_15 <- with(Amodel_pve, Cpred_net_15*LA_pred)
# 
# A15 <- subset(Amodel_pve, volume=="15")
# 
# #day total
# Amodel_day <- summaryBy(Cgain_15 ~ ID + Date, data= Amodel_pve, FUN=sum, keep.names=TRUE)
# Amodel_day <- merge(Amodel_day, plotsumm[,3:4])
# Amodel_day$volume <- as.factor(Amodel_day$volume)
# names(Amodel_day)[3] <- "C_day"

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
