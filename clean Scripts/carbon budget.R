#calculate Aplant for the entire day and then multiply by the leaf area
#use A from farquhar model and met data, M from yplant, and leaf area to calculate carbon gain for seedlings
#convert to C gain

source("functions and packages/startscripts.R")

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

#plot(M~Date, data=M_predict, pch=pchs[volume],col=volume)

#read in predicted Aleaf form model (15min rate umols m2s)
Aleaf <- read.csv("calculated data/Aleaf_pred_15min.csv")

#interpolated leaf area in m2 by pot
leafarea <- read.csv("calculated data/LApred.csv")
  leafarea$Date <- as.Date(ymd(leafarea$Date))
  leafarea <- merge(leafarea, plotsumm[, 3:4])
  leafarea$volume <- as.factor(leafarea$volume)

#seedling mass form harvest
mass <- read.csv("calculated data/seedling mass.csv")

##MODELED A---------------------------------------------------------------------
#assume that totA and totAo are total mols of CO2day?
#convert Aleaf to day sum then apply M 


####new calc
Aleaf2 <- summaryBy(Anet ~ Date+volume, data=Aleaf, FUN=mean, keep.names=TRUE )
  Aleaf2$Date <- as.Date(Aleaf2$Date)

Aleaf3 <- merge(Aleaf2, leafarea)
  Aleaf3$Adayumol <- with(Aleaf3, Anet*canopysqm_pred)

#functions to turn A15rate into netCgainday (m2)
gctotal_fun <- function(z)mean(z)*99*24*60*60*10^-6*12
gcday_fun <- function(z)mean(z)*24*60*60*10^-6*12

C_day <- summaryBy(Adayumol ~ ID+Date, FUN=gcday_fun, data=Aleaf3, keep.names=T)
names(C_day)[3] <- "carbon_day"
write.csv(C_day, "calculated data/cgain_date.csv", row.names=FALSE)

c_gain <- summaryBy(Adayumol ~ ID, FUN=gctotal_fun, data=Aleaf3, keep.names=T)
  names(c_gain)[2] <- "carbon_gain"

massC <- data.frame(ID=mass$ID, volume=mass$volume, totalC=mass$totalmass*0.5)

euc_cgain <- merge(massC, c_gain)
  euc_cgain$volume <- as.factor(euc_cgain$volume)
write.csv(euc_cgain, "calculated data/euc_cgain.csv", row.names=FALSE)

with(euc_cgain, plot(carbon_gain, totalC, pch=pchs[volume], col=volume))
abline(0,1)
abline(lm(totalC ~ carbon_gain, data=euc_cgain), lty=5)

#look at simple models
lmfit <- lm(totalC ~ carbon_gain * volume, data=euc_cgain)
lmfit2 <- lm(totalC ~ carbon_gain + volume, data=euc_cgain)
require(visreg)
visreg(lmfit2)


#read in met data and graph PAR to look at dips with C day---------------------------------------
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
