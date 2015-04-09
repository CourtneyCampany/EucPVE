#apply photosynthesis model to generate PS over the experiment
#apply values to leaf area that has been generated
#compare to actual measurements

###made change
source("functions and packages/load packages.R")
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")
library(plantecophys)

#READ DATA, calculated data for A parameters, met data, and leaf area

#generated jmax and vcmax
jmax_vcmax <- read.csv("calculated data/aci_parameters.csv")

#site weather data, rename variables, format date stuff
eucpve_met <- read.csv("calculated data/eucpve_met.csv")
names(eucpve_met)[2:5] <- c("record", "PPFD", "temp", "RH")
eucpve_met$Date <- as.Date(eucpve_met$DateTime15)

#need to turn the datetime 15 into hms
eucpve_met$DateTime15 <- ymd_hms(eucpve_met$DateTime15)
eucpve_met$time15 <- format(eucpve_met$DateTime15, format='%H:%M:%S')


#subset by Date range of experiment
eucpve_met1 <- subset(eucpve_met[,3:7], Date  >= "2013-01-21" & Date  <= "2013-05-21")

#plot summary
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#interpolated leaf area in m2 by pot
leafpred <- read.csv("calculated data/LApredbypot.csv")
leafpred$Date <- as.Date(ymd(leafpred$Date))

#observed Ps data
#Read in Asat treatment means and add ID variable
Asat_means <- read.csv("calculated data/Asat treatment means.csv")
Asat_obs <- read.csv("calculated data/Asat.csv")
Asat_obs$Date <- as.Date(Asat_obs$Date)

#Rdark Q10 equations by volume
rdarkq10 <- read.csv("calculated data/rdarkq10.csv")


#---------------------------------------------------------------------------------------------
#A_model, use met data from site to generate parameters to enter into the model
#enter a vector of parameters over the period of the study to model PS with volume specific vcmax and jmax

#merge ps parameters to met data
A_model <- merge(eucpve_met1, jmax_vcmax)

#-------------------------------------------------------------------------------------------------
#need to calculate Rdark through time using rdarkq10 equation by volume
A_model <- merge(A_model, rdarkq10, by="volume")

#A_model$Rd_pred <- with(A_model, rd12.3 * q10^((temp-12.3)/10))

#with(A_model, plot(Rd_pred~temp, col=volume, ylim=c(0,10)))
#with(A_model, plot(temp ~DateTime15))

#with(subset(A_model, PPFD <= 1.0), plot(temp ~DateTime15))
#with(subset(A_model, PPFD <= 1.0), plot(Rd_pred ~DateTime15, col=volume))

#with(A_model, plot(temp,Rd_pred, col=volume))
#dev.copy2pdf(file= "output/rdbytemp.pdf")
####this q10 is too high, use crous for salinga from wtc1
q10_crous <- 1.95

#refit RD
A_model$Rd_pred2 <- with(A_model, rd12.3 * q10_crous^((temp-12.3)/10))
with(A_model, plot(temp,Rd_pred2, col=volume))

#------------------------------------------------------------------------------------------------------
#input parameters from optimal conductance model (using nls)

g1 <- read.csv("calculated data/g1_pred.csv")
g1_mean <- mean(g1$g1_date)
g1_agg <- summaryBy(g1_vol ~ volume, data=g1, keep.names=TRUE)

A_model <- merge(A_model, g1_agg, by="volume")

#-------------------------------------------------------------------------------------------------
#now run the model (includes pred Rdark from crouseq10, and gs parameters modelled from spot measurements)

#convert RH to VPD
A_model$VPD <- RHtoVPD(A_model$RH, A_model$temp, Pa=101)

#model, should return the aleaf for every 15 minutes. (will retrun Aleaf at 15min interval)
A_pred <- Photosyn(VPD=A_model$VPD,Ca=400, PPFD=A_model$PPFD, Tleaf=A_model$temp, 
                            Jmax=A_model$Jmax, Vcmax=A_model$Vcmax, Rd=A_model$Rd_pred2, g1=A_model$g1_vol)

#new variable ,net A, with aleaf - RD
A_pred$Anet <- with(A_pred, ALEAF-Rd)

#need a new dfr with Aleaf and Anet across the day
Aleaf <- A_pred[,c(1:4, 8:9, 11:12)]
Aleaf_15min <- cbind(Aleaf, A_model[,c(1, 5:6)])
write.csv(Aleaf_15min, "calculated data/Aleaf_pred_15min.csv", row.names=FALSE)

####stops here for now------------------------

#umols CO2 to mols CO2 to g C
A_pred$Cpred <- with(A_pred, (Anet/1000000)*12)

#turns rate of net A (umols m2s)into units of every 15 min
A_pred$Cpred_15 <- with(A_pred, Cpred*15*60)
#also A rate (umols m2s) into every 15 min
A_pred$A_15 <- with(A_pred, Cpred*15*60)

#sum, but need Date metrics
A_pred <- cbind((subset(A_model, select = c("Date", "volume"))),A_pred)

#unit conversion to gc per day per m2
Aday_pred <- summaryBy(ALEAF, Anet, Cpred_15 ~ volume + Date, data= A_pred, FUN=sum)
Aday_pred$volume <- as.factor(Aday_pred$volume)
names(Aday_pred)[3] <- "Cday"

#plot 
with(Aday_pred, plot(Date, Cday, col=volume, pch=pchs[volume], type="p", ylim=c(0, 12)))
with(Aday_pred, plot(Date, Anet, col=volume, pch=pchs[volume], type="p", ylim=c(0, 20)))

#------------------------------------------------------------------------------------
#new dataframe with calculated Aleaf and experiment metrics
LA <- merge(leafpred, plotsumm, by = "ID")
#now merge leaf area (m2) with the rates of A predicted for each volume
treeC_pred <- merge(Aday_pred, LA,  by=c("Date", "volume"))

#multiply Aleaf by leaf area at each date to generate C through time
#make sure that LA is in m2
treeC_pred$Cday_plant <- with(treeC_pred, LA_pred*Cday)

with(treeC_pred, plot(Date, Cday_plant, col=volume, pch=pchs[volume], type="p", ylim=c(0, 6)))


#treatment means
treeC_agg <- summaryBy(Cday_plant ~ Date + volume , data = treeC_pred, FUN = c(mean,se))




###plotting moved to bottom of script to compare with observed data

#__________________________________________________________________________
#look at observed versus predicted

#need to covert observed data to gC day
#how can I compare asat to model with real PAR?
#need to calculate net A first with same equations as above


#calculate Aplant for observed data
Aplant_obs <- merge(Asat_obs, leafpred, by=c("ID", "Date"))
#need R dark
darkresp <- subset(A_model, select=c("Date", "Rd_pred2", "volume"))
darkresp_agg <- summaryBy(Rd_pred2 ~ Date+volume, data=darkresp, FUN=mean)
                   
#merge darkresp with Aplant_obs by measurement dates
Aplant_obs <- merge(Aplant_obs, darkresp_agg, by=c("Date", "volume"))
#calculate Anet
Aplant_obs$Anet <- with(Aplant_obs, Photo-Rd_pred2.mean)

Aplant_obs$Cday <- with(Aplant_obs, ((Anet/1000000)*12*60*60*24))
Aplant_obs$Cplant_DAY <- with(Aplant_obs, Cday*LA_pred )

Aobs_agg <- summaryBy(Cplant_DAY ~ Date + volume , data = Aplant_obs, FUN = c(mean,se))
plot(Cplant_DAY.mean~Date, data=Aobs_agg, pch=16,col=volume)

#look at a subset

free_pred <- subset(treeC_agg, volume=="15")
free_obs <- subset(Aobs_agg, volume=="15")


#plot test subset
with(free_pred, plot (Cday_plant.mean~Date, type="p", ylim=c(0,2)))
  points(Cplant_DAY.mean~Date, data=free_obs, pch=15, col="blue")

with(free_obs, plot (Cplant_DAY.mean~Date, type="p"))

#plot modelled versus observed
#legend labels
leglab <- levels(treeC_pred$volume)
leglab[7] <- "free"
pch=c(rep(16,6),17)
#no free
leglab2 <- c("5", "10", "15", "20", "25", "35")


#first plot modelled, then add lines for observed data
with(treeC_agg, plot(Date, Cday_plant.mean, col=palette(), type="p", lwd=3, cex=.7, pch=c(rep(16,6),17),ylim=c(0, 10)))
  points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="5"), type="l", lty="solid", lwd=3,col="#FF0000")
  points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="10"), type="l", lty="solid", lwd=3,col="#D4002A")
  points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="15"), type="l", lty="solid", lwd=3,col="#AA0055")
  points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="20"), type="l", lty="solid", lwd=3,col="#7F007F")
  points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="25"), type="l", lty="solid", lwd=3,col="#5500AA")
  points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="35"), type="l", lty="solid", lwd=3,col="#2A00D4")
  points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="1000"), type="l", lty="dashed", lwd=3,col="#0000FF")


legend("topleft", leglab, pch=c(rep(16,6),17),text.font=3, inset=0.02, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n')

box()
dev.copy2pdf(file= "output/model_A.pdf")

#with no free

with(subset(treeC_agg, volume !="1000"), plot(Date, Cday_plant.mean, cex=.7, col=palette(), 
  type="p", lwd=3, pch=16,ylim=c(0,3)))
    points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="5"), type="l", lty="solid", lwd=3,col="#FF0000")
    points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="10"), type="l", lty="solid", lwd=3,col="#D4002A")
    points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="15"), type="l", lty="solid", lwd=3,col="#AA0055")
    points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="20"), type="l", lty="solid", lwd=3,col="#7F007F")
    points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="25"), type="l", lty="solid", lwd=3,col="#5500AA")
    points(Cplant_DAY.mean~Date, data=subset(Aobs_agg,volume=="35"), type="l", lty="solid", lwd=3,col="#2A00D4")

legend("topleft", leglab2, pch=16,text.font=3, inset=0.02, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n')
box()
dev.copy2pdf(file= "output/model_A_nofree.pdf")



