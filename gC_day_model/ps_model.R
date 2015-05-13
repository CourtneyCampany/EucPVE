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
  #jmax_vcmax <- read.csv("calculated data/aci_parameters.csv")
  jmax_vcmax <- read.csv("calculated data/jmax_vcmax_clean.csv")
#   
#   phys_agg <- summaryBy(Jmax.mean+Vcmax.mean ~ volume, data=jmax_vcmax2, FUN=mean)
#   names(phys_agg)[2:5]<- c("Jmax", "Vcmax", "Jmax_se", "Vcmax_se")

  #site weather data, rename variables, format date stuff
  eucpve_met <- read.csv("calculated data/eucpve_met.csv")
  names(eucpve_met)[2:5] <- c("record", "PPFD", "temp", "RH")
  eucpve_met$Date <- as.Date(eucpve_met$DateTime15)

  #need to turn the datetime 15 into hms
  eucpve_met$DateTime15 <- ymd_hms(eucpve_met$DateTime15)
  eucpve_met$time15 <- format(eucpve_met$DateTime15, format='%H:%M:%S')

  #subset by Date range of experiment
  eucpve_met1 <- subset(eucpve_met[,3:7], Date  >= "2013-01-21" & Date  <= "2013-05-21")

  #Rdark Q10 equations by volume
  rdarkq10 <- read.csv("calculated data/rdarkq10.csv")
  rd25_clean <- read.csv("calculated data/rdark_clean.csv")

#---------------------------------------------------------------------------------------------
#A_model, use met data from site to generate parameters to enter into the model
#enter a vector of parameters over the period of the study to model PS with volume specific vcmax and jmax

#merge ps parameters to met data
A_model <- merge(eucpve_met1, jmax_vcmax)

#need to calculate Rdark through time using rdarkq10 equation by volume
A_model <- merge(A_model, rdarkq10[,1:2], by="volume")
A_model <- merge(A_model, rd25_clean[,1:2], by="volume")

  #A_model$Rd_pred <- with(A_model, rd12.3 * q10^((temp-12.3)/10))

  #with(A_model, plot(Rd_pred~temp, col=volume, ylim=c(0,10)))
  #with(A_model, plot(temp ~DateTime15))

  #with(subset(A_model, PPFD <= 1.0), plot(temp ~DateTime15))
  #with(subset(A_model, PPFD <= 1.0), plot(Rd_pred ~DateTime15, col=volume))

  #with(A_model, plot(temp,Rd_pred, col=volume))
  #dev.copy2pdf(file= "output/rdbytemp.pdf")
  ####this q10 is too high, use crous for salinga from wtc1
  q10_crous <- 1.95
  q25_drake <- 1.86

#rdark_eq$rd25_euct <- with(rdark_eq, rd12.3*(q25_drake^((abs(CTleaf-25))/10)))
  
#refit RD
A_model$Rd_pred2 <- with(A_model, rd12.3 * q25_drake^((temp-12.3)/10))
#with(A_model, plot(temp,Rd_pred2, col=volume))

#------------------------------------------------------------------------------------------------------
#input parameters from optimal conductance model (using nls)

g1 <- read.csv("calculated data/g1_pred.csv")
  g1_agg <- summaryBy(g1_date ~ volume, data=g1, keep.names=TRUE)


A_model <- merge(A_model, g1_agg, by="volume")

#-------------------------------------------------------------------------------------------------
#now run the model (includes pred Rdark from crouseq10, and gs parameters modelled from spot measurements)

#convert RH to VPD
A_model$VPD <- RHtoVPD(A_model$RH, A_model$temp, Pa=101)

#model, should return the aleaf for every 15 minutes. (will retrun Aleaf at 15min interval)
A_pred <- Photosyn(VPD=A_model$VPD,Ca=400, PPFD=A_model$PPFD, Tleaf=A_model$temp, 
                            Jmax=A_model$Jmax.mean, Vcmax=A_model$Vcmax.mean, Rd=A_model$Rd_pred2, g1=A_model$g1_date)

#need a new dfr with Aleaf and Anet across the day
Aleaf <- A_pred[,c(1:4, 7:11)]
Aleaf_15min <- cbind(Aleaf, A_model[,c(1, 5:6)])
write.csv(Aleaf_15min, "calculated data/Aleaf_pred_15min.csv", row.names=FALSE)

# 
# Aleaf_15min$Date <- as.Date(Aleaf_15min$Date)
# Aleaf_15min$volume <- as.factor(Aleaf_15min$volume)
# Aleaf_15min$photo15gc <- with(Aleaf_15min, ALEAF*15*60*10^-6*12)
# 
# Aleaf <- summaryBy(photo15gc ~ Date+volume, data=Aleaf_15min, FUN=sum, keep.names=TRUE )
# names(Aleaf)[3] <- "carbon_day"
# #write.csv(Aleaf, "calculated data/model_runs/cday_120.csv", row.names=FALSE)
# Aleaf_agg <- summaryBy(carbon_day ~ volume, data=Aleaf, FUN=mean, keep.names=TRUE )




