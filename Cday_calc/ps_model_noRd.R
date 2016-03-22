#apply photosynthesis model to generate PS over the experiment
#do not include respiration in this script
#generate Cday(gross)

###made change
source("functions and packages/startscripts.R")

#READ DATA, calculated data for A parameters, met data, and leaf area

  #generated jmax and vcmax
  jmax_vcmax <- read.csv("calculated data/jmax_vcmax_clean.csv")

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

#input parameters from optimal conductance model (using nls)
g1 <- read.csv("calculated data/g1_pred.csv")
  g1_agg <- summaryBy(g1_date ~ volume, data=g1, keep.names=TRUE)

A_model <- merge(A_model, g1_agg, by="volume")
  #convert RH to VPD
  A_model$VPD <- RHtoVPD(A_model$RH, A_model$temp, Pa=101)
  

#run coupled photo-gs model, will retrun Aleaf at 15min interval
A_pred <- Photosyn(VPD=A_model$VPD, Ca=400, PPFD=A_model$PPFD, Tleaf=A_model$temp, 
          Jmax=A_model$Jmax.mean, Vcmax=A_model$Vcmax.mean, Rd=0, g1=A_model$g1_date)


#need a new dfr with Aleaf and Anet across the day
Aleaf <- A_pred[,c(1:4, 7:11)]
Aleaf_15min <- cbind(Aleaf, A_model[,c(1, 5:6)])
write.csv(Aleaf_15min, "calculated data/Aleaf_15min_gross.csv", row.names=FALSE)


Aleaf_15min$Date <- as.Date(Aleaf_15min$Date)
Aleaf_15min$volume <- as.factor(Aleaf_15min$volume)
Aleaf_15min$photo15gc <- with(Aleaf_15min, ALEAF*15*60*10^-6*12)

Aleaf <- summaryBy(photo15gc ~ Date+volume, data=Aleaf_15min, FUN=sum, keep.names=TRUE )
names(Aleaf)[3] <- "carbon_day"

write.csv(Aleaf, "calculated data/model_runs/cday_120_clean_gross.csv", row.names=FALSE)
Aleaf_agg <- summaryBy(carbon_day ~ volume, data=Aleaf, FUN=mean, keep.names=TRUE )
write.csv(Aleaf_agg, "calculated data/model_runs/gCday_means_clean_gross.csv", row.names=FALSE)




