library(doBy)
library(lubridate)

#use A from farquhar model or observed, M from yplant, and leaf area to calculate carbon gain for seedlings

#read in predicted Aleaf form model (15min rate umols m2s)
Aleaf <- read.csv("calculated data/Aleaf_pred_15min.csv")

#M from yplant sim Aplant/Aleaf
M <- read.csv("calculated data/M_eucs.csv")
M_agg <- summaryBy(M ~ volume, data=M, FUN=mean, keep.names=TRUE)

#interpolated leaf area in m2 by pot
leafpred <- read.csv("calculated data/LApredbypot.csv")
leafpred$Date <- as.Date(ymd(leafpred$Date))



##MODELED A---------------------------------------------------------------------
#calculate leaf A with self shading
Aadj <- merge(Aleaf, M_agg)
Aadj$Aleaf_ss <- with(Aadj, ALEAF*M)
Aadj$Anet_ss <- with(Aadj, Anet*M)
#multiple the adjusted rate by the leaf area, covert to g C and sum to day
Aplant <- Aadj[, c(1, 10:11, 13:14)]
Aplant$Date <- as.Date(Aplant$Date)

Amodel_pve <- merge(leafpred, Aplant, by="Date")

#umols CO2 to mols CO2 to g C
Amodel_pve$Cpred <- with(Amodel_pve, (Aleaf_ss/1000000)*12)
#turns rate of A (umols m2s)into units of every 15 min
Amodel_pve$Cpred_15 <- with(Amodel_pve, Cpred*15*60)

#calculate Anet and do the same
Amodel_pve$Cpred_net <- with(Amodel_pve, (Anet_ss/1000000)*12)
Amodel_pve$Cpred_net_15 <- with(Amodel_pve, Cpred_net*15*60)

#day total
Amodel_day <- summaryBy(Cpred_net_15+Cpred_15 ~ ID + Date, data= Amodel_pve, FUN=sum)
names(Amodel_day)[3:4] <- c("C_day", "Cnet_day")






#calculate Aplant for the entire day and then multiply by the leaf area
#convert to C gain


##OBSERVED A (Asat licor)

#Read in Asat treatment means and add ID variable
Asat_obs <- read.csv("calculated data/Asat.csv")
Asat_obs$Date <- as.Date(Asat_obs$Date)
Asat <- Asat_obs[, c(1:2, 5, 9:10)]

Apve <- merge(Asat, leafpred, by=c("ID", "Date"))
Apve <- merge(Apve, M_agg)
Apve$Aplant_ss <- with(Apve, Photo*M)

