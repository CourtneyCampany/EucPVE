source("functions and packages/load model packages.R")

#First need to run the sample trees through construct plant and get summary for each to use with raw data
#use readplantlist and summary.plant3d

#use the csv file to create the correct list of files and add the path so that readplant can use them
euckey <- read.csv("yplant/euc_plfiles/euc_key.csv")
euckey <- as.data.frame(euckey)
euckey$pfile <- paste("yplant/euc_plfiles/", euckey$pfile, sep = "")
euckey$lfile <- paste("yplant/euc_plfiles/", euckey$lfile, sep = "")

#test
test <- constructplant(pfile="yplant/euc_plfiles/Eletr5.p", lfile="yplant/euc_plfiles/Elelf5.l")
summary(test)

#use readplant list in order to construct multiple plants, complete my species 
euc3d <- readplantlist(pfile=euckey$pfile, lfiles=euckey$lfile)
#look at one random tree
plot(euc3d[[14]])

#summarise
summary(euc3d, writefile=TRUE)


#--------------------------------------------------------------------------------------------------------
#run simulations for a sunny data from met data

#1. set location with long and latitude (can plot() to see if it is correct)
richmond <- setLocation(lat=-33.6, long=150.75, tzlong=150)

#2. get one met day 
#met data on 15 minute average from jan to june from HIEV (ros shelters)
weather <- read.csv("calculated data/eucpve_met.csv")
weather$DateTime15 <- ymd_hms(weather$DateTime15)
weather$Date <- as.Date(weather$DateTime15)
weather$time <- format(weather$DateTime15, format='%H:%M:%S')

#search for some sunny days
sunmax <- subset(weather, PPFD_Avg.mean >= 1800)
unique(sunmax$Date)
#search for some cloudy days
sunmin <- subset(weather,  weather$time== "12:00:00" & weather$PPFD_Avg.mean <= 1000)
#run something like this if I want to choose a random day
#a<- sample(1:nrow(sunmin), 1)

#subset one sunny day
metday <- subset(weather, Date=="2013-02-07")
plot(PPFD_Avg.mean~DateTime15, data=metday)
#calculate total par
metday$par15 <- metday$PPFD_Avg.mean*15*60
plot(par15~DateTime15, data=metday)
daypar <- (sum(metday$par15)/1000000) #molsm2d

#subset one cloudy day
metday2 <- subset(weather, Date=="2013-03-14")
#calculate total par
metday2$par15 <- metday2$PPFD_Avg.mean*15*60
plot(par15~DateTime15, data=metday2)
day2par <- (sum(metday2$par15)/1000000) #molsm2d


#object with weather data from chosen date
sunnyday <- setMet(richmond, month=2, day=07, nsteps=12, Tmin=14.83, Tmax=31.25, PARday=daypar)
plot(sunnyday)

cloudyday <- setMet(richmond, month=3, day=14, nsteps=12, Tmin=16.73, Tmax=26.04, PARday=day2par)
plot(cloudyday)

# Test direct vs. diffuse--------------------------------------------------------------------
sunny_fbeam0 <- setMet(richmond, month=2, day=16, nsteps=12, Tmin=14.44, Tmax=25.83, PARday=daypar,
                     fbeam=0, fbeammethod="constant")
plot(sunny_fbeam0)
sunny_fbeam1 <- setMet(richmond, month=2, day=16, nsteps=12, Tmin=14.44, Tmax=25.83, PARday=daypar,
                     fbeam=1, fbeammethod="constant")
plot(sunny_fbeam1)
#------------------------------------------------------------------------------------------------
#3. measured and predicted photosynthetic parameters (from my data)
phys <- read.csv("calculated data/jmax_vcmax.csv")
phys_agg <- summaryBy(Jmax.mean+Vcmax.mean~ volume, data=phys, keep.names=TRUE)
names(phys)[2:3]<- c("Jmax", "Vcmax")

g1 <- read.csv("calculated data/g1_pred.csv")
g1_mean <- mean(g1$g1_date)
g1_agg <- summaryBy(g1_vol ~ volume, data=g1, keep.names=TRUE)

rd <- read.csv("calculated data/Rd_leaf.csv")
rd_agg <- rd[,c(7,9,15)]
rd_agg <- summaryBy(.~ volume, data=rd_agg, keep.names=TRUE)
names(rd_agg)[2]<- "respdark"

Aparam <- merge(rd_agg[,1:2], g1_agg)
Aparam <- merge(Aparam, phys_agg)

#for now just use free seedling
A_free <- subset(Aparam, volume=="1000")
A_5 <- subset(Aparam, volume=="5")
A_10 <- subset(Aparam, volume=="10")
A_15 <- subset(Aparam, volume=="15")
A_20 <- subset(Aparam, volume=="20")
A_25 <- subset(Aparam, volume=="25")
A_35 <- subset(Aparam, volume=="35")

#5.setPhy = Constructs an object of class 'ypphy', which calculates A and E from weather data and PAR.
eucphy5 <- setPhy("Farquhar",leafpars=list(Vcmax=A_5[1,5], Jmax=A_5[1,4], G1=A_5[1,3], Rd0=-(A_5[1,2])))
eucphy10 <- setPhy("Farquhar",leafpars=list(Vcmax=A_10[1,5], Jmax=A_10[1,4], G1=A_10[1,3], Rd0=-(A_10[1,2])))
eucphy15 <- setPhy("Farquhar",leafpars=list(Vcmax=A_15[1,5], Jmax=A_15[1,4], G1=A_15[1,3], Rd0=-(A_15[1,2])))
eucphy20 <- setPhy("Farquhar",leafpars=list(Vcmax=A_20[1,5], Jmax=A_20[1,4], G1=A_20[1,3], Rd0=-(A_20[1,2])))
eucphy25 <- setPhy("Farquhar",leafpars=list(Vcmax=A_25[1,5], Jmax=A_25[1,4], G1=A_25[1,3], Rd0=-(A_25[1,2])))
eucphy35 <- setPhy("Farquhar",leafpars=list(Vcmax=A_35[1,5], Jmax=A_35[1,4], G1=A_35[1,3], Rd0=-(A_35[1,2])))
#------------------------------------------------------------------------------------------
#test one plant (sunny vs cloudy)

#sun
testday <- YplantDay(test, phy=eucphy, met=sunnyday)
testday20 <- YplantDay(test, phy=eucphy20, met=sunnyday)
plot(testday20)
testdata<-psrdata(testday)

# #direct v. diffuse
# run2 <- YplantDay(plant, phy=eucphy, met=sunny_fbeam0)
# run3 <- YplantDay(plant, phy=eucphy, met=sunny_fbeam1)
# 
# with(psrdata(run2), plot(timeofday, A/A0, type='l'))
# with(psrdata(run3), points(timeofday, A/A0, type='l', col="red"))

#cloudy
testday2 <- YplantDay(test, phy=eucphy, met=cloudyday)
plot(testday2)
test2data <- psrdata(testday)

#diurnal simulation of all euc plants---------------------------------------------------------

#from summary of these plants i can get variables in which to correlate to self shading multiplier
#only uses mean photosynthesis parameters from the free plants (at the moment)

#sunny day
eucs_all<- YplantDay(euc3d, phy=eucphy, met=sunnyday)
eucs_5<- YplantDay(euc3d, phy=eucphy5, met=sunnyday)
eucs_10<- YplantDay(euc3d, phy=eucphy10, met=sunnyday)
eucs_15<- YplantDay(euc3d, phy=eucphy15, met=sunnyday)
eucs_20<- YplantDay(euc3d, phy=eucphy20, met=sunnyday)
eucs_25<- YplantDay(euc3d, phy=eucphy25, met=sunnyday)
eucs_35<- YplantDay(euc3d, phy=eucphy35, met=sunnyday)

plot(eucs_all)
plot(eucs_all[[14]])
plot(eucs_5[[14]])


# add summary variables:
sunny_summ <- summary(eucs_all)  
sun5_summ <- summary(eucs_5)
sun10_summ <- summary(eucs_10)
sun15_summ <- summary(eucs_15)
sun20_summ <- summary(eucs_20)
sun25_summ <- summary(eucs_25)
sun35_summ <- summary(eucs_35)

write.csv(sunny_summ, "yplant/euc_plfiles/sunny free/sunny_stats.csv", row.names=FALSE)
write.csv(sun5_summ, "yplant/euc_plfiles/sunny_5/sunny_stats.csv", row.names=FALSE)
write.csv(sun10_summ, "yplant/euc_plfiles/sunny_10/sun10_stats.csv", row.names=FALSE)
write.csv(sun15_summ, "yplant/euc_plfiles/sunny_15/sun15_stats.csv", row.names=FALSE)
write.csv(sun20_summ, "yplant/euc_plfiles/sunny_20/sun20_stats.csv", row.names=FALSE)
write.csv(sun25_summ, "yplant/euc_plfiles/sunny_25/sun25_stats.csv", row.names=FALSE)
write.csv(sun35_summ, "yplant/euc_plfiles/sunny_35/sun35_stats.csv", row.names=FALSE)


#run on a cloudy day
# eucs_cloud <- YplantDay(euc3d, phy=eucphy, met=cloudyday)
# plot(eucs_cloud)
# # add summary variables:
# cloudy_summary <- summary(eucs_cloud) 






