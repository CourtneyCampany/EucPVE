source("functions and packages/load model packages.R")

#install_bitbucket("yplantqmc","remkoduursma", quick=FALSE)

require(YplantQMC)

#First need to run the sample trees through construct plant and get summary for each to use with raw data
#use readplantlist and summary.plant3d

#use the csv file to create the correct list of files and add the path so that readplant can use them
euckey <- read.csv("yplant/euc_plfiles/euc_key.csv")
euckey <- as.data.frame(euckey)
euckey$pfile <- paste("yplant/euc_plfiles/", euckey$pfile, sep = "")
euckey$lfile <- paste("yplant/euc_plfiles/", euckey$lfile, sep = "")

#test
test <- constructplant("yplant/euc_plfiles/Eletr5.p", "yplant/euc_plfiles/Elelf5.l")
summary(test)
plot(test)

#use readplant list in order to construct multiple plants, complete my species 
euc3d <- readplantlist(pfiles=euckey$pfile, lfiles=euckey$lfile)
#look at one random tree
plot(euc3d[[15]])

#summarise
eucs3d <- summary(euc3d, writefile=TRUE)
write.csv(eucs3d, "yplant/eucs_constructplant.csv", row.names=FALSE)


#--------------------------------------------------------------------------------------------------------
#run simulations for a sunny data from met data

#1. set location with long and latitude (can plot() to see if it is correct)
richmond <- setLocation(lat=-33.6, long=150.75, tzlong=150)

#2. get one met day 
#met data on 15 minute average from jan to june from HIEV (ros shelters)
weather <- read.csv("calculated data/eucpve_met.csv")
weather$DateTime15 <- ymd_hms(weather$DateTime15)
weather$Date <- as.Date(weather$DateTime15)
weather$time <- format(weather$DateTime15, format='%H:%M')

#search for some sunny days
sunmax <- subset(weather, PPFD_Avg.mean >= 1800)
unique(sunmax$Date)
#search for some cloudy days
sunmin <- subset(weather,  weather$time== "12:00:00" & weather$PPFD_Avg.mean <= 1000)

#subset one sunny day
metday <- subset(weather, Date=="2013-02-07")
plot(PPFD_Avg.mean~DateTime15, data=metday, ylim=c(0,2250))
metday$timeofday <- as.numeric(gsub(":", ".", metday$time))

#calculate total par
metday$par15_mol <- metday$PPFD_Avg.mean/1000000
plot(par15_mol~DateTime15, data=metday)
metday$par15_mol_s <- metday$par15_mol*15*60
daypar <- sum(metday$par15_mol_s)#molsm2d
daypar_mj <- daypar/(4.57/2)


#object with weather data from chosen date
sunnyday <- setMet(richmond, month=2, day=07, year=2013, nsteps=12, Tmin=14.83, Tmax=31.25, PARday=daypar_mj)
plot(sunnyday)

parpred <- as.data.frame(sunnyday[1][[1]])
#compare PAR from weather station and from setMET
plot(PAR~timeofday, data=parpred, ylim=c(0,2500), xlim=c(0,24))
  points(PPFD_Avg.mean~timeofday, data=metday, pch=16)


# Test direct vs. diffuse--------------------------------------------------------------------
# sunny_fbeam0 <- setMet(richmond, month=2, day=16, nsteps=12, Tmin=14.44, Tmax=25.83, PARday=daypar,
#                      fbeam=0, fbeammethod="constant")
# plot(sunny_fbeam0)
# sunny_fbeam1 <- setMet(richmond, month=2, day=16, nsteps=12, Tmin=14.44, Tmax=25.83, PARday=daypar,
#                      fbeam=1, fbeammethod="constant")
# plot(sunny_fbeam1)

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
write.csv(Aparam, "calculated data/A_parameters.csv", row.names=FALSE)

#A parameters for each volume
A_free <- subset(Aparam, volume=="1000")
A_5 <- subset(Aparam, volume=="5")
A_10 <- subset(Aparam, volume=="10")
A_15 <- subset(Aparam, volume=="15")
A_20 <- subset(Aparam, volume=="20")
A_25 <- subset(Aparam, volume=="25")
A_35 <- subset(Aparam, volume=="35")

#5.setPhy = Constructs an object of class 'ypphy', which calculates A and E from weather data and PAR.
eucphy_free <- setPhy("Farquhar",leafpars=list(Vcmax=A_free[1,5], Jmax=A_free[1,4], G1=A_free[1,3], Rd0=-(A_free[1,2])))
eucphy5 <- setPhy("Farquhar",leafpars=list(Vcmax=A_5[1,5], Jmax=A_5[1,4], G1=A_5[1,3], Rd0=-(A_5[1,2])))
eucphy10 <- setPhy("Farquhar",leafpars=list(Vcmax=A_10[1,5], Jmax=A_10[1,4], G1=A_10[1,3], Rd0=-(A_10[1,2])))
eucphy15 <- setPhy("Farquhar",leafpars=list(Vcmax=A_15[1,5], Jmax=A_15[1,4], G1=A_15[1,3], Rd0=-(A_15[1,2])))
eucphy20 <- setPhy("Farquhar",leafpars=list(Vcmax=A_20[1,5], Jmax=A_20[1,4], G1=A_20[1,3], Rd0=-(A_20[1,2])))
eucphy25 <- setPhy("Farquhar",leafpars=list(Vcmax=A_25[1,5], Jmax=A_25[1,4], G1=A_25[1,3], Rd0=-(A_25[1,2])))
eucphy35 <- setPhy("Farquhar",leafpars=list(Vcmax=A_35[1,5], Jmax=A_35[1,4], G1=A_35[1,3], Rd0=-(A_35[1,2])))


#test one plant
testday <- YplantDay(test, phy=eucphy20, met=sunnyday)
plot(testday)
testdata<-psrdata(testday)


# #direct v. diffuse
# run2 <- YplantDay(plant, phy=eucphy, met=sunny_fbeam0)
# run3 <- YplantDay(plant, phy=eucphy, met=sunny_fbeam1)
# 
# with(psrdata(run2), plot(timeofday, A/A0, type='l'))
# with(psrdata(run3), points(timeofday, A/A0, type='l', col="red"))



####diurnal simulation of all euc plants---------------------------------------------------------
#from summary of these plants  get variables in which to correlate to self shading multiplier

#create list of setPhy objects
eucphyList <- list(eucphy_free = eucphy_free, 
                   eucs5 = eucphy5, 
                   eucs10 = eucphy10, 
                   eucs15 = eucphy15, 
                   eucs20 = eucphy20,
                   eucs25 = eucphy25, 
                   eucs35 = eucphy35)

#run yplantday on the eucphylist, with richmond sunnday, and euc3d plants (61) 
# euc_test <- lapply(list(eucphyList[[1]], eucphyList[[2]]), function(x) YplantDay(test, phy = x, met = sunnyday))
# summary(euc_test[1])
# plot(euc_test[[2]])

#run simulatiojn, make empty list, run each phy=volume, and output psr files for each tree, then lapply for summary
euc_list <- list()
for(i in 1:length(eucphyList)){
  euc_list[[i]] <- YplantDay(euc3d, phy = eucphyList[[i]], met = sunnyday, PSRsuffix=names(eucphyList)[i])
}

saveRDS(euc_list, "yplant/euc_sim.rds")
#euc_list <- readRDS("somefilename.rds")
eucsumm <- lapply(euc_list, summary)
#need to add names the list by their volume
listnames <- c("eucs_5", "eucs_10", "eucs_15", "eucs_20", "eucs_25", "eucs_35", "eucs_free")
names(eucsumm) <- listnames

#save each list as a dfr with the name (##use names of list for apply function, see use of [[x]] for func arg)

#lapply(names(eucsumm), function(x) write.csv(eucsumm[[x]], file = paste(x, ".csv", sep = "")))
l_ply(names(eucsumm), function(x) write.csv(eucsumm[[x]], file = paste("yplant/simulation_summary/", x, ".csv", sep = "")))

#run on a cloudy day
# eucs_cloud <- YplantDay(euc3d, phy=eucphy, met=cloudyday)
# plot(eucs_cloud)
# # add summary variables:
# cloudy_summary <- summary(eucs_cloud) 





