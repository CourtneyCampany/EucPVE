
require(doBy)

#experiment length---------------------------------------------------------------------
numdays <- as.numeric(as.Date("2013-05-21") - as.Date("2013-01-21"))
volume <- c("5", "10", "15", "20", "25", "35", "1000")

#date is for plotting, starts day2
uniqueDate <- seq.Date(from=as.Date("2013/01/22"), to=as.Date("2013/05/21"), by="days")

#harvest mass and leaf area for model comparison----------------------------------------------------------------
harvestmass <- read.csv("calculated data/seedling mass.csv")
mass_total <- harvestmass[,c(1:2,11)]
mass_agg <- summaryBy(totalmass ~volume, data=mass_total, FUN=mean, keep.names=TRUE)
LA_harvest <- read.csv("calculated data/LA_harvest.csv")
LA_agg <- summaryBy(totalarea ~volume, data=LA_harvest, FUN=mean, keep.names=TRUE)

mass_actual <- data.frame(volume = LA_agg$volume, mass = mass_agg$totalmass, leafarea = (LA_agg$totalarea * 10^-4))
#write.csv(mass_actual, "calculated data/harvest_mass_means.csv", row.names=FALSE)

#pre seedling data for intial biomass and leaf area (use mean)--------------------------------------------------
seedling_pre <- read.csv("raw data/seedling_initial.csv")
seedling_pre$seedling_mass <- with(seedling_pre, leaf_mass+root_mass+wood_mass)
seedling_pre$rootshoot <- with(seedling_pre, root_mass/(leaf_mass+wood_mass))
leaffractions <- mean (seedling_pre$leaf_mass/seedling_pre$seedling_mass)
#average mass of seedlings at start
#mass_mean <- mean(seedling_pre$seedling_mass)
mean_leafnum <- mean(seedling_pre$leaf_numb)
pre_root <- mean(seedling_pre$root_mass)
pre_stem <- mean(seedling_pre$wood_mass)

#root-shoot ratios, and froot and croot mass fractions---------------------------------------------------------

#harvest
ratio <- subset(harvestmass, select = c("ID", "volume", "fineroot", "Croot", "stemmass", "leafmass",
                                        "root", "shoot", "totalmass"))
ratio$rootshoot <-with(ratio, root/shoot)
ratio$froot_frac <- with(ratio, fineroot/totalmass)
ratio$croot_frac <- with(ratio, Croot/totalmass)
ratio$stem_frac <- with(ratio, stemmass/totalmass)
ratio$leaf_frac <- with(ratio, leafmass/totalmass)
ratio_agg <- summaryBy(rootshoot+froot_frac+croot_frac ~volume, data=ratio, FUN=mean, keep.names=TRUE)
#mean component fractions
rs_mean <- mean(ratio$rootshoot)
fr_frac_mean <- mean(ratio$froot_frac)
cr_frac_mean <- mean(ratio$croot_frac)
stem_frac_mean <- mean(ratio$stem_frac)
#fraction volume means
fr_frac_vol <-  summaryBy(froot_frac ~volume, data=ratio, FUN=mean, keep.names=TRUE)
cr_frac_vol <- summaryBy(croot_frac ~volume, data=ratio, FUN=mean, keep.names=TRUE)
stem_frac_vol <- summaryBy(stem_frac ~volume, data=ratio, FUN=mean, keep.names=TRUE)
leaf_frac_vol <- summaryBy(leaf_frac ~volume, data=ratio, FUN=mean, keep.names=TRUE)

#pre
rootshoot_pre_mean <- mean(seedling_pre$rootshoot)

#mean lma and leafarea (apply to intial leaf )------------------------------------------------------------------
lma <- read.csv("calculated data/leafmassarea.csv")
lma_vol <- summaryBy(massarea ~volume, data=lma, FUN=mean, keep.names=TRUE)

#lma_mean <- mean(lma$massarea)
leafarea_mean <- (mean(lma$area))/10000
#average starting leaf area
#LA_start <- (mean_leafnum * leafarea_mean) #(m2)

#read in Anet conver to gC day for each volume-----------------------------------------

##need to convert modelled 15min A into g C m2 day
Amodel <- read.csv("calculated data/Aleaf_pred_15min.csv")
Amodel$Date <- as.Date(Amodel$Date)
Amodel$volume <- as.factor(Amodel$volume)
Amodel$photo15gc <- with(Amodel, ALEAF*15*60*10^-6*12)

Aleaf <- summaryBy(photo15gc ~ Date+volume, data=Amodel, FUN=sum, keep.names=TRUE )
names(Aleaf)[3] <- "carbon_day"
#write.csv(Aleaf, "calculated data/model_runs/cday_120.csv", row.names=FALSE)
Aleaf_agg <- summaryBy(carbon_day ~ volume, data=Aleaf, FUN=mean, keep.names=TRUE )
#write.csv(Aleaf_agg, "calculated data/model_runs/gCday_means.csv", row.names=FALSE)SE)


####MODEL---------------------------------------------------------------------------------

##model start values
lma_mean <- mean(lma$massarea)#average lma from harvest
LA_start <- (mean_leafnum * leafarea_mean) #(m2)
mass_mean <- mean(seedling_pre$seedling_mass)
Cday <- as.vector(Aleaf_agg[,2]) 

fr_resp = .010368 #gC/gFroot day Marsden et al
cr_resp = .00124#gC/gCroot day
wd_resp = .001877 #gc/gwood day Drake with 1.86 q10 to 20C

#vectors of 7 treaments in order
lma_trt <- as.vector(lma_vol[,2])
frfrac_trt <- as.vector(fr_frac_vol[,2])
crfrac_trt <- as.vector(cr_frac_vol[,2])
stemfrac_trt <- as.vector(stem_frac_vol[,2])
leaffrac_trt <- as.vector(leaf_frac_vol[,2])

lf <- mean(leaffrac_trt)
#volumeid <- as.factor(c(5,10,15,20,25,35,"free"))

# model as a function
productionmodel <- function(leaffrac = .25,
                            crfrac = .25,
                            frfrac = .25,
                            stemfrac=.25,
                            gCday = 1,
                            conversionEfficiency = 0.65,
                            fr_resp = .010368, #gC/gFroot day Marsden et al
                            cr_resp = .00124, #gC/gCroot day
                            wd_resp = .001877, #gc/gwood day Drake with 1.86 q10 to 20C
                            turnover = 1/365, #production/standing crop
                            numdays=120,
                            lma = 97.5,
                            returnwhat=c("lastval","all")
){
  
  
  sumf <- leaffrac + crfrac + frfrac + stemfrac
  leaffrac <- leaffrac / sumf
  crfrac <- crfrac / sumf
  frfrac <- frfrac / sumf
  stemfrac <- stemfrac / sumf
  
  returnwhat <- match.arg(returnwhat)
  
  leafarea <- vector()
  leafarea[1] <- LA_start
  
  biomass <- vector()
  biomass[1] <- mass_mean
  
  frootmass <- vector()
  frootmass[1] <- pre_root*.5
  
  crootmass <- vector()
  crootmass[1] <- pre_root*.5
  
  stemmass <- vector()
  stemmass[1] <- pre_stem
  
  leafmass <- vector()
  leafmass[1] <- LA_start*lma
  
  LMF <- vector()
  LMF[1] <- (LA_start*lma)/mass_mean
  
  
  if(length(gCday) == 1)gCday <- rep(gCday,numdays)
  if(length(gCday) < numdays)stop("Need at least ",numdays," photosynthesis values.")
  
  #run model simulation------------------------------------------------------------------
  for (i in 2:numdays) {
    grossbiomassprod <- leafarea[i-1] * gCday[i]/conversionEfficiency  # gc day-1
    
    netbiomassprod <- grossbiomassprod - fr_resp*frootmass[i-1] - cr_resp*crootmass[i-1] - wd_resp*frootmass[i-1]
    
    #fractions,stems and wood need respiration
    leafmass[i] <- leafmass[i-1] + netbiomassprod*leaffrac - turnover*leafmass[i-1]
    leafarea[i] <- leafmass[i] / lma
    
    stemmass[i] <- stemmass[i-1] + netbiomassprod*stemfrac  ##turnover = 0
    frootmass[i] <- frootmass[i-1] + netbiomassprod*frfrac - turnover*frootmass[i-1]
    crootmass[i] <- crootmass[i-1] + netbiomassprod*crfrac - turnover*crootmass[i-1]
    
    #total biomass day
    biomass[i] <- leafmass[i-1] + frootmass[i-1] + crootmass[i-1] + stemmass[i-1]
    
    #leaf mass fraction
    LMF[i] <- leafmass[i]/biomass[i]
  }
  
  if(returnwhat == "lastval")
    return(c(biomass=biomass[numdays],leafarea=leafarea[numdays], leafmass = leafmass[numdays], LMF = LMF[numdays]))
  
  if(returnwhat == "all")
    return(list(biomass=biomass,leafarea=leafarea, leafmass = leafmass, LMF = LMF))
  
}


#Extra parameters to run model  

#sequence is the range of mean values (over 120 days) reperenting the mean of all days per volume
gcday_seq_obs <- seq(max(Cday), min(Cday), length=101) 


#Scenario#1b: Cday seqm allocation in trt
#component allocation and lma by volume (7 sims) in loop
allsims <- list()
for (i in 1:7){
  sim <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_trt[i],frfrac=frfrac_trt[i], 
         crfrac=crfrac_trt[i], stemfrac=stemfrac_trt[i],leaffrac=leaffrac_trt[i], SIMPLIFY=F)))
  
  sim$gCday <- gcday_seq_obs
  
  allsims[[i]] <- sim
}
#save run  
saveRDS(allsims, file = "calculated data/model_runs/allocation_sim.rds")

####mass as a function of largest container or free (btw 0 and 1)
vol35 <- as.data.frame(allsims[6])
vol35$volume <- "35"
sim35 <- data.frame(mass35 = vol35$biomass)
vol35 <- cbind(vol35, sim35)

volfree <- as.data.frame(allsims[7])
volfree$volume <- "1000"
simfree <- data.frame(massfree = volfree$biomass)
volfree <- cbind(volfree, simfree)

volfree <- cbind(volfree, sim35)
vol35 <- cbind(vol35, simfree)

bigmass <- data.frame(sim35, simfree)

#add vol35 free to the others 
vol5 <- as.data.frame(allsims[1])
vol5$volume <- "5"
vol5 <- cbind(vol5, bigmass)

vol10 <- as.data.frame(allsims[2])
vol10$volume <- "10"
vol10 <- cbind(vol10, bigmass)

vol15 <- as.data.frame(allsims[3])
vol15$volume <- "15"
vol15 <- cbind(vol15, bigmass)

vol20 <- as.data.frame(allsims[4])
vol20$volume <- "20"
vol20 <- cbind(vol20, bigmass)

vol25 <- as.data.frame(allsims[5])
vol25$volume <- "25"
vol25 <- cbind(vol25, bigmass)

simcarbon <- rbind(vol5, vol10, vol15, vol20, vol25, vol35, volfree)
simcarbon$volume <- as.factor(simcarbon$volume)
#new parameter that standardizes mass to the largest volume
simcarbon$mass_stnd_pot <- with(simcarbon, biomass/mass35)
simcarbon$mass_stnd_free <- with(simcarbon, biomass/massfree)

write.csv(simcarbon, "calculated data/model_runs/sim_gCseq_allocation.csv", row.names=FALSE)

###plotting

sim_sp <- dlply(simcarbon, .(volume))

mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
Date<- seq(as.Date("2013-01-22"),as.Date("2013-05-21"), by="days")
cols <- gradient(7)


par(mar=c(5,5,2,2))
plot(1, xlab="", ylab=LAm2, ylim=c(0, .6), xlim=range(Date), type='n', axes=FALSE)
axis.Date(Date, side=1)
axis(2)

l_ply(sim_sp, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                                                   col=x$volume, pch = pchs[x$volume],type='b', lwd=2))
box()








