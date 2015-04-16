
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

####scaling (possible one for M, photo, or scaling)
#mu <- .6 #cf for gCday, not in use when scaling
#M <- 


####models sims use sequence of carbon day and test scenarios-----------------------------------------------------

#Scenario#1: Gcday seq, allocation = mean------------------------------------------------------------------------------------
sim_means_obs <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                frfrac=fr_frac_mean, crfrac=cr_frac_mean, stemfrac=stem_frac_mean,         
                                                leaffrac=lf,SIMPLIFY=F)))
  sim_means_obs$gCday <- gcday_seq_obs
#save run
write.csv(sim_means_obs, "calculated data/model_runs/sim_gCseq_obs.csv" , row.names=FALSE)

#Scenario #2: Increase allocation to fine roots (accounts exudation, increasesed turnover, respiration)---------------------

#need to use proportions of partitioning so the model runs correctly

fr_exude_max <- fr_frac_mean*1.5
  ofrac <- 1-fr_exude_max
  exude_max_LF <- ofrac*lf
  exude_max_CR <- ofrac*cr_frac_mean
  exude_max_ST <- ofrac*stem_frac_mean

sim_frexude_high <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                       frfrac=fr_exude_max, crfrac=exude_max_CR, stemfrac=exude_max_ST,         
                                                       leaffrac=exude_max_LF,SIMPLIFY=F)))

fr_exude_min <- fr_frac_mean*.5
  ofrac1 <- 1-fr_exude_min
  exude_min_LF <- ofrac1*lf
  exude_min_CR <- ofrac1*cr_frac_mean
  exude_min_ST <- ofrac1*stem_frac_mean


sim_frexude_low <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                     frfrac=fr_exude_min, crfrac=exude_min_CR, stemfrac=exude_min_ST,         
                                                     leaffrac=exude_min_LF,SIMPLIFY=F)))



  sim_frexude_low$gCday <- gcday_seq_obs
  sim_frexude_high$gCday <- gcday_seq_obs

#save run2
write.csv(sim_frexude_low, "calculated data/model_runs/sim_frexudelow.csv" , row.names=FALSE)
write.csv(sim_frexude_high, "calculated data/model_runs/sim_frexudehigh.csv" , row.names=FALSE)


#save run
#write.csv(sim_exudate, "calculated data/model_runs/sim_exudate.csv" , row.names=FALSE) #moved the script for this below


####Scenario #3: increases in root respiration (+-50%)-------------------------------------------------------------------
respmax_fr <- fr_resp*1.5
respmax_cr <- cr_resp*1.5

respmin_fr <- fr_resp*.5
respmin_cr <- cr_resp*.5


sim_rootresp_high <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                    frfrac=fr_frac_mean, crfrac=cr_frac_mean, stemfrac=stem_frac_mean,         
                                                    leaffrac=lf,fr_resp=respmax_fr, cr_resp= respmax_cr, SIMPLIFY=F)))
sim_rootresp_high$gCday <- gcday_seq_obs

sim_rootresp_low <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                       frfrac=fr_frac_mean, crfrac=cr_frac_mean, stemfrac=stem_frac_mean,         
                                                       leaffrac=lf,fr_resp=respmin_fr, cr_resp= respmin_cr, SIMPLIFY=F)))
sim_rootresp_low$gCday <- gcday_seq_obs

#save run2
#write.csv(sim_rootresp, "calculated data/model_runs/sim_rootresp.csv" , row.names=FALSE) #previous run with +50
write.csv(sim_rootresp_low, "calculated data/model_runs/sim_rootresp_low.csv" , row.names=FALSE)
write.csv(sim_rootresp_high, "calculated data/model_runs/sim_rootresp_high.csv" , row.names=FALSE)



####scernario #4:  increase to leaf allocation (includes increases in leaf turnover)-----------------------------

#sequence of allocation to froots reprenting +/- 50% from harvest value, respiration stays same, Cgay(min/max)
lf_turn_max <- lf*1.5
  ofrac2 <- 1-lf_turn_max
  lf_max_FR <- ofrac2*fr_frac_mean
  lf_max_CR <- ofrac2*cr_frac_mean
  lf_max_ST <- ofrac2*stem_frac_mean

sim_lfturn_high <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                      frfrac=lf_max_FR, crfrac=lf_max_CR, stemfrac=lf_max_ST,         
                                                      leaffrac=lf_turn_max,SIMPLIFY=F)))


lf_turn_min <- lf*.5
  ofrac3 <- 1-lf_turn_min
  lf_min_FR <- ofrac3*fr_frac_mean
  lf_min_CR <- ofrac3*cr_frac_mean
  lf_min_ST <- ofrac3*stem_frac_mean


sim_lfturn_low <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                   frfrac=lf_min_FR, crfrac=lf_min_CR, stemfrac=lf_min_ST,         
                                                   leaffrac=lf_turn_min,SIMPLIFY=F)))



sim_lfturn_low$lf_alloc <- lf_turn_min
sim_lfturn_low$lf_alloc_mean <- lf
sim_lfturn_low$gCday <- gcday_seq_obs

sim_lfturn_high$lf_alloc <- lf_turn_max
sim_lfturn_high$lf_alloc_mean <- lf
sim_lfturn_high$gCday <- gcday_seq_obs

#save runs
write.csv(sim_lfturn_low, "calculated data/model_runs/sim_leaflow.csv" , row.names=FALSE)
write.csv(sim_lfturn_high, "calculated data/model_runs/sim_leafhigh.csv" , row.names=FALSE)

