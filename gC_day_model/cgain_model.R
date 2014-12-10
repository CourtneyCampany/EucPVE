
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
  Amodel$photo15gc <- with(Amodel, Anet*15*60*10^-6*12)

#first need sum over day and then means by treatment
Aleaf <- summaryBy(photo15gc ~ Date+volume, data=Amodel, FUN=sum, keep.names=TRUE )
  names(Aleaf)[3] <- "carbon_day"
Aleaf_agg <- summaryBy(carbon_day ~ volume, data=Aleaf, FUN=mean, keep.names=TRUE )
#write.csv(Aleaf_agg, "calculated data/model_runs/gCday_means.csv", row.names=FALSE)


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
                    #fr_turn = .48, #production/standing crop
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
    leafmass[i] <- leafmass[i-1] + netbiomassprod*leaffrac
    leafarea[i] <- leafmass[i] / lma
   
    stemmass[i] <- stemmass[i-1] + netbiomassprod*stemfrac
    frootmass[i] <- frootmass[i-1] + netbiomassprod*frfrac
    crootmass[i] <- crootmass[i-1] + netbiomassprod*crfrac

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
#mu <- .6 #cf for gCday, not in use when scaling


####models sims use sequence of carbon day and test scenarios-----------------------------------------------------

#Scenario#1: Gcday seq, allocation = mean------------------------------------------------------------------------------------
sim_means_obs <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                frfrac=fr_frac_mean, crfrac=cr_frac_mean, stemfrac=stem_frac_mean,         
                                                leaffrac=lf,SIMPLIFY=F)))
  sim_means_obs$gCday <- gcday_seq_obs
#save run
write.csv(sim_means_obs, "calculated data/model_runs/sim_gCseq_obs.csv" , row.names=FALSE)



#Scenario #2: Increase allocation to fine roots (accounts exudation, increasesed turnover, respiration)---------------------
fr_exude_max <- fr_frac_mean*1.5
fr_exude_min <- fr_frac_mean*.5
ofrac <- (1 - fr_exude_max)/3
ofrac1 <- (1 - fr_exude_min)/3

sim_frexude_low <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                     frfrac=fr_exude_min, crfrac=ofrac1, stemfrac=ofrac1,         
                                                     leaffrac=ofrac1,SIMPLIFY=F)))

sim_frexude_high <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                      frfrac=fr_exude_max, crfrac=ofrac, stemfrac=ofrac,         
                                                      leaffrac=ofrac,SIMPLIFY=F)))


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
lf_turn_min <- lf*.5
ofrac2 <- (1 - lf_turn_max)/3
ofrac3 <- (1 - lf_turn_min)/3

sim_lfturn_low <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                   frfrac=ofrac3, crfrac=ofrac3, stemfrac=ofrac3,         
                                                   leaffrac=lf_turn_min,SIMPLIFY=F)))

sim_lfturn_high <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gcday_seq_obs, lma=lma_mean, 
                                                    frfrac=ofrac2, crfrac=ofrac2, stemfrac=ofrac2,         
                                                    leaffrac=lf_turn_max,SIMPLIFY=F)))

sim_lfturn_low$lf_alloc <- lf_turn_min
sim_lfturn_low$lf_alloc_mean <- lf
sim_lfturn_low$gCday <- gcday_seq_obs

sim_lfturn_high$lf_alloc <- lf_turn_max
sim_lfturn_high$lf_alloc_mean <- lf
sim_lfturn_high$gCday <- gcday_seq_obs

#save runs
write.csv(sim_lfturn_low, "calculated data/model_runs/sim_leaflow.csv" , row.names=FALSE)
write.csv(sim_lfturn_high, "calculated data/model_runs/sim_leafhigh.csv" , row.names=FALSE)



#####------------------------old sims---------------------------------------------------------------------------------
#1.Run the model for each day with individual gCday by treatment and day  (gcday = ALeaf )
require(plyr)

###run sim with all aleaf and mean alllocation
Aleaf_mean <- dlply(Aleaf, .(volume), function(x) as.data.frame(do.call(rbind,mapply(
  productionmodel, gCday=mu*(x$carbon_day), 
  lma=lma_mean,frfrac=fr_frac_mean, 
  crfrac=cr_frac_mean, stemfrac=stem_frac_mean,
  leaffrac=lf, SIMPLIFY=F))))

#extract final number and then save
Aleaf_mean2 <- do.call(rbind, lapply(Aleaf_mean , function(x) x[121,])) 
Aleaf_mean2$volume = as.factor(rownames(Aleaf_mean2))
row.names(Aleaf_mean2) <- NULL
write.csv(Aleaf_mean2, "calculated data/model_runs/Aleaf_mean.csv", row.names=FALSE)

#2. run sim with gcday by treatment and day with allocation 

#dfr for input
Aleaf_day <- Aleaf[,2:3]
Aleaf_day <- merge(Aleaf_day, lma_vol)
Aleaf_day <- merge(Aleaf_day, fr_frac_vol)
Aleaf_day <- merge(Aleaf_day, cr_frac_vol)
Aleaf_day <- merge(Aleaf_day, stem_frac_vol)
Aleaf_day <- merge(Aleaf_day, leaf_frac_vol)

Aleaf_sim <- dlply(Aleaf_day, .(volume), function(x) as.data.frame(do.call(rbind,mapply(
  productionmodel, gCday=mu*(x$carbon_day), 
  lma=x$massarea,frfrac=x$froot_frac, 
  crfrac=x$croot_frac, stemfrac=x$stem_frac,
  leaffrac=x$leaf_frac, SIMPLIFY=F))))
#merge Cday with Aleaf
Aleaf_sp <- dlply(Aleaf_day[1:2], .(volume)) #list of Cday
Aleaf_sim2 <- mapply(c, Aleaf_sim, Aleaf_sp, SIMPLIFY=FALSE) #merge two lists

#creates a list of lists, simply to list of dfrs
Aleaf_sim3 <- llply(Aleaf_sim2, function(x) as.data.frame(sapply(x, rbind)))

Aleaf_sim4 <- do.call(rbind, lapply(Aleaf_sim3 , function(x) x[121,])) 
Aleaf_sim4$volume = as.factor(rownames(Aleaf_sim4))
row.names(Aleaf_sim4) <- NULL

#save Aleaf_sim as rds and as simple dfr of last dates
write.csv(Aleaf_sim4, "calculated data/model_runs/Aleaf_sim_final.csv", row.names=FALSE)
saveRDS(Aleaf_sim3, file = "calculated data/model_runs/Aleaf_sim.rds")



#component allocation and lma by volume (7 sims) in loop
allsims <- list()
for (i in 1:7){
sim <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=mu*gcday_seq_obs, lma=lma_trt[i],frfrac=frfrac_trt[i], 
                  crfrac=crfrac_trt[i], stemfrac=stemfrac_trt[i],leaffrac=leaffrac_trt[i], SIMPLIFY=F)))

  sim$gCday <- gcday_seq_obs

allsims[[i]] <- sim
}
#save run  
saveRDS(allsims, file = "calculated data/model_runs/allocation_sim.rds")


####need to build this into a function when running scenarios

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


####model with parameters and Cday by volume to compare with final harvest----------------------

modelmass <- as.data.frame(do.call(rbind, mapply(productionmodel, 
            gCday=0.6*Cday, lma=lma_trt,frfrac=frfrac_trt, crfrac=crfrac_trt, stemfrac=stemfrac_trt,
            leaffrac=leaffrac_trt,SIMPLIFY=FALSE)))
modelmass <- cbind(volume, modelmass)
write.csv(modelmass, "calculated data/model_runs/mass_sim.csv", row.names=FALSE)

modelmass_all <- as.data.frame(do.call(rbind, mapply(productionmodel, gCday=Cday,lma=lma_trt, frfrac=frfrac_trt, 
                                      crfrac=crfrac_trt, stemfrac=stemfrac_trt,
                                      leaffrac=leaffrac_trt,returnwhat="all",SIMPLIFY=FALSE)))
#saveRDS(modelmass_all, file = "calculated data/model_runs/mass_sim_alldays.rds")



####---  scripts for making polygon of scenairos
#sequence of allocation to froots reprenting +/- 50% from harvest value, respiration stays same, Cgay(min/max)
fr_exude <- seq(fr_frac_mean*.5, fr_frac_mean*1.5, length=2)
ofrac <- (1 - fr_exude)/3

sim_exud_low <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=mu*gc_min, lma=lma_mean, 
                                                   frfrac=fr_exude, crfrac=ofrac, stemfrac=ofrac,         
                                                   leaffrac=ofrac,SIMPLIFY=F)))

sim_exud_high <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=mu*gc_max, lma=lma_mean, 
                                                    frfrac=fr_exude, crfrac=ofrac, stemfrac=ofrac,         
                                                    leaffrac=ofrac,SIMPLIFY=F)))

sim_exud_low$fr_alloc <- fr_exude
sim_exud_high$fr_alloc <- fr_exude

sim_exudate <- rbind(sim_exud_low, sim_exud_high)
sim_exudate$change <- c("-50", "+50", "-50", "+50" )
sim_exudate$Cday <- c(gc_min,gc_min,gc_max,gc_max)


