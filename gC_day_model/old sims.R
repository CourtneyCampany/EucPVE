###old simulations runs and attempts from gcday


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


