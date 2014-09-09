#model start values
lma_mean <- mean(lma$massarea)#average lma from harvest
LA_start <- (mean_leafnum * leafarea_mean) #(m2)
mass_mean <- mean(seedling_pre$seedling_mass)
Cday <- as.vector(Aleaf_agg[,2]) #vector of 7 treaments in order
sla_trt <- as.vector(sla_vol[,2])
lma_trt <- as.vector(lma_vol[,2])

#volumeid <- as.factor(c(5,10,15,20,25,35,"free"))

# model as a function
productionmodel <- function(leafrac = .25,
                            crfrac = .25,
                            frfrac = .25,
                            stemfrac=.25,
                            gCday = 1,
                            conversionEfficiency = 0.65,
                            fr_resp = .010368, #gC/gFroot day Marsden et al
                            cr_resp = .00124, #gC/gCroot day
                            wd_resp = .00269, #gc/gwood day Ryan et al. eucs 1yr
                            #fr_turn = .48, #production/standing crop
                            numdays=120,
                            lma = 97.5,
                            returnwhat=c("lastval","all")
){
  
  returnwhat <- match.arg(returnwhat)
  
  leafarea <- vector()
  leafarea[1] <- LA_start
  
  biomass <- vector()
  biomass[1] <- mass_mean
  
  root <- vector()
  root[1] <- pre_root
  
  stem <- vector()
  stem[1] <- pre_stem
  
  leafmass <- vector()
  leafmass[1] <- LA_start*lma_mean
  
  LMF <- vector()
  LMF[1] <- (LA_start*lma_mean)/mass_mean
  
  if(length(gCday) == 1)gCday <- rep(gCday,numdays)
  if(length(gCday) < numdays)stop("Need at least ",numdays," photosynthesis values.")
  
  #run model simulation------------------------------------------------------------------
  for (i in 2:numdays) {
    biomassprod <- leafarea[i-1] * gCday[i]/conversionEfficiency  # gc day-1
    
    biomassprodnet <- biomassprod - 
      ((biomass[i-1]*fr_resp)+(biomass[i-1]*cr_resp)+(biomass[i-1]*wd_resp))
    ######need to implement biomass fractions above and belowground with respiration
    biomass[i] <- biomass[i-1] + biomassprodnet
    
    leafmass[i] <- leafmass[i-1] + biomassprodnet*leafrac
    leafarea[i] <- leafmass[i] / lma
    
    LMF[i] <- leafmass[i]/biomass[i]
  }
  
  if(returnwhat == "lastval")
    return(c(biomass=biomass[numdays],leafarea=leafarea[numdays], leafmass = leafmass[numdays], LMF = LMF[numdays]))
  
  if(returnwhat == "all")
    return(list(biomass=biomass,leafarea=leafarea, leafmass = leafmass, LMF = LMF))
  
}
