# model as a function
testmodel <- function(  leaffrac = .25,
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
  for (i in 2:length(gCday)) {
    gross <- leafarea[i-1] * gCday[i]/conversionEfficiency 
    
    net <- gross- fr_resp*frootmass[i-1] - cr_resp*crootmass[i-1] - wd_resp*frootmass[i-1]
    
    #fractions,stems and wood need respiration
    leafmass[i] <- leafmass[i-1] + net*leaffrac - turnover*leafmass[i-1]
    leafarea[i] <- leafmass[i] / lma
    
    stemmass[i] <- stemmass[i-1] + net*stemfrac  ##turnover = 0
    frootmass[i] <- frootmass[i-1] + net*frfrac - turnover*frootmass[i-1]
    crootmass[i] <- crootmass[i-1] + net*crfrac - turnover*crootmass[i-1]
    
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


sim_means5<- as.data.frame(do.call(rbind,mapply(testmodel, gCday=b,SIMPLIFY=F)))
