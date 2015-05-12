library(doBy)
library(scales)

source("gC_day_model/model_start.R")
source("functions and packages/massmodel_LAconstrain.R")
source("functions and packages/massmodel.R")
source("functions and packages/massmodel2.R")

##Build an optimization model for leaf mass fraction based on both leaf area and biomass
##used self shading as a function of leaf area (daily increment in model)


#1. list of Cday over 120 days by trt
Cday_120 <- dlply(Aleaf, .(volume))

c120_trt<- lapply(Cday_120, "[", 3)

#2. set up mass allocation based on LMF
free_harvest <- harvestmass[harvestmass$volume == 1000,]

###observed values for parameter optimization
LMF_measured <- mean(free_harvest$leafmass) / mean(free_harvest$totalmass)
MASS_measured <- mean(free_harvest$totalmass)
LEAF_measured <- mean(free_harvest$leafmass)

#3. vector of gcday 120 for free plant
free <- as.vector(c120_trt[[7]][1:121,])

#4. ex of Base Model ex. using mean allocation, 120 days of Cday for reference
# 
#     sim_free <- productionmodel2(gCday=free, lma=lma_mean,frfrac=fr_frac_mean,crfrac=cr_frac_mean,
#                 stemfrac=stem_frac_mean, leaffrac=lf, M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
#                 returnwhat="lastval"returnwhat="lastval")


#5. optimise leaf mass fraction from free plant with harvest mass and leaf area
O.lmf <- function(leaffrac, ..., returnhow=c("objective","output")){
  
  returnhow <- match.arg(returnhow)
  
  ofrac <- (1-leaffrac)/3
  
  p <- productionmodel2(leaffrac=leaffrac, frfrac=ofrac,crfrac=ofrac,stemfrac=ofrac,
                        gCday=free, lma=lma_trt[7],
                        ...)
  if(returnhow == "output")return(p)
  
  LEAF <- p[3]
  MASS <- p[1]
  
  (LEAF - LEAF_measured)^2 + (MASS - MASS_measured)^2
  
}

#6. Visualize if there is an optimum
lfs <- seq(0.05,0.3, length=101)
test_opt <- mapply(O.lmf, leaffrac=lfs)
plot(lfs,test_opt)  

#7. Rerun model to see correspondence 

opt_free<- optimize(O.lmf, c(0,1))

O.lmf(leaffrac=opt_free$minimum, returnhow="output")

opt_lmf <- opt_free$minimum

opt_ofrac <- (1-opt_lmf)/3

#model now uses optimized LMF for all volume trts, includes self shadeing
optmassmodel<- list()
for(i in 1:7){
  optmassmodel[[i]] <- data.frame(productionmodel2(gCday=c120_trt[[i]][[1]], 
                                                   lma=lma_trt[i],frfrac=opt_ofrac, crfrac=opt_ofrac, stemfrac=opt_ofrac,
                                                   leaffrac=opt_lmf,M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
                                                   returnwhat="lastval"))
}  
##return all values
optmassmodel2<- list()
for(i in 1:7){
  optmassmodel2[[i]] <- data.frame(productionmodel2(gCday=c120_trt[[i]][[1]], 
                                                    lma=lma_trt[i],frfrac=opt_ofrac, crfrac=opt_ofrac, stemfrac=opt_ofrac,
                                                    leaffrac=opt_lmf,M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
                                                    returnwhat="all"))
}  


#8. Test scenarios--------------------------------------------------------------------------------------------------------


#S1: Increase allocation to fine roots (accounts exudation, increasesed turnover, respiration)

  ##use optimize leaf fraction,  adjust fine root C alocation (+-50%)
  
  nonleaf_frac <- 1- opt_lmf
  
  fr_frac_up <- opt_ofrac*1.5  #opt_ofrac from above
  opt_ofrac_fr1 <- (nonleaf_frac-fr_frac_up)/2
  
  #fine root allocation up
  fineroot_up<- list()
  for(i in 1:7){
    fineroot_up[[i]] <- data.frame(productionmodel2(gCday=c120_trt[[i]][[1]], 
                                                    lma=lma_trt[i],frfrac=fr_frac_up, crfrac=opt_ofrac_fr1, 
                                                    stemfrac=opt_ofrac_fr1,leaffrac=opt_lmf,M_slope=Mcoef$b[i], 
                                                    M_intercept= Mcoef$intercept[i],
                                                    returnwhat="lastval"))
  }  
  
  
  fr_frac_down <- opt_ofrac*.5
  opt_ofrac_fr2 <- (nonleaf_frac-fr_frac_down)/2
  
  #fine root allocation down
  fineroot_down<- list()
  for(i in 1:7){
    fineroot_down[[i]] <- data.frame(productionmodel2(gCday=c120_trt[[i]][[1]], 
                                                      lma=lma_trt[i],frfrac=fr_frac_down, crfrac=opt_ofrac_fr2, 
                                                      stemfrac=opt_ofrac_fr2,leaffrac=opt_lmf,M_slope=Mcoef$b[i], 
                                                      M_intercept= Mcoef$intercept[i],
                                                      returnwhat="lastval"))
  }  


#S2: increase to leaf allocation (includes increases in leaf turnover)
  
  ######only to containers????? leaves opt for free????
  
  
  
#S3: increases in root respiration (+-50%) 
  respmax_fr <- fr_resp*1.5
  respmax_cr <- cr_resp*1.5
  
  respmin_fr <- fr_resp*.5
  respmin_cr <- cr_resp*.5
  
  
  #root respiration up
  rootresp_up<- list()
  for(i in 1:7){
  rootresp_up[[i]] <- data.frame(productionmodel2(gCday=c120_trt[[i]][[1]], lma=lma_trt[i],frfrac=opt_ofrac, 
                                                      crfrac=opt_ofrac, stemfrac=opt_ofrac,leaffrac=opt_lmf,
                                                      M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
                                                      fr_resp=respmax_fr, cr_resp= respmax_cr,
                                                      returnwhat="all"))
    }   
  
  #root respiration down
  rootresp_down<- list()
  for(i in 1:7){
  rootresp_down[[i]] <- data.frame(productionmodel2(gCday=c120_trt[[i]][[1]], lma=lma_trt[i],frfrac=opt_ofrac, 
                                                        crfrac=opt_ofrac, stemfrac=opt_ofrac,leaffrac=opt_lmf,
                                                        M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
                                                        fr_resp=respmin_fr, cr_resp= respmin_cr,
                                                        returnwhat="all"))
    }  
