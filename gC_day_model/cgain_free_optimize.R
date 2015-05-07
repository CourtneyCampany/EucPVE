source("functions and packages/plot objects.R")
source("functions and packages/functions.R")

library(plyr)
library(doBy)
library(scales)

source("gC_day_model/model_start.R")
source("functions and packages/massmodel_LAconstrain.R")
source("functions and packages/massmodel.R")

####model #1------------------------------------------------------------------------------------------------------------

##120 days of ALEAF

#1. list of Cday over 120 days by trt
  Cday_120 <- dlply(Aleaf, .(volume))

  c120_trt<- lapply(Cday_120, "[", 3)

#2: new M here later

#3. set up mass allocation based on LMF
  free_harvest <- harvestmass[harvestmass$volume == 1000,]

  ###observed values for parameter optimization
  LMF_measured <- mean(free_harvest$leafmass) / mean(free_harvest$totalmass)
  MASS_measured <- mean(free_harvest$totalmass)

  ###ofrac setup 
  ofrac <- (1-lf)/3


#5. vector of gcday 120 for free plant
  free <- as.vector(c120_trt[[7]][1:121,])

#6. ex of Base Model ex. using mean allocation, 120 days of Cday for reference

  sim_free <- data.frame(productionmodel(gCday=free, lma=lma_mean, 
                         frfrac=fr_frac_mean,crfrac=cr_frac_mean,stemfrac=stem_frac_mean, 
                         leaffrac=lf, returnwhat="lastval"))


###RUN production model optimization-----------------------------------------------------------------------------------

#1. optimise leaf mass fraction from free plant

O.lmf <- function(leaffrac, ...){
  
  p <- productionmodel(leaffrac=leaffrac, ...)
  
  LMF <- p[4]
  
  (LMF - LMF_measured)^2
  
}

opt_free<- optimize(O.lmf, c(0,1), gCday=free, lma=lma_mean, 
         frfrac=ofrac,crfrac=ofrac,stemfrac=ofrac, returnwhat="lastval")

opt_lmf <- as.numeric(opt_free[1])

opt_ofrac <- (1-opt_lmf)/3


##rerun model with all treatments using optimized leaf mass fraction
for (i in 1:7){
  opt120_alloc <- lapply(c120_trt, function(x) {data.frame(productionmodel(gCday=as.vector(x[1:121,]), 
                  lma=lma_trt[i],frfrac=opt_ofrac, crfrac=opt_ofrac, stemfrac=opt_ofrac,
                  leaffrac=opt_lmf,returnwhat="all"))
  })
}


#2. optimise gCday with free plant using lmf from above ############(doesnt seem to work right)############

O.cday<- function(gCday, ...){
  
  p <- productionmodel(gCday=gCday, ...)
  
  MASS <- p[1]
  
  (MASS - MASS_measured)^2
  
}

opt_free2<- optimize(O.cday, c(0,20),  lma=lma_mean, leaffrac=opt_lmf,
                    frfrac=ofrac,crfrac=ofrac,stemfrac=ofrac, returnwhat="lastval")

opt_cday <- as.numeric(opt_free2[1])

##rerun free model using optimized leaf mass fraction and gcday

opt_free <- data.frame(productionmodel(gCday=opt_cday,lma=lma_trt[7],frfrac=opt_ofrac, crfrac=opt_ofrac, stemfrac=opt_ofrac,
           leaffrac=opt_lmf,returnwhat="lastval"))

##PLOTTING optimized models with volume trts of cday 120

#1: use total C gain per platn, draw leaf area from model with optimized LMF

totalC_list <- list()
for(i in 1:7){
  totalC_calc <- opt120_alloc[[i]][2] * c120_trt[[i]][1:120,]
  
  totalC_list[[i]] <- totalC_calc
}                 

totalC_trt <- lapply(totalC_list, function(x) sum(x))

totalC_trt2 <- unlist(totalC_trt)

#2. plot production with optmized LMF sim vs total c gain (LA from sim cday120)
windows(8,10)
par(mar=c(5,5,2,2))
plot(mass_actual$mass ~ totalC_trt2,pch=pchs,col=palette(),cex=1.6, xlim=c(0, 500), 
     ylim=c(0, 500), ylab="Biomass (g)", xlab="Total Carbon Gain (g)")
for(i in 1:7){
  points(opt120_alloc[[i]][120,1]~ totalC_trt2[i], pch=pch2, col=cols[i], cex=1.6)
}
abline(0,1)
# dev.copy2pdf(file= "gC_day_model/model_output/.pdf")  
# dev.off() 

