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
free_agg <- summaryBY(leafmass+totalmass)
###leaf frac
LMF_measured <- mean(free_harvest$leafmass) / mean(free_harvest$totalmass)

###ofrac setup 
ofrac <- (1-lf)/3


#4. For optimization need to work with free plant only
free <- as.vector(c120_trt[[7]][1:121,])
Mfree <- M_trt[7]

#1. Model using mean allocation, 120 days of Cday, constrainted by LA

sim_free <- data.frame(productionmodel(gCday=free, lma=lma_mean, 
                         frfrac=fr_frac_mean,crfrac=cr_frac_mean,stemfrac=stem_frac_mean, 
                         leaffrac=lf, returnwhat="all"))

O <- function(leaffrac, ...){
  
  p <- productionmodel(leaffrac=lf, ...)
  
  LMF <- p[4]
  
  (LMF - LMF_measured)^2
  
}

optimize(O, c(0,1), gCday=free, lma=lma_mean, 
         frfrac=ofrac,crfrac=ofrac,stemfrac=ofrac, 
         leaffrac=lf, returnwhat="lastval")
