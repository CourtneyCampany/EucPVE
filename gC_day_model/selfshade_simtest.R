source("functions and packages/plot objects.R")
source("functions and packages/functions.R")

library(plyr)
library(doBy)
library(scales)

source("gC_day_model/model_start.R")
source("functions and packages/massmodel.R")
source("functions and packages/massmodel2.R")

####120 days of ALEAF---------------------------------------------------------------------------------------------------

#list of Cday over 120 days by trt
Cday_120 <- dlply(Aleaf, .(volume))

c120_trt<- lapply(Cday_120, "[", 3)


#1: test with means

test <- list()
for(i in 1:7){
  test[[i]] <- data.frame(productionmodel(gCday=c120_trt[[i]][[1]], 
                lma=lma_mean,frfrac=fr_frac_mean, crfrac=cr_frac_mean, stemfrac=stem_frac_mean,
                leaffrac=lf,returnwhat="lastval"))
}

test2 <- list()
for(i in 1:7){
  test2[[i]] <- data.frame(productionmodel2(gCday=c120_trt[[i]][[1]], 
               lma=lma_mean,frfrac=fr_frac_mean, crfrac=cr_frac_mean, stemfrac=stem_frac_mean,
               leaffrac=lf,M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],returnwhat="lastval"))
}

#3. test with 

test3 <- list()
for (i in 1:7){
  test3[[i]] <- data.frame(productionmodel2(gCday=c120_trt[[i]][[1]],
                  lma=lma_trt[i],frfrac=frfrac_trt[i], crfrac=crfrac_trt[i], stemfrac=stemfrac_trt[i],
                   leaffrac=leaffrac_trt[i],M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],returnwhat="lastval"))
}
