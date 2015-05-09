source("functions and packages/plot objects.R")
source("functions and packages/functions.R")

library(plyr)
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
                         leaffrac=opt_lmf,M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],returnwhat="lastval"))
  }  
  ##return all values
  optmassmodel2<- list()
  for(i in 1:7){
    optmassmodel2[[i]] <- data.frame(productionmodel2(gCday=c120_trt[[i]][[1]], 
                                                     lma=lma_trt[i],frfrac=opt_ofrac, crfrac=opt_ofrac, stemfrac=opt_ofrac,
                                                     leaffrac=opt_lmf,M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
                                                     returnwhat="all"))
  }  
  
#8. Calculate total C gain per plant, draw leaf area from model with optimized LMF
  
  totalC_list <- list()
  for(i in 1:7){
    totalC_calc <- optmassmodel2[[i]][2] * c120_trt[[i]][1:120,]
    
    totalC_list[[i]] <- totalC_calc
  }                 
  
  totalC_trt <- lapply(totalC_list, function(x) sum(x))
  
  totalC_trt2 <- unlist(totalC_trt)
  
#9. plot plant carbon with optmized LMF sim vs total c gain (LA from sim cday120)
  windows(7,5)
  par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
  plot(0.5*mass_actual$mass ~ totalC_trt2,pch=pchs,col=palette(),cex=1.6, xlim=c(0, 200),
       ylim=c(0, 200), ylab="Plant Carbon (g)", xlab="Total Carbon Gain (g)")
  for(i in 1:7){
    points(0.5*optmassmodel[[i]][1,1]~ totalC_trt2[i], pch=pch2, col=cols[i], cex=1.6)
  }
  abline(0,1, lty=2)
  legend("topleft", leglab, pch=pchs,text.font=1, inset=0.025, title=vollab, col=palette(), bty='n',cex=1.0)
  legend("bottomright", simleg, pch=simpch,text.font=1,   inset=0.025,bty='n',cex=1.0)
   dev.copy2pdf(file= "master_scripts/manuscript_figs/massmodel_totalC.pdf")  
   dev.off() 
  
  
#10. plot mass production with optmized LMF sim vs daily photosynthesis
  windows(7,5)
  par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
  plot(mass_actual$mass ~ Cday,pch=pchs,col=palette(),cex=1.6, xlim=c(4,8), 
       ylim=c(0, 250), ylab="Biomass (g)",xlab=cdaylab)
  for(i in 1:7){
    points(optmassmodel[[i]][1,1]~ Cday[i], pch=pch2, col=cols[i], cex=1.6)
  }
  legend("topleft", leglab, pch=pchs,text.font=1, inset=0.025, title=vollab, col=palette(), bty='n',cex=1.0)
  legend("topright", simleg, pch=simpch,text.font=1,   bty='n',cex=1.0)
   dev.copy2pdf(file= "master_scripts/manuscript_figs/massmodel_Cday.pdf")  
   dev.off() 
  

