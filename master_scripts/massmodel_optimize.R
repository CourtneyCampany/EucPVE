source("functions and packages/startscripts.R")

source("master_scripts/model_start.R")
source("master_scripts/massmodel.R")

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

#4. ex of Base Model ex. using mean allocation, 120 days of Cday for reference (missing M)
    # sim_free <- productionmodel(gCday=free, lma=lma_mean,frfrac=fr_frac_mean,crfrac=cr_frac_mean,
    #             stemfrac=stem_frac_mean, leaffrac=lf, 
    #             returnwhat="lastval")

    
#5. optimise leaf mass fraction from free plant with harvest mass and leaf mass
  O.lmf <- function(leaffrac, ..., returnhow=c("objective","output")){
    
    returnhow <- match.arg(returnhow)
    
    ofrac <- (1-leaffrac)/3
    
    p <- productionmodel(leaffrac=leaffrac, frfrac=ofrac,crfrac=ofrac,stemfrac=ofrac,
                         gCday=free, lma=lma_trt[7], M_slope=Mcoef$b[7], M_intercept= Mcoef$intercept[7],
                         ...)
    if(returnhow == "output")return(p)
    
    LEAF <- p[3]
    MASS <- p[1]
    
    (LEAF - LEAF_measured)^2 + (MASS - MASS_measured)^2
    
  }
  
#6. Visualize if there is an optimum
  # lfs <- seq(0.05,0.3, length=101)
  # test_opt <- mapply(O.lmf, leaffrac=lfs)
  # plot(lfs,test_opt)
  
#7. Rerun model to see correspondence 
  
  opt_free<- optimize(O.lmf, c(0,1))
  
  O.lmf(leaffrac=opt_free$minimum, returnhow="output")
  
  opt_lmf <- opt_free$minimum
  
  opt_ofrac <- (1-opt_lmf)/3
  
  #model now uses optimized LMF for all volume trts, includes self shadeing
  optmassmodel<- list()
  for(i in 1:7){
    optmassmodel[[i]] <- data.frame(productionmodel(gCday=c120_trt[[i]][[1]], 
                         lma=lma_trt[i],frfrac=opt_ofrac, crfrac=opt_ofrac, stemfrac=opt_ofrac,
                         leaffrac=opt_lmf,M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],returnwhat="lastval"))
  }  
  ##return all values
  optmassmodel2<- list()
  for(i in 1:7){
    optmassmodel2[[i]] <- data.frame(productionmodel(gCday=c120_trt[[i]][[1]], 
                                                     lma=lma_trt[i],frfrac=opt_ofrac, crfrac=opt_ofrac, stemfrac=opt_ofrac,
                                                     leaffrac=opt_lmf,M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
                                                     returnwhat="all"))
  }  
  
###save dataframe of biomass over time for each trt
biomass_time <- rbind.fill(optmassmodel2)
  potvol <- c(rep(5, 120), rep(10, 120), rep(15, 120), rep(20, 120), rep(25, 120), rep(35, 120), rep(1000, 120))
biomass_time$volume <- potvol
write.csv(biomass_time, "calculated data/biomass_time.csv", row.names=FALSE)
  
#8. Calculate total C gain per plant, draw leaf area from model with optimized LMF
  #add self shading
  sigma <- read.csv("calculated data/M_leafarea_model.csv")
  
  totalC_list <- list()
  for(i in 1:7){
    totalC_calc <- optmassmodel2[[i]][2] * c120_trt[[i]][1:120,] * (sigma[i,3] * optmassmodel2[[i]][2] + sigma[i,2])
    
    totalC_list[[i]] <- totalC_calc
  }                  
  
  totalC_trt <- lapply(totalC_list, function(x) sum(x))
  totalC_trt2 <- unlist(totalC_trt)
  

#9. plot plant carbon with optmized LMF sim vs total c gain (LA from sim cday120) and scaled------------------------------
  pch3 <- c(rep(1,6), 6)
  
  #harvest mass (treatment means)
  mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
  mass_actual$Date <- as.Date("2013-05-21")
  mass_actual$mass_adj <- with(mass_actual, mass/mass[7])
  
  
  #vector of model output to make line for plot (a)
  plantC <- lapply(optmassmodel, function(x){as.data.frame(.5 * x[1,1])})
    library(plyr)
    plantC <- rbind.fill(plantC)
  plantC2 <- cbind(plantC,totalC_trt2 )
    names(plantC2)[1] <- "netCgain"
  
  #data for CUE figure (b)
  cue <- read.csv("calculated data/CUEdaily.csv") 
  #Compare estimated Daily C gain with final mass from harvest
  harvestC <- merge(mass_actual[,1:2], cue)
    harvestC$massC <- harvestC$mass*.5  
    harvestC$volume <- gsub(1000, 40, harvestC$volume)
    

  # windows(7,7)
  par(cex.axis=.96, cex.lab=1.2,mfrow=c(2,1),oma=c(0.1,0.1,0.1,0.1), las=1)   
  
  par(mar=c(4,5.5,2,2), cex.axis=0.8, las=1)
  #plot(a)
  plot(0.5*mass_actual$mass ~ totalC_trt2,pch=pchs,col=palette(),cex=1.6, xlim=c(0, 200),
       ylim=c(0, 200), ylab="Seedling Mass (g C)", xlab="Net Total Leaf Carbon Gain (g C)")
  segments(x0=min(plantC2$totalC_trt2), x1=max(plantC2$totalC_trt2),y0=min(plantC2$netCgain), y1=max(plantC2$netCgain), 
           lwd=3, lty=3, col="darkblue")
  abline(0,1, lwd=2, lty=2, col="grey35")
  text(0,195,"(a)", cex=1.2)

  legend("bottomright", leglab, pch=pchs,text.font=1, title=vollab, col=palette(), bty='n',cex=1.0)
  #plot(b)
  par(mar=c(4,5.5,1,2))
  plot(massC/tdc_net.sum ~ volume, data=harvestC, ylim=c(.25, .45), xaxt='n', cex=1.5, pch=pchs, col=cols, 
       ylab="Seedling Mass (g C) / \nModelled Total Net C Gain (g C)", 
       xlab="Soil Volume (l)")
  axis(1, at=c(5,10,15,20,25,35,40), labels=c(5,10,15,20,25,35,"Free"))
  text(5,.445,"(b)", cex=1.2)

##results section-----------------------------------------------------------------------------------------------------------
mean(totalC_trt2[1:6])
se(totalC_trt2[1:6])

leftoverC <- (totalC_trt2-cmass)/totalC_trt2



