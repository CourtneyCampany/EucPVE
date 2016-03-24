# source("functions and packages/startscripts.R")

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

#4. ex of Base Model using mean allocation, 120 days of Cday for reference
# 
#     sim_free <- productionmodel2(gCday=free, lma=lma_mean,frfrac=fr_frac_mean,crfrac=cr_frac_mean,
#                 stemfrac=stem_frac_mean, leaffrac=lf, M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
#                 returnwhat="lastval")


#5. optimise leaf mass fraction from free plant with harvest mass and leaf area
O.lmf <- function(leaffrac, ..., returnhow=c("objective","output")){
  
  returnhow <- match.arg(returnhow)
  
  ofrac <- (1-leaffrac)/3
  
  p <- productionmodel(leaffrac=leaffrac, frfrac=ofrac,crfrac=ofrac,stemfrac=ofrac,
                        gCday=free, lma=lma_trt[7],M_slope=Mcoef$b[7], M_intercept= Mcoef$intercept[7],
                        ...)
  if(returnhow == "output")return(p)
  
  LEAF <- p[3]
  MASS <- p[1]
  
  (LEAF - LEAF_measured)^2 + (MASS - MASS_measured)^2
  
}

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
                                                   leaffrac=opt_lmf,M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
                                                   returnwhat="lastval"))
}  

#8. Test scenarios--------------------------------------------------------------------------------------------------------

#S1: alter leaf allocation (by using harvested lmf this shows how over/under estimate of model and observed still miss with C
harvest_lmf <-  list()
for(i in 1:7){
  harvest_lmf[[i]] <- data.frame(productionmodel(gCday=c120_trt[[i]][[1]], lma=lma_trt[i],leaffrac=leaffrac_trt[i],
                                                  frfrac=(1-leaffrac_trt[i])/3, 
                                                  crfrac=(1-leaffrac_trt[i])/3, 
                                                  stemfrac=(1-leaffrac_trt[i])/3,
                                                  M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
                                                  returnwhat="lastval"))
}   
  
  
  
#S2: increases in root respiration (+50%) , keep free that same

  #need a vector of respiration to pass that alters root respiration for containers but not free 
  fr_default <- fr_resp
  cr_default <- cr_resp
  wd_default <- wd_resp
  
  fr_up <- fr_default*1.5
  cr_up <- cr_default*1.5
  wd_up <- wd_default*1.5
  
  fr_resp_up <- c(rep(fr_up,6),fr_default)
  cr_resp_up <- c(rep(cr_up,6),cr_default)
  wd_resp_up <- c(rep(wd_up,6),wd_default)
  
  
  #root respiration up
  resp_up<- list()
  for(i in 1:7){
  resp_up[[i]] <- data.frame(productionmodel(gCday=c120_trt[[i]][[1]], lma=lma_trt[i],frfrac=opt_ofrac, 
                                                      crfrac=opt_ofrac, stemfrac=opt_ofrac,leaffrac=opt_lmf,
                                                      M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
                                                      fr_resp=fr_resp_up[i], cr_resp= cr_resp_up[i],
                                                      wd_resp=wd_resp_up[i],
                                                      returnwhat="lastval"))
    }   
  

####PLOT scenarios----------------------------------------------------------------------------------------------------------
  
  ##add actual mass and Cday scaled
  ##mean daily carbon gain scaled
  C_stnd <- read.csv("calculated data/Aleaf_model/gCday_means_clean.csv")
    C_stnd$C_stnd_free <- with(C_stnd, carbon_day/carbon_day[7])
    
    ##modelled biomass scaled
    C_stnd$modelmass <- c(optmassmodel[[1]][1,1], optmassmodel[[2]][1,1], optmassmodel[[3]][1,1], optmassmodel[[4]][1,1], 
                          optmassmodel[[5]][1,1],optmassmodel[[6]][1,1],optmassmodel[[7]][1,1])
    C_stnd$model_stnd_free <- with(C_stnd, modelmass/modelmass[7])
  
    ##modelled biomass scaled (scenario 1)
    C_stnd$harvest_lmf <- c(harvest_lmf[[1]][1,1], harvest_lmf[[2]][1,1], harvest_lmf[[3]][1,1], harvest_lmf[[4]][1,1], 
                          harvest_lmf[[5]][1,1],harvest_lmf[[6]][1,1],harvest_lmf[[7]][1,1])
    C_stnd$lmf_stnd_free <- with(C_stnd, harvest_lmf/harvest_lmf[7])
    ##modelled biomass scaled (scenario 2,3)
    C_stnd$resp_up <- c(resp_up[[1]][1,1], resp_up[[2]][1,1], resp_up[[3]][1,1], resp_up[[4]][1,1], 
                        resp_up[[5]][1,1],resp_up[[6]][1,1],resp_up[[7]][1,1])

    
    C_stnd$resp_up_stnd_free <- with(C_stnd, resp_up/resp_up[7])

    
#harvest mass (treatment means)
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
  mass_actual$Date <- as.Date("2013-05-21")
  mass_actual$mass_adj <- with(mass_actual, mass/mass[7])
  
#####colors and symbols
    ###use scales to get .5 col on palettee
    volcols <- palette()
    lmfcols <- alpha(volcols, alpha=0.5)
    lmfcols2 <- alpha(volcols, alpha=0.25)
    lmfcols3 <- alpha(volcols, alpha=0.75)
    blksha <- alpha("black", alpha=0.5)
  
    pch3 <- c(rep(21,6), 25)
    #modelegend
    legendpch <- c(21,21,21)
    legcol <-c("white", blksha, "black")
    legname <- c("model", "scenario", "measured")
  
    
# windows(7,10)
  ####multipanel plot of s1, s2
  par(cex.axis=.96, cex.lab=1.2, las=1,mgp=c(3.5,1,0),mfrow=c(2,1),  
      omi=c(.5,0,0.1,0.1))
  
  #scenario 1 
  par(mar=c(0,7,2,2))
  plot(C_stnd$lmf_stnd_free ~ C_stnd$C_stnd_free , xlim=c(1,.6), ylim=c(0, 1),pch=pch3,bg=lmfcols2,cex=1.6,
       xlab="",
       ylab= "",
       xaxt='n')
  axis(1, at=c(1,.9,.8,.7, .6), labels=FALSE, tcl=0.5)
  points(mass_actual$mass_adj ~ C_stnd$C_stnd_free , pch=pchs,col=palette(),cex=1.6)
  points(C_stnd$model_stnd_free ~ C_stnd$C_stnd_free, pch=pch2, col=palette(),cex=1.6)
  text(x=.925,y=1, "(a) Harvest LMF", cex=1)
  mtext(expression(Seedling~Carbon~Scaled[free]), mgp=c(3, 1, 0), side=2, padj=-3, adj=-.75,las=0, cex=1.2)
  legend("topright",legname, pch=legendpch,text.font=3,cex=1, pt.bg=legcol, bty='n')
  
  #scenario 2 
  par(mar=c(5,7,0,2))
  plot(C_stnd$resp_up_stnd_free ~ C_stnd$C_stnd_free , xlim=c(1,.6), ylim=c(0, 1),pch=pch3,bg=lmfcols2,cex=1.6,
       xlab=expression(Mean~Daily~Carbon~Assimilation~Scaled[free]),
       ylab= "")
  axis(1, at=c(1,.9,.8,.7, .6), labels=FALSE, tcl=0.5)
  points(mass_actual$mass_adj ~ C_stnd$C_stnd_free , pch=pchs,col=palette(),cex=1.6)
  points(C_stnd$model_stnd_free ~ C_stnd$C_stnd_free, pch=pch2, col=palette(),cex=1.6)
  text(x=.91,y=1, "(b) Respiration +50%", cex=1)
    legend("topright", leglab, pch=c(rep(16,6),17),text.font=3, inset=0.025, title=vollab, 
           cex=1, col=palette(), bty='n')

  # dev.copy2pdf(file= "master_scripts/manuscript_figs/massmodel_resp.pdf")
  # dev.off()
  


###calculate percent diff between model and observed biomass----------------------------------------------------------
#     Rup <- 0
#     for(i in 1:7){
#       Rup[i] <- (resp_up[[i]][[1,1]] - mass_actual$mass[i]) / resp_up[[i]][[1,1]]
#     }
#       
#       mean(Rup[1:6])
#       se(Rup[1:6])
#   
#      lmf <- 0
#       for(i in 1:7){
#         lmf[i] <- (harvest_lmf[[i]][[1,1]] - mass_actual$mass[i]) / harvest_lmf[[i]][[1,1]]
#       }
#       
#       mean(lmf[1:6])
#       se(lmf[1:6])
#       
#       
#      mean(C_stnd$resp_up_stnd_free)
#      se(C_stnd$resp_up_stnd_free)
#      
#      mean(C_stnd$lmf_stnd_free)
#      se(C_stnd$lmf_stnd_free)
#      
#      mean(C_stnd$model_stnd_free)
#      se(C_stnd$model_stnd_free)
                          