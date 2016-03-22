library(doBy)
library(scales)

source("functions and packages/plot objects.R")

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

# #6. Visualize if there is an optimum
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

#S3: increases in root respiration (+-50%) , keep free that same

#need a vector of respiration to pass that alters root respiration for containers but not free 
fr_default <- fr_resp
cr_default <- cr_resp
wd_default <- wd_resp

fr_up <- fr_default*5
cr_up <- cr_default*5
wd_up <- wd_default*5

fr_resp_up <- c(rep(fr_up,6),fr_default)
cr_resp_up <- c(rep(cr_up,6),cr_default)
wd_resp_up <- c(rep(wd_up,6),wd_default)


#root respiration up
resp_up<- list()
for(i in 1:7){
  resp_up[[i]] <- data.frame(productionmodel2(gCday=c120_trt[[i]][[1]], lma=lma_trt[i],frfrac=opt_ofrac, 
                                              crfrac=opt_ofrac, stemfrac=opt_ofrac,leaffrac=opt_lmf,
                                              M_slope=Mcoef$b[i], M_intercept= Mcoef$intercept[i],
                                              fr_resp=fr_resp_up[i], cr_resp= cr_resp_up[i],
                                              wd_resp=wd_resp_up[i],
                                              returnwhat="lastval"))
}   



####PLOT scenarios

####add actual mass and Cday scaled
##mean daily carbon gain scales
C_stnd <- read.csv("calculated data/model_runs/gCday_means_clean.csv")
C_stnd$C_stnd_free <- with(C_stnd, carbon_day/carbon_day[7])

##modelled biomass scaled
C_stnd$modelmass <- c(optmassmodel[[1]][1,1], optmassmodel[[2]][1,1], optmassmodel[[3]][1,1], optmassmodel[[4]][1,1], 
                      optmassmodel[[5]][1,1],optmassmodel[[6]][1,1],optmassmodel[[7]][1,1])
C_stnd$model_stnd_free <- with(C_stnd, modelmass/modelmass[7])


##modelled biomass scaled (scenario 1)
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
legname <- c("model", "scenario", "acutal")


windows(7,10)


#scenario 2 
par(mar=c(5,5,2,2))
plot(C_stnd$resp_up_stnd_free ~ C_stnd$C_stnd_free , xlim=c(1,.6), ylim=c(0, 1),pch=pch3,bg=lmfcols2,cex=1,
     xlab="",
     ylab= expression(Seedling~Carbon~Scaled[free]),
     xaxt='n')
axis(1, at=c(1,.9,.8,.7, .6), labels=FALSE, tcl=0.5)
points(mass_actual$mass_adj ~ C_stnd$C_stnd_free , pch=pchs,col=palette(),cex=1)
points(C_stnd$model_stnd_free ~ C_stnd$C_stnd_free, pch=pch2, col=palette(),cex=1)
text(x=.65,y=.985, "(b) Respiration +250%", cex=1.51)


