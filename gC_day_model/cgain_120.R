library(plyr)
library(doBy)

#experiment length---------------------------------------------------------------------
numdays <- as.numeric(as.Date("2013-05-21") - as.Date("2013-01-21"))
volume <- c("5", "10", "15", "20", "25", "35", "1000")

#date is for plotting, starts day2
uniqueDate <- seq.Date(from=as.Date("2013/01/22"), to=as.Date("2013/05/21"), by="days")

#harvest mass and leaf area for model comparison----------------------------------------------------------------
harvestmass <- read.csv("calculated data/seedling mass.csv")
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")


#pre seedling data for intial biomass and leaf area (use mean)--------------------------------------------------
seedling_pre <- read.csv("raw data/seedling_initial.csv")
  seedling_pre$seedling_mass <- with(seedling_pre, leaf_mass+root_mass+wood_mass)
  seedling_pre$rootshoot <- with(seedling_pre, root_mass/(leaf_mass+wood_mass))

  leaffractions <- mean (seedling_pre$leaf_mass/seedling_pre$seedling_mass)
  #average mass of seedlings at start
  mean_leafnum <- mean(seedling_pre$leaf_numb)
  pre_root <- mean(seedling_pre$root_mass)
  pre_stem <- mean(seedling_pre$wood_mass)

#root-shoot ratios, and froot and croot mass fractions---------------------------------------------------------
ratio <- subset(harvestmass, select = c("ID", "volume", "fineroot", "Croot", "stemmass", "leafmass",
                                        "root", "shoot", "totalmass"))
  ratio$rootshoot <-with(ratio, root/shoot)
  ratio$froot_frac <- with(ratio, fineroot/totalmass)
  ratio$croot_frac <- with(ratio, Croot/totalmass)
  ratio$stem_frac <- with(ratio, stemmass/totalmass)
  ratio$leaf_frac <- with(ratio, leafmass/totalmass)

#mean component fractions
rs_mean <- mean(ratio$rootshoot)
fr_frac_mean <- mean(ratio$froot_frac)
cr_frac_mean <- mean(ratio$croot_frac)
stem_frac_mean <- mean(ratio$stem_frac)
#fraction volume means
fr_frac_vol <-  summaryBy(froot_frac ~volume, data=ratio, FUN=mean, keep.names=TRUE)
cr_frac_vol <- summaryBy(croot_frac ~volume, data=ratio, FUN=mean, keep.names=TRUE)
stem_frac_vol <- summaryBy(stem_frac ~volume, data=ratio, FUN=mean, keep.names=TRUE)
leaf_frac_vol <- summaryBy(leaf_frac ~volume, data=ratio, FUN=mean, keep.names=TRUE)

#pre
rootshoot_pre_mean <- mean(seedling_pre$rootshoot)

#mean lma and leafarea (apply to intial leaf )------------------------------------------------------------------
lma <- read.csv("calculated data/leafmassarea.csv")
lma_vol <- summaryBy(massarea ~volume, data=lma, FUN=mean, keep.names=TRUE)
#lma_mean <- mean(lma$massarea)
leafarea_mean <- (mean(lma$area))/10000

#read gC day for each volume-----------------------------------------
Aleaf <- read.csv("calculated data/model_runs/cday_120.csv")
Aleaf_agg <- read.csv("calculated data/model_runs/gCday_means.csv")
Aleaf_agg$Cday_scale<- with(Aleaf_agg, carbon_day/carbon_day[7])


####MODEL---------------------------------------------------------------------------------

##model start values
lma_mean <- mean(lma$massarea)#average lma from harvest
LA_start <- (mean_leafnum * leafarea_mean) #(m2)
mass_mean <- mean(seedling_pre$seedling_mass)
Cday <- as.vector(Aleaf_agg[,2]) 

fr_resp = .010368 #gC/gFroot day Marsden et al
cr_resp = .00124#gC/gCroot day
wd_resp = .001877 #gc/gwood day Drake with 1.86 q10 to 20C

#vectors of 7 treaments in order
lma_trt <- as.vector(lma_vol[,2])
frfrac_trt <- as.vector(fr_frac_vol[,2])
crfrac_trt <- as.vector(cr_frac_vol[,2])
stemfrac_trt <- as.vector(stem_frac_vol[,2])
leaffrac_trt <- as.vector(leaf_frac_vol[,2])

lf <- mean(leaffrac_trt)

####model
source("functions and packages/massmodel.R")

#####120 days of ALEAF

  #1 sequence over the range of mean values
  gcday_seq_obs <- seq(max(Cday), min(Cday), length=101) 

  #2 list of Cday over 120 days by trt
  Cday_120 <- dlply(Aleaf, .(volume))

  c120_trt<- lapply(Cday_120, "[", 3)

test1 <- data.frame(c120_trt[1])
test2 <- as.vector(test1[1:121,])


C5 <- data.frame(c120_trt[1])
C5 <- as.vector(C5[1:121,])

C10 <- data.frame(c120_trt[2])
C10 <- as.vector(C10[1:121,])

C15 <- data.frame(c120_trt[3])
C15 <- as.vector(C15[1:121,])

C20 <- data.frame(c120_trt[4])
C20 <- as.vector(C20[1:121,])

C25 <- data.frame(c120_trt[5])
C25 <- as.vector(C25[1:121,])

C35 <- data.frame(c120_trt[6])
C35 <- as.vector(C35[1:121,])

Cf <- data.frame(c120_trt[7])
Cf <- as.vector(Cf[1:121,])

###simple run for each treatemtn using 120 values of Cday
mod5 <- data.frame(productionmodel(gCday=C5, lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean,
                                   stemfrac=stem_frac_mean, leaffrac=lf, returnwhat="all"))

mod10 <- data.frame(productionmodel(gCday=C10, lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                    stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))

mod15 <- data.frame(productionmodel(gCday=C15, lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                    stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))

mod20 <- data.frame(productionmodel(gCday=C20, lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                    stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))


mod25 <- data.frame(productionmodel(gCday=C25, lma=lma_mean, 
                                    frfrac=fr_frac_mean, crfrac=cr_frac_mean, stemfrac=stem_frac_mean,         
                                    leaffrac=lf, returnwhat="all"))


mod35 <- data.frame(productionmodel(gCday=C35, lma=lma_mean,  frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                    stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))


modfree <- data.frame(productionmodel(gCday=Cf, lma=lma_mean,  frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                      stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))


##compare to actual
source("functions and packages/plot objects.R")
source("functions and packages/functions.R")
Date<- seq(as.Date("2013-01-22"),as.Date("2013-05-21"), by="days")
cols <- gradient(7)

#####harvest mass
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
mass_actual$Date <- as.Date("2013-05-21")

###leaf area
#leaf area interpolated--------------------------------------------------------------------------
leafarea_time <- read.csv("calculated data/cumulative leaf area.csv")
leafarea_time <-datevol_func (leafarea_time)

la_sp <- dlply(leafarea_time, .(volume))

windows(10,8)
par(mar=c(5,5,2,2))
plot(mod5$leafarea~Date, col=cols[1], pch=16, xlab="", ylab=LAm2, ylim=c(0, .6))
  points(mod10$leafarea~Date, col=cols[2], pch=16)
  points(mod15$leafarea~Date, col=cols[3], pch=16)
  points(mod20$leafarea~Date, col=cols[4], pch=16)
  points(mod25$leafarea~Date, col=cols[5], pch=16)
  points(mod35$leafarea~Date, col=cols[6], pch=16)
  points(modfree$leafarea~Date, col=cols[7], pch=17)
d_ply(leafarea_time, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                      col=x$volume, pch = pchs[x$volume],type='b', lwd=2))

windows(10,8)
par(mar=c(5,5,2,2))
plot(mod5$biomass~Date, col=cols[1], pch=16, xlab="", ylab="Biomass  (g)", ylim=c(0, 200))
  points(mod10$biomass~Date, col=cols[2], pch=16)
  points(mod15$biomass~Date, col=cols[3], pch=16)
  points(mod20$biomass~Date, col=cols[4], pch=16)
  points(mod25$biomass~Date, col=cols[5], pch=16)
  points(mod35$biomass~Date, col=cols[6], pch=16)
  points(modfree$biomass~Date, col=cols[7], pch=17)

  points(mass~Date, data=mass_actual, pch=pchs,  col=cols,cex=2)


######no use these values and scalers with 101 seqence 

###scaling factors here

#reduction in photosynthesis
cdayscale <- seq(1, min(Aleaf_agg$Cday_scale), length=101)
#self shading
#M

##### sun model for each volume with 120days of Cday
  
out5 <- list()
for(j in 1:101){
  out5[[j]] <- productionmodel(gCday=C5*cdayscale[j], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                              stemfrac=stem_frac_mean,leaffrac=lf)
}

out10 <- list()
for(j in 1:101){
  out10[[j]] <- productionmodel(gCday=C10*cdayscale[j], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                               stemfrac=stem_frac_mean,leaffrac=lf)
}

out15 <- list()
for(j in 1:101){
  out15[[j]] <- productionmodel(gCday=C15*cdayscale[j], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                stemfrac=stem_frac_mean,leaffrac=lf)
}
out20 <- list()
for(j in 1:101){
  out20[[j]] <- productionmodel(gCday=C20*cdayscale[j], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                stemfrac=stem_frac_mean,leaffrac=lf)
}
out25 <- list()
for(j in 1:101){
  out25[[j]] <- productionmodel(gCday=C25*cdayscale[j], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                stemfrac=stem_frac_mean,leaffrac=lf)
}
out35 <- list()
for(j in 1:101){
  out35[[j]] <- productionmodel(gCday=C35*cdayscale[j], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                stemfrac=stem_frac_mean,leaffrac=lf)
}

outfree <- list()
for(j in 1:101){
  outfree[[j]] <- productionmodel(gCday=Cf*cdayscale[j], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                stemfrac=stem_frac_mean,leaffrac=lf)
}

library(plyr)
outf <- llply(outfree, function(x) as.data.frame(x))
outf1 <- rbind.fill(outf)


