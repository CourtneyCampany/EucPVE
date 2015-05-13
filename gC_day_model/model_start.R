library(plyr)

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
Aleaf <- read.csv("calculated data/model_runs/cday_120_clean.csv")
Aleaf_agg <- read.csv("calculated data/model_runs/gCday_means_clean.csv")
Aleaf_agg$Cday_scale<- with(Aleaf_agg, carbon_day/carbon_day[7])

test <- read.csv("calculated data/Aleaf_pred_15min.csv")
test_agg <- read.csv("calculated data/model_runs/gCday_means.csv")
test_agg$Cday_scale<- with(Aleaf_agg, carbon_day/carbon_day[7])


#leaf area interpolated
leafarea_time <- read.csv("calculated data/LApred_volume.csv")

#read in M regression coefs for model
Mcoef <- read.csv("gC_day_model/M_leafarea_model.csv")

####MODEL---------------------------------------------------------------------------------
LA_sp <- dlply(leafarea_time, .(volume))
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