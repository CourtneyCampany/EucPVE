source("functions and packages/massmodel.R")
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")
library(plyr)
library(doBy)

#experiment length---------------------------------------------------------------------
numdays <- as.numeric(as.Date("2013-05-21") - as.Date("2013-01-21"))
volume <- c("5", "10", "15", "20", "25", "35", "1000")

#date is for plotting, starts day2
uniqueDate <- seq.Date(from=as.Date("2013/01/22"), to=as.Date("2013/05/21"), by="days")

#harvest mass and leaf area for model comparison----------------------------------------------------------------
harvestmass <- read.csv("calculated data/seedling mass.csv")
mass_total <- harvestmass[,c(1:2,11)]
mass_agg <- summaryBy(totalmass ~volume, data=mass_total, FUN=mean, keep.names=TRUE)
LA_harvest <- read.csv("calculated data/LA_harvest.csv")
LA_agg <- summaryBy(totalarea ~volume, data=LA_harvest, FUN=mean, keep.names=TRUE)

mass_actual <- data.frame(volume = LA_agg$volume, mass = mass_agg$totalmass, leafarea = (LA_agg$totalarea * 10^-4))
#write.csv(mass_actual, "calculated data/harvest_mass_means.csv", row.names=FALSE)

#pre seedling data for intial biomass and leaf area (use mean)--------------------------------------------------
seedling_pre <- read.csv("raw data/seedling_initial.csv")
seedling_pre$seedling_mass <- with(seedling_pre, leaf_mass+root_mass+wood_mass)
seedling_pre$rootshoot <- with(seedling_pre, root_mass/(leaf_mass+wood_mass))
leaffractions <- mean (seedling_pre$leaf_mass/seedling_pre$seedling_mass)
#average mass of seedlings at start
#mass_mean <- mean(seedling_pre$seedling_mass)
mean_leafnum <- mean(seedling_pre$leaf_numb)
pre_root <- mean(seedling_pre$root_mass)
pre_stem <- mean(seedling_pre$wood_mass)

#root-shoot ratios, and froot and croot mass fractions---------------------------------------------------------

#harvest
ratio <- subset(harvestmass, select = c("ID", "volume", "fineroot", "Croot", "stemmass", "leafmass",
                                        "root", "shoot", "totalmass"))
ratio$rootshoot <-with(ratio, root/shoot)
ratio$froot_frac <- with(ratio, fineroot/totalmass)
ratio$croot_frac <- with(ratio, Croot/totalmass)
ratio$stem_frac <- with(ratio, stemmass/totalmass)
ratio$leaf_frac <- with(ratio, leafmass/totalmass)
ratio_agg <- summaryBy(rootshoot+froot_frac+croot_frac ~volume, data=ratio, FUN=mean, keep.names=TRUE)
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
#average starting leaf area
#LA_start <- (mean_leafnum * leafarea_mean) #(m2)

#read in Anet conver to gC day for each volume-----------------------------------------

##need to convert modelled 15min A into g C m2 day
Amodel <- read.csv("calculated data/Aleaf_pred_15min.csv")
Amodel$Date <- as.Date(Amodel$Date)
Amodel$volume <- as.factor(Amodel$volume)
Amodel$photo15gc <- with(Amodel, Anet*15*60*10^-6*12)

Aleaf <- summaryBy(photo15gc ~ Date+volume, data=Amodel, FUN=sum, keep.names=TRUE )
names(Aleaf)[3] <- "carbon_day"
#write.csv(Aleaf, "calculated data/model_runs/cday_120.csv", row.names=FALSE)
Aleaf_agg <- summaryBy(carbon_day ~ volume, data=Aleaf, FUN=mean, keep.names=TRUE )
#write.csv(Aleaf_agg, "calculated data/model_runs/gCday_means.csv", row.names=FALSE)SE)


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


###model run for each volume

###uses hearvest allocation, generate 7 outpus and compare them against interpolated leaf area

sim5 <- data.frame(productionmodel(gCday=Cday[1], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                   stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))

sim10 <- data.frame(productionmodel(gCday=Cday[2], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                   stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))
sim15 <- data.frame(productionmodel(gCday=Cday[3], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                   stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))
sim20 <- data.frame(productionmodel(gCday=Cday[4], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                   stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))
sim25 <- data.frame(productionmodel(gCday=Cday[5], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                   stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))
sim35 <- data.frame(productionmodel(gCday=Cday[6], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                   stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))
simfr <- data.frame(productionmodel(gCday=Cday[7], lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, 
                                   stemfrac=stem_frac_mean,leaffrac=lf, returnwhat="all"))


#####harvest mass
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
mass_actual$Date <- as.Date("2013-05-21")

###leaf area
#leaf area interpolated--------------------------------------------------------------------------
leafarea_time <- read.csv("calculated data/cumulative leaf area.csv")
leafarea_time <-datevol_func (leafarea_time)

la_sp <- dlply(leafarea_time, .(volume))
Date<- seq(as.Date("2013-01-22"),as.Date("2013-05-21"), by="days")


#colors
cols <- gradient(7)

windows(10,8)
par(mar=c(5,5,2,2))
plot(sim5$leafarea~Date, col=cols[1], pch=16, xlab="", ylab=LAm2, ylim=c(0, .6))
  points(sim10$leafarea~Date, col=cols[2], pch=16)
  points(sim15$leafarea~Date, col=cols[3], pch=16)
  points(sim20$leafarea~Date, col=cols[4], pch=16)
  points(sim25$leafarea~Date, col=cols[5], pch=16)
  points(sim35$leafarea~Date, col=cols[6], pch=16)
  points(simfr$leafarea~Date, col=cols[7], pch=17)
  #add real
  d_ply(leafarea_time, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                                                   col=x$volume, pch = pchs[x$volume],type='b', lwd=2))
dev.copy2pdf(file= "gc_day_model/model_ouput/leafarea.pdf")
dev.off()


###Test biomass projects
windows(10,8)
par(mar=c(5,5,2,2))
plot(sim5$biomass~Date, col=cols[1], pch=16, xlab="", ylab="Biomass  (g)", ylim=c(0, 225))
  points(sim10$biomass~Date, col=cols[2], pch=16)
  points(sim15$biomass~Date, col=cols[3], pch=16)
  points(sim20$biomass~Date, col=cols[4], pch=16)
  points(sim25$biomass~Date, col=cols[5], pch=16)
  points(sim35$biomass~Date, col=cols[6], pch=16)
  points(simfr$biomass~Date, col=cols[7], pch=17)
  #add real mass
  points(mass~Date, data=mass_actual, pch=pchs,  col=cols,cex=2)
dev.copy2pdf(file= "gc_day_model/model_ouput/biomass.pdf")
dev.off()

