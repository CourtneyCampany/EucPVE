
##implement other fraction to simplify


require(doBy)

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

#plot(Anet~Date, data=Amodel, subset=volume==35)

#first need sum over day and then means by treatment
Aleaf <- summaryBy(photo15gc ~ Date+volume, data=Amodel, FUN=sum, keep.names=TRUE )
names(Aleaf)[3] <- "carbon_day"
Aleaf_agg <- summaryBy(carbon_day ~ volume, data=Aleaf, FUN=mean, keep.names=TRUE )
#Aleaf25 <- subset(Aleaf_agg, volume == "25")

####MODEL---------------------------------------------------------------------------------

##model start values
lma_mean <- mean(lma$massarea)#average lma from harvest
LA_start <- (mean_leafnum * leafarea_mean) #(m2)
mass_mean <- mean(seedling_pre$seedling_mass)
Cday <- as.vector(Aleaf_agg[,2]) 
#vectors of 7 treaments in order
lma_trt <- as.vector(lma_vol[,2])
frfrac_trt <- as.vector(fr_frac_vol[,2])
crfrac_trt <- as.vector(cr_frac_vol[,2])
stemfrac_trt <- as.vector(stem_frac_vol[,2])
leaffrac_trt <- as.vector(leaf_frac_vol[,2])

lf <- mean(leaffrac_trt)
#volumeid <- as.factor(c(5,10,15,20,25,35,"free"))

# model as a function
productionmodel <- function(leaffrac = .25,
                    crfrac = .25,
                    frfrac = .25,
                    stemfrac=.25,
                    gCday = 1,
                    conversionEfficiency = 0.65,
                    fr_resp = .010368, #gC/gFroot day Marsden et al
                    cr_resp = .00124, #gC/gCroot day
                    wd_resp = .001877, #gc/gwood day Drake with 1.86 q10 to 20C
                    #fr_turn = .48, #production/standing crop
                    numdays=120,
                    lma = 97.5,
                    returnwhat=c("lastval","all")
                    ){
  
  returnwhat <- match.arg(returnwhat)
  
  leafarea <- vector()
  leafarea[1] <- LA_start
  
  biomass <- vector()
  biomass[1] <- mass_mean

  frootmass <- vector()
  frootmass[1] <- pre_root*.5

  crootmass <- vector()
  crootmass[1] <- pre_root*.5
   
  stemmass <- vector()
  stemmass[1] <- pre_stem

  leafmass <- vector()
  leafmass[1] <- LA_start*lma
  
  LMF <- vector()
  LMF[1] <- (LA_start*lma)/mass_mean
  
  
  if(length(gCday) == 1)gCday <- rep(gCday,numdays)
  if(length(gCday) < numdays)stop("Need at least ",numdays," photosynthesis values.")
  
  #run model simulation------------------------------------------------------------------
  for (i in 2:numdays) {
    grossbiomassprod <- leafarea[i-1] * gCday[i]/conversionEfficiency  # gc day-1
    
    netbiomassprod <- grossbiomassprod - fr_resp*frootmass[i-1] - cr_resp*crootmass[i-1] - wd_resp*frootmass[i-1]
    
   #fractions,stems and wood need respiration
    leafmass[i] <- leafmass[i-1] + netbiomassprod*leaffrac
    leafarea[i] <- leafmass[i] / lma
   
    stemmass[i] <- stemmass[i-1] + netbiomassprod*stemfrac
    frootmass[i] <- frootmass[i-1] + netbiomassprod*frfrac
    crootmass[i] <- crootmass[i-1] + netbiomassprod*crfrac

    #total biomass day
    biomass[i] <- leafmass[i-1] + frootmass[i-1] + crootmass[i-1] + stemmass[i-1]
    
    #leaf mass fraction
    LMF[i] <- leafmass[i]/biomass[i]
  }

  if(returnwhat == "lastval")
    return(c(biomass=biomass[numdays],leafarea=leafarea[numdays], leafmass = leafmass[numdays], LMF = LMF[numdays]))
  
  if(returnwhat == "all")
    return(list(biomass=biomass,leafarea=leafarea, leafmass = leafmass, LMF = LMF))
  
}

#run model simulations with sequence of g Cday, change parameter assumptions with each sim----------------------

  gCday_seq <- seq(7,2,length=101)
  mu <- .6

sim_means <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=mu*gCday_seq, lma=lma_mean, 
                                                frfrac=fr_frac_mean, crfrac=cr_frac_mean, stemfrac=stem_frac_mean,         
                                                leaffrac=lf,SIMPLIFY=F)))
sim_means$gCday <- gCday_seq


leaffrac=.25
ofrac <- (1 - leaffrac)/3
modelmass <- as.data.frame(do.call(rbind, mapply(productionmodel, 
                                                 gCday=mu*gCday_seq, 
                                                 lma=lma_mean, 
                                                 frfrac=ofrac, 
                                                 crfrac=ofrac, 
                                                 stemfrac=ofrac,
                                                 leaffrac=leaffrac,
                                                 SIMPLIFY=F)))


#plot bits
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
pchs = c(rep(16,6),17)
ypos <- c(2.5,1,0)

cols <- as.vector(palette())
require(scales)
cols1 <- alpha(cols[1], 0.45)
cols2 <- alpha(cols[2], 0.45)
cols3 <- alpha(cols[3], 0.45)
cols4 <- alpha(cols[4], 0.45)
cols5 <- alpha(cols[5], 0.45)
cols6 <- alpha(cols[6], 0.45)
cols7 <- alpha(cols[7], 0.45)

col_bl <- alpha("black", .45)

treelab<- paste("Seedling Mass Production over ",numdays," days (g)", sep="")
cdaylab <- expression(Daily~Carbon~Gain~~(g~m^-2~d^-1))



#windows()
png(filename = "output/presentations/Cmodel_meanC_massactual.png", width = 12, height = 8, units = "in", res= 400)
par(cex.axis=1.3, cex.lab=1.3)
with(sim_means, plot(biomass~gCday, ylim=c(0,175), xlim=c(0,8), ylab= "", xlab=cdaylab,pch=16,cex=1.6, col=col_bl))
  points( mass_actual$mass~Cday,pch=pchs,col=palette(),cex=1.6)
  #points( modelmass$gCday~modelmass$biomass,pch=pchs,col=palette())
title(ylab=treelab, mgp=ypos)

dev.off()

#make no free
png(filename = "output/presentations/Cmodel_nofree.png", width = 12, height = 8, units = "in", res= 400)
par(cex.axis=1.3, cex.lab=1.3)
with(sim_means, plot(gCday~biomass, xlim=c(0,75), ylim=c(0,8), ylab= "", xlab=treelab, cex=1.6))
  points( mass_actual$mass, Cday,pch=pchs,col=palette(), cex=1.6)
  title(ylab=cdaylab, mgp=ypos)
dev.off()
#component allocation and lma by volume (7 sims) in loop

allsims <- list()
for (i in 1:7){
gCday_seq <- seq(7,2,length=101)

sim <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=mu*gCday_seq, lma=lma_trt[i],frfrac=frfrac_trt[i], 
                  crfrac=crfrac_trt[i], stemfrac=stemfrac_trt[i],leaffrac=leaffrac_trt[i], SIMPLIFY=F)))

  sim$gCday <- gCday_seq

allsims[[i]] <- sim
}
  
#model plotting

png(filename = "output/presentations/Cmodel_leaffrac.png", width = 12, height = 8, units = "in", res= 400)
par(cex.axis=1.3, cex.lab=1.3)
  with(as.data.frame(allsims[1]),plot(biomass~gCday, col=cols1, ylim=c(0,175), xlim=c(0,8), cex=1.6, ylab="", xlab=cdaylab))
    with(as.data.frame(allsims[2]),points(biomass~gCday,col=cols2, cex=1.6))
    with(as.data.frame(allsims[3]),points(biomass~gCday,col=cols3, cex=1.6))
    with(as.data.frame(allsims[4]),points(biomass~gCday,col=cols4, cex=1.6))
    with(as.data.frame(allsims[5]),points(biomass~gCday,col=cols5, cex=1.6))
    with(as.data.frame(allsims[6]),points(biomass~gCday,col=cols6, cex=1.6))
    with(as.data.frame(allsims[7]),points(biomass~gCday,col=cols7, pch=17, cex=1.6))
  points( mass_actual$mass~Cday,pch=pchs,col=palette(), cex=1.6)
  with(sim_means, points(biomass~gCday,  col=col_bl, pch=16,cex=1.6))
title(ylab=treelab, mgp=ypos)
dev.off()
####model with parameters and Cday by volume to compare with final harvest----------------------

modelmass <- as.data.frame(do.call(rbind, mapply(productionmodel, 
            gCday=0.6*Cday, lma=lma_trt,frfrac=frfrac_trt, crfrac=crfrac_trt, stemfrac=stemfrac_trt,
            leaffrac=leaffrac_trt,SIMPLIFY=FALSE)))
#mm <- cbind(volume, modelmass)

#modelmass_all <- as.data.frame(do.call(rbind, mapply(productionmodel, gCday=Cday,
                                                 #lma=lma_trt, returnwhat="all",SIMPLIFY=FALSE)))

#plotting
windows(8,4)
par(mfrow=c(1,2))
plot(modelmass$biomass, mass_actual$mass)
abline(0,1)
plot(modelmass$leafarea, mass_actual$leafarea)
abline(0,1)


windows()
plot(Cday, modelmass$biomass, ylim=c(0,500))
points(Cday, mass_actual$mass, col="red")

plot(modelmass$biomass, mass_actual$mass)

plot(mass_actual$leafarea, modelmass$leafarea,ylim=c(0,5))
abline(0,1)

#A and plot mass, leafmass, and LMF vs A
plot(Aleaf_agg$carbon_day, modelmass$leafmass, pch=pchs, col=palette())
plot(Aleaf_agg$carbon_day, modelmass$LMF,  pch=pchs, col=palette())
plot(Aleaf_agg$carbon_day, modelmass$biomass,  pch=pchs, col=palette())



# DOES NOT REALLY WORK - FORGET THIS FOR NOW
fp <- function(p, i=1:7, leafareaweight=10, returnwhat=c("objective","simulation"),...){
  
  returnwhat <- match.arg(returnwhat)
  
  leaffrac <- p[1]
  m <- p[2]
  
  ofrac <- (1 - leaffrac)/3
  modelmass <- as.data.frame(do.call(rbind, mapply(productionmodel, 
                                                   gCday=m*Cday[i], 
                                                   lma=lma_trt[i], 
                                                   frfrac=ofrac, 
                                                   crfrac=ofrac, 
                                                   stemfrac=ofrac,
                                                   leaffrac=leaffrac,
                                                   ...,
                                                   SIMPLIFY=FALSE)))
  
  if(returnwhat == "objective"){
    O <- leafareaweight*sum((modelmass$leafarea - mass_actual$leafarea)^2) + 
    sum((modelmass$biomass - mass_actual$mass)^2)
    return(O)
  }
  
  if(returnwhat == "simulation"){
    return(modelmass)
  }
}


opt <- optim(c(0.2,1), fp, method="L-BFGS-B", lower=c(0, 0), upper=c(0.9, 5))

# opt <- optim(c(0.2,1), fp)

optmodel <- fp(opt$par, returnwhat="sim")

windows(8,4)
par(mfrow=c(1,2))
plot(optmodel$biomass, mass_actual$mass)
abline(0,1)
plot(optmodel$leafarea, mass_actual$leafarea)
abline(0,1)


opt <- optim(c(0.2,1), fp, i=2, method="L-BFGS-B", lower=c(0, 0), upper=c(0.9, 5))


leaffrac <- 0.5
ofrac <- (1-leaffrac)/3

modelmass <- as.data.frame(do.call(rbind, mapply(productionmodel, 
                                                 gCday=0.25*Cday, lma=lma_trt,frfrac=ofrac, 
                                                 crfrac=ofrac, stemfrac=ofrac,
                                                 leaffrac=leaffrac,SIMPLIFY=FALSE)))
windows(8,4)
par(mfrow=c(1,2))
plot(modelmass$biomass, mass_actual$mass)
abline(0,1)
plot(modelmass$leafarea, mass_actual$leafarea)
abline(0,1)


p <- productionmodel(gCday = 6.9, leaffrac=0.25, lma=80)


