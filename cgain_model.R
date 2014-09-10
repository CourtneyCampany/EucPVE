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

mass_actual <- data.frame(volume = LA_agg$volume, mass = mass_agg$totalmass, leafarea = (LA_agg$totalarea/1000))

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
pre_- mean(seedling_pre$leaf_numb)

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
                    wd_resp = .00269, #gc/gwood day Ryan et al. eucs 1yr
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
    frootmass[1] <- pre_root*.9
  
    crootmass <- vector()
    crootmass[1] <- pre_root*.1
     
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
    biomassprod <- leafarea[i-1] * gCday[i]/conversionEfficiency  # gc day-1
    
    biomassprodnet <- biomassprod - 
      ((biomass[i-1]*fr_resp)+(biomass[i-1]*cr_resp)+(biomass[i-1]*wd_resp))
    biomass[i] <- biomass[i-1] + biomassprodnet
   
   #fractions,stems and wood need respiration
    leafmass[i] <- leafmass[i-1] + biomassprod*leaffrac
#    
#    stemgain <- (stemmass[i-1] + biomassprod*stemfrac)
#     stemmass[i]<- stemgain -(stemgain* wd_resp)
#    
#    frootgain <- frootmass[i-1] + biomassprod*frfrac
#     frootmass[i] <- frootgain-(frootgain * fr_resp)
#    
#    crootgain <- crootmass[i-1] + biomassprod*crfrac 
#     crootmass[i] <- crootgain -(crootgain* cr_resp)
#    
#     #total biomass day
#     biomass[i] <- leafmass[i-1] + frootmass[i-1] + crootmass[i-1]+stemmass[i-1]
#     
  #leaf area and leaf mass fraction
    leafarea[i] <- leafmass[i] / lma
    
    LMF[i] <- leafmass[i]/biomass[i]
  }

  if(returnwhat == "lastval")
    return(c(biomass=biomass[numdays],leafarea=leafarea[numdays], leafmass = leafmass[numdays], LMF = LMF[numdays]))
  
  if(returnwhat == "all")
    return(list(biomass=biomass,leafarea=leafarea, leafmass = leafmass, LMF = LMF))
  
}

#run model simulations with sequence of g Cday, change parameter assumptions with each sim

#only lma by mean, components equal
gCday_seq <- seq(7,4,length=101)
free_sim <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gCday_seq, lma=lma_mean, SIMPLIFY=F)))
  free_sim$gCday <- gCday_seq




#component allocation and lma by volume (7 sims)

#5l
sim5 <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gCday_seq, lma=lma_trt[1],frfrac=frfrac_trt[1], 
                  crfrac=crfrac_trt[1], stemfrac=stemfrac_trt[1],leaffrac=leaffrac_trt[1], SIMPLIFY=F)))
#10l
sim10 <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gCday_seq, lma=lma_trt[2],frfrac=frfrac_trt[2], 
                  crfrac=crfrac_trt[2], stemfrac=stemfrac_trt[2],leaffrac=leaffrac_trt[2], SIMPLIFY=F)))
#15
sim15 <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gCday_seq, lma=lma_trt[3],frfrac=frfrac_trt[3],
                   crfrac=crfrac_trt[3], stemfrac=stemfrac_trt[3],leaffrac=leaffrac_trt[3], SIMPLIFY=F)))
#20
sim20 <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gCday_seq, lma=lma_trt[4],frfrac=frfrac_trt[4],
                   crfrac=crfrac_trt[4], stemfrac=stemfrac_trt[4],leaffrac=leaffrac_trt[4], SIMPLIFY=F)))
#25
sim25 <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gCday_seq, lma=lma_trt[5],frfrac=frfrac_trt[5],
                   crfrac=crfrac_trt[5], stemfrac=stemfrac_trt[5],leaffrac=leaffrac_trt[5], SIMPLIFY=F)))
#35
sim35 <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gCday_seq, lma=lma_trt[6],frfrac=frfrac_trt[6],
                   crfrac=crfrac_trt[6], stemfrac=stemfrac_trt[6],leaffrac=leaffrac_trt[6], SIMPLIFY=F)))
#free
simfree <- as.data.frame(do.call(rbind,mapply(productionmodel, gCday=gCday_seq, lma=lma_trt[7],frfrac=frfrac_trt[7],
                   crfrac=crfrac_trt[7], stemfrac=stemfrac_trt[7],leaffrac=leaffrac_trt[7], SIMPLIFY=F)))

sim5$gCday <- gCday_seq
sim10$gCday <- gCday_seq
sim15$gCday <- gCday_seq
sim20$gCday <- gCday_seq
sim25$gCday <- gCday_seq
sim35$gCday <- gCday_seq
simfree$gCday <- gCday_seq

#plot bits
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
pchs = c(rep(16,6),17)

cols <- as.vector(palette())

#model plotting
with(sim5, plot(gCday~biomass, xlim=c(150,0),col=cols[1]))
  points( sim10$gCday~sim10$biomass,col=cols[2])
  points( sim15$gCday~sim15$biomass,col=cols[3])
  points( sim20$gCday~sim20$biomass,col=cols[4])
  points( sim25$gCday~sim25$biomass,col=cols[5])
  points( sim35$gCday~sim35$biomass,col=cols[6])

  points( simfree$gCday~simfree$biomass,col=cols[7])

  points( mass_actual$mass, Cday,pch=16,col=palette())












modelmass <- as.data.frame(do.call(rbind, mapply(productionmodel, 
            gCday=Cday, lma=lma_trt,frfrac=frfrac_trt, crfrac=crfrac_trt, stemfrac=stemfrac_trt,
            leaffrac=leaffrac_trt,SIMPLIFY=FALSE)))
#mm <- cbind(volume, modelmass)

modelmass_all <- as.data.frame(do.call(rbind, mapply(productionmodel, gCday=Cday,
                                                 lma=lma_trt, returnwhat="all",SIMPLIFY=FALSE)))

#plotting


plot(Cday, modelmass$biomass, ylim=c(0,200))
points(Cday, mass_actual$mass, col="red")

plot(modelmass$biomass, mass_actual$mass)

plot(mass_actual$leafarea, modelmass$leafarea,ylim=c(0,5))
abline(0,1)

#A and plot mass, leafmass, and LMF vs A
plot(Aleaf_agg$carbon_day, modelmass$leafmass, pch=pchs)
plot(Aleaf_agg$carbon_day, modelmass$LMF,  pch=pchs)
plot(Aleaf_agg$carbon_day, modelmass$biomass,  pch=pchs)

