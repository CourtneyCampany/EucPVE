require(doBy)

#experiment length---------------------------------------------------------------------
numdays <- as.numeric(as.Date("2013-05-21") - as.Date("2013-01-21"))
volume <- c("5", "10", "15", "20", "25", "35", "1000")

#date is for plotting, starts day2
uniqueDate <- seq.Date(from=as.Date("2013/01/22"), to=as.Date("2013/05/21"), by="days")

#harvest mass and leaf area for model comparison---------------------------------------
harvestmass <- read.csv("calculated data/seedling mass.csv")
mass_total <- harvestmass[,c(1:2,11)]
mass_agg <- summaryBy(totalmass ~volume, data=mass_total, FUN=mean, keep.names=TRUE)
LA_harvest <- read.csv("calculated data/LA_harvest.csv")
LA_agg <- summaryBy(totalarea ~volume, data=LA_harvest, FUN=mean, keep.names=TRUE)

mass_actual <- data.frame(volume = LA_agg$volume, mass = mass_agg$totalmass, leafarea = (LA_agg$totalarea/1000))

#pre seedling data for intial biomass and leaf area (use mean)-------------------------
seedling_pre <- read.csv("raw data/seedling_initial.csv")
seedling_pre$seedling_mass <- with(seedling_pre, leaf_mass+root_mass+wood_mass)
leaffractions <- mean (seedling_pre$leaf_mass/seedling_pre$seedling_mass)
#average mass of seedlings at start
#mass_mean <- mean(seedling_pre$seedling_mass)
mean_leafnum <- mean(seedling_pre$leaf_numb)

#mean lma and leafarea (apply to intial leaf )-----------------------------------------
#need to pull leaf area from somewhere
lma <- read.csv("calculated data/leafmassarea.csv")
sla_vol <- summaryBy(sla ~volume, data=lma, FUN=mean, keep.names=TRUE)

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

####RUN MODEL
#model empty vectors and start values

lma_mean <- mean(lma$massarea)#average lma from harvest
LA_start <- (mean_leafnum * leafarea_mean) #(m2)
mass_mean <- mean(seedling_pre$seedling_mass)
Cday <- as.vector(Aleaf_agg[,2]) #vector of 7 treaments in order
sla_trt <- as.vector(sla_vol[,2])
#sla <-  1/lma_mean
#volumeid <- as.factor(c(5,10,15,20,25,35,"free"))

# model as a function--------------------------------------------------------------
productionmodel <- function(leafrac = .18,
                    sla = .0102,
                    gCday = .2,
                    conversionEfficiency = 0.65,
                    fr_resp = .010368, #gC/gFroot day Marsden et al
                    cr_resp = .00124, #gC/gCroot day
                    wd_resp = .00269, #gc/gwood day Ryan et al. eucs 1yr
                    numdays=120,
                    lma = 97.5,
                    returnwhat=c("lastval","all")
                    ){
  
  returnwhat <- match.arg(returnwhat)
  
  leafarea <- vector()
  leafarea[1] <- LA_start
  
  biomass <- vector()
  biomass[1] <- mass_mean
  
  leafmass <- vector()
  leafmass[1] <- LA_start*lma_mean
  
  LMF <- vector()
  LMF[1] <- (LA_start*lma_mean)/mass_mean
  
  if(length(gCday) == 1)gCday <- rep(gCday,numdays)
  if(length(gCday) < numdays)stop("Need at least ",numdays," photosynthesis values.")
  
  #run model simulation------------------------------------------------------------------
  for (i in 2:numdays) {
    biomassprod <- leafarea[i-1] * gCday[i]/conversionEfficiency  # gc day-1
    biomass[i] <- biomass[i-1] + biomassprod - ((biomass[i-1]*fr_resp)+(biomass[i-1]*cr_resp)+(biomass[i-1]*wd_resp))
    leafarea[i] <- leafarea[i-1] + (biomassprod*leafrac*sla)
    leafmass[i] <- leafarea[i] * lma
    LMF[i] <- leafmass[i]/biomass[i]
  }

  if(returnwhat == "lastval")
    return(c(biomass=biomass[numdays],leafarea=leafarea[numdays], leafmass = leafmass[numdays], LMF = LMF[numdays]))
  
  if(returnwhat == "all")
    return(list(biomass=biomass,leafarea=leafarea, leafmass = leafmass, LMF = LMF))
  
}

#run mean gCday for each volume through model
modelmass <- as.data.frame(do.call(rbind, mapply(productionmodel, gCday=Cday,
                                                 sla=sla_trt,leafrac=.25, SIMPLIFY=FALSE)))
mm <- cbind(volume, modelmass)

modelmass_all <- as.data.frame(do.call(rbind, mapply(productionmodel, gCday=Cday,
                                                 sla=sla_trt,leafrac=.25, returnwhat="all",SIMPLIFY=FALSE)))

# plot(biomass~Date, data=Cgain_25)
# plot(leafarea~Date, data=Cgain_25)


plot(Cday, modelmass$biomass, ylim=c(0,200))
points(Cday, mass_actual$mass, col="red")

#add A and plot mass, leafmass, and LMF vs A

#colors
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
pchs = c(rep(16,6),17)

plot(Aleaf_agg$carbon_day, mm$leafmass, pch=pchs)
plot(Aleaf_agg$carbon_day, mm$LMF,  pch=pchs)
plot(Aleaf_agg$carbon_day, mm$biomass,  pch=pchs)

