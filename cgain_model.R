require(doBy)

#experiment length---------------------------------------------------------------------
numdays <- as.numeric(as.Date("2013-05-21") - as.Date("2013-01-21"))

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
Cday <- as.vector(Aleaf_agg[,2]) #vector of 76 treaments in order
sla <-  1/lma_mean
volumeid <- as.factor(c(5,10,15,20,25,35,"free"))

# model as a function--------------------------------------------------------------
productionmodel <- function(leafrac = .18,
                    sla = .0102,
                    gCday = .2,
                    Cperc = 50,
                    constrt_resp = 0.65,
                    numdays=120){
  
  leafarea <- vector()
  leafarea[1] <- LA_start
  
  biomass <- vector()
  biomass[1] <- mass_mean
  
  if(length(gCday) == 1)gCday <- rep(gCday,numdays)
  if(length(gCday) < numdays)stop("Need at least ",numdays," photosynthesis values.")
  
  #run model simulation------------------------------------------------------------------
  for (i in 2:numdays) {
    production <- gCday[i] * constrt_resp* leafarea[i-1]  # gc day-1
    biomassprod <- production*(100/Cperc)
    biomass[i] <- biomass[i-1] + biomassprod
    leafarea[i] <- leafarea[i-1] + (biomassprod*leafrac*sla)
  }

return(c(biomass=biomass[numdays],leafarea=leafarea[numdays]))
}

#run mean gCday for each volume through model
modelmass <- as.data.frame(do.call(rbind, mapply(productionmodel, gCday=Cday,leafrac=.25, SIMPLIFY=FALSE)))

#plot modelled vs obseved

# # when driver
# res <- lapply(*spliytdataframe*, function(x)runProd(aperla=x$PHTOSYN))
# # when constant
# res <- as.data.frame(do.call(rbind, mapply(runProd, aperla=c(3,3.2,4.1), SIMPLIFY=FALSE)))
# 
# productionmodel(aperla=)



# #model parameters and empty vectors----------------------------------------------------
# gCday <- Aleaf25[,2]
# 
# leafrac <- .10
# sla <- 1/lma_mean  # calculate based on lma_mean above
# Cperc <- 50
# constrt_resp <- (0.65)
# 
# leafarea <- vector()
#   leafarea[1] <- LA_start
# 
# biomass <- vector()
#   biomass[1] <- mass_mean
# 
# #run model simulation------------------------------------------------------------------
# for (i in 2:numdays) {
#   production <- gCday * constrt_resp* leafarea[i-1]  # gc day-1
#   biomassprod <- production*(100/Cperc)
#   biomass[i] <- biomass[i-1] + biomassprod 
#   leafarea[i] <- leafarea[i-1] + (biomassprod*leafrac*sla)
# }
# Cgain_25 <- data.frame(biomass=biomass, leafarea=leafarea, Date=uniqueDate)
# 
# plot(biomass~Date, data=Cgain_25)
# plot(leafarea~Date, data=Cgain_25)



