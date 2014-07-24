require(doBy)

#experiment length---------------------------------------------------------------------
numdays <- as.numeric(as.Date("2013-05-21") - as.Date("2013-01-21"))

#pre seedling data for intial biomass and leaf area (use mean)-------------------------
seedling_pre <- read.csv("raw data/seedling_initial.csv")
seedling_pre$seedling_mass <- with(seedling_pre, leaf_mass+root_mass+wood_mass)
leaffractions <- mean (seedling_pre$leaf_mass/seedling_pre$seedling_mass)
#average mass of seedlings at start
mass_mean <- mean(seedling_pre$seedling_mass)
mean_leafnum <- mean(seedling_pre$leaf_numb)

#mean lma and leafarea (apply to intial leaf )-----------------------------------------
#need to pull leaf area from somewhere
lma <- read.csv("calculated data/leafmassarea.csv")
lma_mean <- mean(lma$massarea)
leafarea_mean <- (mean(lma$area))/10000
#average starting leaf area
LA_start <- (mean_leafnum * leafarea_mean) #(m2)

#read in Cnetm2day---------------------------------------------------------------------

##need to convert modelled 15min A into g C m2 day
Amodel <- read.csv("calculated data/Aleaf_pred_15min.csv")
Amodel$photo15gc <- with(Amodel, Anet*15*60*10^-6*12)

#first need sum over day and then means by treatment
Aleaf <- summaryBy(photo15gc ~ Date+volume, data=Amodel, FUN=sum, keep.names=TRUE )
names(Aleaf)[3] <- "carbon_day"
Aleaf_agg <- summaryBy(carbon_day ~ volume, data=Aleaf, FUN=mean, keep.names=TRUE )
Aleaf5 <- subset(Aleaf_agg, volume == "5")
#date is for plotting, starts day2
uniqueDate <- seq.Date(from=as.Date("2013/01/22"), to=as.Date("2013/05/21"), by="days")

#model parameters and empty vectors----------------------------------------------------
aperla <- Aleaf5[,2]

leafrac <- .25
sla <- 1/lma_mean  # calculate based on lma_mean above
#aperla <- 3 # fill in average gC m-2 day-1 from plantecophys simulations.
Cperc <- 50

leafarea <- vector()
  leafarea[1] <- LA_start

biomass <- vector()
  biomass[1] <- mass_mean

#run model simulation------------------------------------------------------------------
for (i in 2:numdays) {
  production <- aperla * leafarea[i-1]  # gc day-1
  biomassprod <- production*(100/Cperc)
  biomass[i] <- biomass[i-1] + biomassprod
  leafarea[i] <- leafarea[i-1] + (biomassprod*leafrac*sla)
}
Cgain_5 <- data.frame(biomass=biomass, leafarea=leafarea, Date=uniqueDate)
Cgain_agg <- sum(Cgain_5$biomass)

plot(biomass~Date, data=Cgain_5)
plot(leafarea~Date, data=Cgain_5)


# function
runProd <- function(leafrac = .25,
                    sla = 1/lma_mean,
                    aperla = aperla,
                    Cperc = 50,
                    numdays=120){
  
  leafarea <- vector()
  leafarea[1] <- LA_start
  
  biomass <- vector()
  biomass[1] <- mass_mean
  
  if(length(aperla) == 1)aperla <- rep(aperla,numdays)
  if(length(aperla) < numdays)stop("Need at least ",numdays," photosynthesis values.")
  
  #run model simulation------------------------------------------------------------------
  for (i in 2:numdays) {
    production <- aperla[i] * leafarea[i-1]  # gc day-1
    biomassprod <- production*(100/Cperc)
    biomass[i] <- biomass[i-1] + biomassprod
    leafarea[i] <- leafarea[i-1] + (biomassprod*leafrac*sla)
  }

return(c(biomass=biomass[numdays],leafarea=leafarea[numdays]))
}

# when driver
res <- lapply(*spliytdataframe*, function(x)runProd(aperla=x$PHTOSYN))

# when constant
res <- as.data.frame(do.call(rbind, mapply(runProd, aperla=c(3,3.2,4.1), SIMPLIFY=FALSE)))

runProd(aperla)







