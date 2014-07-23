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
Cday <- read.csv("calculated data/cgain_date.csv")

Cday_test1 <- subset(Cday, ID == "1-8") #5l
#Cday_test2 <- subset(Cday, ID == "3-2") #free

#date is for plotting, starts day2
Date <- as.Date(Cday_test1[2:121,2])

#model parameters and empty vectors----------------------------------------------------

#vector with only Cgain
Cday_id <- Cday_test1[,3]



leafrac <- .25
sla <- 1/lma_mean  # calculate based on lma_mean above
aperla <- 3 # fill in average gC m-2 day-1 from plantecophys simulations.
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


# function
runProd <- function(leafrac = .25,
                    sla = 1/lma_mean,
                    aperla = 3,
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




Cgain_loop <- data.frame(biomass=biomass, leafarea=leafarea, Date=Date)
plot(biomass~Date, data=Cgain_loop)
plot(leafarea~Date, data=Cgain_loop)


####attempts to run through all ID's
require(plyr)
#need to split Cday, but also get a list of 49 that only include the Cgain vector

modelfunction <- function(x) {
  for (i in 2:numdays) {
    production <- x[i-1] * leafarea[i-1]
    biomass[i] <- biomass[i-1] + production
    leafarea[i] <- leafarea[i-1] + (production*leafrac*sla)
    dfr <- data.frame(mass=biomass, leafarea=leafarea, Date=Date)
    return(dfr)
  }
}

cday_sp2 <- dlply(Cday, .(ID))


runmodel<- modelfunction(cday_sp2[["carbon_day"]]) 

runmodel2 <- apply(cday_sp2[3],2, modelfunction)
runmodel3 <- lapply(cday_sp2[3], modelfunction)


