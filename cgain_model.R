#experiment length---------------------------------------------------------------------
numdays <- as.numeric(as.Date("2013-05-21") - as.Date("2013-01-21"))

#pre seedling data for intial biomass and leaf area (use mean)-------------------------
seedling_pre <- read.csv("raw data/seedling_initial.csv")
seedling_pre$seedling_mass <- with(seedling_pre, leaf_mass+root_mass+wood_mass)
leaffractions <- mean (seedling_pre$leaf_mass/seedling_pre$seedling_mass)
mass_mean <- mean(seedling_pre$seedling_mass)
mean_leafnum <- mean(seedling_pre$leaf_numb)

#mean lma and leafarea (apply to intial leaf )-----------------------------------------
#need to pull leaf area from somewhere
lma <- read.csv("calculated data/leafmassarea.csv")
lma_mean <- mean(lma$massarea)
leafarea_mean <- (mean(lma$area))/10000

LA_start <- (mean_leafnum * leafarea_mean) #(m2)

#read in Cnetm2day---------------------------------------------------------------------
Cday <- read.csv("calculated data/cgain_date.csv")

Cday_test <- subset(Cday, ID == "7-1")
Cday_id <- Cday_test[,3]
Date <- as.Date(Cday_test[2:121,2])


#model parameters and empty vectors----------------------------------------------------
leafrac <- .25
sla <- .3

leafarea <- vector()
  leafarea[1] <- LA_start

biomass <- vector()
  biomass[1] <- mass_mean

#run model simulation------------------------------------------------------------------
for (i in 2:numdays) {
  production <- Cday_id[i-1] * leafarea[i-1]
  biomass[i] <- biomass[i-1] + production
  leafarea[i] <- leafarea[i-1] + (production*leafrac*sla)
}

Cgain_loop <- data.frame(biomass=biomass, leafarea=leafarea, Date=Date)
plot(biomass~Date, data=Cgain_loop)
plot(leafarea~Date, data=Cgain_loop)


####attempts to run through all ID's

cday_sp <- split(Cday[,c(1,3)], "ID", drop=TRUE)
cday_sp2 <- cday_sp[[1]]$Adayumol

modelfunction <- function(x) {
  for (i in 2:numdays) {
    production <- x[i-1] * leafarea[i-1]
    biomass[i] <- biomass[i-1] + production
    leafarea[i] <- leafarea[i-1] + (production*leafrac*sla)
  }
}

Cid <- Cday[,c(1,3)]
require(plyr)
runmodel <- dlply(Cid, .(id), function(x) c(x<- as.vector(x[,2]), modelfunction(x)))

test <- dlply(Cid, .(id), function(x) y<-as.vector(x[,2]))


