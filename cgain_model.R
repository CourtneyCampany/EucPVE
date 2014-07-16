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
Anetm2 <- read.csv()


#model parameters and empty vectors----------------------------------------------------
leafrac <- .25
leafarea <- c()
leafarea[1] <- LA_start
biomass <- c()
biomass[1] <- mass_mean
sla <- .3

#run model simulation------------------------------------------------------------------
for (i in 2:numdays {
  
  production <- Anet_m2[i-1] * leafarea[i-1]
  biomass[i] <- biomass[i-1] + production
  leafarea <- leafarea[i-1] + (production*fleaf*sla)
}
