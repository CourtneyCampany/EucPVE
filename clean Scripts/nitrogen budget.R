#source functions and packages
source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

#N uptake (plant N pool /unit root uptake-----------------------------------

#leaves ()
leafN <- read.csv("calculated data/leaf N content.csv")

leafNharvest <- subset(leafN, campaign ==6)
  leafNharvest$volume <- gsub("^5", "05", leafNharvest$volume)
  names(leafNharvest)[5] <- "leafNperc"
  #the intial leaf N doesnt seem to include the pre samples.....;

##roots
rootN <- read.csv("calculated data/root_chem.csv")
  rootN$volume <- gsub("^5", "05", rootN$volume)
  rootN$volume <- gsub("free", "1000", rootN$volume)

root10 <- subset(rootN, volume==10)
root1000 <- subset(rootN, volume==1000)
  #fill missing Npec with average of volume
  Nroot10 <- mean(root10$N_perc,  na.rm=TRUE)/100 #mean Nperc
  Nroot1000 <- mean(root1000$N_perc,  na.rm=TRUE)/100 #mean Nperc
  massmiss <- rootN[1,11]
  mass1000 <- rootN[49,11]
  rootreplace <-Nroot10 * massmiss
  rootreplace1000 <-Nroot1000 * mass1000

##harvest mass
mass <- read.csv("calculated data/seedling mass.csv")
  mass$volume <- gsub("^5", "05", mass$volume)
mass <- mass[,c(1:2,5:6)]

Nplant <- merge(mass, leafNharvest[,c(1,4:5)])
  Nplant$leafN <- with(Nplant, leafmass*leafNperc)
  Nplant$stemNperc <- .005
  Nplant$stemN <- with(Nplant, stemNperc*stemmass)

Nplant <- merge(Nplant, rootN[, c(1:2, 10:11)])
#replace one missing value
Nplant[1,9] <- rootreplace
Nplant[4,9] <- rootreplace1000

euc_npool <- Nplant[,c(1:2,6,8:9)]
  euc_npool$plantN <- with(euc_npool, leafN+stemN+rootN)

Nplant_agg <- summaryBy(rootN+plantN~volume, data=euc_npool, FUN=mean, keep.names=TRUE)

#soils-------------------------------------------------------------------------------------------
soil <- read.csv("raw data/soil CN.csv")
  #need volume for foree
  freevol <- 500 #liters
  #replace 1000 with freevol for pool size calc
  soil$volume <- gsub(1000, freevol, soil$volume)

#pre experiment soils
soilpre <- soil[48:53,]
  soilpre$volume <- as.numeric(gsub("pre", "", soilpre$ID))
  volumeorder<-order(soilpre$volume, by=soilpre$ID)
  soilpre <- soilpre[volumeorder,]

###the 5vol N% seems too high, use mean of others for pre
preN <- mean(soilpre[2:6,4])

#soil harvest
soil_harvest <- soil[0:47,]
  #soil_harvest$volume <- gsub("1000", "free", soil_harvest$volume)
  soil_harvest$volume <- gsub("^5", "05", soil_harvest$volume)
  soil_harvest$volume <- as.numeric(soil_harvest$volume)

#run Npool function
postNpool <- npool_func(soil_harvest)
  names(postNpool)[7] <- "postNPool"

bd<-1.7
#calculate preN pool size wiht Nmean and bulkdensity
postNpool$preN <- with(postNpool, (bd*volume)*preN)

soilNpool <- postNpool[,c(1, 5:8)]
  soilNpool$volume <- as.factor(soilNpool$volume)
  soilNpool$Nuptake <- with(soilNpool, preN-postNPool)


Npool_agg <- summaryBy(Nuptake~volume, data=soilNpool, FUN=mean, keep.names=TRUE)
  Npool_agg$volume <- gsub("500", "1000", Npool_agg$volume)
  Npool_agg$volume <- gsub("^5", "05", Npool_agg$volume)

#upake
Nuptake <- merge(Npool_agg, Nplant_agg)
  Nuptake$volume <- as.numeric(Nuptake$volume)
  volumeorderN<-order(Nuptake$volume, by=Nuptake$Nuptake)
  Nuptake <- Nuptake[volumeorderN,]
  Nuptake$volume <- as.factor(Nuptake$volume)


#total plant N per unit uptake????
plot(plantN ~Nuptake, data=Nuptake, col=volume, pch=pchs[volume], ylim=c(0,2),
     xlim=c(0,5))
abline(0,1)
