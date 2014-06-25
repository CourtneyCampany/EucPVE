#source functions and packages
source("functions and packages/functions.R")
source("functions and packages/load packages.R")

#leaves------------------------------------------------------------------------------------------
leafN <- read.csv("calculated data/leaf N content.csv")
#the intial leaf N doesnt seem to include the pre samples.....;

#soils-------------------------------------------------------------------------------------------
soil <- read.csv("raw data/soil CN.csv")

#pre experiment soils
soilpre <- soil[48:53,]
soilpre$volume <- as.numeric(gsub("pre", "", soilpre$ID))
volumeorder<-order(soilpre$volume, by=soilpre$ID)
soilpre <- soilpre[volumeorder,]

#soil harvest
soil_harvest <- soil[0:47,]
#soil_harvest$volume <- gsub("1000", "free", soil_harvest$volume)
soil_harvest$volume <- gsub("^5", "05", soil_harvest$volume)
soil_harvest$volume <- as.numeric(soil_harvest$volume)

#run Npool function
preNpool <- npool_func(soilpre)
names(preNpool)[7] <- "preNPool"
postNpool <- npool_func(soil_harvest)
names(postNpool)[7] <- "postNPool"

soilNpool <- merge(preNpool[,c(5,7)], postNpool[,c(1,5:7)], by="volume", all=TRUE)


#means
soilN_mean = round(mean(soilpre$n_perc),3)
harvestmean <- summaryBy(n_perc~volume, data=soil_harvest, keep.names=TRUE)
Hmean <- round(mean(harvestmean$n_perc),3)
#calculate soil N poot (pre and harvest)

#roots-------------------------------------------------------------------------------------------
root_chem <- read.csv("calculated data/root_chem.csv)")