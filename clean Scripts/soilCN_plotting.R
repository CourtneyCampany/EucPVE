#source("functions and packages/startscripts.R")

soil <- read.csv("raw data/soil CN.csv")

#replace 1000 with free for pool size calc
soil$volume <- gsub(1000, "free", soil$volume)
soil[is.na(soil)] <- "pre"

soilN <- soil[1:52, 4:5]
soilC <- soil[1:52, c(3,5)]
  #soilN$volume <- as.factor(soilN$volume)

# New sorting order
desired_order <- c("pre", "5", "10", "15", "20", "25", "35", "free")

# Re-order the levels
soilN$volume <- factor(as.character(soilN$volume), levels=desired_order )
soilC$volume <-  factor(as.character(soilC$volume), levels=desired_order )

# Re-order the data.frame
soilN <- soilN[order(soilN$volume),]
soilC <- soilC[order(soilC$volume),]

#simple plot

#windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,n_perc, data=soilN,  xlab = "Pot Volume (l)",  col="grey98", 
            ylab = "Soil Nitrogen (%)",ylim = c(0, .06))
box()


par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,c_perc, data=soilC,  xlab = "Pot Volume (l)",  col="grey98", 
            ylab = "Soil Carbon (%)",ylim = c(0, .65))
box()
