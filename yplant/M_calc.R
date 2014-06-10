#use simulations from sunny day and A parameters for each pot volume to calc M
#M = Aplant/Aobs

#read data from each run
sun5 <- read.csv("yplant/euc_plfiles/sunny_5/sunny_stats.csv")
sun10 <- read.csv("yplant/euc_plfiles/sunny_10/sun10_stats.csv")
sun15 <- read.csv("yplant/euc_plfiles/sunny_15/sun15_stats.csv")
sun20 <- read.csv("yplant/euc_plfiles/sunny_20/sun20_stats.csv")
sun25<- read.csv("yplant/euc_plfiles/sunny_25/sun25_stats.csv")
sun35 <- read.csv("yplant/euc_plfiles/sunny_35/sun35_stats.csv")
sun_free <- read.csv("yplant/euc_plfiles/sunny_5/sunny_stats.csv")


sun5_sum<- summary()

#function to calculate M
Mcalc <- function(dfr){
dfr$plant_id <- gsub("yplant/euc_plfiles/", "", dfr$pfile)
dfr$plant_id <- gsub(".p", "", dfr$plant_id)
dfr$M <- dfr$totA/dfr$totA0
M_dfr <- subset(dfr, select = c("plant_id", "M", "totPARleaf"))
return(M_dfr)
}

#M dfrs for each volume on sunny day
M5 <- Mcalc(sun5)
M5$volume <- 5 
M10 <- Mcalc(sun10)
M10$volume <- 10 
M15 <- Mcalc(sun15)
M15$volume <- 15 
M20 <- Mcalc(sun20)
M20$volume <- 20
M25 <- Mcalc(sun25)
M25$volume <- 25 
M35 <- Mcalc(sun35)
M35$volume <- 35 
M_free <- Mcalc(sun_free)
M_free$volume <- "free" 

#create master dfr with all volumes
M_eucs <- rbind(M5, M10)
M_eucs <- rbind(M_eucs, M15)
M_eucs <- rbind(M_eucs, M20)
M_eucs <- rbind(M_eucs, M25)
M_eucs <- rbind(M_eucs, M35)
M_eucs <- rbind(M_eucs, M_free)
M_eucs$volume<- as.factor(M_eucs$volume)

#colors
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
require(doBy)

#plot
plot(M~totPARleaf, pch=16, col=volume, data=M_eucs)

write.csv(M_eucs, "calculated data/M_eucs.csv", row.names=FALSE)

M_agg <- summaryBy(M ~ volume, data=M_eucs, FUN=mean, keep.names=TRUE)


#plot M against leaf#-----------------------------------------------------------------------------

leaf_no <- read.csv("calculated data/leaf_number.csv")
leafM <- merge(leaf_no, M_agg, by="volume")




