#calculate M from yplant sim and then regression against leaf number for each plant ()
#use simulations from sunny day and A parameters for each pot volume to calc M
#M = Aplant/Aobs

require(plyr)
require(doBy)
source("functions and packages/functions.R")

#read eucs3d summary from construct plant
eucs3d <- read.csv("yplant/eucs_constructplant.csv")

#read in summary files for all volumes into a list
eucs_summs <- list.files(path = "yplant/simulation_summary/", pattern="*.csv", full.names = TRUE)
eucs_list <- lapply(eucs_summs, function(x) read.csv(x))

#name list elements
listnames <- c("10", "15", "20", "25", "35", "5", "free")
names(eucs_list) <- listnames

#function to calculate M (twp arguments:1st is dfr, 2nd is the name of each list element)
Mcalc <- function(dfr, vol){
dfr$plant_id <- gsub("yplant/euc_plfiles/", "", dfr$pfile)
dfr$plant_id <- gsub(".p", "", dfr$plant_id)
dfr$M <- dfr$totA/dfr$totA0
M_dfr <- subset(dfr, select = c("plant_id", "M", "totPARleaf"))
M_dfr$volume <- as.factor(vol)
return(M_dfr)
}

#run the function on each element of the list 
#call the names of the list into plyr, run the function on each element (eucs_list[[x]]) where vol argument = names
M_eucs <- ldply(names(eucs_list), function(x) Mcalc(dfr = eucs_list[[x]], vol = x))
M_eucs$plant_id <- as.factor(M_eucs$plant_id)
#write.csv(M_eucs, "calculated data/M_eucs.csv", row.names=FALSE)

#means
M_agg <- summaryBy(M+totPARleaf~ volume, data=M_eucs, FUN=mean, keep.names=TRUE)

#####no regress M for each plant against leaf 
eucs3d$plant_id <- gsub("yplant/euc_plfiles/", "", eucs3d$pfile)
eucs3d$plant_id <- gsub(".p", "", eucs3d$plant_id)
eucs_allom <- subset(eucs3d, select = 
                       c("plant_id", "LA", "meanleafsize", "stemdiam", "leaflen", "nleavesp", "crownsurf"))

#merge 3d with M
M_eucs3d <- merge(M_eucs, eucs_allom, by="plant_id")

plot(M~LA, data=M_eucs3d, col=volume, pch=pchs[volume], ylim=c(0.5,1), xlim=c(0,5), 
     xlab=(expression(Leaf~Area~~(m^2))))
      d_ply(M_eucs3d, .(volume), function(x) add_trend_line("LA", "M", x))

plot(M~nleavesp, data=M_eucs3d, col=volume, pch=pchs[volume], ylim=c(0.6,1), xlim=c(0,1500))
  d_ply(M_eucs3d, .(volume), function(x) add_trend_line("nleavesp", "M", x))

#plot bits
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
pchs = c(rep(16,6),17)


#run model
M_mod <- dlply(M_eucs3d, .(volume), function(x) lm(M~nleavesp, data=x))
M_pval <- as.data.frame(lapply(M_mod, function(x) getP(x)))





