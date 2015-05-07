#calculate M from yplant sim and then regression against leaf number for each plant ()
#use simulations from sunny day and A parameters for each pot volume to calc M
#M = Aplant/Aobs

require(plyr)
require(doBy)
require(plotrix)
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

#read eucs3d summary from construct plant
eucs3d <- read.csv("yplant/eucs_constructplant.csv")

#read in summary files for all volumes into a list
eucs_summs <- list.files(path = "yplant/sim2_summary/", pattern="*.csv", full.names = TRUE)
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

#call the names of the list into plyr, run the function on each element (eucs_list[[x]]) where vol argument = names
M_eucs <- llply(names(eucs_list), function(x) Mcalc(dfr = eucs_list[[x]], vol = x))
#write.csv(M_eucs, "calculated data/M_eucs.csv", row.names=FALSE)


####now regress M for each plant against leaf------------------------------------------------------------------------ 

#1: first merge M for each plant (7 volumes) with leaf# for each plant id
eucs3d$plant_id <- gsub("yplant/euc_plfiles/", "", eucs3d$pfile)
eucs3d$plant_id <- gsub(".p", "", eucs3d$plant_id)
eucs_allom <- eucs3d[,c("plant_id", "LA")]

M_eucs2<- lapply(M_eucs, function(x) merge(x, eucs_allom, by="plant_id", all=TRUE))
#write.csv(M_eucs3d, "calculated data/M_eucs3d.csv", row.names=FALSE)


#2. for each volume trt (list) run model of M vs leafN, 

M_regress <- lapply(M_eucs2, function(x) lm(M ~ LA, data=x))

test <- M_eucs2[[1]]
test_lm <- lm(M ~ LA, data=test)
  visreg(test_lm)
  
#3. extract coefs for each model by trt

M_coefs <- lapply(M_regress, function(x) extract_func(x))
##make a dfr but order of list these come from was 5-free, need to reorder carefully
M_coefs2 <-rbind.fill(M_coefs)
volorder <- names(eucs_list)
M_coefs2$volume <- as.factor(volorder)
M_coefs2$volume <- gsub("free", 1000, M_coefs2$volume)
#reorder
M_coefs3 <- M_coefs2[c(6, 1:5, 7),]

#write.csv(M_regress, "stats output/M_leaf#_model.csv", row.names=FALSE)