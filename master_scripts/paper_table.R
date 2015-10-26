#this script creates the plant based data table for the mauscripts
library(doBy)
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

###variables:   Mass, SLA,  Starct, Sugar, leaf N, 13C
###change units to fit table and make rounding easier

#1: seedling mass--------------------------------------------------------------------------------------------------------
harvestmass <- read.csv("calculated data/seedling mass.csv") 

#####totalmass
table1 <- summaryBy(totalmass~volume, data=harvestmass, FUN=c(mean, se))
  names(table1)[2:3]<- c("Seedling mass", "Seedling mass_se")


#### SLAnotnc-------------------------------------------------------------------------------------------------
sla_free <- read.csv("calculated data/sla_free_clean.csv")
table1 <- merge(table1, sla_free)


### starch, sugars----------------------------------------------------------------------------------------------
photo_chem <- read.csv("calculated data/Amax_chem.csv")
  #run volume format func
  photo_chem<- vollab_func(photo_chem)

leaf_param <- photo_chem[,c(1:3,16:17 )]
    leaf_param$volume <- gsub("05", "5", leaf_param$volume)

leaf_param_agg <- summaryBy(starch+sugars~ volume, data=leaf_param, FUN=c(mean, se))
  leaf_param_agg$sug <- leaf_param_agg$sugars.mean*100
  leaf_param_agg$sug.se <- leaf_param_agg$sugars.se*100
  leaf_param_agg$star <- leaf_param_agg$starch.mean*100
  leaf_param_agg$star.se <- leaf_param_agg$starch.se*100

table1 <- merge(table1,leaf_param_agg[,c(1, 6:9)])

###leaf N no tnc-------------------------------------------------------------------------------------------------------------
leafN <- read.csv("calculated data/leafnitrogen_notnc.csv")

leafN_agg <- summaryBy(Nperc_notnc~ volume, data=leafN, FUN=c(mean, se))

table1 <- merge(table1,leafN_agg)

####d13c---------------------------------------------------------------------------------------------------------------------
c13 <- read.csv("calculated data/c13_means.csv")

table1 <- merge(table1,c13)


#seperate dfr in two with mean and se, omit volume for now------------------------------------------------------------------
tree_means <- table1[, c(2,4,6,8,10,12)]
tree_se <- table1[, c(3,5,7,9,11,13)]

###now paste together and round
dat1 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,1], 1)), " (", sprintf("%3.2f", round(tree_se[,1],2)),")"))
dat2 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,2], 1)), " (", sprintf("%3.2f", round(tree_se[,2],2)),")"))
dat3 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,3], 1)), " (", sprintf("%3.2f", round(tree_se[,3],2)),")"))
dat4 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,4], 1)), " (", sprintf("%3.2f", round(tree_se[,4],2)),")"))
dat5 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,5], 2)), " (", sprintf("%3.2f", round(tree_se[,5],2)),")"))
dat6 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,6], 1)), " (", sprintf("%3.2f", round(tree_se[,6],2)),")"))

# Note:
# in R, sprintf("%2.1f", round(25.01,1)) avoids "25", and gives "25.0" instead.


pve_table1 <- cbind(leglab, dat1)
  pve_table1 <- cbind(pve_table1, dat2)
  pve_table1 <- cbind(pve_table1, dat3)
  pve_table1 <- cbind(pve_table1, dat4)
  pve_table1 <- cbind(pve_table1, dat5)
  pve_table1 <- cbind(pve_table1, dat6)

#change variable order
pve_table2 <- pve_table1[, c(1:3,5,4,6:7)]


###read in sigletters from sigletters folder
sigletter_files <- list.files(path = "master_scripts/sigletters/sigletts_plant/", pattern="*.csv", full.names = TRUE)
##make names of list with file names minus extension
sigletter_vars <- gsub("master_scripts/sigletters/sigletts_plant/", "", sigletter_files)
sigletter_vars <- gsub(".csv", "", sigletter_vars)
sigletter_list <- lapply(sigletter_files, function(x) read.csv(x))
##add names to list
names(sigletter_list) <- sigletter_vars

##order of sig letters is free, 5, 10, 15, 20, 25, 35...need to chance so paste below is correct
sigvol <- c(1000, 5, 10, 15, 20, 25, 35)
siglet <- lapply(sigletter_list, function(x) cbind(x, sigvol))
siglet2 <- lapply(siglet, function(x) as.data.frame(x))
siglet3 <- list()
for(i in 1:6) {
  siglet3[[i]] <- siglet2[[i]][c(2,3,4,5,6,7,1),] 
}


###add sigletters to table
#1. amax
pve_table2[[2]] <- paste(pve_table2[[2]], siglet3[[3]][,1])
pve_table2[[3]] <- paste(pve_table2[[3]], siglet3[[4]][,1])
pve_table2[[4]] <- paste(pve_table2[[4]], siglet3[[5]][,1])
pve_table2[[5]] <- paste(pve_table2[[5]], siglet3[[6]][,1])
pve_table2[[6]] <- paste(pve_table2[[6]], siglet3[[2]][,1])
pve_table2[[7]] <- paste(pve_table2[[7]], siglet3[[1]][,1])


pval_row <- c("Volume Effect (P value)", 0.001, 0.001, 0.029, 0.125, 0.001, 0.372)

pve_table2$leglab <- as.character(pve_table2$leglab)

pve_table3 <- rbind(pve_table2, pval_row)

write.csv(pve_table3, "master_scripts/pve_table1.csv", row.names=FALSE)


