#this script creates the plant based data table for the mauscripts
library(doBy)
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

###variables:   Mass, SLA, SRL, Starct, Sugar, leaf N
###change units to fit table and make rounding easier

#1: seedling mass
harvestmass <- read.csv("calculated data/seedling mass.csv") 

  #####start with means of totalmass
table1 <- summaryBy(totalmass~volume, data=harvestmass, FUN=c(mean, se))
  names(table1)[2:3]<- c("Seedling mass", "Seedling mass_se")

#2:SRL
srl <- read.csv("calculated data/srl_means.csv")
  srl$srl <- srl$SRL.mean*100
  srl$srl.se <- srl$SRL.se*100

table1 <- merge(table1, srl[,c(1, 4:5)])


#3-6: SLA, starch, sugars, Nmass

photo_chem <- read.csv("calculated data/Amax_chem.csv")
  #run volume format func
  photo_chem<- vollab_func(photo_chem)
#recalculate sla in proper si units (m2/kg)
  photo_chem$SLA <- with(photo_chem, (area/10000) / (mass/1000))
#simple leaf N %
  photo_chem$leafnperc <- with(photo_chem, Nperc*100)

leaf_param <- photo_chem[,c(1:3,22:23,11:12, 14:17 )]
    leaf_param$volume <- gsub("05", "5", leaf_param$volume)

leaf_param_agg <- summaryBy(SLA +starch+sugars+ leafnperc~ volume, data=leaf_param, FUN=c(mean, se))
  #units
#   leaf_param_agg$leafN <- leaf_param_agg$Nmass.mean*1000
#   leaf_param_agg$leafN.se <- leaf_param_agg$Nmass.se*1000
  leaf_param_agg$sug <- leaf_param_agg$sugars.mean*100
  leaf_param_agg$sug.se <- leaf_param_agg$sugars.se*100
  leaf_param_agg$star <- leaf_param_agg$starch.mean*100
  leaf_param_agg$star.se <- leaf_param_agg$starch.se*100

table1 <- merge(table1,leaf_param_agg[,c(1:2,6, 5, 9, 10:13)])

###Root nitrogen
# rootN <- read.csv("calculated data/root_chem.csv")
#   root_all <-rootN[complete.cases(rootN),]
  rootN <- read.csv("calculated data/root_N_clean.csv")  

root_agg <- summaryBy(N_perc~ volume, data=rootN, FUN=c(mean, se))
  root_agg$volume <- gsub("05", "5", root_agg$volume)
  root_agg$volume <- gsub("free", "1000", root_agg$volume)

table1 <- merge(table1,root_agg)

####d13c
c13 <- read.csv("calculated data/c13_means.csv")

table1 <- merge(table1,c13)


###now sort the table into correct var + se--------------------------------------------------------------------
#tree_tab <-table1[,c(1,2,3,6,7,8,9,10,11,12,13,4,5,14,15)]

#seperate dfr in two with mean and se, omit volume for now
tree_means <- table1[, c(2,4,6,8,10,12,14,16)]
tree_se <- table1[, c(3,5,7,9,11,13,15,17)]

###now paste together and round
dat1 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,1], 1)), " (", sprintf("%3.2f", round(tree_se[,1],2)),")"))
dat2 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,2], 1)), " (", sprintf("%3.2f", round(tree_se[,2],2)),")"))
dat3 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,3], 1)), " (", sprintf("%3.2f", round(tree_se[,3],2)),")"))
dat4 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,4], 1)), " (", sprintf("%3.2f", round(tree_se[,4],2)),")"))
dat5 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,5], 2)), " (", sprintf("%3.2f", round(tree_se[,5],2)),")"))
dat6 <- data.frame(paste0(sprintf("%2.1f",round(tree_means[,6], 1)), " (", sprintf("%3.2f",round(tree_se[,6],2)),")"))
dat7 <- data.frame(paste0(signif(tree_means[,7], 2), " (", round(tree_se[,7],2),")"))
dat8 <- data.frame(paste0(sprintf("%3.1f",round(tree_means[,8], 3)), " (", round(tree_se[,8],2),")"))
# Note:
# in R, sprintf("%2.1f", round(25.01,1)) avoids "25", and gives "25.0" instead.


pve_table1 <- cbind(leglab, dat1)
  pve_table1 <- cbind(pve_table1, dat2)
  pve_table1 <- cbind(pve_table1, dat3)
  pve_table1 <- cbind(pve_table1, dat4)
  pve_table1 <- cbind(pve_table1, dat5)
  pve_table1 <- cbind(pve_table1, dat6)
  pve_table1 <- cbind(pve_table1, dat7)
  pve_table1 <- cbind(pve_table1, dat8)

#change variable order
pve_table2 <- pve_table1[, c(1:2, 4:7, 3,8,9)]


###read in sigletters from sigletters folder
sigletter_files <- list.files(path = "master_scripts/sigletters/sigletts_plant/", pattern="*.csv", full.names = TRUE)
##make names of list with file names minus extension
sigletter_vars <- gsub("master_scripts/sigletters/sigletts_plant/", "", sigletter_files)
sigletter_vars <- gsub(".csv", "", sigletter_vars)
sigletter_list <- lapply(sigletter_files, function(x) read.csv(x))
##add names to list
names(sigletter_list) <- sigletter_vars


###add sigletters to table
#1. amax
pve_table2[[2]] <- paste(pve_table2[[2]], sigletter_list[[3]][,1])
pve_table2[[3]] <- paste(pve_table2[[3]], sigletter_list[[5]][,1])
pve_table2[[4]] <- paste(pve_table2[[4]], sigletter_list[[2]][,1])
pve_table2[[5]] <- paste(pve_table2[[5]], sigletter_list[[8]][,1])
pve_table2[[6]] <- paste(pve_table2[[6]], sigletter_list[[7]][,1])
pve_table2[[7]] <- paste(pve_table2[[7]], sigletter_list[[6]][,1])
pve_table2[[8]] <- paste(pve_table2[[8]], sigletter_list[[4]][,1])
pve_table2[[9]] <- paste(pve_table2[[9]], sigletter_list[[1]][,1])


pval <- as.vector(c("Container Effect", 0.001, 0.001, 0.001, 0.128, 0.039, 0.662, 0.015, 0.458))

pve_table2$leglab <- as.character(pve_table2$leglab)

pve_table3 <- rbind(pve_table2, pval)

write.csv(pve_table3, "master_scripts/pve_table1.csv", row.names=FALSE)


