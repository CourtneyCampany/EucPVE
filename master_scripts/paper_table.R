#this script creates the plant based data table for the mauscripts
library(doBy)
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

###variables:   Mass, SLA, SRL, Starct, Sugar, leaf N

#1: seedling mass
harvestmass <- read.csv("calculated data/seedling mass.csv") 

  #####start with means of totalmass
table1 <- summaryBy(totalmass~volume, data=harvestmass, FUN=c(mean, se))
  names(table1)[2:3]<- c("Seedling mass", "Seedling mass_se")

#2:SRL
srl <- read.csv("calculated data/srl_means.csv")

table1 <- merge(table1, srl)


#3-6: SLA, starch, sugars, Nmass

photo_chem <- read.csv("calculated data/Amax_chem.csv")
  #run volume format func
  photo_chem<- vollab_func(photo_chem)

  leaf_param <- photo_chem[,c(1:3,9,11:12, 14:17 )]
  leaf_param$volume <- gsub("05", "5", leaf_param$volume)

  leaf_param_agg <- summaryBy(sla +starch+sugars+ Nmass~ volume, data=leaf_param, FUN=c(mean, se))

table1 <- merge(table1,leaf_param_agg)


###now sort the table into correct var + se--------------------------------------------------------------------
tree_tab <-table1[,c(1,2,3,4,5,6,10,7,11,8,12,9,13)]

#seperate dfr in two with mean and se, omit volume for now
tree_means <- tree_tab[, c(2,4,6,8,10,12)]
tree_se <- tree_tab[, c(3,5,7,9,11,13)]

###now paste together and round
dat1 <- data.frame(paste0(signif(tree_means[,1], 3), " (", signif(tree_se[,1],2),")"))
dat2 <- data.frame(paste0(signif(tree_means[,2], 2), " (", signif(tree_se[,2],2),")"))
dat3 <- data.frame(paste0(signif(tree_means[,3], 2), " (", signif(tree_se[,3],2),")"))
dat4 <- data.frame(paste0(signif(tree_means[,4], 2), " (", signif(tree_se[,4],2),")"))
dat5 <- data.frame(paste0(signif(tree_means[,5], 2), " (", signif(tree_se[,5],1),")"))
dat6 <- data.frame(paste0(signif(tree_means[,6], 2), " (", signif(tree_se[,6],2),")"))


pve_table1 <- cbind(leglab, dat1)
  pve_table1 <- cbind(pve_table1, dat2)
  pve_table1 <- cbind(pve_table1, dat3)
  pve_table1 <- cbind(pve_table1, dat4)
  pve_table1 <- cbind(pve_table1, dat5)
  pve_table1 <- cbind(pve_table1, dat6)


write.csv(pve_table1, "master_scripts/pve_table1.csv", row.names=FALSE)


