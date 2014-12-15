#this script creates the data table for the mauscripts
library(doBy)
source("functions and packages/functions.R")

#start with harvest dfr
harvestmass <- read.csv("calculated data/seedling mass.csv") 

#####start with means of totalmass
table1 <- summaryBy(totalmass~volume, data=harvestmass, FUN=c(mean, se))
names(table1)[2:3]<- c("Seedling mass", "Seedling mass_se")

##gas exchange parameters jmax, vcmax, g1,Rd 
phys <- read.csv("calculated data/jmax_vcmax.csv")
phys_agg <- summaryBy(Jmax.mean+Vcmax.mean ~ volume, data=phys, FUN=c(mean, se))
  names(phys_agg)[2:5]<- c("Jmax", "Vcmax", "Jmax_se", "Vcmax_se")

####these have no se, only 7 values move to 
# g1 <- read.csv("calculated data/g1_pred.csv")
# g1_agg <- summaryBy(g1_vol ~ volume, data=g1, FUN=c(mean, se))
#   names(g1_agg)[2:3]<- c("G1", "G1_se")

rd <- read.csv("calculated data/Rd_leaf.csv")
rd_agg <- summaryBy(Photo+resppermass ~ volume, data=rd, FUN=c(mean, se))
  names(rd_agg)[2:5]<- c("Rd", "Rd_mass", "Rd_se", "Rd_mass_se")

#Asat and Amax
A_means <- read.csv("calculated data/A_treatment_means.csv")
  A_means$volume <- as.numeric(c("5", "10", "15", "20", "25", "35", "1000"))


####add Photo parameters to data table
table1 <- merge(table1, phys_agg)
#table1 <- merge(table1, g1_agg)
table1 <- merge(table1, rd_agg)
table1 <- merge(table1, A_means)


##TNC/SLA/LEAFN (should I include mgperg or actual content mased on leaf mass)
photo_chem <- read.csv("calculated data/Amax_chem.csv")
  #run volume format func
  photo_chem<- vollab_func(photo_chem)
leaf_param <- photo_chem[,c(1:3,9,11:12, 14:17 )]
  leaf_param$volume <- gsub("05", "5", leaf_param$volume)

leaf_param_agg <- summaryBy(sla + Nmass+	Narea+	starch_mgperg+	sugars_mgperg+	starch+	sugars~ volume, 
                            data=leaf_param, FUN=c(mean, se))
names(leaf_param_agg)[2:15] <- c("SLA", "Nleaf_mass", "Nleaf_area", "Starch mg/g", "Soluble Sugars mg/g",
                                 "Starch leaf", "Soluble Sugars leaf"," SLA_se","Nleaf_mass_se", 
                                 "Nleaf_area_se","Starch_mg/g_se", "Soluble Sugars mg/g_se","Starch leaf_se", 
                                 "Soluble Sugars leaf_se")

####add TNC to data table, for not merge mg/g for starch and sugars
table1 <- merge(table1, leaf_param_agg[, c(1:6, 9:13)])

pve_data <- table1[, c(1:3, 9,11:13, 16:17, 19:22, 24:25)]
sort <- c(1,2,3,6,7,8,12,10,14,11,15,9,13,4,5)


pve_data2 <- pve_data[, c(1,2,3,6,7,8,12,10,14,11,15,9,13,4,5)]
write.csv(pve_data2, "master_scripts/pve_table1.csv", row.names=FALSE)

