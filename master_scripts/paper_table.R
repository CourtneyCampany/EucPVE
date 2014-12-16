#this script creates the data table for the mauscripts
library(doBy)
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

#start with harvest dfr
harvestmass <- read.csv("calculated data/seedling mass.csv") 

#####start with means of totalmass
table1 <- summaryBy(totalmass~volume, data=harvestmass, FUN=c(mean, se))
  names(table1)[2:3]<- c("Seedling mass", "Seedling mass_se")

# ##gas exchange parameters jmax, vcmax, g1,Rd 
# phys <- read.csv("calculated data/jmax_vcmax.csv")
# phys_agg <- summaryBy(Jmax.mean+Vcmax.mean ~ volume, data=phys, FUN=c(mean, se))
#   names(phys_agg)[2:5]<- c("Jmax", "Vcmax", "Jmax_se", "Vcmax_se")

####these have no se, only 7 values move to 
# g1 <- read.csv("calculated data/g1_pred.csv")
# g1_agg <- summaryBy(g1_vol ~ volume, data=g1, FUN=c(mean, se))
#   names(g1_agg)[2:3]<- c("G1", "G1_se")

#Amax
A_means <- read.csv("calculated data/A_treatment_means.csv")
  A_means$volume <- as.numeric(c("5", "10", "15", "20", "25", "35", "1000"))
Amax <- A_means[, c(1, 4:5)]

####add Amax to table
table1 <- merge(table1, Amax)

####add SRL and add to table
srl <- read.csv("calculated data/srl_means.csv")

table1 <- merge(table1, srl)


####add SLA, starch, sugars, Nmass

photo_chem <- read.csv("calculated data/Amax_chem.csv")
#run volume format func
photo_chem<- vollab_func(photo_chem)

leaf_param <- photo_chem[,c(1:3,9,11:12, 14:17 )]
leaf_param$volume <- gsub("05", "5", leaf_param$volume)

leaf_param_agg <- summaryBy(sla +starch+sugars+ Nmass~ volume, data=leaf_param, FUN=c(mean, se))

table1 <- merge(table1,leaf_param_agg)

####add Rd
rd <- read.csv("calculated data/Rd_leaf.csv")
rd_agg <- summaryBy(resppermass ~ volume, data=rd, FUN=c(mean, se))

table1 <- merge(table1, rd_agg)


###now sort the table into correct var + se, 
pve_data <-table1[,c(1,2,3,4,5,6,7,8,12,9,13,10,14,11,15,16,17)]

#seperate dfr in two with mean and se, omit volume for now
pve_means <- pve_data[, c(2,4,6,8,10,12,14,16)]
pve_se <- pve_data[, c(3,5,7,9,11,13,15,17)]

###now paste together and round
dat1 <- data.frame(paste0(signif(pve_means[,1], 3), " (", signif(pve_se[,1],3),")"))
dat2 <- data.frame(paste0(signif(pve_means[,2], 3), " (", signif(pve_se[,2],2),")"))
dat3 <- data.frame(paste0(signif(pve_means[,3], 5), " (", signif(pve_se[,3],5),")"))
dat4 <- data.frame(paste0(signif(pve_means[,4], 3), " (", signif(pve_se[,4],3),")"))
dat5 <- data.frame(paste0(signif(pve_means[,5], 3), " (", signif(pve_se[,5],3),")"))
dat6 <- data.frame(paste0(signif(pve_means[,6], 5), " (", signif(pve_se[,6],5),")"))
dat7 <- data.frame(paste0(signif(pve_means[,7], 5), " (", signif(pve_se[,7],5),")"))

pve_table <- cbind(leglab, dat1)
  pve_table <- cbind(pve_table, dat2)
  pve_table <- cbind(pve_table, dat3)
  pve_table <- cbind(pve_table, dat4)
  pve_table <- cbind(pve_table, dat5)
  pve_table <- cbind(pve_table, dat6)
  pve_table <- cbind(pve_table, dat7)


write.csv(pve_table, "master_scripts/pve_table1.csv", row.names=FALSE)



# 
# pve_means <- pve_data2[, c(2,4,6,8,10,12,14)]
#   pve_means[,1] <- as.character(round(pve_means[,1], 1))
#   pve_means[,2] <- as.character(round(pve_means[,2], 1))
#   pve_means[,3] <- as.character(round(pve_means[,3], 4))
#   pve_means[,4] <- as.character(round(pve_means[,4], 1))
#   pve_means[,5] <- as.character(round(pve_means[,5], 1))
#   pve_means[,6] <- as.character(round(pve_means[,6], 4))
#   pve_means[,7] <- as.character(round(pve_means[,7], 4))
# 
# pve_se <- pve_data2[, c(3,5,7,9,11,13,15)]
#   pve_se[,1] <- as.character(round(pve_se[,1], 2))
#   pve_se[,2] <- as.character(round(pve_se[,2], 2))
#   pve_se[,3] <- as.character(round(pve_se[,3], 5))
#   pve_se[,4] <- as.character(round(pve_se[,4], 2))
#   pve_se[,5] <- as.character(round(pve_se[,5], 2))
#   pve_se[,6] <- as.character(round(pve_se[,6], 5))
#   pve_se[,7] <- as.character(round(pve_se[,7], 5))
# 
# pve_se <- apply(pve_se, 2, function(i) paste('(', i, ')', sep=''))
# 
# dat1 <- data.frame(paste(pve_means[,1], pve_se[,1], sep=" "))
# dat2 <- data.frame(paste(pve_means[,2], pve_se[,2], sep=" "))
# dat3 <- data.frame(paste(pve_means[,3], pve_se[,3], sep=" "))
# dat4 <- data.frame(paste(pve_means[,4], pve_se[,4], sep=" "))
# dat5 <- data.frame(paste(pve_means[,5], pve_se[,5], sep=" "))
# dat6 <- data.frame(paste(pve_means[,6], pve_se[,6], sep=" "))
# dat7 <- data.frame(paste(pve_means[,7], pve_se[,7], sep=" "))
# 
# 
# pve_table <- cbind(leglab, dat1)
#   pve_table <- cbind(pve_table, dat2)
#   pve_table <- cbind(pve_table, dat3)
#   pve_table <- cbind(pve_table, dat4)
#   pve_table <- cbind(pve_table, dat5)
#   pve_table <- cbind(pve_table, dat6)
#   pve_table <- cbind(pve_table, dat7)
# 
# #names(pve_table) <- c("Volume (L)", "Seedling mass (g)", expression(A[max]~~(mu*mol~m^-2~s^-1)),
#                       #expression(SLA[TNC~free]~~(m^2~g^-1)),starchlab, suglab,nmasslab,rdlab)
# 
# write.csv(pve_table, "master_scripts/pve_table1.csv", row.names=FALSE)


