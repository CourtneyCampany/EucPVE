
#this script creates the physiology  based data table for the mauscripts
library(doBy)
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")
       
###variabvles:  Amax, Rd, Jmax, VCmax, gs, g1
  
#1:Amax
A_means <- read.csv("calculated data/A_treatment_means.csv")
  A_means$volume <- as.numeric(c("5", "10", "15", "20", "25", "35", "1000"))

table2 <- A_means[, 1:3]

#2: Rd

rd <- read.csv("calculated data/rdark_clean.csv")

table2 <- merge(table2, rd[,c(1:2,4)])
       
       
##2-3: jmax, vcmax
phys <- read.csv("calculated data/jmax_vcmax_clean.csv")
  names(phys)[2:5]<- c("Jmax", "Vcmax", "Jmax_se", "Vcmax_se")

table2 <- merge(table2, phys)
       
#4. gs
gs <- read.csv("calculated data/conductance.csv")
  gs2 <- gs[gs$gs <= 0.75,]

  gs_agg <- summaryBy(gs ~ volume, data=gs2, FUN=c(mean, se))

table2 <- merge(table2, gs_agg)

#5. g1 (uses mean of g1 date)
g1 <- read.csv("calculated data/g1_pred.csv")
  
  g1_agg <- summaryBy(g1_date ~ volume, data=g1, FUN=c(mean, se))
  names(g1_agg)[2:3]<- c("g1", "g1_se")

table2 <- merge(table2, g1_agg)


###now sort the table into correct var + se--------------------------------------------------------------------
phys_tab <-table2[,c(1,2,3,4,5,6,8,7,9,10,11,12,13)]


#seperate dfr in two with mean and se, omit volume for now
phys_means <- phys_tab[, c(2,4,6,8,10,12)]
phys_se <- phys_tab[, c(3,5,7,9,11,13)]

###now paste together and round
phys1 <- data.frame(paste0(sprintf("%2.1f", round(phys_means[,1], 1)), " (", sprintf("%2.1f", round(phys_se[,1],1)),")"))
phys2 <- data.frame(paste0(sprintf("%3.2f", round(phys_means[,2], 2)), " (", sprintf("%3.2f", round(phys_se[,2],2)),")"))
phys3 <- data.frame(paste0(sprintf("%2.1f", round(phys_means[,3], 1)), " (", sprintf("%2.1f", round(phys_se[,3],1)),")"))
phys4 <- data.frame(paste0(sprintf("%2.1f", round(phys_means[,4], 1)), " (", sprintf("%2.1f", round(phys_se[,4],1)),")"))
phys5 <- data.frame(paste0(sprintf("%3.2f", round(phys_means[,5], 2)), " (", sprintf("%2.3f", round(phys_se[,5],3)),")"))
phys6 <- data.frame(paste0(sprintf("%2.1f", round(phys_means[,6], 1)), " (", sprintf("%2.2f", round(phys_se[,6],2)),")"))


pve_table2 <- cbind(leglab, phys1)
  pve_table2 <- cbind(pve_table2, phys2)
  pve_table2 <- cbind(pve_table2, phys3)
  pve_table2 <- cbind(pve_table2, phys4)
  pve_table2 <- cbind(pve_table2, phys5)
  pve_table2 <- cbind(pve_table2, phys6)
  
  ###read in sigletters from sigletters folder
  sigletter_files <- list.files(path = "master_scripts/sigletters/sigletts_phys/", pattern="*.csv", full.names = TRUE)
  ##make names of list with file names minus extension
  sigletter_vars <- gsub("master_scripts/sigletters/sigletts_phys/", "", sigletter_files)
  sigletter_vars <- gsub(".csv", "", sigletter_vars)
  sigletter_list <- lapply(sigletter_files, function(x) read.csv(x))
  ##add names to list
  names(sigletter_list) <- sigletter_vars
  ##order of sig letters is free, 5, 10, 15, 20, 25, 35...need to chance so paste below is correct
  sigvol <- c(1000, 5, 10, 15, 20, 25, 35)
  siglet <- lapply(sigletter_list, function(x) cbind(x, sigvol))
  siglet2 <- lapply(siglet, function(x) as.data.frame(x))
  siglet3 <- list()
  for(i in 1:7) {
    siglet3[[i]] <- siglet2[[i]][c(2,3,4,5,6,7,1),] 
  }


###add sigletters to table
  #1. amax
  pve_table2[[2]] <- paste(pve_table2[[2]], siglet3[[1]][,1])
  pve_table2[[3]] <- paste(pve_table2[[3]], siglet3[[6]][,1])
  pve_table2[[4]] <- paste(pve_table2[[4]], siglet3[[5]][,1])
  pve_table2[[5]] <- paste(pve_table2[[5]], siglet3[[7]][,1])
  pve_table2[[6]] <- paste(pve_table2[[6]], siglet3[[4]][,1])
  pve_table2[[7]] <- paste(pve_table2[[7]], siglet3[[2]][,1])

pval <- as.vector(c("Container Effect (P value)", 0.001, 0.269, 0.004, 0.005, 0.007, 0.001))

pve_table2$leglab <- as.character(pve_table2$leglab)

pve_table3 <- rbind(pve_table2, pval)

  
write.csv(pve_table3, "master_scripts/pve_table2.csv", row.names=FALSE)




