### new table 1 based on reviewer suggestions:

#Biomass, SLA, SRL FRLD, Model C

library(doBy)
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")


# table1 variables ---------------------------------------------------------

#1. seedling mass
harvestmass <- read.csv("calculated data/seedling mass.csv") 

  #totalmass
  table1 <- summaryBy(totalmass~volume, data=harvestmass, FUN=c(mean, se))
  names(table1)[2:3]<- c("Seedling mass", "Seedling mass_se")
  
#2 harvested leaf area
  leafarea <- read.csv("calculated data/LA_harvest.csv")
    leafarea$lam2 <- with(leafarea, totalarea/10000) 
  la_agg <- summaryBy(lam2~ volume, FUN=c(mean,se), data=leafarea)
  table1 <- merge(table1, la_agg)
  
#3. SLAnotnc
sla_free <- read.csv("calculated data/sla_free_clean.csv")
  
  table1 <- merge(table1, sla_free)

#4. SRL
srl <- read.csv("calculated data/srl_means.csv")
  srl$srl <- srl$SRL2.mean
  srl$srl.se <- srl$SRL2.se
  
  table1 <- merge(table1,srl[,c(1,4:5)])
  
#5. RLD
  
rld <- read.csv("calculated data/RLD_agg.csv")
  
  table1 <- merge(table1,rld, all=TRUE)
  
#seperate dfr in two with mean and se------------------------------------------------------------------
  tab1_means <- table1[, c(2,4,6,8,10)]
  tab1_se <- table1[, c(3,5,7,9,11)]
  
  ###now paste together and round
  dat1 <- data.frame(paste0(sprintf("%2.1f",round(tab1_means[,1], 1)), " (", sprintf("%3.2f", round(tab1_se[,1],2)),")"))
  dat2 <- data.frame(paste0(sprintf("%1.2f",round(tab1_means[,2], 2)), " (", sprintf("%1.3f", round(tab1_se[,2],3)),")"))
  dat3 <- data.frame(paste0(sprintf("%2.1f",round(tab1_means[,3], 1)), " (", sprintf("%3.2f", round(tab1_se[,3],2)),")"))
  dat4 <- data.frame(paste0(sprintf("%2.1f",round(tab1_means[,4], 1)), " (", sprintf("%2.2f", round(tab1_se[,4],2)),")"))
  dat5 <- data.frame(paste0(sprintf("%2.1f",round(tab1_means[,5], 1)), " (", sprintf("%2.2f", round(tab1_se[,5],2)),")"))
 
pve_table1 <- cbind(leglab, dat1)
  pve_table1 <- cbind(pve_table1, dat2)
  pve_table1 <- cbind(pve_table1, dat3)
  pve_table1 <- cbind(pve_table1, dat4)  
  pve_table1 <- cbind(pve_table1, dat5) 
  
# add model mass (no stats) -----------------------------------------------
  
#5. Modelled C mass
cue <- read.csv("calculated data/CUEdaily.csv")  
  cue_dat <- cue[, c(1, 2)]
  cue_dat$tdc <- round(cue_dat$tdc_net.sum,1)
  
  pve_table1 <- cbind(pve_table1,cue_dat[,3])  
  
#read in sigletters from sigletters folder-------------------------------------------------------------------
  sigletter_files <- list.files(path = "master_scripts/sigletters/siglets_table1/", pattern="*.csv", full.names = TRUE)
    ##make names of list with file names minus extension
    sigletter_vars <- gsub("master_scripts/sigletters/siglets_table1/", "", sigletter_files)
    sigletter_vars <- gsub(".csv", "", sigletter_vars)
    sigletter_list <- lapply(sigletter_files, function(x) read.csv(x))
    ##add names to list
    names(sigletter_list) <- sigletter_vars
  
  la_sigs <-   sigletter_list[1] #this one is in correct order
  table1_sigs <- sigletter_list[2:4]
    
  ##order of sig letters is free, 5, 10, 15, 20, 25, 35...need to chance so paste below is correct
  sigvol <- c(1000, 5, 10, 15, 20, 25, 35)
  siglet <- lapply(table1_sigs, function(x) cbind(x, sigvol))
  siglet2 <- lapply(siglet, function(x) as.data.frame(x))
  siglet3 <- list()
  for(i in 1:3) {
    siglet3[[i]] <- siglet2[[i]][c(2,3,4,5,6,7,1),] 
  }
  
  ##Add in LA stats
  vols <-  c(5, 10, 15, 20, 25, 35, 1000)
  la_sigs2 <- lapply(la_sigs, function(x) cbind(x, vols))
  
  
  
  ###add sigletters to table
  pve_table1[[2]] <- paste(pve_table1[[2]], siglet3[[1]][,1])
  pve_table1[[3]] <- paste(pve_table1[[3]], la_sigs2[[1]][,1])
  pve_table1[[4]] <- paste(pve_table1[[4]], siglet3[[2]][,1])
  pve_table1[[5]] <- paste(pve_table1[[5]], siglet3[[3]][,1])


  #add RLD seperate
  rldsigs <- read.csv("master_scripts/sigletters/sl_rld.csv") # order is this: 5,10,15,20,25,35,1000 
  
  pve_table1[[6]] <- paste(pve_table1[[6]][1:7], rldsigs[[1]][1:7])
  
  ###replace the free in RLD with nothing
  pve_table1[7,6] <- ""
  
  ##rld an volume p<0.0001
  pval_row <- c("Volume Effect (P value)", 0.001, 0.001, 0.001, 0.009, 0.001, "")
  
  pve_table1$leglab <- as.character(pve_table1$leglab)
  
  pve_table1_new<- rbind(pve_table1, pval_row)
  
  write.csv(pve_table1_new, "master_scripts/pve_table1_new.csv", row.names=FALSE)
  
  
  
  
