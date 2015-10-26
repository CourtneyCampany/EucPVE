##calcualte a table of root parameters
##N%, RLD, SRL

library(doBy)
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")


#1: root N
rootN <- read.csv("calculated data/root_N_clean.csv")  

root_agg <- summaryBy(N_perc~ volume, data=rootN, FUN=c(mean, se))
  root_agg$volume <- gsub("05", "5", root_agg$volume)
  root_agg$volume <- gsub("free", "1000", root_agg$volume)


#2:SRL
srl <- read.csv("calculated data/srl_means.csv")
  srl$srl <- srl$SRL2.mean
  srl$srl.se <- srl$SRL2.se

roottable <- merge(root_agg,srl[,c(1,4:5)])


#3:RLD

rld <- read.csv("calculated data/RLD_agg.csv")
# rld_agg <- summaryBy(rootlengthdensity ~ volume, data=rld, FUN=c(mean, se))

roottable2 <- merge(roottable,rld, all=TRUE)


##order of sig letters is free, 5, 10, 15, 20, 25, 35...need to change so paste below is correct-----------------------
roottable3 <- roottable2[c(7,1,3,4,5,6,2),]


#seperate dfr in two with mean and se, omit volume for now
root_means <- roottable3[, c(2,4,6)]
root_se <- roottable3[, c(3,5,7)]

dat1 <- data.frame(paste0(sprintf("%1.2f",round(root_means[,1], 2)), " (", sprintf("%1.2f", round(root_se[,1],2)),")"))
dat2 <- data.frame(paste0(sprintf("%2.1f",round(root_means[,2], 1)), " (", sprintf("%2.2f", round(root_se[,2],2)),")"))
dat3 <- data.frame(paste0(sprintf("%2.1f",round(root_means[,3], 1)), " (", sprintf("%2.2f", round(root_se[,3],2)),")"))


root_tab1 <- cbind(leglab, dat1)
root_tab1 <- cbind(root_tab1, dat2)
root_tab1 <- cbind(root_tab1, dat3)


###read in sigletters from sigletters folder
sigletter_files <- list.files(path = "master_scripts/sigletters/siglets_root/", pattern="*.csv", full.names = TRUE)
##make names of list with file names minus extension
sigletter_vars <- gsub("master_scripts/sigletters/sigletts_root/", "", sigletter_files)
sigletter_vars <- gsub(".csv", "", sigletter_vars)
sigletter_list <- lapply(sigletter_files, function(x) read.csv(x))
##add names to list
names(sigletter_list) <- sigletter_vars

##order of sig letters is free, 5, 10, 15, 20, 25, 35...need to chance so paste below is correct
sigvol <- c(1000, 5, 10, 15, 20, 25, 35)
siglet <- lapply(sigletter_list, function(x) cbind(x, sigvol))
siglet2 <- lapply(siglet, function(x) as.data.frame(x))
siglet3 <- list()
for(i in 1:2) {
  siglet3[[i]] <- siglet2[[i]][c(2,3,4,5,6,7,1),] 
}

###add sigletters to table, will have to add RLD seperate

root_tab1[[2]] <- paste(root_tab1[[2]], siglet3[[1]][,1])
root_tab1[[3]] <- paste(root_tab1[[3]], siglet3[[2]][,1])

rldsigs <- read.csv("master_scripts/sigletters/sl_rld.csv") # order is this: 5,10,15,20,25,35,1000 

root_tab1[[4]] <- paste(root_tab1[[4]][1:7], rldsigs[[1]][1:7])

###replace the free in RLD with nothing
root_tab1[7,4] <- ""

##rld an volume p<0.0001
pval_row <- c("Volume Effect (P value)",  0.017, 0.009, 0.001) #rootN, SRL, RLD

root_tab1$leglab <- as.character(root_tab1$leglab)

root_tab1 <- rbind(root_tab1, pval_row)

write.csv(root_tab1, "master_scripts/root_table.csv", row.names=FALSE)


