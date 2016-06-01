#Read amax, leafN, and TNC
amax <- read.csv("calculated data/Amax.csv")

leafN <- read.csv("calculated data/leaf N content.csv")
##am i using no tnc leaf N in manuscript figures
leafN_notnc <- read.csv("calculated data/leafnitrogen_notnc.csv")

tnc <- read.csv("calculated data/tnc_leaf.csv")
  tnc$ID <- gsub("'", "",tnc$ID)

#merge data sets-----------------------------------------------------------------------------------------------------
photo_N <- merge(amax[,c(1:2, 5:6, 9)], leafN, all=TRUE)
photo_N2 <- merge(photo_N, leafN_notnc, all=TRUE)  
  
  #remove the missing values for amax (4-7 #3)
  photo_N_noNA <-  subset(photo_N2, !is.na(Photo))
  #re work labels for volume
  photo_N_noNA$volume <- gsub("1000", "free", photo_N_noNA$volume)
  photo_N_noNA$volume <- gsub("^5", "05", photo_N_noNA$volume)
  photo_N_noNA$volume <- as.factor(photo_N_noNA$volume)

#add tnc
photo_chem <- merge(photo_N_noNA, tnc, all=TRUE)
  #remove the missing vales from both the N and tnc dataset
  photo_chem <-  subset(photo_chem, !is.na(Nperc))
  photo_chem <-  subset(photo_chem, !is.na(starch_mgperg))
  row.names(photo_chem) <- NULL

#calculate variables for stats
#tnc variables are g g-1, everything else should be g m-2

photo_chem$Nmass_notnc <- with(photo_chem, Nmass/(1-tnc))
photo_chem$lma_notnc <- with(photo_chem, ((mass/area)*10000)/(1-tnc))
photo_chem$A_mass <- with(photo_chem, Photo*sla*1000) #amax on a mass basis on nanomoles

write.csv(photo_chem, "calculated data/Amax_chem.csv", row.names=FALSE)