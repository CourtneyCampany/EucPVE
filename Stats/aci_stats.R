
source("functions and packages/load packages.R")

#read 2 ACi datasets (they are from two seperate dates during the experiment...deal with this later)
source("read data scripts/physiology read data.R")


acifunction_raw <- function(x) {
  
  aci_model <- fitacis(x, "ID",varnames = list(ALEAF="Photo", Tleaf = "Temp", Ci="Ci", PPFD="PPFD"), 
                       Tcorrect=TRUE)
  
  aci_coef <- coef(aci_model)
  aci_coef <- merge(aci_coef, plotsumm, by = "ID")
  #aci_means <- summaryBy(Vcmax+Jmax+Rd ~ volume , data = aci_coef,  FUN=c(mean,se))
  #aci1_means$Date <- as.Date("2013-03-14")
  
  return(aci_coef)
}

aci1coef <- acifunction_raw(aci1)
aci1coef$campaign <- "a"

aci2coef <- acifunction_raw(aci2)
aci2coef$campaign <- "b"

acitstats <- rbind(aci1coef, aci2coef)

#test if two sets are different by volume


aci_means <- summaryBy(Vcmax+Jmax ~ volume+campaign , data = acitstats,  FUN=c(mean,se))

boxplot(Vcmax.mean ~ volume, data = subset(aci_means, campaing="a"))
boxplot(Vcmax.mean ~ volume, data = subset(aci_means, campaing="b"))
boxplot(Jmax.mean ~ volume, data = aci_means)

#models

volumeVc <- lm(Vcmax ~ volume, data = acitstats)
volumeJ <- lm(Jmax ~ volume, data = acitstats)


Vc_campaign <- lm(Vcmax ~ volume+campaign, data = acitstats)
J_campaign <- lm(Jmax ~ volume+campaign, data = acitstats)

#-----------------------------------------------------------------
#jmax and vcmax are difffent by volume but not date...use means of volume across samples to model A
aci_parameters <- aggregate(cbind(Vcmax, Jmax) ~ volume, data=acitstats, FUN="mean")

write.csv(aci_parameters, "calculated data/aci_parameters.csv", row.names=FALSE)
