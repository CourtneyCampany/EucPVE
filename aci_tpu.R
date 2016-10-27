
library(plantecophys)

#read aci curves-------------------------------------------------------------------------------------------
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

aci1 <- read.csv("raw data/ACi#1.csv")
  aci1$ID <- paste(aci1$plot, aci1$pot, sep = "-")

aci2 <- read.csv("raw data/ACi#2.csv")
  aci2$ID <- paste(aci2$plot, aci2$pot, sep = "-")

#clean bad curves
aci1_clean <- aci1[! aci1$ID == "7-8",]
aci2_clean <- aci2[! aci2$ID == "7-8",]


#run test with TPU=true for fit acit
testcurve <- aci1_clean[aci1_clean$plot==6 & aci1_clean$pot==4,]
testcurve <- droplevels(testcurve)

g<- fitaci(testcurve, Tleaf = "Temp", PPFD="PPFD",fitTPU=TRUE)

#use acifunction to generate jmax and vcmax and get mean of parameters by volume-------------------------------

acifunction_tpu <- function(x) {
  
  aci_model <- fitacis(x, "ID",varnames = list(ALEAF="Photo", Tleaf = "Temp", Ci="Ci", PPFD="PPFD"), 
                       Tcorrect=TRUE, fitTPU=TRUE)
  aci_coef <- coef(aci_model)
  aci_coef <- merge(aci_coef, plotsumm, by = "ID")
  aci_means <- summaryBy(Vcmax+Jmax+Rd+TPU ~ volume , data = aci_coef,  FUN=c(mean,se))
  return(aci_means)
}

aci1_fit2 <- acifunction_tpu(aci1_clean)
aci2_fit2 <- acifunction_tpu(aci2_clean)

aci1_fit2$Date <- as.Date("2013-03-14")
aci2_fit2$Date <- as.Date("2013-05-14")



jmax_vcmax_clean <- rbind(aci1_fit2, aci2_fit2)


boxplot(Jmax.mean ~ volume, data = jmax_vcmax_clean)
boxplot(Vcmax.mean ~ volume, data = jmax_vcmax_clean)