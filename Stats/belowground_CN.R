#how does soil and root N change?
#specific root length?
#root mass by volume?

#source functions and packages
source("functions and packages/functions.R")
source("functions and packages/load packages.R")
#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#root mass and c/n
root_cn <- read.csv("raw data/fine root CN.csv")
root_mass <- read.csv("calculated data/seedling mass.csv")
root_mass <- root_mass[,c(1:3, 8)]

root_chem <- merge(root_cn, root_mass, all=TRUE)
root_chem$Nfr <- with(root_chem, fineroot*(N_perc/100))
root_chem$Ncr <- with(root_chem, Croot*(N_perc/100))
root_chem$rootN <- with(root_chem, Nfr+Ncr)
root_chem$rootmass <- with(root_chem, Croot+fineroot)

#replace 1000 with free
root_chem$volume <- gsub("1000", "free", root_chem$volume)
root_chem$volume <- gsub("^5", "05", root_chem$volume)
root_chem$volume <- as.factor(root_chem$volume)
write.csv(root_chem, "calculated data/root_chem.csv", row.names=FALSE)

#roots no free
root_pot <- subset(root_chem, volume != "1000")

#soil CN
soil <- read.csv("raw data/soil CN.csv")

#pre experiment soils
soilpre <- soil[48:53,]
soilpre$volume <- as.numeric(gsub("pre", "", soilpre$ID))
volumeorder<-order(soilpre$volume, by=soilpre$ID)
soilpre <- soilpre[volumeorder,]

soilN_mean = round(mean(soilpre$n_perc),3)


#soil harvest
soil_harvest <- soil[0:48,]
harvestmean <- summaryBy(n_perc~volume, data=soil_harvest, keep.names=TRUE)
Hmean <- round(mean(harvestmean$n_perc),3)

soil_harvest$volume <- gsub("1000", "free", soil_harvest$volume)
soil_harvest$volume <- gsub("^5", "05", soil_harvest$volume)
soil_harvest$volume <- as.factor(soil_harvest$volume)

srl <- read.csv("raw data/SRLmass.csv")
srl$ID <- paste(srl$plot, srl$pot, sep = "-")
srl <- merge(srl, plotsumm[3:4], all=TRUE)
srl <- subset(srl, !is.na(volume))
srl$SRL <- with(srl, (total_length_cm/100)*(srl_fw*(ss_dw/ss_fw)))
#replace 1000 with free
srl$volume <- gsub("1000", "free", srl$volume)
srl$volume <- gsub("^5", "05", srl$volume)
srl$volume <- as.factor(srl$volume)

#plot stuff

#colors
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))

#PLOT of Root Mass-----------------------------------------------------------------------------------------------

#froot
windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,fineroot, data=root_chem,   border=palette(), col="grey98", 
            ylab = "Fine Root Mass (g)",ylim = c(0, 65), xlab = "Pot Volume (l)")
box()
dev.copy2pdf(file="output/roots and soil/frootmass.pdf")
dev.off()

#froot no free
windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,fineroot, data=root_pot,   border=palette(), col="grey98", 
            ylab = "Fine Root Mass (g)",ylim = c(0, 10), xlab = "Pot Volume (l)")
box()
dev.copy2pdf(file="output/roots and soil/frootmass_nofree.pdf")
dev.off()

#total root mass
windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,rootmass, data=root_chem,   border=palette(), col="grey98", 
            ylab = "Root Mass (g)",ylim = c(0, 100), xlab = "Pot Volume (l)")
box()
dev.copy2pdf(file="output/roots and soil/rootmass.pdf")
dev.off()

#froot no free
windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,rootmass, data=root_pot,   border=palette(), col="grey98", 
            ylab = "Root Mass (g)",ylim = c(0, 30), xlab = "Pot Volume (l)")
box()
dev.copy2pdf(file="output/roots and soil/rootmass_nofree.pdf")
dev.off()

#-----------------------------------------------------------------------------------------------
#Root N %
windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,N_perc, data=root_chem,  xlab = "Pot Volume (l)", border=palette(), col="grey98", 
              ylab = "Root Nitrogen (%)",ylim = c(0, 1.2))
box() 
  
dev.copy2pdf(file="output/roots and soil/rootNperc.pdf")
dev.off() 
#-----------------------------------------------------------------------------------------------  
#fine root N content
  
windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,Nfr, data=root_chem,  xlab = "Pot Volume (l)", border=palette(), col="grey98", 
            ylab = "Root Nitrogen (g)",ylim = c(0, .5))
box()
dev.copy2pdf(file="output/roots and soil/rootN.pdf")
dev.off() 
#no free
windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,Nfr, data=root_pot,  xlab = "Pot Volume (l)", border=palette(), col="grey98", 
            ylab = "Root Nitrogen (g)",ylim = c(0, .08))
box()
dev.copy2pdf(file="output/roots and soil/rootN_nofree.pdf")
dev.off() 

#-----------------------------------------------------------------------------------------------  

#Soil-pre
bargraph.CI(volume,n_perc, data=soilpre,  xlab = "Pot Volume (l)", border=palette(), col="grey98", 
            ylab = "Soil Nitrogen (%)",ylim = c(0, .1))
box()

#soil harvest
windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,n_perc, data=soil_harvest,  xlab = "Pot Volume (l)", border=palette(), col="grey98", 
            ylab = "Soil Nitrogen (%)",ylim = c(0, .06))
box()
title(main=paste("time 0 soil N% = ", soilN_mean), line=-1.5, font.main=1, adj=.05, cex.main=1)
title(main=paste("mean harvest soil N% = ", Hmean), line=-3, font.main=1, adj=.05, cex.main=1)

dev.copy2pdf(file="output/roots and soil/soilN.pdf")
dev.off()

#-----------------------------------------------------------------------------------------------  
#SRL

windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,SRL, data=srl,  xlab = "Pot Volume (l)", border=palette(), col="grey98", 
            ylab = expression(Specific~Root~Length~~(m~g^1)),ylim = c(0, .7))
box()
dev.copy2pdf(file="output/roots and soil/srl.pdf")
dev.off()

srlm <- lm(SRL ~ as.factor(volume), data=srl)
summary(srlm)
anova(srlm)
library(visreg)
visreg(srlm)
