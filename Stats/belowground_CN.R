#how does soil and root N change?
#specific root length?
#root mass by volume?

source("functions and packages/startscripts.R")
require(nlme)
require(visreg)
library(multcomp)

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
#write.csv(root_chem, "calculated data/root_chem.csv", row.names=FALSE)

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
soil_harvest <- soil[0:47,]
  soil_harvest$volume <- as.factor(soil_harvest$volume)

harvestmean <- summaryBy(n_perc~volume, data=soil_harvest, keep.names=TRUE)
Hmean <- round(mean(harvestmean$n_perc),4)


##stats------------------------------------------------------------------------------
require(nlme)
require(visreg)
library(multcomp)

#soilN
n_lm <- lme(n_perc ~ volume, random= ~1|ID, data=soil_harvest)
anova(n_lm)
summary(n_lm)

tukey_N<- glht(n_lm, linfct = mcp(volume = "Tukey"))
cld(tukey_N)
visreg(n_lm)

#root N
root_all <-root_chem[complete.cases(root_chem),]
boxplot(Ntrans ~ volume,data=root_all2)

root_all2 <- root_all[root_all$N_perc <= 1.0,]
root_all2$Ntrans <- asin(sqrt(root_all2$N_perc/100))
#relevel to free to evaluate container effect  
root_all2$volume <- relevel(root_all2$volume, ref="free")

root_all3 <- root_all2[! root_all2$N_perc == 0.71640,]
write.csv(root_all3, "calculated data/root_N_clean.csv", row.names=FALSE)


mean(root_all3$N_perc)

#rootN_lm <- lme(Ntrans ~ volume, random= ~1|ID, data=root_all)
#rootN_lm2<-glm(N_perc,volume,binomial,data=root_all)


rootN_container <- lme(Ntrans ~ volume, random= ~1|ID, data=root_all3)
  anova(rootN_container)
  summary(rootN_container)
  visreg(rootN_container)

tukey_rootN<- glht(rootN_container, linfct = mcp(volume = "Tukey"))
rootN_siglets <-cld(tukey_rootN)

rootN_siglets2 <- rootN_siglets$mcletters$Letters
write.csv(rootN_siglets2, "master_scripts/sigletters/sigletts_plant/sl_rootN.csv", row.names=FALSE)




#PLOT-----------------------------------------------------------------------------------------------
###these changes are after stats for plotting labels
soil_harvest$volume <- gsub("1000", "free", soil_harvest$volume)
soil_harvest$volume <- gsub("^5", "05", soil_harvest$volume)



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
bargraph.CI(volume,N_perc, data=root_all3,  xlab = "Pot Volume (l)", border=palette(), col="grey98", 
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

