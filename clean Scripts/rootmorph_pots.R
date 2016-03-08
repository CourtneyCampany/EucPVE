source("functions and packages/startscripts.R")

#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#SRL------------------------------------------------------------------------------------------------------
srl <- read.csv("raw data/SRLmass.csv")
srl$ID <- paste(srl$plot, srl$pot, sep = "-")
srl <- merge(srl, plotsumm[3:4], all=TRUE)
srl <- subset(srl, !is.na(volume))
srl$SRL <- with(srl, (total_length_cm/100)*(srl_fw*(ss_dw/ss_fw)))
srl$SRL2 <- with(srl, (total_length_cm/100)/(srl_fw*(ss_dw/ss_fw)))
srl$volume <- as.factor(srl$volume)
row.names(srl) <- NULL
#srl <- vollab_func(srl)


###roots from harvest
rootharvest <- read.csv("calculated data/seedling mass.csv")
rootharvest2 <- rootharvest[, c(1,2,8)]


rootmorph <- merge(rootharvest2,srl)
rootmorph$potrootlength <- with(rootmorph, (total_length_cm * fineroot)/ss_dw)

rootmorph$potrootdensity <- with(rootmorph, potrootlength/volume)

rootmorph$SRLpot <- with(rootmorph, (potrootlength/100)/fineroot)##m g-1

rootmorph$volume <- as.factor(rootmorph$volume)

with(rootmorph, boxplot(SRLpot~volume))




##some values really high, remove to pots from boxplot of data  
#srl_clean <- srl[srl$SRL2 <= 100,]  
rootmorph_clean <- rootmorph[rootmorph$ID != "5-8" & rootmorph$ID != "7-8",]  



#stats on RLD
root_pots <- rootmorph[rootmorph$volume != "1000",]
root_pots <- droplevels(root_pots)

rld_container <- lme(potrootdensity ~ volume, random= ~1|ID, data=root_pots)
anova(rld_container)
summary(rld_container)

tukey_rld<- glht(rld_container, linfct = mcp(volume = "Tukey"))
rld_siglets <-cld(tukey_rld)
rld_siglets2 <- rld_siglets$mcletters$Letters
library(visreg)
visreg(rld_container)


###clean data
with(rootmorph, boxplot(potrootdensity~volume))


