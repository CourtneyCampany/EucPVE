#source functions
source("functions and packages/startscripts.R")

#read data
source("read data scripts/harvest read data.R")
leafCN <- read.csv("raw data/leafCN.csv")
leafarea <- read.csv("calculated data/leafareabypot.csv")

lma <- read.csv("raw data/seedling leaf mass area.csv")
lma$ID <- paste(lma$plot, lma$pot, sep = "-")
lma <- merge(lma, plotsumm[3:4], all=TRUE)

#run formatting functions for leafCN/TNC raw data and campaign date
CN_leaf <- leafCN_format(leafCN)
CN_leaf <- add_campaign_date(CN_leaf)
CN_leaf <- merge(CN_leaf, plotsumm[3:4], all=TRUE)

lma <- add_campaign_date(lma)


#calculate C and N content of leaves
leaf_chem <- merge(CN_leaf, lma, all=TRUE)
  leaf_chem$leafC <- with(leaf_chem, mass*Cperc)
  leaf_chem$leafN <- with(leaf_chem, mass*Nperc)
  leaf_chem$volume <- as.factor(leaf_chem$volume)


###stats on leaf N---------------------------------------------------------------------------
leaf_chem2 <- leaf_chem[complete.cases(leaf_chem),]
  require(car)
  leaf_chem2$Ntrans <- with(leaf_chem2, asin(sqrt(Nperc/100)))
  leaf_chem2$Ntrans2 <- logit(leaf_chem2$Nperc, percents=TRUE )
  leaf_chem2$volume <- as.factor(leaf_chem2$volume)

require(nlme)
require(visreg)
library(multcomp)

#Nperc
nperc_lm <- lme(leafN ~ volume, random= ~1|ID, data=leaf_chem2)
anova(nperc_lm)
summary(nperc_lm)

tukey_N<- glht(nperc_lm, linfct = mcp(volume = "Tukey"))
cld(tukey_N)
visreg(nperc_lm)

#N_perc first date

nperc_lm2 <- lme(leafN ~ volume, random= ~1|ID, data=leaf_chem2, subset=Date=="2013-03-07")
anova(nperc_lm2)
summary(nperc_lm2)
visreg(nperc_lm2)
tukey_N2<- glht(nperc_lm2, linfct = mcp(volume = "Tukey"))
cld(tukey_N2)

####plotting---------------------------------------------------------------------------------
N_agg <- summaryBy(Nperc+leafN ~volume + Date, data=leaf_chem, FUN=mean, keep.names=TRUE)

plot(Nperc~Date, pch=pchs[volume],col=volume, data=N_agg)
plot(leafN~Date, pch=pchs[volume],col=volume, data=leaf_chem)
#analyze leaf chem with Asat, area, mass on campaign dates

