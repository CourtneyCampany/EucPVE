#source functions, packages, anbd plot objects
source("functions and packages/startscripts.R")

#Read in spot A measurements and merge plot design
#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
  plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#read in gas exchange master file, all dates
gasexchange <- read.csv("raw data/AsatAmax_master.csv")
  gasexchange$Date <- as.Date(gasexchange$Date)
  gasexchange$ID <- paste(gasexchange$plot, gasexchange$pot, sep = "-")

PS <- merge(plotsumm, subset(gasexchange, select = c("campaign", "ID", "CO2", "Photo")))
  PS$type <- factor(ifelse (PS$CO2 == "400", "Asat", "Amax"))

#run function to add campaign date
PS <- add_campaign_date(PS)


#Asat----------------------------------------------------------------------------------------------
PSsat <- subset(PS, type=="Asat")

#mean of 5 logs per plant
PSsat_spot<- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSsat)
  PSsat_spot$volume <- as.factor(PSsat_spot$volume)
  
#mean by plant over all dates, then treatment means
PSsat_ID <- summaryBy(Photo ~ volume + ID, FUN=mean, keep.names=TRUE, data=PSsat_spot)
  PSsat_ID$volume <- as.factor(PSsat_ID$volume)


##Photosynthesis means for paper table---------------------------------------------------------------------------

# ##overall means
# PSmax_mean <- summaryBy(Photo ~ volume, data= PSmax_ID, FUN=c(mean, se))
#   names(PSmax_mean)[2:3]<- c("Amax", "Amax_se")
# PSsat_mean <- summaryBy(Photo~volume, data=PSsat_ID, FUN=c(mean,se))
#   names(PSsat_mean)[2:3]<- c("Asat", "Asat_se")
# 
# A_means <- merge(PSmax_mean, PSsat_mean)
# write.csv(A_means, "calculated data/A_treatment_means.csv", row.names=FALSE)


####stats on A------------------------------------------------------------------------------------------------
require(nlme)
# require(visreg)
library(multcomp)
  
#relevel to free to evaluate container effect  
PSsat_spot$volume <- relevel(PSsat_spot$volume, ref="1000")
PSsat_spot$block <- as.factor(gsub("-[1-9]", "", PSsat_spot$ID))

#asat
# asat_lm <- lme(Photo ~ volume, random= ~1|ID, data=PSsat_spot)
#   anova(asat_lm)
#   summary(asat_lm)
asat_lm2 <- lme(Photo ~ volume, random= ~1|block/ID, data=PSsat_spot)
  # anova(asat_lm2)

tukey_A<- glht(asat_lm2, linfct = mcp(volume = "Tukey"))
  siglets <-cld(tukey_A)
  siglets_asat <- siglets$mcletters$Letters

##save asat sig letters for table (no longer using this figure)  
write.csv(siglets_asat, "master_scripts/sigletters/sigletts_phys/sl_asat.csv", row.names = FALSE)

#lets prove that asat was immediately different, then use average
# asat_lm_d1 <- lme(Photo ~ volume, random= ~1|block/ID, data=PSsat_spot, subset=Date=="2013-03-07")
#   anova(asat_lm_d1)
#   summary(asat_lm_d1)
# 
#   tukey_A1<- glht(asat_lm_d1, linfct = mcp(volume = "Tukey"))
#     cld(tukey_A1)
#   visreg(asat_lm_d1)

#amax
# PSmax_spot$volume <- relevel(PSmax_spot$volume, ref="1000")  
#   
# amax_lm <- lme(Photo ~ volume, random= ~1|ID, data=PSmax_spot)
#   anova(amax_lm)
#   summary(amax_lm)
# 
# amax_lm2 <- lme(Photo ~ volume, random= ~1|block/ID, data=PSmax_spot)
#   anova(amax_lm2)
# 
# tukey_Amax<- glht(amax_lm2, linfct = mcp(volume = "Tukey"))
#   siglets_amax <-cld(tukey_Amax)
#   siglets_amax2 <- siglets_amax$mcletters$Letters
# 
# write.csv(siglets_amax2, "master_scripts/sigletters/sl_amax.csv", row.names=FALSE)

#lets prove that amax was immediately different, then use average
# amax_lm_d1 <- lme(Photo ~ volume, random= ~1|block/ID, data=PSmax_spot, subset=Date=="2013-03-07")
#   anova(amax_lm_d1)
#   summary(amax_lm_d1)
# 
#   tukey_Amax1<- glht(amax_lm_d1, linfct = mcp(volume = "Tukey"))
#   cld(tukey_Amax1)
#   visreg(amax_lm_d1)


##PLOTTING----------------------------------------------------------------------------------------------------------
SigLetters <- siglets$mcletters$Letters
##need to reorder sigletters for order of bars on figure
sl2 <- SigLetters[c(2:7, 1)]

# windows(7,5)
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
bar(Photo, volume, PSsat_ID,half.errbar=FALSE, xlab="Soil Volume  (L)",ylab="", ylim=c(0,25), names.arg = leglab,
    col="grey", legend=FALSE, bg="white")
title(ylab=satlab, mgp=ypos)
text(c(.7,1.9,3.1,4.3,5.5,6.75,7.9), 10, sl2, cex=1.3)

