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


#Amax_means for Data table-----------------------------------------------------------------------
PSmax <- subset(PS, type=="Amax")

#mean of 5 logs per plant
PSmax_spot<- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSmax)
  PSmax_spot$volume <- as.factor(PSmax_spot$volume)

#mean by plant over all dates, then treatment means
PSmax_ID <- summaryBy(Photo+volume ~ ID, FUN=mean, keep.names=TRUE, data=PSmax_spot)
  PSmax_ID$volume <- as.factor(PSmax_ID$volume)


#Asat----------------------------------------------------------------------------------------------
PSsat <- subset(PS, type=="Asat")

#mean of 5 logs per plant
PSsat_spot<- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSsat)
  PSsat_spot$volume <- as.factor(PSsat_spot$volume)

#mean by plant over all dates, then treatment means
PSsat_ID <- summaryBy(Photo+volume ~ ID, FUN=mean, keep.names=TRUE, data=PSsat_spot)
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
require(visreg)
library(multcomp)

#asat
asat_lm <- lme(Photo ~ volume, random= ~1|ID, data=PSsat_spot)
#   anova(asat_lm)
#   summary(asat_lm)

  tukey_A<- glht(asat_lm, linfct = mcp(volume = "Tukey"))
  siglets <-cld(tukey_A)
#   visreg(asat_lm)

# #lets prove that asat was immediately different, then use average
# asat_lm_d1 <- lme(Photo ~ volume, random= ~1|ID, data=PSsat_spot, subset=Date=="2013-03-07")
#   anova(asat_lm_d1)
#   summary(asat_lm_d1)
# 
#   tukey_A1<- glht(asat_lm_d1, linfct = mcp(volume = "Tukey"))
#     cld(tukey_A1)
#   visreg(asat_lm_d1)

# #amax
# amax_lm <- lme(Photo ~ volume, random= ~1|ID, data=PSmax_spot)
#   anova(amax_lm)
#   summary(amax_lm)
# 
# tukey_Amax<- glht(amax_lm, linfct = mcp(volume = "Tukey"))
# cld(tukey_Amax)
# visreg(amax_lm)

# #lets prove that asat was immediately different, then use average
# amax_lm_d1 <- lme(Photo ~ volume, random= ~1|ID, data=PSmax_spot, subset=Date=="2013-03-07")
#   anova(amax_lm_d1)
#   summary(amax_lm_d1)
# 
#   tukey_Amax1<- glht(amax_lm_d1, linfct = mcp(volume = "Tukey"))
#   cld(tukey_Amax1)
#   visreg(amax_lm_d1)


##PLOTTING----------------------------------------------------------------------------------------------------------
SigLetters <- siglets$mcletters$Letters

windows(7,5)
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
###barplot with asat values, drop colors for now ( col=palette(),)
bar(Photo, volume, PSsat_ID,half.errbar=FALSE, xlab="Soil Volume  (l)",ylab="", ylim=c(0,25), names.arg = leglab,
    col="grey", legend=FALSE)
title(ylab=satlab, mgp=ypos)
text(c(.7,1.9,3.1,4.3,5.5,6.75,7.9), 10, SigLetters, cex=1.3)

dev.copy2pdf(file= "master_scripts/manuscript_figs/Asat.pdf")
dev.off()


###png

# png(filename = "master_scripts/manuscript_figs/png/asat.png", width = 11, height = 8.5, units = "in", res= 400)
# par(mar=c(5,5,2,2), cex.axis=1.5,cex.lab=1.75 ,las=1)
# bar(Photo, volume, PSsat_ID,half.errbar=FALSE, xlab="Soil Volume  (l)",ylab="", ylim=c(0,25), names.arg = leglab,
#     col="grey", legend=FALSE)
# title(ylab=satlab, mgp=ypos)
# text(c(.7,1.9,3.1,4.3,5.5,6.75,7.9), 10, SigLetters, cex=1.3)
# 
# dev.off()
