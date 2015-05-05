
source("functions and packages/startscripts.R")
require(broom)
require(nlme)
require(visreg)
library(multcomp)
require(plantecophys)

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

aci_stats <- rbind(aci1coef, aci2coef)
  aci_stats$ratio <- with(aci_stats, Jmax/Vcmax)
  aci_stats$volume <- as.factor(aci_stats$volume)
  aci_stats$campaign <- as.factor(aci_stats$campaign)
  
  #plotting
  gradient <- colorRampPalette(c("red", "blue"))
  palette(gradient(7))
  
  with(aci_stats, plot(Vcmax~Jmax, col=volume, pch=16, ylim=c(0, 125), xlim=c(0, 250)))

  
  
#test if two sets are different by volume-----------------------------------------------------------------------------
vc_diff<- lme(Vcmax ~ volume+campaign, random= ~1|ID, data=aci_stats)
summary(vc_diff)
anova(vc_diff)
tukey_vdiff<- glht(vc_diff, linfct = mcp(campaign = "Tukey"))
cld(tukey_vdiff)

j_diff<- lme(Jmax ~ volume+campaign, random= ~1|ID, data=aci_stats)
summary(j_diff)
anova(j_diff)
tukey_jdiff<- glht(j_diff, linfct = mcp(campaign = "Tukey"))
cld(tukey_jdiff)

#means, they are not different so can report means
aci_means <- summaryBy(Vcmax+Jmax+ratio ~ volume , data = aci_stats,  FUN=c(mean,se))
boxplot(Jmax.mean ~ volume, data = aci_means)
boxplot(Vcmax.mean ~ volume, data = aci_means)

# 'Being in a pot' effect---------------------------------------------------------------------------------------

#relevel by 1000
aci_stats$volume <- relevel(aci_stats$volume, ref="1000") 

#VCmax
vcmax_lm <- lme(Vcmax ~ volume, random= ~1|ID, data=aci_stats)
anova(vcmax_lm)
summary(vcmax_lm)
visreg(vcmax_lm)

tukey_vc<- glht(vcmax_lm, linfct = mcp(volume = "Tukey"))
siglets_vc <- cld(tukey_vc)

siglets_vc2 <- siglets_vc$mcletters$Letters
write.csv(siglets_vc2, "master_scripts/sigletters/sl_vcmax.csv", row.names=FALSE)

#JmAX,
Jmax_lm <- lme(Jmax ~ volume, random= ~1|ID, data=aci_stats)
anova(Jmax_lm)
summary(Jmax_lm)
visreg(Jmax_lm)

tukey_j<- glht(Jmax_lm, linfct = mcp(volume = "Tukey"))
siglets_j <- cld(tukey_j)

siglets_j2 <- siglets_j$mcletters$Letters
write.csv(siglets_j2, "master_scripts/sigletters/sl_jmax.csv", row.names=FALSE)


#Summary, both vcmax and jmax are different by treatment, the ratio is not different
#the fact that the ratio is not different and linear confirms that treatments are affecting both

#-----------------------------------------------------------------
#jmax and vcmax are difffent by volume but not date...use means of volume across samples to model A
aci_parameters <- aggregate(cbind(Vcmax, Jmax) ~ volume, data=acitstats, FUN="mean")

write.csv(aci_parameters, "calculated data/aci_parameters.csv", row.names=FALSE)
