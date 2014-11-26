
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

#test if two sets are different by volume
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

#means
aci_means <- summaryBy(Vcmax+Jmax+ratio ~ volume , data = aci_stats,  FUN=c(mean,se))


#colors
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))

bar(ratio, c(volume, campaign), aci_stats, col=palette(),half.errbar=FALSE)
bar(Jmax, c(volume, campaign), aci_stats, col=palette(),half.errbar=FALSE)
bar(Vcmax, c(volume, campaign), aci_stats, col=palette(),half.errbar=FALSE) 
with(aci_stats, plot(Vcmax~Jmax, col=volume))
abline()

# 'Being in a pot' effect.
ratio_container <- lm(ratio ~ as.factor(volume), data=aci_stats)
ratio_lm1 <- tidy(ratio_container)
ratio1_stat <- extract_func(ratio_container)
anova(ratio_container)
summary(ratio_container)
visreg(ratio_container)

boxplot(Vcmax~ volume, data = aci_stats, subset=campaign=="a") 
boxplot(Vcmax~ volume, data = aci_stats, subset=campaign=="b") 

boxplot(Jmax.mean ~ volume, data = aci_means)

#VCmax
vc_lm <- lme(Vcmax ~ volume, random= ~1|ID, data=aci_stats)
anova(vc_lm)
summary(vc_lm)

tukey_vc<- glht(vc_lm, linfct = mcp(volume = "Tukey"))
cld(tukey_vc)
visreg(vc_lm)

#JmAX,
J_lm <- lme(Jmax ~ volume, random= ~1|ID, data=aci_stats)
anova(J_lm)
summary(J_lm)

tukey_j<- glht(J_lm, linfct = mcp(volume = "Tukey"))
cld(tukey_j)
visreg(J_lm)

#ratio
ratio_lm <- lme(ratio ~ volume, random= ~1|ID, data=aci_stats)
anova(ratio_lm)
summary(ratio_lm)

tukey_r<- glht(ratio_lm, linfct = mcp(volume = "Tukey"))
cld(tukey_r)
visreg(ratio_lm)

#Summary, both vcmax and jmax are different by treatment, the ratio is not different
#the fact that the ratio is not different and linear confirms that treatments are affecting both

#-----------------------------------------------------------------
#jmax and vcmax are difffent by volume but not date...use means of volume across samples to model A
aci_parameters <- aggregate(cbind(Vcmax, Jmax) ~ volume, data=acitstats, FUN="mean")

write.csv(aci_parameters, "calculated data/aci_parameters.csv", row.names=FALSE)
