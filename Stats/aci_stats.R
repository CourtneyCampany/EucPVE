
source("functions and packages/load packages.R")
source("functions and packages/functions.R")
require(broom)
require(visreg)

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
#test if two sets are different by volume

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

# Pot size effect.
ratio_potsize <- lm(ratio ~ as.factor(volume), data=aci_stats, subset=volume != "1000")
ratio_lm <- tidy(ratio_potsize)

anova(ratio_potsize)
summary(ratio_potsize)


boxplot(Vcmax.mean ~ volume, data = subset(aci_means, campaing="a"))
boxplot(Vcmax.mean ~ volume, data = subset(aci_means, campaing="b"))
boxplot(Jmax.mean ~ volume, data = aci_means)

#models

volumeVc <- lm(Vcmax ~ volume, data = acitstats)
anova(volumeVc)
summary(volumeVc)
visreg(volumeVc)

volumeVc_nofree <- lm(Vcmax ~ volume, data = acitstats, subset=volume != "1000")
anova(volumeVc_nofree)
summary(volumeVc_nofree)
visreg(volumeVc_nofree)


volumeJ <- lm(Jmax ~ volume, data = acitstats)
anova(volumeJ)
summary(volumeJ)
visreg(volumeJ)

volumeJ_nofree <- lm(Jmax ~ volume, data = acitstats, subset=volume != "1000")
anova(volumeJ_nofree)
summary(volumeJ_nofree)
visreg(volumeJ_nofree)

Vc_campaign <- lm(Vcmax ~ volume+campaign, data = acitstats)
J_campaign <- lm(Jmax ~ volume+campaign, data = acitstats)

#Summary, both vcmax and jmax are different by treatment, the ratio is not different
#the fact that the ratio is not different and linear confirms that treatments are affecting both

#-----------------------------------------------------------------
#jmax and vcmax are difffent by volume but not date...use means of volume across samples to model A
aci_parameters <- aggregate(cbind(Vcmax, Jmax) ~ volume, data=acitstats, FUN="mean")

write.csv(aci_parameters, "calculated data/aci_parameters.csv", row.names=FALSE)
