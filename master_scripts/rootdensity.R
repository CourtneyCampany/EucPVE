###fine root length vs leaf area
source("functions and packages/startscripts.R")

###root lenght density (lenght of roots per unit soil volume)

##read srl data
rootlength <- read.csv("calculated data/srl_clean.csv")

##read final harvest leaf area
leafarea <- read.csv("calculated data/LA_harvest.csv")

##calculate root lenght density
rootlength$rootlengthdensity <- with(rootlength, total_length_cm/volume) ##cm/l

##now make volume a factor and relevel
rootlength$volume <- as.factor(rootlength$volume)
rootlength$volume <- relevel(rootlength$volume, ref="1000")
rootlength$block <- as.factor(gsub("-[1-9]", "", rootlength$ID))

##is rootlength density different by soil volume-------------------------------------------------------------------------------

bargraph.CI(volume, rootlengthdensity, data=rootlength)

rootdensity_mod <- lme(rootlengthdensity ~ volume, random= ~1|block/ID, data=rootlength)
anova(rootdensity_mod)
summary(rootdensity_mod)
library(visreg)
library(multcomp)
visreg(rootdensity_mod)

###test this within plots
rootlength_pot <- rootlength[rootlength$volume != 1000,]
rootlength_pot <- droplevels(rootlength_pot)

rootdensity_mod2 <- lme(rootlengthdensity ~ volume, random= ~1|block/ID, data=rootlength_pot)
anova(rootdensity_mod2)
visreg(rootdensity_mod2)
tukey_density<- glht(rootdensity_mod2, linfct = mcp(volume = "Tukey"))
  siglets_density<-cld(tukey_density)
  siglets_density2 <- siglets_density$mcletters$Letters

###root length density significantly higher in the two smalles containers (5,10)
  

###-root length vs leaf area----------------------------------------------------------------------------------------------

rootleaf <- merge(rootlength[, c(1, 7)], leafarea)
  rootleaf$volume <- as.factor(rootleaf$volume)
  rootleaf$block <- as.factor(gsub("-[1-9]", "", rootleaf$ID))


rootleaf_mod <- lm(total_length_cm ~ totalarea, data=rootleaf)
anova(rootleaf_mod)
summary(rootleaf_mod)
library(visreg)
visreg(rootleaf_mod)
rootdensity_mod2 <- lme(total_length_cm ~ totalarea, random= ~1|block/ID, data=rootleaf)

library()
plot(total_length_cm ~ totalarea, data=rootleaf, pch=pchs[volume], col=volume, cex=1, ylim=c(0, 1000), xlim=c(0, 7000) )
ablineclip(rootleaf_mod, x1=min(rootleaf$totalarea), x2=max(rootleaf$totalarea),2)


###no relationship (add to methods)