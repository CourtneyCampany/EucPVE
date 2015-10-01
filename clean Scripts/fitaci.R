#Determine jmax and vcmax from so they can be included in the A model for each pot
#generate treatment means for each pot

source("functions and packages/load packages.R")
source("functions and packages/load model packages.R")
source("functions and packages/functions.R")

library(plantecophys)

#read 2 ACi datasets (they are from two seperate dates during the experiment...deal with this later)
source("read data scripts/physiology read data.R")

#use acifunction to generate jmax and vcmax and get mean of parameters by volume
aci1_clean <- aci1[! aci1$ID == "7-8",]
aci2_clean <- aci2[! aci2$ID == "7-8",]

aci1_fit2 <- acifunction(aci1_clean)
aci2_fit2 <- acifunction(aci2_clean)

aci1_fit2$Date <- as.Date("2013-03-14")
aci2_fit2$Date <- as.Date("2013-05-14")

# aci1_fit <- acifunction(aci1)
# aci2_fit <- acifunction(aci2)

# aci1_fit$Date <- as.Date("2013-03-14")
# aci2_fit$Date <- as.Date("2013-05-14")

# jmax_vcmax <- rbind(aci1_fit, aci2_fit)

jmax_vcmax_clean <- rbind(aci1_fit2, aci2_fit2)

# write.csv(jmax_vcmax, file = "calculated data/jmax_vcmax.csv", row.names=FALSE)  

boxplot(Jmax.mean ~ volume, data = jmax_vcmax_clean)
boxplot(Vcmax.mean ~ volume, data = jmax_vcmax_clean)

# 'Being in a pot' effect---------------------------------------------------------------------------------------
library(visreg)
library(multcomp)
library(doBy)

#For Stats re-run but new function that does generate means
aci1_stats <- acifunction2(aci1_clean)
aci2_stats <- acifunction2(aci2_clean)

aci_stats <- rbind(aci1_stats, aci2_stats)
aci_stats$block <- as.factor(gsub("-[1-9]", "", aci_stats$ID))

#relevel by 1000
aci_stats$volume <- as.factor(aci_stats$volume)
aci_stats$volume <- relevel(aci_stats$volume, ref="1000") 

#VCmax
vcmax_lm <- lme(Vcmax ~ volume, random= ~1|ID, data=aci_stats)
anova(vcmax_lm)
summary(vcmax_lm)
visreg(vcmax_lm)

vcmax_lm2 <- lme(Vcmax ~ volume, random= ~1|block/ID, data=aci_stats)
anova(vcmax_lm2)

tukey_vc<- glht(vcmax_lm2, linfct = mcp(volume = "Tukey"))
siglets_vc <- cld(tukey_vc)
siglets_vc2 <- siglets_vc$mcletters$Letters

write.csv(siglets_vc2, "master_scripts/sigletters/sl_vcmax.csv", row.names=FALSE)

#JmAX,
Jmax_lm <- lme(Jmax ~ volume, random= ~1|ID, data=aci_stats)
anova(Jmax_lm)
summary(Jmax_lm)
visreg(Jmax_lm)

Jmax_lm2 <- lme(Jmax ~ volume, random= ~1|block/ID, data=aci_stats)
anova(Jmax_lm2)

tukey_j<- glht(Jmax_lm2, linfct = mcp(volume = "Tukey"))
siglets_j <- cld(tukey_j)
siglets_j2 <- siglets_j$mcletters$Letters

write.csv(siglets_j2, "master_scripts/sigletters/sl_jmax.csv", row.names=FALSE)

#need to means of all ids for phys table, not by campaign

aci_agg <- summaryBy(Jmax + Vcmax ~ volume, data=aci_stats, FUN=c(mean, se))
write.csv(aci_agg, file = "calculated data/jmax_vcmax_clean.csv", row.names=FALSE)  

