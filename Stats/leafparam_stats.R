###stats fpr leaf level data with block/id as random effect

#source functions
source("functions and packages/startscripts.R")


photo_chem <- read.csv("calculated data/Amax_chem.csv")
#run volume format func
photo_chem<- vollab_func(photo_chem)
#recalculate sla in proper si units (m2/kg)
photo_chem$SLA <- with(photo_chem, (area/10000) / (mass/1000))
#simple leaf N %
photo_chem$leafnperc <- with(photo_chem, Nperc*100)

leaf_param <- photo_chem[,c(1:3,22:23,11:12, 14:17, 7:8)]
#leaf_param$volume <- gsub("05", "5", leaf_param$volume)


###stats on leaf parameters for manuscript
library(visreg)
library(multcomp)
leaf_param$volume <- as.factor(leaf_param$volume)
leaf_param$volume <- relevel(leaf_param$volume, ref="1000")
leaf_param$block <- as.factor(gsub("-[1-9]", "", leaf_param$ID))

#1: SLA (redone with mass-tnc mass)
boxplot(SLA ~ volume, data=leaf_param)

  #calculate tnc free mass
  leaf_param$mass_notnc <- with(leaf_param, mass-(mass*(starch_mgperg+sugars_mgperg)/1000))
  #TNC free sla
  leaf_param$sla_free <- with(leaf_param, (area/10000)/(mass_notnc/1000))
  
  #get means
  sla_agg <- summaryBy(sla_free ~ volume, data=leaf_param, FUN=c(mean, se))

sla_container <- lme(sla_free ~ volume, random= ~1|ID, data=leaf_param)
  anova(sla_container)
  summary(sla_container)
  visreg(sla_container)
  
sla_container2 <- lme(sla_free ~ volume, random= ~1|block/ID, data=leaf_param)
  anova(sla_container2)
  summary(sla_container2)

tukey_sla<- glht(sla_container2, linfct = mcp(volume = "Tukey"))
sla_siglets <-cld(tukey_sla)
sla_siglets2 <- sla_siglets$mcletters$Letters

write.csv(sla_siglets2, "master_scripts/sigletters/sigletts_plant/sl_sla.csv", row.names=FALSE)
##also save sla free numbers to put in paper table
write.csv(sla_agg, "calculated data/sla_free_clean.csv", row.names = FALSE)


#2. Starch
boxplot(starch ~ volume, data=leaf_param)

starch_container <- lme(starch ~ volume, random= ~1|ID, data=leaf_param)
anova(starch_container)
summary(starch_container)
visreg(starch_container)

starch_container2 <- lme(starch ~ volume, random= ~1|block/ID, data=leaf_param)
anova(starch_container2)
summary(starch_container2)

anova(starch_container,starch_container2)

tukey_starch<- glht(starch_container2, linfct = mcp(volume = "Tukey"))
starch_siglets <-cld(tukey_starch)
starch_siglets2 <- starch_siglets$mcletters$Letters

write.csv(starch_siglets2, "master_scripts/sigletters/sigletts_plant/sl_starch_.csv", row.names=FALSE)

#3. Sugars
boxplot(sugars ~ volume, data=leaf_param)

sugar_container <- lme(sugars ~ volume, random= ~1|ID, data=leaf_param)
  anova(sugar_container)
  summary(sugar_container)
  visreg(sugar_container)
  
sugar_container2 <- lme(sugars ~ volume, random= ~1|block/ID, data=leaf_param)
  anova(sugar_container2)
  summary(sugar_container2)


tukey_sugar<- glht(sugar_container2, linfct = mcp(volume = "Tukey"))
  sugar_siglets <-cld(tukey_sugar)
  sugar_siglets2 <- sugar_siglets$mcletters$Letters
  
write.csv(sugar_siglets2, "master_scripts/sigletters/sigletts_plant/sl_sugar.csv", row.names=FALSE)


#4. leaf N
library(lme4)
boxplot(leafnperc ~ volume, data=leaf_param)

leaf_param$Ntrans <- asin(sqrt(leaf_param$leafnperc/100))

leafN_container2 <- lmer(leafnperc ~ 1 + (1|ID), data=leaf_param)
leafN_container <- lmer(leafnperc ~ volume + (1|ID), data=leaf_param)
anova(leafN_container2,leafN_container)

leafN_container3 <- lme(leafnperc ~ volume, random= ~1|ID, data=leaf_param)
  anova(leafN_container3)
  summary(leafN_container3)
  visreg(leafN_container3)
  
leafN_container4 <- lme(leafnperc ~ volume, random= ~1|block/ID, data=leaf_param)
  anova(leafN_container4)
  summary(leafN_container4)


tukey_leafN<- glht(leafN_container4, linfct = mcp(volume = "Tukey"))
  leafN_siglets <-cld(tukey_leafN)
  leafN_siglets2 <- leafN_siglets$mcletters$Letters
  
write.csv(leafN_siglets2, "master_scripts/sigletters/sigletts_plant/sl_leafN.csv", row.names=FALSE)
  
  
