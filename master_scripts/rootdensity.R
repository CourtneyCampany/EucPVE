###fine root length vs leaf area
source("functions and packages/startscripts.R")
library(visreg)
library(multcomp)

###root lenght density (lenght of roots per unit soil volume)

##read srl data
rootlength <- read.csv("calculated data/srl_clean.csv")

##read final harvest leaf area
leafarea <- read.csv("calculated data/LA_harvest.csv")

##calculate root lenght density
rootlength$rootlengthdensity <- with(rootlength, total_length_cm/volume) ##cm/l
rootlength$rootlengthdensity2 <- with(rootlength, rootlengthdensity*.01) ##m/dm3
rootlength$rootlengthdensity3 <- with(rootlength, rootlengthdensity*.00001) ##m/cm3

##now make volume a factor and relevel
rootlength$volume <- as.factor(rootlength$volume)
rootlength$volume <- relevel(rootlength$volume, ref="1000")
rootlength$block <- as.factor(gsub("-[1-9]", "", rootlength$ID))

write.csv(rootlength[,c(1:3,7:9,12)], "calculated data/RLD.csv", row.names=FALSE)


###make a means dataframe with empty spaces for free and save for table----------------------------------------------------------

rld_agg <- summaryBy(rootlengthdensity2 ~ volume, data=rootlength[rootlength$volume != 1000,], FUN=c(mean,se))
emptyfree <- data.frame(volume = 1000, rootlengthdensity2.mean = "", rootlengthdensity2.se = "")

rld_agg2 <- rbind(rld_agg, emptyfree)
write.csv(rld_agg2, "calculated data/RLD_agg.csv", row.names=FALSE)




##is rootlength density different by soil volume-------------------------------------------------------------------------------

bargraph.CI(volume, rootlengthdensity, data=rootlength)

###this isnt that great because soil volume is unlimited for free
# rootdensity_mod <- lme(rootlengthdensity ~ volume, random= ~1|block/ID, data=rootlength)
# anova(rootdensity_mod)
# summary(rootdensity_mod)
# 
# visreg(rootdensity_mod)
# tukey_rootdens<- glht(rootdensity_mod, linfct = mcp(volume = "Tukey"))
#   siglets_rootdens<-cld(tukey_rootdens)
#   siglets_rootdens2 <- siglets_rootdens$mcletters$Letters

#write.csv(siglets_rootdens2, "master_scripts/sigletters/siglets_root/sl_rld.csv", row.names=FALSE)  


###test this within plots, report this value
rootlength_pot <- rootlength[rootlength$volume != 1000,]
rootlength_pot <- droplevels(rootlength_pot)

rootdensity_mod2 <- lme(rootlengthdensity ~ volume, random= ~1|block/ID, data=rootlength_pot)
anova(rootdensity_mod2)
visreg(rootdensity_mod2)
tukey_density<- glht(rootdensity_mod2, linfct = mcp(volume = "Tukey"))
  siglets_density<-cld(tukey_density)
  siglets_density2 <- siglets_density$mcletters$Letters
  
siglets_density3 <- c(siglets_density2,"")
names(siglets_density3) <- c(5,10,15,20,25,35,1000)
  
##calculate p values for rld with containers, relevel with 35, add empth space for free with saved vector
  
##save in siglets folder but not in roots folder, easier to add to final table
write.csv(siglets_density3, "master_scripts/sigletters/sl_rld.csv", row.names=FALSE) 

###root length density significantly higher in the two smalles containers (5,10)
  
  
###test the free versus largest soil volumes and see if they were still occupying space
# rootlength_big <- rootlength[rootlength$volume == 1000 | rootlength$volume == 35,c(1,9,13,15)]
# rootlength_big <- droplevels(rootlength_big)
# 
# rootlength_big2 <- rootlength_big[rootlength_big$volume == 1000,]
# rootlength_big2$rld_convert <- with(rootlength_big2, (rootlengthdensity2 *1000)/35) 
# rootlength_big3 <- rootlength_big2[,c(1,2,4,5)]
# names(rootlength_big3)[4] <- "rootlengthdensity2"
# 
# rootlength_35 <- rootlength[rootlength$volume == 35,c(1,9,13,15)]
# rootlength_35 <- rootlength_35[,c(1,2,4,3)]
# 
# rootlength_2535 <- rootlength[rootlength$volume == 35 | rootlength$volume == 25,c(1,9,13,15)]
# rootlength_2535 <- rootlength_35[,c(1,2,4,3)]
# 
# test35free <- rbind(rootlength_big3,rootlength_35)
# test2535free <- rbind(rootlength_big3,rootlength_2535)
#   
# rootdensity_mod3 <- lme(rootlengthdensity2 ~ volume, random= ~1|block/ID, data=test35free)
# anova(rootdensity_mod3)
# visreg(rootdensity_mod3)  



###-root length vs leaf area----------------------------------------------------------------------------------------------

rootleaf <- merge(rootlength[, c(1, 7)], leafarea)
  rootleaf$volume <- as.factor(rootleaf$volume)
  rootleaf$block <- as.factor(gsub("-[1-9]", "", rootleaf$ID))


rootleaf_mod <- lm(total_length_cm ~ totalarea, data=rootleaf)
anova(rootleaf_mod)
summary(rootleaf_mod)
library(visreg)
visreg(rootleaf_mod)
rootleaf_mod2 <- lme(total_length_cm ~ totalarea, random= ~1|block/ID, data=rootleaf)
summary(rootleaf_mod2)

library(plotrix)
plot(total_length_cm ~ totalarea, data=rootleaf, pch=pchs[volume], col=volume, cex=1, ylim=c(0, 1000), xlim=c(0, 7000) )
ablineclip(rootleaf_mod, x1=min(rootleaf$totalarea), x2=max(rootleaf$totalarea),2)

###is there a relationship inside plots
rootleaf_mod3 <- lme(total_length_cm ~ totalarea, random= ~1|block/ID, data=rootleaf[rootleaf$volume !=1000,])
summary(rootleaf_mod3)

##only free?
rootleaf_mod4 <- lme(total_length_cm ~ totalarea, random= ~1|block/ID, data=rootleaf[rootleaf$volume ==1000,])
summary(rootleaf_mod4)
visreg(rootleaf_mod4)


