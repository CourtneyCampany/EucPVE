
source("functions and packages/load packages.R")
source("functions and packages/functions.R")
require(broom)
require(visreg)

#Read in R dark spot measurements,  plot design
source("read data scripts/physiology read data.R")
leafmass <- read.csv("raw data/seedling leaf mass area.csv")

rdark <- merge(plotsumm, subset(resp_dark, select = c("Date", "plot", "pot", "Photo", "Cond", 
                                                      "Ci", "CTleaf", "Area", "Trmmol")))

boxplot(Photo~ volume, data=rdark)

rdark_clean <- rdark[rdark$Photo >= -.5,]

boxplot(Photo~ volume, data=rdark_clean)

#---------------------------------------------------------------------------------------------
###need to calculate a q10 function to model Rdark through time
##use rdark at 25c for upper end, average my data by volume

q25_drake <- 1.86
q10_crous <- 1.95

#pot volume means
rdark_agg <- aggregate(cbind(Photo, Cond, Ci, Trmmol, CTleaf) ~  volume, data=rdark_clean, FUN=mean)
rdark_agg$Photo <- -1*rdark_agg$Photo

rdark_eq <- cbind(subset(rdark_agg, select = c("volume", "Photo", "CTleaf")), q25_drake)
rdark_eq <- cbind(rdark_eq, q10_crous)
mean(rdark_eq$CTleaf)
names(rdark_eq)[2] <- "rd12.3"

#q10 equation 
rdark_eq$rd25_euct <- with(rdark_eq, rd12.3*(q25_drake^((abs(CTleaf-25))/10)))
rdark_eq$rd25_eucs <- with(rdark_eq, rd12.3*(q10_crous^((abs(CTleaf-25))/10)))

#--------------------------------------------------------------------------------------------

#calculate Respiration/Leaf area
lma <- subset(leafmass, select=c("pot","plot","campaign","mass"))
lma <- subset(lma, campaign =="6")
#format data
lma <- subset(lma, !is.na(mass))
lma$ID <- paste(lma$plot, lma$pot, sep = "-")
#merge
rdark2 <- merge(lma, rdark_clean, by=c("plot", "pot", "ID"))

  rdark2$rd25_eucs <- with(rdark2, abs(Photo*(q25_drake^((abs(CTleaf-25))/10))))
  #new variable
  rdark2$resppermass <- with(rdark2, abs(Photo/mass))
  rdark2$resppermass25 <- with(rdark2, abs(rd25_eucs/mass))

rdark3 <- summaryBy(. ~ ID, FUN=mean, na.rm=TRUE, data=rdark2, id=~volume, keep.names=TRUE)

#remove outliers
boxplot(mass~ volume, data=rdark3)

rdark4 <- rdark3[rdark3$ID != c("2-4", "6-6"), ]
rdark5 <- rdark4[rdark4$resppermass25 <= 5.1,]
rdark5$rd <- with(rdark5, -1*Photo)

boxplot(mass~ volume, data=rdark5)
boxplot(resppermass25~ volume, data=rdark5)
boxplot(resppermass~ volume, data=rdark5)

rdark5_agg <- summaryBy(rd25_eucs+resppermass25~volume, data=rdark5, FUN=c(mean, se), keep.names=TRUE)

##write clean data for manuscript
write.csv(rdark5_agg, "calculated data/rdark_clean.csv", row.names=FALSE)


#PLOTTING
with(rdark5, bargraph.CI(as.factor(volume), rd25_eucs, 
                         border="blue",
                         ylim = c(0,1), ylab = "Dark Respiration", 
                         xlab = "Pot Volume (l)"))
box()


with(rdark5, bargraph.CI(as.factor(volume), resppermass, 
                         border="blue",
                         ylim = c(0,2.5), ylab = "Rdark per Leaf Mass", 
                         xlab = "Pot Volume (l)"))
box()


with(rdark5, bargraph.CI(as.factor(volume), resppermass25, 
                         border="blue",
                         ylim = c(0, 5), ylab = "Rdark per Leaf Mass", 
                         xlab = "Pot Volume (l)"))
box()


####stats--------------------------------------------------------------------------------------------------
library(multcomp)

#Rd q10 to 25 with tereticornis
# 'Being in a pot' effect.
rdark5$volume <- as.factor(rdark5$volume)
rdark5$volume <- relevel(rdark5$volume, ref="1000")
rdark5$block <- as.factor(gsub("-[1-9]", "", rdark5$ID))

rd25_container <- lme(rd25_eucs ~ volume, random= ~1|ID, data=rdark5)
anova(rd25_container)
summary(rd25_container)

rd25_container2 <- lme(rd25_eucs ~ volume, random= ~1|block/ID, data=rdark5)
anova(rd25_container2)

tukey_rd25<- glht(rd25_container2, linfct = mcp(volume = "Tukey"))
rd_siglets <-cld(tukey_rd25)
rd_siglets2 <- rd_siglets$mcletters$Letters

write.csv(rd_siglets2, "master_scripts/sigletters/sigletts_phys/sl_rd.csv", row.names=FALSE)

####no container effect (relevel to 1000 and no volumes differ)


#volume effect
rd25_volume <- lm(Photo ~ volume, data=rdark5, subset=volume != "1000")
rd25v_lm <- tidy(rd25_volume)
rd25v_stat <- extract_func(rd25_volume)

rd25_volume2 <- lm(Photo ~ as.numeric(volume), data=rdark5, subset=volume != "1000")

anova(rd25_volume)
summary(rd25_volume)
summary(rd25_volume2)


require(nlme)
require(visreg)
library(multcomp)

#asat
asat_lm <- lme(Photo ~ volume, random= ~1|ID, data=PSsat_spot)
#   anova(asat_lm)
#   summary(asat_lm)



