
source("functions and packages/load packages.R")
source("functions and packages/functions.R")
require(broom)
require(visreg)

#Read in R dark spot measurements,  plot design
source("read data scripts/physiology read data.R")
leafmass <- read.csv("raw data/seedling leaf mass area.csv")

rdark <- merge(plotsumm, subset(resp_dark, select = c("Date", "plot", "pot", "Photo", "Cond", 
                                                  "Ci", "CTleaf", "Area", "Trmmol")))
#---------------------------------------------------------------------------------------------
###need to calculate a q10 function to model Rdark through time
##use rdark at 25c for upper end, average my data by volume

rd25 <- 1.86

#pot volume means
rdark_agg <- aggregate(cbind(Photo, Cond, Ci, Trmmol, CTleaf) ~  volume, data=rdark, FUN=mean)
rdark_agg$Photo <- -1*rdark_agg$Photo

rdark_eq <- cbind(subset(rdark_agg, select = c("volume", "Photo", "CTleaf")), rd25)
mean(rdark_eq$CTleaf)
names(rdark_eq)[2] <- "rd12.3"

#q10 equation 
rdark_eq$q10 <- with(rdark_eq, (rd25/rd12.3)^(10/(25-CTleaf)))


write.csv(subset(rdark_eq, select = c("volume","q10", "rd12.3")), "calculated data/rdarkq10.csv", row.names=FALSE)

#--------------------------------------------------------------------------------------------

#calculate Respiration/Leaf area
lma <- subset(leafmass, select=c("pot","plot","campaign","mass"))
lma <- subset(lma, campaign =="6")
#format data
lma <- subset(lma, !is.na(mass))
lma$ID <- paste(lma$plot, lma$pot, sep = "-")
#merge
rdark <- merge(lma, rdark, by=c("plot", "pot", "ID"))
#new variable
rdark$resppermass <- with(rdark, Photo/mass)

write.csv(rdark, "calculated data/Rd_leaf.csv")

#PLOTTING
with(rdark, bargraph.CI(as.factor(volume), Photo, 
                        border="blue",
                        ylim = c(-1,0), ylab = "Dark Respiration", 
                        xlab = "Pot Volume (l)"))
box()
dev.copy2pdf(file= "output/Rdark.pdf")

with(rdark, bargraph.CI(as.factor(volume), resppermass, 
                        border="blue",
                        ylim = c(-3.5,0), ylab = "Rdark per Leaf Mass", 
                        xlab = "Pot Volume (l)"))
box()
dev.copy2pdf(file= "output/Rdark_per_leafmass.pdf")

#mass Rd
# 'Being in a pot' effect.
rd_container <- lm(resppermass ~ as.factor(volume), data=rdark)
rd_lm <- tidy(rd_container)
rd_stat <- extract_func(rd_container)

anova(rd_container)
summary(rd_container)

#volume effect
rd_volume <- lm(resppermass ~ as.factor(volume), data=rdark, subset=volume != "1000")
rd2_lm <- tidy(rd_volume)
rd2_stat <- extract_func(rd_volume)

anova(rd_volume)
summary(rd_volume)

#area Rd
# 'Being in a pot' effect.
rda_container <- lm(Photo ~ as.factor(volume), data=rdark)
rda_lm <- tidy(rda_container)
rda_stat <- extract_func(rda_container)

anova(rda_container)
summary(rda_container)

#volume effect
rda_volume <- lm(Photo ~ as.factor(volume), data=rdark, subset=volume != "1000")
rd2a_lm <- tidy(rda_volume)
rd2a_stat <- extract_func(rda_volume)

anova(rda_volume)
summary(rda_volume)
