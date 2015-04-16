
source("functions and packages/load packages.R")
source("functions and packages/functions.R")
require(broom)
require(visreg)

#Read in R dark spot measurements,  plot design
source("read data scripts/physiology read data.R")
leafmass <- read.csv("raw data/seedling leaf mass area.csv")

rdark <- merge(plotsumm, subset(resp_dark, select = c("Date", "plot", "pot", "Photo", "Cond", 
                                                      "Ci", "CTleaf", "Area", "Trmmol")))
rdark_clean <- rdark[rdark$Photo >= -.5,]


#---------------------------------------------------------------------------------------------
###need to calculate a q10 function to model Rdark through time
##use rdark at 25c for upper end, average my data by volume

rd25 <- 1.86
q10_crous <- 1.95

#pot volume means
rdark_agg <- aggregate(cbind(Photo, Cond, Ci, Trmmol, CTleaf) ~  volume, data=rdark_clean, FUN=mean)
rdark_agg$Photo <- -1*rdark_agg$Photo

rdark_eq <- cbind(subset(rdark_agg, select = c("volume", "Photo", "CTleaf")), rd25)
rdark_eq <- cbind(rdark_eq, q10_crous)
mean(rdark_eq$CTleaf)
names(rdark_eq)[2] <- "rd12.3"

#q10 equation 
rdark_eq$rd25_euct <- with(rdark_eq, (rd25/rd12.3)^(10/(25-CTleaf)))
rdark_eq$rd25_eucs <- with(rdark_eq, (q10_crous/rd12.3)^(10/(25-CTleaf)))

#--------------------------------------------------------------------------------------------

#calculate Respiration/Leaf area
lma <- subset(leafmass, select=c("pot","plot","campaign","mass"))
lma <- subset(lma, campaign =="6")
#format data
lma <- subset(lma, !is.na(mass))
lma$ID <- paste(lma$plot, lma$pot, sep = "-")
#merge
rdark2 <- merge(lma, rdark_clean, by=c("plot", "pot", "ID"))
rdark2$rd25_eucs <- with(rdark2, (1.86/(-1*Photo))^(10/(25-CTleaf)))

#new variable
rdark2$resppermass <- with(rdark2, Photo/mass)
rdark2$resppermass25 <- with(rdark2, rd25_eucs/mass)



#PLOTTING
with(rdark2, bargraph.CI(as.factor(volume), rd25_eucs, 
                         border="blue",
                         ylim = c(0,7.5), ylab = "Dark Respiration", 
                         xlab = "Pot Volume (l)"))
box()


with(rdark2, bargraph.CI(as.factor(volume), resppermass, 
                         border="blue",
                         ylim = c(-3.5,0), ylab = "Rdark per Leaf Mass", 
                         xlab = "Pot Volume (l)"))
box()
)

with(rdark2, bargraph.CI(as.factor(volume), resppermass25, 
                         border="blue",
                         ylim = c(0, 40), ylab = "Rdark per Leaf Mass", 
                         xlab = "Pot Volume (l)"))
box()




#area Rd
# 'Being in a pot' effect.
rdark2$volume <- as.factor(rdark2$volume)
rdark2$volume <- relevel(rdark2$volume, ref="1000")

rda_container <- lm(rd25_eucs ~ volume, data=rdark2)
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
