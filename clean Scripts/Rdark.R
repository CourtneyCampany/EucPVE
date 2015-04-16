
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
rdark_agg <- aggregate(cbind(Photo, Cond, Ci, Trmmol, CTleaf) ~  volume, data=rdark, FUN=mean)
rdark_agg$Photo <- -1*rdark_agg$Photo

rdark_eq <- cbind(subset(rdark_agg, select = c("volume", "Photo", "CTleaf")), rd25)
rdark_eq <- cbind(rdark_eq, q10_crous)
mean(rdark_eq$CTleaf)
names(rdark_eq)[2] <- "rd12.3"

#q10 equation 
rdark_eq$rd25_euct <- with(rdark_eq, (rd25/rd12.3)^(10/(25-CTleaf)))
rdark_eq$rd25_eucs <- with(rdark_eq, (q10_crous/rd12.3)^(10/(25-CTleaf)))

write.csv(rdark_eq[,c(1:2, 6:7)], "calculated data/rdarkq10.csv", row.names=FALSE)

#--------------------------------------------------------------------------------------------

#calculate Respiration/Leaf area
lma <- subset(leafmass, select=c("pot","plot","campaign","mass"))
lma <- subset(lma, campaign =="6")
#format data
lma <- subset(lma, !is.na(mass))
lma$ID <- paste(lma$plot, lma$pot, sep = "-")
#merge
rdark2 <- merge(lma, rdark, by=c("plot", "pot", "ID"))
rdark2$rd25_eucs <- with(rdark, (1.86/(-1*Photo))^(10/(25-CTleaf)))

#new variable
rdark2$resppermass <- with(rdark2, Photo/mass)
rdark2$resppermass25 <- with(rdark2, rd25_eucs/mass)

write.csv(rdark2, "calculated data/Rd_leaf.csv")

#PLOTTING
with(rdark2, bargraph.CI(as.factor(volume), Photo, 
                        border="blue",
                        ylim = c(-1,0), ylab = "Dark Respiration", 
                        xlab = "Pot Volume (l)"))
box()
dev.copy2pdf(file= "output/Rdark.pdf")

with(rdark2, bargraph.CI(as.factor(volume), resppermass, 
                        border="blue",
                        ylim = c(-3.5,0), ylab = "Rdark per Leaf Mass", 
                        xlab = "Pot Volume (l)"))
box()
dev.copy2pdf(file= "output/Rdark_per_leafmass.pdf")

with(rdark2, bargraph.CI(as.factor(volume), resppermass25, 
                        border="blue",
                        ylim = c(0, 40), ylab = "Rdark per Leaf Mass", 
                        xlab = "Pot Volume (l)"))
box()
dev.copy2pdf(file= "output/Rdark_per_leafmass25.pdf")

s