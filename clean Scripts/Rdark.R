
source("functions and packages/load packages.R")

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

with(rdark, bargraph.CI(as.factor(volume),  Ci, 
                        border="blue",
                        ylim = c(0,750), ylab = "Rdark Ci", 
                        xlab = "Pot Volume (l)"))
box()
dev.copy2pdf(file= "output/Rdark_Ci.pdf")


with(rdark, bargraph.CI(as.factor(volume),  Trmmol, 
                        border="blue",
                        ylim = c(0,0.15), ylab = "Dark Transpiration", 
                        xlab = "Pot Volume (l)"))
box()
dev.copy2pdf(file= "output/Rdark_transpiration.pdf")
