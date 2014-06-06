library(reshape)
library(RColorBrewer)
library(doBy)
library(sciplot)

#read in plot design
plotsumm <- read.csv("plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#HEIGHT
height <- read.csv("seedling height.csv")

#format data
height <- melt(height, id=c("plot", "pot"))
height$variable <- gsub ("X", "", height$variable)
names(height)[3:4] <- c("Date", "height")
height$Date <- as.Date(height$Date, format = "%m.%d.%Y")
height <- subset(height, !is.na(height))
height$ID <- paste(height$plot, height$pot, sep = "-")

#merge with plot summary
height <- merge(height, plotsumm, by = c("pot", "plot", "ID"))
height$volume <- as.factor(height$volume)

#order date for clarity
dateorder<-order(height$ID, by=height$Date)
height <- height[dateorder,]


allNheight<-list()

#make a loop to caluclate normailized height
for (i in unique(height$ID)) {
  treeid <- subset(height, ID == i)  
  timax <- max(treeid$height)
  Nheight <- treeid$height/timax
allNheight[[i]] <- Nheight
}

normalH <- do.call(cbind, allNheight)

#transpose normalH so I can merge them with height
normalH <- melt(normalH, id=c("ID", "Nheight"))
names(normalH)[2:3] <- c("ID", "Nheight")

#add new variable to the height dfr, should be in correct date order
heightstop <- cbind(height, normalH[3])


###now I have the  normalized height
###approx to determine when 95% growth stopped

#first I create a column for the x axis that has the lenght of the experiment in days
heightstop$durationday <- with(heightstop, Date - min(Date))
heightstop$durationday <- as.numeric(heightstop$durationday)


stopgrowth <-list()

for (i in unique(heightstop$ID)) {
  treeid <- subset(heightstop, ID == i)
  growthdone <- approx(treeid$Nheight, treeid$durationday, xout=0.95)
  stopgrowth[[i]] <- growthdone
  
}

datestop <- do.call(rbind, stopgrowth)
datestop <- as.data.frame(datestop)

datestop <- cbind(datestop[2], plotsumm)
names(datestop)[1] <- "ceasegrow"
datestop$ceasegrow <- as.numeric(datestop$ceasegrow)


#volume means
datestop_agg <- summaryBy(ceasegrow ~ volume, data = datestop,  FUN=c(mean, sd, length))
datestop_agg$ceasegrow_SE <- with(datestop_agg, ceasegrow.sd/sqrt(ceasegrow.length))

#plotting
with(datestop, bargraph.CI(as.factor(volume), ceasegrow))

#stats
getP <- function(x)anova(x)[[5]][1]

datestop_pot <- lm(ceasegrow ~ as.factor(volume), data=datestop)
datestop_potsize <- lm(ceasegrow ~ as.factor(volume), data=datestop, subset=volume != "1000")

getP(datestop_pot)
getP(datestop_potsize)

