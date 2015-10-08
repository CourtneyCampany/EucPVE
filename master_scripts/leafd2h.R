###leaf mass vs d2h

source("functions and packages/startscripts.R")

leaf <- read.csv("calculated data/seedling mass.csv")   


##get last value for dimeter and height
height <- read.csv("calculated data/height.csv")
  height$Date <- as.Date(height$Date)

diam <- read.csv("calculated data/diameter.csv")
  diam$Date <- as.Date(diam$Date)

height2 <- height[height$Date == max(height$Date),]
diam2 <- diam[diam$Date == max(diam$Date),]


##merge everything and calculate d2h
allom <- merge(diam2, height2)
allom <- merge(allom, leaf[, c(1:2,5)])

allom$d2h <- with(allom, diameter^2 * height)
allom$volume <- as.factor(allom$volume)


##plot leafmass * d2h

plot(leafmass ~ d2h, data=allom, pch=pchs[volume], col=volume, cex=1.5)
