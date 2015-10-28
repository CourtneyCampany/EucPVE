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


leafd2h_mod <- lm(log10(leafmass) ~ log10(d2h), data=allom)
anova(leafd2h_mod)
summary(leafd2h_mod)
library(visreg)
visreg(leafd2h_mod)


##plot leafmass * d2h
library(magicaxis)
library(plotrix)

windows(7,7)
plot(log10(leafmass) ~ log10(d2h), data=allom, pch=pchs[volume], col=volume, cex=1.5, axes=FALSE)
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
box()
ablineclip(leafd2h_mod, x1=min(log10(allom$d2h)), x2=max(log10(allom$d2h)),lwd=2)

dev.copy2pdf(file= "master_scripts/manuscript_figs/leafmassd2h.pdf")
dev.off()



