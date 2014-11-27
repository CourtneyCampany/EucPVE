source("functions and packages/startscripts.R")

#read data
wp <- read.csv("raw data/waterpotential.csv")
  wp$Date <- as.Date(wp$Date, format = "%Y/%m/%d")

gasexchange <- read.csv("raw data/PS_waterpotential.csv")
  gasexchange$Date <- as.Date(gasexchange$Date, format = "%m/%d/%Y")

plotsumm <- read.csv("raw data/plot_summary.csv")
  plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")


#WATER POTENTIAL 
wp <- merge(wp, plotsumm)
wp <- subset(wp, !is.na(bar_pre))

  #convert to millipascals
  wp$predawn <- with(wp, ((bar_pre/10)*-1))
  wp$peak <- with(wp, ((bar_peak/10)*-1))

#predawn dfr
wp_pre <- wp[, c(3, 6:8)]
  wp_pre$type <- "predawn"
  names(wp_pre)[4] <- "waterpotential"

#midday dfr
wp_peak <- wp[, c(3, 6:7, 9)]
  wp_peak$type <- "peak"
  names(wp_peak)[4] <- "waterpotential"

waterpotential <- rbind(wp_pre, wp_peak)
  waterpotential$volume <- as.factor(waterpotential$volume)
  row.names(waterpotential) <- NULL

#means
wp_agg <- summaryBy(waterpotential ~ Date+volume+type, data=waterpotential, FUN=mean, keep.names=TRUE)

#PLOT----------------------------------------------------------------------------------
windows()
bargraph.CI(as.factor(volume), waterpotential, group=type,data=waterpotential,
            ylim = c(-2,0), ylab = "Water Potential (mPa)", 
            xlab = "Pot Volume (l)", legend=TRUE, col=c("grey40", "grey60"), x.leg=.1, y.leg=-1.5)

box()
#dev.copy2pdf(file= "output/waterpotential.pdf")
#dev.off()


##stats------------------------------------------------------------------------------
require(nlme)
require(visreg)
library(multcomp)

#pre
pre_lm <- lme(waterpotential ~ volume, random= ~1|ID, data=waterpotential, subset=type=="predawn")
anova(pre_lm)
summary(pre_lm)

tukey_pre<- glht(pre_lm, linfct = mcp(volume = "Tukey"))
cld(pre_lm)
visreg(pre_lm)

peak_lm <- lme(waterpotential ~ volume, random= ~1|ID, data=waterpotential, subset=type=="peak")
anova(peak_lm)
summary(peak_lm)

meanpeak <- mean(wp_peak$waterpotential)


