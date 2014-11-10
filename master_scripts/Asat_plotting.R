#source functions, packages, anbd plot objects
source("functions and packages/startscripts.R")

#Read in spot A measurements and merge plot design
#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
  plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#read in gas exchange master file, all dates
gasexchange <- read.csv("raw data/AsatAmax_master.csv")
  gasexchange$Date <- as.Date(gasexchange$Date)
  gasexchange$ID <- paste(gasexchange$plot, gasexchange$pot, sep = "-")

PS <- merge(plotsumm, subset(gasexchange, select = c("campaign", "ID", "CO2", "Photo")))
  PS$type <- factor(ifelse (PS$CO2 == "400", "Asat", "Amax"))

#run function to add campaign date
PS <- add_campaign_date(PS)

#Asat----------------------------------------------------------------------------------------------
PSsat <- subset(PS, type=="Asat")

#mean of 5 logs per plant
PSsat_spot<- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSsat)

#mean by plant over all dates, then treatment means
PSsat_ID <- summaryBy(. ~ ID, FUN=mean, keep.names=TRUE, data=PSsat_spot)
PSsat_ID$volume <- as.factor(PSsat_ID$volume)

###barplot with asat values, drop colors for now ( col=palette(),)
bar(Photo, volume, PSsat_ID,half.errbar=FALSE, xlab="Soil Volume  (L)",ylab="", ylim=c(0,25), names.arg = leglab,
    col="grey", legend=FALSE)
title(ylab=satlab, mgp=ypos)
