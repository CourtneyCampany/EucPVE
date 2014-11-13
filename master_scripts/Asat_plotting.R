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


#Amax_means for Data table-----------------------------------------------------------------------
PSmax <- subset(PS, type=="Amax")

#mean of 5 logs per plant
PSmax_spot<- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSmax)

#mean by plant over all dates, then treatment means
PSmax_ID <- summaryBy(Photo+volume ~ ID, FUN=mean, keep.names=TRUE, data=PSmax_spot)
  PSmax_ID$volume <- as.factor(PSmax_ID$volume)


#Asat----------------------------------------------------------------------------------------------
PSsat <- subset(PS, type=="Asat")

#mean of 5 logs per plant
PSsat_spot<- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSsat)

#mean by plant over all dates, then treatment means
PSsat_ID <- summaryBy(Photo+volume ~ ID, FUN=mean, keep.names=TRUE, data=PSsat_spot)
  PSsat_ID$volume <- as.factor(PSsat_ID$volume)


##PLOTTING

###barplot with asat values, drop colors for now ( col=palette(),)
bar(Photo, volume, PSsat_ID,half.errbar=FALSE, xlab="Soil Volume  (L)",ylab="", ylim=c(0,25), names.arg = leglab,
    col="grey", legend=FALSE)
    title(ylab=satlab, mgp=ypos)



##Photosynthesis means for paper table---------------------------------------------------------------------------

##overall means
PSmax_mean <- summaryBy(Photo ~ volume, data= PSmax_ID, FUN=c(mean, se))
  names(PSmax_mean)[2:3]<- c("Amax", "Amax_se")
PSsat_mean <- summaryBy(Photo~volume, data=PSsat_ID, FUN=c(mean,se))
  names(PSsat_mean)[2:3]<- c("Asat", "Asat_se")

A_means <- merge(PSmax_mean, PSsat_mean)
write.csv(A_means, "calculated data/A_treatment_means.csv", row.names=FALSE)

