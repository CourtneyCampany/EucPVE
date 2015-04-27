###interpolate leaf area and number of leaves across all experiment dates
source("functions and packages/functions.R")
library(doBy)

#leaf area across sample dates
leafarea <- read.csv("calculated data/leafareabypot.csv")

#plot summary
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#need to interpolate leaf area in m2 across all dates
leafarea1 <- subset(leafarea, select = c("Date", "ID", "canopysqm"))
#need to interpolate leaf area in m2 across all dates
leafnum <- subset(leafarea, select = c("Date", "ID", "count"))

#FIRST create dfr with all days for interpolation

#empty list
datels <- list()
#date seq loop
datemaker <- for (i in unique(plotsumm$ID)){
  datels[[i]] <- data.frame(Date = seq(from = as.Date("2013-1-21"), to = as.Date("2013-5-21"), by = "day"), 
                            ID = i)} 
# row-bind everything together:
dateseq <- do.call(rbind,datels)

#SECOND merge leaf area and count dataset with dateseq dfr and interpolate
leafarea_alldays <- merge(dateseq, leafarea1, by = c("Date", "ID"), all=TRUE)
leafnum_alldays <- merge(dateseq, leafnum, by = c("Date", "ID"), all=TRUE)


#use interpolate function
LApred_sp<- leafpred_func(leafarea_alldays, "canopysqm")
LA_pred <- do.call(rbind, LApred_sp)
lapred <- subset(LA_pred, select = c("Date", "ID", "canopysqm_pred"))

leafnum_sp<- leafpred_func(leafnum_alldays, "count")
leafnum_pred <- do.call(rbind, leafnum_sp)
countpred <- subset(leafnum_pred, select = c("Date", "ID", "count_pred"))
countpred$count_pred<- round(countpred$count_pred, digits = 0)

#get mean of LA by volume for each day
lapred2 <- merge(lapred, plotsumm)
lapred_agg <- summaryBy(canopysqm_pred~Date+volume, data=lapred2, FUN=mean, keep.names=TRUE)

write.csv(lapred, "calculated data/LApred.csv", row.names=FALSE)
write.csv(countpred, "calculated data/L#pred.csv", row.names=FALSE)
write.csv(lapred_agg, "calculated data/LApred_volume.csv", row.names=FALSE)
