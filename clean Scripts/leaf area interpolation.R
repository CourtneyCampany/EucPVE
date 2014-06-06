#this script reads in a leaf area calculated from another script

#leaf area across sample dates
leafarea <- read.csv("calculated data/leafareabypot.csv")

#plot summary
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#need to interpolate leaf area in m2 across all dates
leafarea1 <- subset(leafarea, select = c("Date", "ID", "canopysqm"))


#FIRST create dfr with all days for interpolation

#empty list
datels <- list()
#date seq loop
datemaker <- for (i in unique(plotsumm$ID)){
  datels[[i]] <- data.frame(Date = seq(from = as.Date("2013-1-21"), to = as.Date("2013-5-21"), by = "day"), 
                            ID = i)} 

# row-bind everything together:
dateseq <- do.call(rbind,datels)

#SECOND merge leaf area dataset with dateseq dfr and interpolate
leafarea_alldays <- merge(dateseq, leafarea1, by = c("Date", "ID"), all=TRUE)


#use dateseq for interpolation
leafarea_sp <- split(leafarea_alldays, leafarea_alldays$ID)
leafarea_sp <- lapply(leafarea_sp, function(z){
  
  apfun_LA <- approxfun(x=z$Date, y=z$canopysqm)
  
  z$LA_pred <- apfun_LA(z$Date)
  
  
  return(z)
})
leafarea_pred <- do.call(rbind, leafarea_sp)

leafpred <- subset(leafarea_pred, select = c("Date", "ID", "LA_pred"))

write.csv(leafpred, "calculated data/LApredbypot.csv", row.names=FALSE)
