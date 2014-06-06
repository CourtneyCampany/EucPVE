library(reshape)
library(RColorBrewer)
library(doBy)
library(sciplot)

#read in plot design
plotsumm <- read.csv("plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#LEAF COUNT
leafno <- read.csv("seedling leaf count.csv")

#format data
leafno$X1.21.2013   <- as.integer(leafno$X1.21.2013)
leafno <- melt(leafno, id=c("plot", "pot"))
names(leafno)[3:4] <- c("Date", "count")
leafno$Date <- gsub ("X", "", leafno$Date)
leafno$Date <- as.Date(leafno$Date, format = "%m.%d.%Y")
leafno <- subset(leafno, !is.na(count))
leafno$ID <- paste(leafno$plot, leafno$pot, sep = "-")
leafno$count <- as.numeric(leafno$count)

#merge with plot summary and sort
leafno <- merge(leafno, plotsumm, by = c("pot", "plot", "ID"))
leafno$volume <- as.factor(leafno$volume)

#order date for clarity
dateorder<-order(leafno$ID, by=leafno$Date)
leafno <- leafno[dateorder,]

allNleafno<-list()

#make a loop to caluclate normailized height
for (i in unique(leafno$ID)) {
  treeid <- subset(leafno, ID == i)  
  timax <- max(treeid$count)
  Nleaf <- treeid$count/timax
  allNleafno[[i]] <- Nleaf
}

normalH <- do.call(cbind, allNleafno)







