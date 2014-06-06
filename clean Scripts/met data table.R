require(lubridate)
require(doBy)
require(data.table)
library(gridExtra)

eucpve_met<- read.csv("calculated data/eucpve_met.csv")
eucpve_met$DateTime15<- ymd_hms(eucpve_met$DateTime15)
eucpve_met$month <- month(eucpve_met$DateTime15)
names(eucpve_met)[3:5]<-c("Par", "Temperature", "RH")

met_agg <- summaryBy(Par+Temperature+RH ~ month, data=eucpve_met, FUN=mean, keep.names=TRUE)
met_agg$month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
met_agg<- met_agg[1:5,]

met_table <- as.table <- met_agg

pdf(file = "output/temp1.pdf")
capture.output(met_table, file= "output/temp1.txt")


write.csv(met_table, "calculated data/met data.csv", row.names=FALSE)


EmptyLine <- data.frame(month = "",Par = "",Temperature = "", RH = "")
Eqmonthdf <- as.data.frame(met_agg[1,])

pdf(file = "output/temp.pdf")

for (i in 2:nrow(met_agg)) 
{
  if (as.vector(met_agg$month[i])  ==  as.vector(met_agg$month[i-1])) 
  {Eqmonthdf <- rbind(Eqmonthdf, met_agg[i,])}
  
  else {
    Eqmonthdf <- rbind(Eqmonthdf, EmptyLine)
    Eqmonthdf <- rbind(Eqmonthdf, met_agg[i,]) 
  }
}

grid.table(Eqmonthdf, show.rownames = FALSE)
dev.off()
