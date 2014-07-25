source("functions and packages/startscripts.R")

#read  leaf data from harvest and surveys
source("read data scripts/harvest read data.R")
source("read data scripts/survey read data.R")

leafharvest <- subset(leafharvest, !is.na(leaf_area))
leafharvest$areaperleaf <- with(leafharvest, leaf_area/leaf_count)
leafharvest$totalarea <- with(leafharvest, leaf_area + newleaf_area)
leafharvest$totalcount <- with(leafharvest, leaf_count + newleaf_count)

#merge with plot summary
leafharvest <- merge(leafharvest, plotsumm, by = c("pot", "plot", "ID"))
leafharvest$volume <- as.factor(leafharvest$volume)

write.csv(leafharvest[,c(3, 13:15)], "calculated data/LA_harvest.csv", row.names=FALSE)
#treatment means for total area and count
leafharvest_agg <- aggregate(cbind(totalarea, totalcount) ~ volume , data = leafharvest, FUN = mean)

#calculate leaf area trough time
leafindex <- subset(leafharvest, select = c("ID", "areaperleaf", "volume"))

#format leaf count data
leafcount$X1.21.2013   <- as.integer(leafcount$X1.21.2013)
leafcount <- melt(leafcount, id=c("plot", "pot"))
names(leafcount)[3:4] <- c("Date", "count")
leafcount$Date <- gsub ("X", "", leafcount$Date)
leafcount$Date <- as.Date(leafcount$Date, format = "%m.%d.%Y")
leafcount <- subset(leafcount, !is.na(count))
leafcount$ID <- paste(leafcount$plot, leafcount$pot, sep = "-")

leaftime <- merge(leafindex, leafcount, by = "ID")

#calculate cumulative leaf area through time
leaftime$canopyarea <- with(leaftime, areaperleaf * count)
leaftime$canopysqm <- with(leaftime, areaperleaf * count/10000)

write.csv(leaftime, "calculated data/leafareabypot.csv", row.names=FALSE)

#treatment means for total area and count
leaftime_agg <- summaryBy(canopysqm ~ Date + volume , data = leaftime,  FUN=c(mean,se))

write.csv(leaftime_agg, "calculated data/cumulative leaf area.csv", row.names=FALSE)
leaf2 <- subset(leaftime_agg,volume != "1000" )

#leaftime_agg$canopysqm <- leaftime_agg$canopyarea / 10000
#----------------------------------------------------------------------------------------------------
#stats
require(nlme)
lmeLA <- lme(canopysqm ~ volume, random= ~1|ID, data=leaftime)
anova(lmeLA)

lmeLA2 <- lme(canopysqm ~ volume, random= ~1|ID, data=leaftime, subset=volume != "1000")
anova(lmeLA2)

#plot---------------------------------------------------------------------------

#windows(11,8)
png(filename = "output/png/leafarea.png", width = 11, height = 8, units = "in", res= 400)
par(cex.axis=1.5,  cex.lab=1.5)
#png(filename = "output/png/leafarea.png", width = 8, height = 11.5, units = "in", res= 400)
#windows()
plot(canopysqm.mean ~ Date, data=leaftime_agg,xlab="", ylab="", ylim=c(0,.6), type='n')
title(ylab=leaf, mgp=ypos)
with(leaftime_agg, arrows(Date, canopysqm.mean, Date, canopysqm.mean+SE, angle=90, col=palette(),length=0.03))
with(leaftime_agg, arrows(Date, canopysqm.mean, Date, canopysqm.mean-SE, angle=90, col=palette(),length=0.03))
d_ply(leaftime_agg, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                                                  col=x$volume, type="b", pch = pchs[x$volume], cex=1.3,))
legend("topleft", leglab, pch=pchs,text.font=3, inset=0.02, title=expression(Pot~volume~(l)),
       col=palette(),cex=1.5, bty='n')
#dev.copy2pdf(file= "output/canopyleafarea.pdf")
dev.off()

