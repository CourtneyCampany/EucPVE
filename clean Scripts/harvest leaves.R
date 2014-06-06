source("functions and packages/load packages.R")

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
leaftime_agg <- summaryBy(canopysqm ~ Date + volume , data = leaftime,  FUN=c(mean,sd,length))
leaftime_agg$SE <- with(leaftime_agg, canopysqm.sd/sqrt(canopysqm.length))

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

#----------------------------------------------------------------------------------------------------
#plot bits

#color scheme
gradient <- colorRampPalette(c("red", "blue"))
color <- palette(gradient(7))
pchs = c(rep(16,6),17)
leglab <- c(5, 10, 15, 20, 25, 35, "free")
leglab1 <- c(5, 10, 15, 20, 25, 35)
leaf <- expression(Seedling~Leaf~Area~~(m^2))
#----------------------------------------------------------------------------------------------------

windows(12,10)
par(mfrow=c(2,1), omi=c(1,0,0.5,0.5),mar=c(0,5,0,0),mgp = c(2.5, 1, 0))

#par(oma = c(0, 1, 0, 0), mgp = c(2.5, 1, 0), cex.axis=1.0, cex.lab=1.3)

plot(canopysqm.mean ~ Date, data=leaftime_agg, axes=FALSE,xlab="", ylab="",
     type='n', ylim=c(0,.6))
box()
axis(2, labels=TRUE)  
title(ylab=leaf)
with(leaftime_agg, arrows(Date, canopysqm.mean, Date, canopysqm.mean+SE, angle=90, col=palette(),length=0.03))
with(leaftime_agg, arrows(Date, canopysqm.mean, Date, canopysqm.mean-SE, angle=90, col=palette(),length=0.03))
d_ply(leaftime_agg, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                                                  col=x$volume, type="b", pch = pchs[x$volume]))
legend("topleft", leglab, pch=pchs,text.font=3, inset=0.02, title=expression(Pot~volume~(l)), col=palette(), bty='n')
#dev.copy2pdf(file= "output/canopyleafarea.pdf")
#no free
plot(canopysqm.mean ~ Date, data=leaf2, xlab="", ylab=leaf,
      ylim=c(0,.2))
for(i in 1:7){
  dat <- subset(leaf2, volume==levels(volume)[i])
  with(dat, arrows(Date, canopysqm.mean, Date, canopysqm.mean+SE, angle=90, length=0.03, col=palette()[i], lwd=2))
  with(dat, arrows(Date, canopysqm.mean, Date, canopysqm.mean-SE, angle=90, length=0.03, col=palette()[i], lwd=2))
  with(dat,points(Date, canopysqm.mean, type='b', lwd=2, col=palette()[i],  pch=16))  
}
title(main="No Free", line=-1.5, font.main=1)
legend("topleft", legend, leglab1, pch=16,text.font=3, inset=0.02, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n')

#dev.copy2pdf(file= "output/canopyleafareapots.pdf")
dev.copy2pdf(file= "output/leafarea_2panel.pdf")
dev.off()

