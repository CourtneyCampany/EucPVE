source("functions and packages/startscripts.R")

#read  leaf data from harvest and surveys
source("read data scripts/harvest read data.R")
source("read data scripts/survey read data.R")

leafharvest <- subset(leafharvest, !is.na(leaf_area))
  leafharvest$areaperleaf <- with(leafharvest, leaf_area/leaf_count)
  leafharvest$totalarea <- with(leafharvest, leaf_area + newleaf_area)
  leafharvest$totalcount <- with(leafharvest, leaf_count + newleaf_count)
#merge with plot summary
leafharvest <- merge(leafharvest, plotsumm[,3:4], by ="ID")
  leafharvest$volume <- as.factor(leafharvest$volume)

#harvest leaf length
ll_harvest <- read.csv("raw data/harvest leaf length.csv")
  ll_harvest <- melt(ll_harvest, id=c("pot", "plot"))
  ll_harvest$ID <- with(ll_harvest, paste(plot, pot, sep="-"))
  names(ll_harvest)[4]<- "leaf_length"
ll_agg <- aggregate(leaf_length ~ ID, ll_harvest, FUN=mean,  na.action = na.omit)

####merge leafharvest with leaf length mean
leafharvest <- merge(leafharvest, ll_agg, by ="ID")
  row.names(leafharvest) <- NULL

write.csv(leafharvest[,c(1, 13:15)], "calculated data/LA_harvest.csv", row.names=FALSE)

#treatment means for total area and count
leafharvest_agg <- aggregate(cbind(totalarea, totalcount) ~ volume , data = leafharvest, FUN = mean)


####relationship between leaf length and area-------------------------------------------------------------------
area_length <- leafharvest[,c(1, 12, 15:16)]
  area_length$ratio <- with(area_length, areaperleaf/leaf_length)

ll_mod <- lm(areaperleaf~leaf_length, data=area_length)
  extract_func(ll_mod)
ll_mod_1000<- lm(areaperleaf~leaf_length, data=area_length, subset=volume==1000)
extract_func(ll_mod_1000)

ll_mod2 <- lm(ratio~volume, data=area_length)
  anova(ll_mod2)
  summary(ll_mod2)
  library(visreg)
  visreg(ll_mod2)

plot(areaperleaf~leaf_length, ylim=c(0,40), xlim=c(0,15),col=volume,pch=16,data=area_length, subset=volume==1000)
abline(ll_mod)
####half the volumes have a pretty bad relationship, so continue using mean leafarea and leaf count 


#calculate leaf area trough time------------------------------------------------------------------------------
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
#png(filename = "output/presentations/leafarea.png", width = 12, height = 8, units = "in", res= 400)
par(cex.axis=1.3,  cex.lab=1.3)
plot(canopysqm.mean ~ Date, data=leaftime_agg,xlab="", ylab="", ylim=c(0,.6), type='n')
title(ylab=leaflab, mgp=ypos)
with(leaftime_agg, arrows(Date, canopysqm.mean, Date, canopysqm.mean+canopysqm.se, angle=90, col=palette(),length=0.03))
with(leaftime_agg, arrows(Date, canopysqm.mean, Date, canopysqm.mean-canopysqm.se, angle=90, col=palette(),length=0.03))
d_ply(leaftime_agg, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                                                  col=x$volume, type="b", pch = pchs[x$volume], cex=1.3,))
legend("topleft", leglab, pch=pchs,text.font=1.3, inset=0.02, title=expression(Pot~volume~(l)),
       col=palette(),cex=1.3, bty='n')
#dev.copy2pdf(file= "output/canopyleafarea.pdf")
#dev.off()

