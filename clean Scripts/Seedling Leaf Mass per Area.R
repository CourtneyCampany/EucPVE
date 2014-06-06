source("functions and packages/load packages")

#read leaf data from surveys
source("read data scripts/survey read data.R")

#format data
lma <- subset(lma, !is.na(area))
lma <- merge(lma, plotsumm, by ="ID")
lma$volume <- as.factor(lma$volume)
lma$massperarea <- with(lma, mass/area)
#average lma per pot
lma_avg <- aggregate(massperarea ~ Date + ID, data = lma, FUN = mean)
#treatment means
lma_agg <- aggregate(cbind(area , massperarea) ~ Date + volume, data = lma, FUN = mean)

#ACI leaves (subset of whole experiment with random leaves from each volume)
#can merge with with other leaf traits but not a complete data set!!!!!!!
acileaf <- subset (acileaf, select = c("Date", "area",  "length",	"mass", "ID"))
acileaf <- merge(acileaf, plotsumm, by = "ID")
acileaf$massperarea <- with(acileaf, mass/area)
acileaf$volume <- as.factor(acileaf$volume)
#treatment means
acileaf_agg <- aggregate(cbind(area, massperarea, length) ~ Date + volume, data = acileaf, FUN = mean)



#2 panel plot with LMA

#plot bits
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))

# ticks on the X axis as a Date object ('DateTime').
xAT <- seq.Date(from=as.Date("2013-2-15"), length=25, by="week")

#legend labels
leglab <- levels(acileaf$volume)
leglab[7] <- "free"


#PLOT
windows(8,12)
par(mar=c(5,5,1,1), mgp = c(2.5, 1, 0), cex.axis=1.0, cex.lab=1.3)

# first panel    
plot(massperarea ~ Date, data=lma_agg, 
     ylab=expression(Leaf~Mass~per~Area~~(g~cm^-2)),
     xlab="",
     pch=c(rep(16,6),17)[volume], 
     col = volume,
     cex=1.3,
     ylim=c(0,.020))   

axis.Date(1, at=xAT, labels=FALSE)  #axis needs no labels
axis(2)  # Y axis     

#legend("topleft", leglab, pch=16,text.font=3, inset=0.02, title=expression(Pot~volume~(l)), col=palette(), bty="n")


box()

dev.copy2pdf(file= "output/LMApots.pdf")

#two panel
plot(massperarea ~ Date, data=lma_agg, 
     type='n',
     ylab=expression(Leaf~Mass~per~Area~~(g~cm^-2)),
     ylim=c(0,.015), 
     axes = FALSE, xlab="")

axis.Date(1, at=xAT, labels=FALSE)  #axis needs no labels
axis(2)  # Y axis     
legend("bottomright", leglab, pch=16,text.font=3, inset=0.02, title=expression(Pot~volume~(dm^3)), col=palette())

points(massperarea ~ Date, 
       data=lma_agg, 
       pch=c(rep(16,6),17)[volume], 
       cex=1.3,
       col = volume)
box()

#second panel
plot(area ~ Date, data=lma_agg, 
     type='n',
     ylab=expression(Leaf~Area~~(cm^2)), 
     ylim=c(5,50), axes = FALSE, xlab="")
axis.Date(1, at=xAT, labels=TRUE)  #axis needs no labels
axis(2)  # Y axis    

points(area ~ Date, 
       data=lma_agg, 
       pch=c(rep(16,6),17)[volume], 
       cex=1.3,
       col = volume)
box()
mtext("Date", side=1, outer=TRUE, line=3, cex=1.3)

