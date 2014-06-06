source("functions and packages/functions.R")
source("functions and packages/load packages.R")

#read leaf traits data from surveys
source("read data scripts/survey read data.R")

#LEAF LENGTH, format data
leaflength <- melt(leaflength, id=c("plot", "pot", "LeafSS"))
names(leaflength)[4:5] <- c("Date", "length")
leaflength$Date <- gsub("X", "", leaflength$Date)
leaflength$Date <- as.Date(leaflength$Date, format = "%m.%d.%Y")
leaflength <- subset(leaflength, !is.na(length))
leaflength$ID <- paste(leaflength$plot, leaflength$pot, sep = "-")
#merge with plot summary
leaflength <- merge(leaflength, plotsumm, by =  "ID")
leaflength$volume <- as.factor(leaflength$volume)

#average length per pot
leaflength_avg <- aggregate(length ~ Date + ID, data = leaflength, FUN = mean)
#treatment means
leaflength_agg <- aggregate(length ~ Date + volume, data = leaflength, FUN = mean)


#LEAF MASS AREA
lma1 <- subset(lma, select=c("ID","campaign",	"area",	"mass"))
#format data
lma1 <- subset(lma1, !is.na(area))
lma1 <- merge(lma1, plotsumm, by =  "ID")
lma1$volume <- as.factor(lma1$volume)
lma1$massperarea <- with(lma1, mass/area)
#add campaign date
lma2 <- add_campaign_date(lma1)

#average lma per pot
lma_avg <- aggregate(massperarea ~ Date + ID, data = lma2, FUN = mean)
#treatment means
lma_agg <- aggregate(cbind(area , massperarea) ~ Date + volume, data = lma2, FUN = mean)

#----------------------------------------------------------------------------------------------------
#plot stuff

#colors
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))

# ticks on the X axis as a Date object ('DateTime').
xAT <- seq.Date(from=as.Date("2013-3-1"), length=5, by="month")
pchs = c(rep(16,6),17)
#labels
leglab <- c(5, 10, 15, 20, 25, 35, "free")
vollab <- expression(Pot~volume~(l))

#3 panel plot with allometry mean data-------------------------------------------

windows(8,12)
par(cex.axis=0.9,  cex.lab=1.3, mfrow=c(3,1), omi=c(1,0,0.1,0.1),  mar=c(0,7,0,0))  

#first panel  
plot(area ~ Date, data=lma_agg, 
     type='n',
     ylab=expression(Leaf~Area~~(cm^2)), 
     ylim=c(5,50), axes = FALSE, xlab="")

axis.Date(1, at=xAT, labels=FALSE) 
axis(2, labels=T)  

points(area ~ Date, data=lma_agg, pch=pchs[volume], cex=1.3,col = volume)
box()

#second panel
plot(massperarea ~ Date, data=lma_agg, type='n',ylab=expression(LMA~~(g~cm^-2)), 
     ylim=c(0,.015), axes = FALSE, xlab="")

axis.Date(1, at=xAT, labels=FALSE)
axis(2)    

points(massperarea ~ Date, data=lma_agg, pch=pchs[volume], cex=1.3,col = volume)
box()

#third panel
plot(length ~ Date, data=leaflength_agg,type='n',ylab=expression(Length~~(cm)),  
     ylim=c(5,12),  axes=FALSE, xlab="") 

axis.Date(1, at=xAT, labels=TRUE) 
axis(2, labels=T) 

#legend("topleft", leglab, pch=c(rep(16,6),17),text.font=3, inset=0.02, title=expression(Pot~volume~(dm^3)), col=palette(), bty='n')

points(length ~ Date, 
       data=leaflength_agg,
       pch=pchs[volume], 
       cex=1.3,
       col = volume)
box()

#mtext("Date", side=1, outer=TRUE, line=3, cex=1.3)

dev.copy2pdf(file= "output/leaftraits.pdf")

