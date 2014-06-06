
source("functions and packages/load packages.R")

#read in water potential data from weekly surveys
source("read data scripts/survey read data.R")

#format pre-SM
SMpre <- melt(SMpre, id=c("plot", "pot"))
SMpre$variable <- gsub ("X", "", SMpre$variable)
names(SMpre)[3:4] <- c("Date", "perc_water")
SMpre$Date <- as.Date(SMpre$Date, format = "%m.%d.%Y")
SMpre <- subset(SMpre, !is.na(SMpre))
SMpre$ID <- paste(SMpre$plot, SMpre$pot, sep = "-")
SMpre <- merge(SMpre, plotsumm, by = c("pot", "plot", "ID"))
SMpre$volume <- as.factor(SMpre$volume)

#treatment means
SMpre_agg <- aggregate(perc_water ~ Date + volume , data = SMpre, FUN = mean)


#format SM
SM <- melt(SM, id=c("plot", "pot"))
SM$variable <- gsub ("X", "", SM$variable)
names(SM)[3:4] <- c("Date", "perc_water")
SM$Date <- as.Date(SM$Date, format = "%m.%d.%Y")
SM <- subset(SM, !is.na(perc_water))
SM$ID <- paste(SM$plot, SM$pot, sep = "-")
SM <- merge(SM, plotsumm, by = c("pot", "plot", "ID"))
SM$volume <- as.factor(SM$volume)
#treatment means
SM_agg <- aggregate(perc_water ~ Date + volume , data = SM, FUN = mean)

#2 panel plot with allometry mean data
#plot bits
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))

# ticks on the X axis as a Date object ('DateTime').
xAT <- seq.Date(from=as.Date("2013-3-20"), length=30, by="week")

#legend labels
leglab <- c(5, 10, 15, 20, 25, 35, "free")
volume <- SMpre_agg$volume
pchs<-c(rep(16,6),17)

#PLOT
windows(8,12)
par(cex.axis=0.9,
    cex.lab= 1.3, 
    mfrow=c(2,1),  
    omi=c(1,1,0.2,0.2),  # outer margin (inches)
    mar=c(0,0,0,0))  # margin around plots (they are tight together)   

# First Panel
plot(perc_water ~ Date, data=SM_agg, type='n',ylim=c(0,25), axes = FALSE, ann = FALSE)

axis.Date(1, at=xAT, labels=FALSE)  #axis needs no labels
axis(2)  # Y axis      
title(main="target soil moisture", line=-1.5, font.main=1, adj=.05, cex.main=1)
points(perc_water ~ Date, data=SM_agg, pch=pchs[volume], cex=1.3,col = volume)
box()

# Second Panel

plot(perc_water ~ Date, data=SMpre_agg, type='n',ylim=c(0,25), axes=FALSE, ann=FALSE) 

axis.Date(1, at=xAT, labels=TRUE) #axis needs no labels
axis(2, labels=TRUE)  # Y axis
#legend("topright", leglab, pch=c(rep(16,6),17),text.font=3, inset=0.02, title=expression(Pot~volume~(dm^3)), col=palette(), bty='n')
title(main="water use", line=-1.5, font.main=1, adj=.05, cex.main=1)
points(perc_water ~ Date, data=SMpre_agg,pch=pchs[volume], cex=1.3,col = volume)
box()

# Axis titles
#mtext("Date", side=1, outer=TRUE, line=3, cex=1.3)
mtext("Soil Moisture (%)", side=2, outer=TRUE, line=3, cex=1.0, at=.5)

dev.copy2pdf(file= "output/soilmoisture.pdf")
dev.off()

