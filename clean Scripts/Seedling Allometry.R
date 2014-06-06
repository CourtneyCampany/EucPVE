
source("functions and packages/load packages.R")
source("read data scripts/survey read data.R")


#HEIGHT

#format data
height <- melt(height, id=c("plot", "pot"))
height$variable <- gsub ("X", "", height$variable)
names(height)[3:4] <- c("Date", "height")
height$Date <- as.Date(height$Date, format = "%m.%d.%Y")
height <- subset(height, !is.na(height))
height$ID <- paste(height$plot, height$pot, sep = "-")
#merge with plot summary
height <- merge(height, plotsumm, by = c("pot", "plot", "ID"))
height$volume <- as.factor(height$volume)

write.csv(height, "calculated data/height.csv", row.names=FALSE)

#treatment means
height_agg <- summaryBy(height ~ Date + volume , data = height, FUN = c(mean,sd,length))

#subset with no free plant, and treatment means
height_allpot <- subset(height, volume != "1000")
height_allpot <- droplevels(height_allpot)
height_allpot_agg <- summaryBy(height ~ Date + volume, data = height_allpot, FUN = c(mean,sd,length))


#DIAMETER

#format data
diam <- subset(diam, select = -c(Base..Time.0.))
diam <- melt(diam, id=c("plot",  "pot"))
names(diam)[3:4] <- c("Date", "diameter")
diam$Date <- gsub ("X", "", diam$Date)
diam$Date <- as.Date(diam$Date, format = "%m.%d.%Y")
diam <- subset(diam, !is.na(diameter))
diam$ID <- paste(diam$plot, diam$pot, sep = "-")
#merge with plot summary
diam <- merge(diam, plotsumm, by = c("pot", "plot", "ID"))
diam$volume <- as.factor(diam$volume)

write.csv(diam, "calculated data/diameter.csv", row.names=FALSE)

#treatment means
diam_agg <- summaryBy(diameter ~ Date + volume , data = diam, FUN = c(mean,sd,length))

#subset with no free plant, and treatment means
diam_allpot <- subset(diam, volume != "1000")
diam_allpot_agg <- summaryBy(diameter ~ Date + volume, data = diam_allpot, FUN = c(mean,sd,length))


#LEAF COUNT

#format data
leafno$X1.21.2013   <- as.integer(leafno$X1.21.2013)
leafno <- melt(leafno, id=c("plot", "pot"))
names(leafno)[3:4] <- c("Date", "count")
leafno$Date <- gsub ("X", "", leafno$Date)
leafno$Date <- as.Date(leafno$Date, format = "%m.%d.%Y")
leafno <- subset(leafno, !is.na(count))
leafno$ID <- paste(leafno$plot, leafno$pot, sep = "-")
#merge with plot summary and sort
leafno <- merge(leafno, plotsumm, by = c("pot", "plot", "ID"))
leafno$volume <- as.factor(leafno$volume)

write.csv(leafno, "calculated data/leaf_number.csv", row.names=FALSE)

#treatment means
leafno_agg <- summaryBy(count ~ Date + volume , data = leafno, FUN = c(mean,sd,length))

#subset with no free plant, and treatment means
leafno_allpot <- subset(leafno, volume != "1000")
leafno_allpot_agg <- summaryBy(count ~ Date + volume, data = leafno_allpot, FUN = c(mean,sd,length))


#PLOT
#3 panel plot with allometry mean data (with and without "free" seedling)

#plot bits
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
# ticks on the X axis as a Date object ('DateTime').
xAT <- seq.Date(from=as.Date("2013-1-1"), length=20, by="month")
PTcex <- 1.6
#legend labels
leglab <- levels(height$volume)
leglab[7] <- "free"

leglab2 <- levels(height_allpot$volume)

#se with free
height_agg$SE <- with(height_agg, height.sd/sqrt(height.length))
diam_agg$SE <- with(diam_agg, diameter.sd/sqrt(diameter.length))
leafno_agg$SE <- with(leafno_agg, count.sd/sqrt(count.length))

#se with no free
height_allpot_agg$SE <- with(height_allpot_agg, height.sd/sqrt(height.length))
diam_allpot_agg$SE <- with(diam_allpot_agg, diameter.sd/sqrt(diameter.length))
leafno_allpot_agg$SE <- with(leafno_allpot_agg, count.sd/sqrt(count.length))


#PLOT
windows(8,12)
par(cex.axis=1.2, cex.lab=1.8,
    mfrow=c(3,1),  # rows and columns of plots
    omi=c(1,0,0.1,0.1),  # outer margin (inches)
    mar=c(0,7,0,0))   # margin around plots (they are tight together)   

# First Panel
plot(height.mean ~ Date, data=height_agg,type='n',ylab=expression(Height~~(cm)),  ylim=c(20,125), axes=FALSE, xlab="")  

    axis.Date(1, at=xAT, labels=FALSE) #axis needs no labels
    axis(2, labels=TRUE)  # Y axis
    #mtext("Height (cm)", side=2, outer=TRUE, line=3, cex=1.3, at=.85)
    legend("topleft", leglab, pch=c(rep(16,6),17),text.font=3, inset=0.02, title=expression(Pot~volume~(l)), col=palette(), bty='n')
    
  with(height_agg, arrows(Date, height.mean, Date, height.mean+SE, angle=90, col=palette(),length=0.03))
  with(height_agg, arrows(Date, height.mean, Date, height.mean-SE, angle=90, col=palette(),length=0.03))

    points(height.mean ~ Date, data=height_agg,pch=c(rep(16,6),17)[volume], cex=PTcex, col = volume)      
    box()


# Second panel         
plot(diameter.mean ~ Date, data=diam_agg, type='n',ylab=expression(Diameter~~(mm)),ylim=c(0,15), axes = FALSE, xlab="")
 
    axis.Date(1, at=xAT, labels=FALSE)  
    axis(2)     
    #mtext("Diameter (cm)", side=2, outer=TRUE, line=3, cex=1.3, )  

with(diam_agg, arrows(Date, diameter.mean, Date, diameter.mean+SE, angle=90, col=palette(),length=0.03))
with(diam_agg, arrows(Date, diameter.mean, Date, diameter.mean-SE, angle=90, col=palette(),length=0.03))

    points(diameter.mean ~ Date, data=diam_agg, pch=c(rep(16,6),17)[volume],cex=PTcex,col = volume)
     box()
            
#third panel
plot(count.mean ~ Date, data=leafno_agg, 
     type='n',ylab=expression(Leaf~Number~~("#")),ylim=c(0,250), axes = FALSE, xlab="")
     
     axis.Date(1, at=xAT, labels=TRUE)  #axis needs no labels
     axis(2)     
     #mtext("Leaf Number (#)", side=2, outer=TRUE, line=3, cex=1.3, at=.15)

  with(leafno_agg, arrows(Date, count.mean, Date, count.mean+SE, angle=90, col=palette(),length=0.03))
  with(leafno_agg, arrows(Date, count.mean, Date, count.mean-SE, angle=90, col=palette(),length=0.03))

     points(count.mean ~ Date, data=leafno_agg,  pch=c(rep(16,6),17)[volume], cex=PTcex,col = volume)
     box()

# Axis titles
  #mtext("Date", side=1, outer=TRUE, line=3, cex=1.3)

dev.copy2pdf(file= "output/allometry.pdf")


#stats----------------------------------------------------------------------------------------------
# 'Being in a pot' effect.
diam_last <- subset(diam, Date == max(Date))
height_last <- subset(height, Date == max(Date))
leafno_last <- subset(leafno, Date == max(Date))

diam_pot <- lm(diameter ~ as.factor(volume), data=diam_last)
height_pot <- lm(height ~ as.factor(volume), data=height_last)
leafno_pot <- lm(count ~ as.factor(volume), data=leafno_last)

getP <- function(x)anova(x)[[5]][1]
getP(diam_pot)
getP(height_pot)
getP(leafno_pot)


# Pot size effect.
diam_potsize <- lm(diameter ~ as.factor(volume), data=diam_last, subset=volume != "1000")
height_potsize <- lm(height ~ as.factor(volume), data=height_last, subset=volume != "1000")
leafno_potsize <- lm(count ~ as.factor(volume), data=leafno_last, subset=volume != "1000")

getP(diam_potsize)
getP(height_potsize)
getP(leafno_potsize)

#------------------------------------------------------------------------------------------#
#PLOT with no FREE plant

windows(8,12)
par(cex.axis=0.9,  cex.lab=1.3,mfrow=c(3,1),  omi=c(1,0,0.1,0.1),  mar=c(0,7,0,0))  

# First Panel
plot(height.mean ~ Date, data=height_allpot_agg,
     type='n',ylab=expression(Height~~(cm)),  ylim=c(25,100), axes=FALSE, xlab="") 

axis.Date(1, at=xAT, labels=FALSE) 
axis(2, labels=TRUE)  
legend("topleft", leglab2, pch=16, text.font=3, inset=0.02, title=expression(Pot~volume~(l)), col=palette(), bty='n')

with(height_allpot_agg, arrows(Date, height.mean, Date, height.mean+SE, angle=90, col=volume,length=0.03))
with(height_allpot_agg, arrows(Date, height.mean, Date, height.mean-SE, angle=90, col=volume,length=0.03))

points(height.mean ~ Date, data=height_allpot_agg,pch=16, cex=1.3,col = volume)
box()

# Second panel         
plot(diameter.mean ~ Date, data=diam_allpot_agg, type='n',ylab=expression(Diameter~~(mm)),ylim=c(0,10), 
     axes = FALSE, xlab="")

axis.Date(1, at=xAT, labels=FALSE)  
axis(2)  

with(diam_allpot_agg, arrows(Date, diameter.mean, Date, diameter.mean+SE, angle=90, col=volume,length=0.03))
with(diam_allpot_agg, arrows(Date, diameter.mean, Date, diameter.mean-SE, angle=90, col=volume,length=0.03))

points(diameter.mean ~ Date, data=diam_allpot_agg, pch=16, cex=1.3, col = volume)
box()

#third panel
plot(count.mean ~ Date, data=leafno_allpot_agg, 
     type='n',ylab=expression(Leaf~Number~~("#")), ylim=c(0,120), axes = FALSE, xlab="")

axis.Date(1, at=xAT, labels=TRUE)  
axis(2)    

with(leafno_allpot_agg, arrows(Date, count.mean, Date, count.mean+SE, angle=90, col=volume,length=0.03))
with(leafno_allpot_agg, arrows(Date, count.mean, Date, count.mean-SE, angle=90, col=volume,length=0.03))

points(count.mean ~ Date, data=leafno_allpot_agg, pch=16, cex=1.3, col = volume)

box()

#mtext("Date", side=1, outer=TRUE, line=3, cex=1.3)

dev.copy2pdf(file= "output/allometrypots.pdf")
dev.off()
