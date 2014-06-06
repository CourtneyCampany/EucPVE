setwd("C:/Users/90919620/Google Drive/EucPVE/R Database")

library(reshape)
library(RColorBrewer)

#read in plot design
plotsumm <- read.csv("plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#HEIGHT
height <- read.csv("seedling height.csv")

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

#treatment means
height_agg <- aggregate(height ~ Date + volume , data = height, FUN = mean)

#subset with no free plant, and treatment means
height_allpot <- subset(height, volume != "1000")
height_allpot <- droplevels(height_allpot)
height_allpot_agg <- aggregate(height ~ Date + volume, data = height_allpot, FUN = mean)


#DIAMETER
diam <- read.csv("seedling diameter.csv")

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

#treatment means
diam_agg <- aggregate(diameter ~ Date + volume , data = diam, FUN = mean)

#subset with no free plant, and treatment means
diam_allpot <- subset(diam, volume != "1000")
diam_allpot_agg <- aggregate(diameter ~ Date + volume, data = diam_allpot, FUN = mean)


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
#merge with plot summary and sort
leafno <- merge(leafno, plotsumm, by = c("pot", "plot", "ID"))
leafno$volume <- as.factor(leafno$volume)
#treatment means
leafno_agg <- aggregate(count ~ Date + volume , data = leafno, FUN = mean)

#subset with no free plant, and treatment means
leafno_allpot <- subset(leafno, volume != "1000")
leafno_allpot_agg <- aggregate(count ~ Date + volume, data = leafno_allpot, FUN = mean)


#LEAF LENGTH
leaflength <- read.csv("seedling leaf length.csv")

#format data
leaflength <- melt(leaflength, id=c("plot", "pot", "LeafSS"))
names(leaflength)[4:5] <- c("Date", "length")
leaflength$Date <- gsub("X", "", leaflength$Date)
leaflength$Date <- as.Date(leaflength$Date, format = "%m.%d.%Y")
leaflength <- subset(leaflength, !is.na(length))
leaflength$ID <- paste(leaflength$plot, leaflength$pot, sep = "-")
#merge with plot summary
leaflength <- merge(leaflength, plotsumm, by = c("pot", "plot", "ID"))
leaflength$volume <- as.factor(leaflength$volume)

#average length per pot
leaflength_avg <- aggregate(length ~ Date + ID, data = leaflength, FUN = mean)
#treatment means
leaflength_agg <- aggregate(length ~ Date + volume, data = leaflength, FUN = mean)

#subset with no free plant, and treatment means
length_allpot <- subset(leaflength, volume != "1000")
length_allpot_agg <- aggregate(length ~ Date + volume, data = length_allpot, FUN = mean)


#4 panel plots with allometry mean data (with and without "free" seedling)

#plot bits
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
# ticks on the X axis as a Date object ('DateTime').
xAT <- seq.Date(from=as.Date("2013-1-1"), length=20, by="2 weeks")
#legend labels
leglab <- levels(height$volume)
leglab[7] <- "free"

leglab2 <- levels(height_allpot$volume)


#PLOT
windows(12,8)
par(cex.axis=0.9,  # axis label size
    mfrow=c(2,2),  # rows and columns of plots
    omi=c(1,1,0.2,0.2),  # outer margin (inches)
    mar=c(0,0,0,0))  # margin around plots (they are tight together)   

# First Panel
plot(height ~ Date, data=height_agg,
    type='n', 
    ylim=c(20,125), 
    axes=FALSE, ann=FALSE)  # suppresses axes; add customized ones

    axis.Date(1, at=xAT, labels=FALSE) #axis needs no labels
    axis(2, labels=TRUE)  # Y axis
    title(main="Height (cm)", font.main=4)
    #mtext("Height (cm)", side=2, outer=TRUE, line=3, cex=1.3, at=.85)
    legend("topleft", leglab, pch=16,text.font=3, inset=0.02, title=expression(Pot~volume~(dm^3)), col=palette(), bty='n')
    
    points(height ~ Date, 
            data=height_agg,
            pch=c(rep(16,6),17)[volume], 
            cex=1.3, 
            tck=0.5,
            col = volume)      
    box()

# Second panel         
plot(diameter ~ Date, data=diam_agg, 
    type='n',
    ylim=c(0,15), 
    axes = FALSE, ann = FALSE)
 
    axis.Date(1, at=xAT, labels=FALSE)  #axis needs no labels
    axis(2)  # Y axis  
    title(main="Diameter (cm)", font.main=4)
    #mtext("Diameter (cm)", side=2, outer=TRUE, line=3, cex=1.3, )  

    points(diameter ~ Date, 
            data=diam_agg, 
            pch=c(rep(16,6),17)[volume],
            cex=1.3,
            tck=0.5,
            col = volume)
     box()
            
#third panel
plot(count ~ Date, data=leafno_agg, 
     type='n',
     #ylab=expression(italic(Leaf Number)~~(#)),
     ylim=c(0,250), axes = FALSE, ann = FALSE)
     
     axis.Date(1, at=xAT, labels=TRUE)  #axis needs no labels
     axis(2)  # Y axis    
     title(main="Leaf Number (#)", font.main=4)     
     #mtext("Leaf Number (#)", side=2, outer=TRUE, line=3, cex=1.3, at=.15)

     points(count ~ Date, 
            data=leafno_agg, 
            pch=c(rep(16,6),17)[volume], 
            cex=1.3,
            col = volume)
     box()

# Fourth Panel
plot(length ~ Date, data=leaflength_agg,
    type='n',
    ylim=c(5,15), 
    axes=FALSE, xlab="") 

    axis.Date(1, at=xAT, labels=T) 
    axis(2, labels=T) 
    title(main="Leaf Length (cm)", font.main=4)  

    points(length ~ Date, 
    data=leaflength_agg,
    pch=c(rep(16,6),17)[volume], 
    cex=1.3,
    col = volume)

    box()


# Axis titles
  mtext("Date", side=1, outer=TRUE, line=3, cex=1.3)

#------------------------------------------------------------------------------------------#
#PLOT with no FREE plant
windows(12,8)
par(cex.axis=0.9,  # axis label size
    mfrow=c(2,2),  # rows and columns of plots
    omi=c(1,1,0.2,0.2),  # outer margin (inches)
    mar=c(0,0,0,0))  # margin around plots (they are tight together)   

# First Panel
plot(height ~ Date, data=height_allpot_agg,
     type='n', 
     ylim=c(25,100),  
     axes=FALSE, xlab="")  # suppresses axes; add customized ones
     title(main="Height (cm)", font.main=4)

     axis.Date(1, at=xAT, labels=FALSE) #axis needs no labels
     axis(2)  # Y axis
     legend("topleft", leglab2, pch=16,text.font=3, inset=0.02, title=expression(Pot~volume~(dm^3)), col=palette(), bty='n')

points(height ~ Date, 
       data=height_allpot_agg,
       pch=16, cex=1.3,
       col = volume)
box()

# Second panel         
plot(diameter ~ Date, data=diam_allpot_agg, 
     type='n',
     #ylab=expression(bold(Diameter~~(cm))),
     ylim=c(0,10), 
    
     axes = FALSE, xlab="")
     title(main="Diameter (cm)", font.main=4)

     axis.Date(1, at=xAT, labels=FALSE)  #axis needs no labels
     axis(2)  # Y axis     

points(diameter ~ Date, 
       data=diam_allpot_agg, 
       pch=16, cex=1.3,
       col = volume)
box()

#third panel
plot(count ~ Date, data=leafno_allpot_agg, 
     type='n',
     #ylab=expression(bold(Leaf~Number~~("#"))), 
     ylim=c(0,100), 
     
     axes = FALSE, xlab="")
     title(main="Leaf Number (#)", font.main=4) 

     axis.Date(1, at=xAT, labels=TRUE)  
     axis(2)  # Y axis    

points(count ~ Date, 
       data=leafno_allpot_agg, 
       pch=16, cex=1.3,
       col = volume)
box()

# Fourth Panel
plot(length ~ Date, data=length_allpot_agg,
     type='n',
     ylim=c(5,15), 
     axes=FALSE, xlab="") 
     title(main="Leaf Length (cm)", font.main=4)

     axis.Date(1, at=xAT, labels=T) 
     axis(2) 
  

points(length ~ Date, 
       data=length_allpot_agg,
       pch=c(rep(16,6),17)[volume], 
       cex=1.3,
       col = volume)

box()

mtext("Date", side=1, outer=TRUE, line=3, cex=1.3)

