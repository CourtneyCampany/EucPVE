source("functions and packages/load packages.R")

# Read data
aci <- read.csv("raw data/ACi#1.csv")
aci$ID <- paste(aci$plot, aci$pot, sep = "-")
# new variable, treatment, based on 'pot volume'
aci$treatment <- as.factor(aci$Volume)


# Choose one plant.
plant1 <- subset(aci, ID == "7-8")

# Sort data by Ci, because a line plot will connect points in order of the dataframe
plant1 <- plant1[order(plant1$Ci), ]

# Make a simple line plot
with(plant1, plot(Ci, Photo, type = "l"))

#----------------------------------------------------------------------------------------------------
#plot stuff

#colors
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
pchs <- c(rep(16,6),17)
#legend labels
leglab <- c(5, 10, 15, 20, 25, 35, "free")
vollab <- expression(Pot~volume~(l))
anet <- expression(italic(A)[net] ~ ~(mu * mol ~ m^-2 ~ s^-1))
cilab <- expression(C[i]~~(mu*mol~mol^-1))
raw <- "Raw Data"
volmean <- "Volume Means"

ids <- unique(aci$ID)
#----------------------------------------------------------------------------------------------------


#Make a plotting function
plotAci <- function(plantID) {
  dat <- subset(aci, ID == plantID)
  
  dat <- dat[order(dat$Ci), ]
  with(dat, points(Ci, Ps, type = "l", col = treatment))
}


#Set up an empty plot, type='n' will suppress plotting
windows()
with(aci, plot(Ci, Photo, type = "n"))

#Loop through all IDs, call function
ids <- unique(aci$ID)
for (id in ids) plotAci(id)

# Add legend
legend("topleft", leglab, lty = 1, col = palette())


#Smooth lines, one for each plant

# take a subset, order it.
aci5 <- subset(aci, treatment == "5")
aci5 <- aci5[order(aci5$Ci), ]

# Changing the span controls the smoothness of the line This function can
# not be well customized...
with(aci5, scatter.smooth(Ci, Photo, span = 0.3, type = "n"))


#Next we fit a loess smooth regression to each curve. 
#Much like lm, you can predict from this object, for a new dataframe. We first fit the loess, 
#then construct an empty dataframe where Ci takes a range of equally spaced values, 
#and then add the prediction from the loess to that dataframe.

# 1. Set up a plotting function
plotsmoothAci <- function(plantID){
  dat <- subset(aci, ID==plantID)
  dat <- dat[order(dat$Ci),]
  
  # Fit loess
  l <- loess(Photo ~ Ci, data=dat)
  
  # Dataframe with equally spaced Ci values, and predicted Ps.
  dfr <- data.frame(Ci=seq(min(dat$Ci), max(dat$Ci), length=100))
  dfr$Photo_pred <- predict(l, dfr)
  
  # Add the line
  with(dfr, points(Ci,Photo_pred,type='l',col=dat$treatment))
}


# 2. Set up empty plot.
# We have also widened the margins, increased axis title size,
# and made properly formatted axis titles.
windows()
par(mar=c(5,5,1,1), cex.lab=1.3)
with(aci, plot(Ci, Photo, type='n', xlab=cilab,ylab=anet))

# Loop through IDs, add line.
for(id in ids)plotsmoothAci(id)

legend("bottomright", leglab, lty=1, lwd=1,col=palette(), title=vollab, inset=0.03, bty='n')
box()

dev.copy2pdf(file= "output/ACI_all.pdf")
dev.off()


#Smooth lines, one for each treatment
#plot one smooth line per treatment (averaged across all plants). 
#take a subset of the data by treatment rather than ID.
# only meant to visualize the patterns!

  plotsmoothaggAci <- function(treat) {
    dat <- subset(aci, treatment == treat)
    dat <- dat[order(dat$Ci), ]
    
    l <- loess(Photo ~ Ci, data = dat)
    dfr <- data.frame(Ci = seq(min(dat$Ci), max(dat$Ci), length = 100))
    dfr$Ps_pred <- predict(l, dfr)
    
    with(dfr, points(Ci, Ps_pred, type = "l", col = dat$treatment, lwd = 2))
  }


windows()
par(mar = c(5, 5, 4, 1), cex.lab = 1.3, cex.main = 1.2)
with(aci, plot(Ci, Photo, type = "n", xlab = cilab, ylab = anet))

treatments <- levels(aci$treatment)
for (x in treatments) plotsmoothaggAci(x)

legend("bottomright", leglab, lty = 1, lwd = 2, col = palette(), title = vollab, inset = 0.03, bty = "n")
box()

dev.copy2pdf(file= "output/ACI_volume.pdf")
dev.off()


#panel grapth with both raw and means of aci curves-------------------------------------------------------
windows()
par(mfrow=c(2,1), omi=c(1,0,0.5,0.5),mar=c(0,5,0,0),mgp = c(2.5, 1, 0))
#raw
with(aci, plot(Ci, Photo, type='n', axes=FALSE, ylab="", xlab="", ylim=c(0,45)))
box()
title(ylab=anet) 
title(main=raw, line=-1.5, font.main=1, adj=.05, cex.main=1)
axis(1, tick=TRUE, labels=FALSE)
axis(2)
#legend("topleft", leglab, lty = 1, lwd = 2, col = palette(), title = vollab, inset = 0.03, bty = "n")
# Loop through IDs, add line.
for(id in ids)plotsmoothAci(id)

#means
with(aci, plot(Ci, Photo, type = "n", ylab = anet, ylim=c(0,45)))

treatments <- levels(aci$treatment)
for (x in treatments) plotsmoothaggAci(x)
mtext(cilab, side=1, line=2.5)
title(main=volmean, line=-1.5, font.main=1, adj=.05, cex.main=1)
box()
dev.copy2pdf(file= "output/stats_plots/ACI.pdf")
dev.off()

