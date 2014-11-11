
#read data
source("calculated data/seedling mass.csv")


#subset with data for root:shoot comparisons
ratio <- subset(seedlingmass, select = c("ID", "volume", "fineroot", "leafmass", "root", "shoot"))

#PLOTTING
pchs <- c(rep(16,6),17)
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))

#all data points, leaves-fine roots
with(ratio, plot(fineroot, leafmass, cex=1.6, pch=pchs, col=as.factor(volume)))
abline(0,1)

#all data points, root-shoot
windows(8,6)
par(oma = c(0, 1, 0, 0), mgp = c(2.5, 1, 0), cex.axis=1.0, cex.lab=1.3)
with(ratio, plot(root, shoot, cex=1.6, pch=pchs, col=as.factor(volume)))
abline(0,1)
box


#treatment means
ratio_agg <- summaryBy( .~ volume , data = ratio,  FUN=c(mean,sd,length))
#SE
ratio_agg$finerootSE <- with(ratio_agg, fineroot.sd/sqrt(fineroot.length))
ratio_agg$leafmassSE <- with(ratio_agg, leafmass.sd/sqrt(leafmass.length))
ratio_agg$rootSE <- with(ratio_agg, root.sd/sqrt(root.length))
ratio_agg$shootSE <- with(ratio_agg, shoot.sd/sqrt(shoot.length))



#quick plot of means, root:shoot $ leaf:froot
with(ratio_agg, plot(root.mean, shoot.mean, cex=1.6, pch=pchs, col=as.factor(volume)))
abline(0,1)
with(ratio_agg, plot(fineroot.mean, leafmass.mean, cex=1.6, pch=pchs, col=as.factor(volume)))
abline(0,1)

#----------------------------------------------------------------------------------------------------
#PLOT of Froot:leaf means with SE
windows(8,6)
par(mar=c(5,5,1,1), mgp = c(2.5, 1, 0), cex.axis=1.0, cex.lab=1.3)
with(ratio_agg, plot(fineroot.mean, leafmass.mean, cex=1.6, ylim=c(0,60), xlim=c(0,60),
                     pch=pchs, col=as.factor(volume), 
                     xlab = "Fine Root Mass (g)",
                     ylab = "Leaf Mass (g)"))

with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, x1=fineroot.mean+finerootSE, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, x1=fineroot.mean-finerootSE, angle=90, length=0.05,col=palette(), lwd=2))

with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, y1=leafmass.mean+leafmassSE, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, y1=leafmass.mean-leafmassSE, angle=90, length=0.05,col=palette(), lwd=2))

abline(0,1)

leglab <- levels(ratio_agg$volume)
leglab[7] <- "free"
legend("bottomright", leglab, pch=pchs,text.font=3, inset=0.0, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n')

box()

dev.copy2pdf(file= "output/Leaf_Froot.pdf")
#------------------------------------------------------------------------------------------------------
#PLOT of Root:Shoot means with SE

windows(8,6)
par(mar=c(5,5,1,1), mgp = c(2.5, 1, 0), cex.axis=1.0, cex.lab=1.3)
with(ratio_agg, plot(root.mean, shoot.mean, cex=1.6, ylim=c(0,40), xlim=c(0,40),
                     pch=pchs, col=as.factor(volume), 
                     xlab = "Root Mass (g)",
                     ylab = "Shoot Mass (g)"))

with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, x1=root.mean+rootSE, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, x1=root.mean-rootSE, angle=90, length=0.05,col=palette(), lwd=2))

with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, y1=shoot.mean+shootSE, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, y1=shoot.mean-shootSE, angle=90, length=0.05,col=palette(), lwd=2))

abline(0,1)

leglab <- levels(ratio_agg$volume)
leglab[7] <- "free"
legend("bottomright", leglab, pch=pchs,text.font=3, inset=0.0, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n')

box()
dev.copy2pdf(file= "output/Shoot_Root.pdf")
#-------------------------------------------------------------------------------------------------------------------------
