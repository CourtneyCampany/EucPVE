
source("functions and packages/startscripts.R")
#read data
source("read data scripts/harvest read data.R")

#merge harvest mass with plot summary
harvest_mass <- merge(harvest_mass, plotsumm, by = c("pot", "plot", "ID"))
  harvest_mass$volume <- as.factor(harvest_mass$volume)

#subset with dry mass
seedlingmass <- subset(harvest_mass, select = c("ID", "volume", "Croot", "Froot", "leafmass", "stemmass"))

#FINE ROOT SUBSAMPLES from SRL-----------------------------------------------------------------------------

#calculate and add  SRL froot SS dry mass to bulk froot DW
srlmass$srl_dw <- with(srlmass, srl_fw * (1-(ss_fw - ss_dw)/ss_fw))

#add extra mass from doubles of plot one SS
extramass <- data.frame(ID = c("1-1", "1-2", "1-4", "1-5", "1-6", "1-7", "1-8"), 
             extramass = c(.27, .30, .53, 1.2, .96, .91, .58))

#new data frame with just total dry mass from froot subsample
srlmass <- merge(srlmass, extramass, by = "ID", all=TRUE)
  srlmass$extramass <- ifelse(is.na(srlmass$extramass), 0, srlmass$extramass)
  srlmass$frootSS_dw <- with(srlmass, srl_dw + ss_dw + extramass)
frootSS <- subset(srlmass, select = c("ID", "frootSS_dw"))
#------------------------------------------------------------------------------------------------------------

#dfr with all dry weights
seedlingmass <- merge(seedlingmass, frootSS, by = "ID", all=TRUE)

  seedlingmass$fineroot <- with(seedlingmass, frootSS_dw + Froot)
  seedlingmass$root <- with(seedlingmass, Croot + fineroot)
  seedlingmass$shoot <- with(seedlingmass, leafmass + stemmass)

  #total mass, shoot:root, fineroot:leafmass
  seedlingmass$totalmass <- with(seedlingmass, shoot + root)
  seedlingmass$rootshoot <- with(seedlingmass, root/shoot)
  seedlingmass$frootleaf <- with(seedlingmass, fineroot/leafmass)

massvol <- summaryBy( .~volume, data=seedlingmass, FUN=se)


#write to csv
write.csv(seedlingmass, file = "calculated data/seedling mass.csv", row.names=FALSE)   

#PLOT ROOT:SHOOT------------------------------------------------------------------------------------

#subset with data for root:shoot comparisons
ratio <- subset(seedlingmass, select = c("ID", "volume", "fineroot", "leafmass", "root", "shoot"))
ratio_nofree <- subset(ratio, volume !="1000")
#treatment means
ratio_agg <- summaryBy( .~ volume , data = ratio,  FUN=c(mean,se))
ratio_nofree_agg <- summaryBy( .~ volume , data = ratio_nofree,  FUN=c(mean,se))
#linear model

RS_fit <- lm(root~ shoot,data=ratio)
summary(RS_fit)
anova(RS_fit)
#no free
RS_nofree <- lm(root~ shoot,data=ratio_nofree)
summary(RS_nofree)
anova(RS_nofree)


FRL_fit <- lm(fineroot~ leafmass,data=ratio)
summary(FRL_fit)
anova(FRL_fit)

#----------------------------------------------------------------------------------------------------
#plot bits

# #color scheme
# gradient <- colorRampPalette(c("red", "blue"))
# color <- palette(gradient(7))
# pchs = c(rep(16,6),17)
# ltys <- c(rep(1,6),4)
# leglab <- c(5, 10, 15, 20, 25, 35, "free")
# leglab2 <- c(5, 10, 15, 20, 25, 35)
# vollab <- expression(Pot~volume~(l))
#----------------------------------------------------------------------------------------------------
#two panel plot with raw data for root:shoot and froot:leaf (raw and means)

#root:shoot 
windows(10,10)
par(mfrow=c(2,1), omi=c(1,0,0.5,0.5),mar=c(0,5,0,0),mgp = c(2.5, 1, 0))
#raw
plot(shoot ~ root, data=ratio, axes=FALSE,xlab="", ylab="",xlim=c(0,100), ylim=c(0,120),type='n')
box()
axis(2, labels=TRUE)  
title(ylab="Shoot Mass (g)")
points(shoot ~ root, data=ratio, pch=pchs[volume], col=volume,cex=1.6)
d_ply(ratio, .(volume), function(x) add_trend_line("shoot", "root", x, ))
legend("topleft", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, col=palette(), bty='n')
axis(1, tick=TRUE, label=FALSE )

plot(shoot.mean~root.mean, data=ratio_agg, cex=1.6, pch=pchs[volume], ylab="", xlim=c(0,100),ylim=c(0,120),
     xlab="", col=volume)
title(ylab="Shoot Mass (g)")
mtext("Root Mass (g)", side=1, line=2.5)

with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, x1=root.mean+root.se, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, x1=root.mean-root.se, angle=90, length=0.05,col=palette(), lwd=2))

with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, y1=shoot.mean+shoot.se, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, y1=shoot.mean-shoot.se, angle=90, length=0.05,col=palette(), lwd=2))
ablineclip(RS_fit, x1=min(ratio_agg$shoot.mean), x2=max(ratio_agg$shoot.mean), col="blue")
abline(0,1, lty=5)

dev.copy2pdf(file= "output/stats_plots/rootshoot.pdf")
dev.off()


#froot:leaf
windows(10,10)
par(mfrow=c(2,1), omi=c(1,0,0.5,0.5),mar=c(0,5,0,0),mgp = c(2.5, 1, 0))
#raw
plot(leafmass ~ fineroot, data=ratio, axes=FALSE,xlab="", ylab="",xlim=c(0,80), ylim=c(0,100),type='n')
box()
axis(2, labels=TRUE)  
title(ylab="Leaf Mass (g)")
points(leafmass ~ fineroot, data=ratio, pch=pchs[volume], col=volume,cex=1.6)
d_ply(ratio, .(volume), function(x) add_trend_line("leafmass", "fineroot", x, ))
legend("topleft", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, col=palette(), bty='n')
axis(1, tick=TRUE, label=FALSE )

plot(leafmass.mean~fineroot.mean, data=ratio_agg, cex=1.6, pch=pchs[volume], ylab="", xlim=c(0,80),ylim=c(0,120),
     xlab="", col=volume)
title(ylab="Leaf Mass (g)")
mtext("Fine Root Mass (g)", side=1, line=2.5)

with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, x1=fineroot.mean+root.se, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, x1=fineroot.mean-root.se, angle=90, length=0.05,col=palette(), lwd=2))

with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, y1=leafmass.mean+shoot.se, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, y1=leafmass.mean-shoot.se, angle=90, length=0.05,col=palette(), lwd=2))
ablineclip(FRL_fit, x1=min(ratio_agg$leafmass.mean), x2=max(ratio_agg$leafmass.mean),col="blue")
abline(0,1, lty=5)

dev.copy2pdf(file= "output/stats_plots/frootleaf.pdf")
dev.off()


#----------------------------------------------------------------------------------------------------
#PLOT of Froot:leaf means with SE
windows(8,6)
par(mar=c(5,5,1,1), mgp = c(2.5, 1, 0), cex.axis=1.0, cex.lab=1.3)
with(ratio_agg, plot(fineroot.mean, leafmass.mean, cex=1.6, ylim=c(0,60), xlim=c(0,60),
                     pch=pchs, col=volume, 
                     xlab = "Fine Root Mass (g)",
                     ylab = "Leaf Mass (g)"))

with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, x1=fineroot.mean+fineroot.se, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, x1=fineroot.mean-fineroot.se, angle=90, length=0.05,col=palette(), lwd=2))

with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, y1=leafmass.mean+leafmass.se, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=fineroot.mean, y0=leafmass.mean, y1=leafmass.mean-leafmass.se, angle=90, length=0.05,col=palette(), lwd=2))

abline(0,1)

leglab <- levels(ratio_agg$volume)
leglab[7] <- "free"
legend("topleft", leglab, pch=pchs,text.font=3, inset=0.0, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n')


dev.copy2pdf(file= "output/Leaf_Froot.pdf")
#------------------------------------------------------------------------------------------------------
#PLOT of Root:Shoot means with SE

windows(8,6)
par(mar=c(5,5,1,1), mgp = c(2.5, 1, 0), cex.axis=1.0, cex.lab=1.3)
with(ratio_agg, plot(root.mean, shoot.mean, cex=1.6, ylim=c(0,40), xlim=c(0,40),
                     pch=pchs[volume],col=volume, 
                     xlab = "Root Mass (g)",
                     ylab = "Shoot Mass (g)"))

with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, x1=root.mean+root.se, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, x1=root.mean-root.se, angle=90, length=0.05,col=palette(), lwd=2))

with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, y1=shoot.mean+shoot.se, angle=90, length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, y1=shoot.mean-shoot.se, angle=90, length=0.05,col=palette(), lwd=2))

abline(0,1)

leglab <- levels(ratio_agg$volume)
leglab[7] <- "free"
legend("bottomright", leglab, pch=pchs,text.font=3, inset=0.0, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n')

box()
dev.copy2pdf(file= "output/Shoot_Root.pdf")
#-------------------------------------------------------------------------------------------------------------------------
#root:shoot no free
windows(10,10)
par(mfrow=c(2,1), omi=c(1,0,0.5,0.5),mar=c(0,5,0,0),mgp = c(2.5, 1, 0))
#raw
plot(shoot ~ root, data=ratio_nofree, axes=FALSE,xlab="", ylab="",xlim=c(0,50), ylim=c(0,60),type='n')
box()
axis(2, labels=TRUE)  
title(ylab="Shoot Mass (g)")
points(shoot ~ root, data=ratio_nofree, pch=16, col=volume,cex=1.6)
#d_ply(subset(ratio, volume!="1000"), .(volume), function(x) add_trend_line("shoot", "root", x, ))
#legend("topleft", leglab2, pch=pchs,text.font=3, inset=0.02, title=vollab, col=palette(), bty='n')
axis(1, tick=TRUE, label=FALSE )

plot(shoot.mean~root.mean, data=ratio_nofree_agg, cex=1.6, pch=16, ylab="", xlim=c(0,50),
    ylim=c(0,60),xlab="", col=volume)
title(ylab="Shoot Mass (g)")
mtext("Root Mass (g)", side=1, line=2.5)

with(subset(ratio_nofree_agg, volume!="1000"), arrows(x0=root.mean, y0=shoot.mean, x1=root.mean+root.se, angle=90, length=0.05,col=palette(), lwd=2))
with(subset(ratio_nofree_agg, volume!="1000"), arrows(x0=root.mean, y0=shoot.mean, x1=root.mean-root.se, angle=90, length=0.05,col=palette(), lwd=2))

with(subset(ratio_nofree_agg, volume!="1000"), arrows(x0=root.mean, y0=shoot.mean, y1=shoot.mean+shoot.se, angle=90, length=0.05,col=palette(), lwd=2))
with(subset(ratio_nofree_agg, volume!="1000"), arrows(x0=root.mean, y0=shoot.mean, y1=shoot.mean-shoot.se, angle=90, length=0.05,col=palette(), lwd=2))
#ablineclip(RS_nofree, x1=min(ratio_nofree_agg$shoot.mean), x2=max(ratio_nofree_agg$shoot.mean), col="blue")
#abline(0,1, lty=5)

dev.copy2pdf(file= "output/stats_plots/rootshoot_nofree.pdf")
dev.off()