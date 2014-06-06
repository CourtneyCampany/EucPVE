#TODO
#Create a plotting functions that will work with any data that has the treament means

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