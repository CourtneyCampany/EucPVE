source("functions and packages/startscripts.R")

leafarea_time <- read.csv("calculated data/cumulative leaf area.csv")
  leafarea_time <-datevol_func (leafarea_time)
leafarea_time2 <- subset(leafarea_time,volume != "1000")


#2panelplot------------------------------------------------------------------------------------------

windows(12,10)
par(mfrow=c(2,1), omi=c(1,0,0.5,0.5),mar=c(0,5,0,0),mgp = c(2.5, 1, 0))

plot(canopysqm.mean ~ Date, data=leafarea_time, axes=FALSE,xlab="", ylab="",
     type='n', ylim=c(0,.6))
box()
axis(2, labels=TRUE)  
title(ylab=LAm2, mgp=ypos)
with(leafarea_time, arrows(Date, canopysqm.mean, Date, canopysqm.mean+canopysqm.se, angle=90, 
                          col=volume,length=0.03))
with(leafarea_time, arrows(Date, canopysqm.mean, Date, canopysqm.mean-canopysqm.se, angle=90, 
                          col=volume,length=0.03))
d_ply(leafarea_time, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                                                  col=x$volume, type="b", pch = pchs[x$volume],cex=PTcex,))
legend("topleft", leglab, pch=pchs,text.font=3, inset=0.02, title=expression(Pot~volume~(l)), 
       col=palette(), bty='n')
#no free
plot(canopysqm.mean ~ Date, data=leafarea_time2, xlab="", ylab=LAm2,ylim=c(0,.2))
  with(leafarea_time2, arrows(Date, canopysqm.mean, Date, canopysqm.mean+canopysqm.se, angle=90, 
                           col=volume,length=0.03))
  with(leafarea_time2, arrows(Date, canopysqm.mean, Date, canopysqm.mean-canopysqm.se, angle=90, 
                           col=volume,length=0.03))
  d_ply(leafarea_time2, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                        col=x$volume, type="b", pch = pchs[x$volume],cex=PTcex,))

title(main="No Free", line=-1.5, font.main=1)
legend("topleft", legend, leglab2, pch=16,text.font=3, inset=0.02, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n')

dev.copy2pdf(file= "output/leafarea_2panel.pdf")
dev.off()

