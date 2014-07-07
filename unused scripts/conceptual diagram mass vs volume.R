
# The non-rectangular hyperbola (for all volumes)

aqfun <- function(x, Amax, phi, theta) {
  (phi * x + Amax - ((phi * x+ Amax)^2 - 4 * theta * phi * x * Amax)^0.5)/(2 *theta)
}

#PLOT colors
leglab <- c(5, 10, 15, 20, 25, 35, "free")

gradient <- colorRampPalette(c("red", "blue"))
color <- palette(gradient(7))


###N limitation
windows(8,6)
par(oma = c(0, 1, 0, 0),
    mgp = c(1, 1, 0), 
    cex.lab=1.2)

curve(aqfun(x, Amax= 3500, phi = 0.5, theta = 0.8), from = 0, to = 35000,
     type='n',
     ylab=expression(Mass),  
     xlab=expression(Time),
     ylim=c(0,4000),
     axes=FALSE)

  curve(aqfun(x, Amax= 3650, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[7], lwd=2,add=TRUE)
  curve(aqfun(x, Amax= 3500, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[6], lwd=2,add=TRUE)
  curve(aqfun(x, Amax= 2750, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[6],lwd=2, add=TRUE)
  curve(aqfun(x, Amax= 2250, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[4], lwd=2,add=TRUE)
  curve(aqfun(x, Amax= 1750, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[3], lwd=2,add=TRUE)
  curve(aqfun(x, Amax= 1250, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[2], lwd=2,add=TRUE)
  curve(aqfun(x, Amax= 750, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[1], lwd=2,add=TRUE)

legend("topleft", legend,leglab, lty = 1, lwd = 2, col = palette(), 
       title = expression(Pot ~ volume ~ (l)), inset = 0.01, bty = "n")

title(main = expression(italic(Resource~Limitation)), cex.main = 1.2, line=-1.5)

box()

dev.copy2pdf(file= "resourcelimit.pdf")
dev.off()


### barrier sensing

windows(8,6)
par(oma = c(0, 1, 0, 0),
    mgp = c(1, 1, 0), 
    cex.lab=1.2)

curve(aqfun(x, Amax= 3500, phi = 0.5, theta = 0.8), from = 0, to = 35000,
      type='n',
      ylab=expression(Mass),  
      xlab=expression(Time),
      ylim=c(0,4000),
      axes=FALSE)

curve(aqfun(x, Amax= 3650, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[7], lwd=2,add=TRUE)
curve(aqfun(x, Amax= 1650, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[6], lwd=2,add=TRUE)
curve(aqfun(x, Amax= 1350, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[5],lwd=2, add=TRUE)
curve(aqfun(x, Amax= 1200, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[4], lwd=2,add=TRUE)
curve(aqfun(x, Amax= 1050, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[3], lwd=2,add=TRUE)
curve(aqfun(x, Amax= 900, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[2], lwd=2,add=TRUE)
curve(aqfun(x, Amax= 750, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = color[1], lwd=2,add=TRUE)

legend("topleft", legend,leglab, lty = 1, lwd = 2, col = palette(), 
       title = expression(Pot ~ volume ~ (l)), inset = 0.01, bty = "n")

title(main = expression(italic(Barrier~Sensing)), cex.main = 1.2, line=-1.5)
box()


dev.copy2pdf(file= "barriersensing.pdf")
dev.off()
#--------------------------------

#two panel hypothesis plot
#PLOT
windows(8,12)
par(
  #cex.axis=0.9,  # axis label size
    #cex.lab=1.3,
    mfrow=c(2,1),  # rows and columns of plots
    omi=c(1,0.1,0.1,0.1),  # outer margin (inches)
    mar=c(0,4,0,0),
    mgp = c(2, 1, 0))  # margin around plots (they are tight together)   


#Nitrogen Limitation
curve(aqfun(x, Amax= 3500, phi = 0.5, theta = 0.8), from = 0, to = 35000,
      type='n', 
      ylim=c(0,4000),
      ann=FALSE,
      axes=FALSE) 

curve(aqfun(x, Amax= 3650, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "black", lwd=2,add=TRUE)
curve(aqfun(x, Amax= 3500, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "red", lwd=2,add=TRUE)
curve(aqfun(x, Amax= 2750, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "orange",lwd=2, add=TRUE)
curve(aqfun(x, Amax= 2250, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "yellow", lwd=2,add=TRUE)
curve(aqfun(x, Amax= 1750, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "green", lwd=2,add=TRUE)
curve(aqfun(x, Amax= 1250, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "blue", lwd=2,add=TRUE)
curve(aqfun(x, Amax= 750, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "violet", lwd=2,add=TRUE)

legend("topleft", legend,leglab, lty = 1, lwd = 2, col = palette(), 
       title = expression(Pot ~ volume ~ (l)), inset = 0.01, bty = "n")
#text(4.5,5.5,"Nitrogen Limitation")
box()


### barrier sensing
curve(aqfun(x, Amax= 3500, phi = 0.5, theta = 0.8), from = 0, to = 35000,
      type='n',
      ylim=c(0,4000),
      ann=FALSE,
      axes=FALSE)

curve(aqfun(x, Amax= 3650, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "black", lwd=2,add=TRUE)
curve(aqfun(x, Amax= 1650, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "red", lwd=2,add=TRUE)
curve(aqfun(x, Amax= 1350, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "orange",lwd=2, add=TRUE)
curve(aqfun(x, Amax= 1200, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "yellow", lwd=2,add=TRUE)
curve(aqfun(x, Amax= 1050, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "green", lwd=2,add=TRUE)
curve(aqfun(x, Amax= 900, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "blue", lwd=2,add=TRUE)
curve(aqfun(x, Amax= 750, phi = 0.5, theta = 0.8), from = 0, to = 35000, col = "violet", lwd=2,add=TRUE)

#text(4.5,5.5,"Nitrogen Limitation")
box()

mtext("Time", side=1, outer=TRUE, line=3, cex=1.3)
mtext("Mass", side=2, outer=TRUE, line=0.1, cex=1.3)
