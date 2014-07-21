source("functions and packages/startscripts.R")

# Read data
aci <- read.csv("raw data/ACi#1.csv")
aci$ID <- paste(aci$plot, aci$pot, sep = "-")
# new variable, treatment, based on 'pot volume'
aci$treatment <- as.factor(aci$Volume)

#Smooth lines, one for each treatment
#plot one smooth line per treatment (averaged across all plants). 
#take a subset of the data by treatment rather than ID.

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

#dev.copy2pdf(file= "output/ACI_volume.pdf")
#dev.off()