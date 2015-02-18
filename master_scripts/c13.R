source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

c13 <- read.csv("raw data/harvest_c13.csv")


#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#merge with plot summary
c13 <- merge(c13[,c(2,4)], plotsumm[,3:4], by ="ID")
c13$volume <- as.factor(c13$volume)


windows(7,5)
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
bar(d13c, volume, c13,half.errbar=FALSE, ylim=c(-32,0), col="grey",, legend=FALSE, xlab=vollab, ylab="")
title(ylab=c13lab, mgp=ypos)

dev.copy2pdf(file= "master_scripts/manuscript_figs/leafc13.pdf")
dev.off()
