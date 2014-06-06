#Leaf Aread and Seedling Mass

source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

#read in harvest data
source("read data scripts/harvest read data.R")

harvest_mass$drymass <- with(harvest_mass, leafmass+stemmass+Croot+Froot )

#determine dry mass of srl sample
srlmass$srl_mass <- with(srlmass, (srl_fw*((ss_dw/ss_fw))))

tree <- harvest_mass[,c(3,13,16)]
#add srl root ss mass
tree <- merge(tree, srlmass[,8:9])
tree$tree_dw <- with(tree, drymass+srl_mass)

#merge with plot summary
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

tree_stats <- merge(tree[,c(1:2,5)], plotsumm[,3:4])
tree_stats$volume <- as.factor(tree_stats$volume)

tree_stats2 <- tree_stats[order(tree_stats$volume),] 
#-----------------------------------------------------------------------------------------
#calculate overall means and means for each date

tree_agg <- summaryBy(leaf_area+tree_dw ~ volume, FUN=c(mean, se), 
                         keep.names=TRUE, data=tree_stats2)

#means(campaign)
tree_campaign <- summaryBy(leaf_area+tree_dw  ~ campaign + volume, FUN=c(mean, se), 
                              keep.names=TRUE, data=tree_stats2)

#linear model 
la_mass_fit <- lm(tree_dw~ leaf_area,data=tree_stats2)
summary(la_mass_fit)
anova(la_mass_fit)

#-----------------------------------------------------------------------------------------------
#panel graph for each relationship with raw, overall mean, and mean by date

#leaf area and seedling mass
windows(10,10)
par(mfrow=c(2,1), omi=c(1,0,0.5,0.5),mar=c(0,5,0,0))

#raw data
plot(tree_dw ~ leaf_area, data = tree_stats2, col=volume, pch=pchs[volume], axes=FALSE,xlab="", ylab="",
     xlim=c(0, 7500), ylim=c(0,300))
box()
axis(2, labels=TRUE)  
title(ylab=leaflab, mgp=ypos)
title(main="Raw Data", line=-1.5, font.main=1, adj=.1)
legend("bottomright", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')
d_ply(tree_stats2, .(volume), function(x) add_trend_line("leaf_area", "tree_dw", x, ))

#volume means
plot(tree_dw.mean ~ leaf_area.mean, data = tree_agg,col=volume, cex=2, pch=pchs[volume], xlab="", ylab="",
     xlim=c(0, 6000), ylim=c(0,300), axes=FALSE)
box()
axis(1, labels=TRUE)  
axis(2, labels=TRUE)  
title(ylab=leaflab, mgp=ypos)
mtext("Seedling Mass (g)", side=1, line=2.5)
title(main=volmean, line=-1.5, font.main=1, adj=.1)
ablineclip(la_mass_fit, x1=min(tree_agg$leaf_area.mean), x2=max(tree_agg$leaf_area.mean),lwd=2)


dev.copy2pdf(file="output/stats_plots/leafarea_mass.pdf")
dev.off()
#----------------------------------------------------------------------------------------------
#plot total mass

dataleg <- tree_agg[, c(1,3)]
names(dataleg)[2] <- "mass"
dataleg$mass <- round(dataleg$mass, 1)
dataleg$volume <- gsub("1000", "free", dataleg$volume)

windows()
par(lwd=1.25, mgp=c(2.5,1,0))
bargraph.CI(volume,tree_dw, data=tree_stats2,   border=palette(), col="grey98", 
            ylab = "Seedling Mass (g)",ylim = c(0, 200), xlab = "Pot Volume (l)")
box()
addtable2plot(.1,140,dataleg, display.colnames=FALSE,display.rownames=FALSE, hlines=TRUE,bty="o")

dev.copy2pdf(file="output/seedlingmass.pdf")
dev.off()


