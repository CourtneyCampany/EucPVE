#Does N change through time within a container volume? both area and mass basis?

#source functions and packages
source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

#read in calculated lma data
lma <- read.csv("calculated data/leafmassarea.csv")

#read in plot design, lma data, merge
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#read in percent CN data
leafCN <- read.csv("raw data/leafCN.csv")

#run formatting functions for leafCN/TNC raw data and campaign date
CN_leaf <- leafCN_format(leafCN)
CN_leaf <- add_campaign_date(CN_leaf)
CN_leaf <- merge(CN_leaf, plotsumm[3:4], all=TRUE)

#merge CN data with leaf lma data
leaf_chem <- merge(CN_leaf, lma, all=TRUE)
leaf_chem$volume <- as.factor(leaf_chem$volume)

#remove missing values, add Date
leafchem_noNA <- subset(leaf_chem, !is.na(Nperc))
leafchem_noNA <- add_campaign_date(leafchem_noNA)
row.names(leafchem_noNA)<-NULL

#calculate N content of leaves (g m-2)
leafchem_noNA$Nmass <- with(leafchem_noNA, mass*Nperc)
#leafchem_noNA$Nmassleaf <- with(leafchem_noNA, Nmass/mass)
#leafchem_noNA$Nmassarea <- with(leafchem_noNA, (Nmass/area)*10000)
leafchem_noNA$Narea <- with(leafchem_noNA, (area/Nmass)/10000) #in m2
leafchem_noNA$Npercentage <- leafchem_noNA$Nperc*100

#write for use with A data
leafchemsave <- leafchem_noNA[, c(1:5,10:16)]
write.csv(leafchemsave, "calculated data/leaf N content.csv", row.names=FALSE)

#means
Nleaf_means<- summaryBy(mass + Npercentage+Nmass+Narea ~ volume+Date, 
                          data=leafchem_noNA, FUN=c(mean, se))

-----------------------------------------------------
#Visualize changes in Nmass and Narea raw data across campaigns (uses campaign# (interval=2wks))

#plotBY extras
#enhance="lm",

#raw
windows()
par(mfrow=c(3,3))
leafN_area<- dlply(leafchem_noNA, .(volume), function(x) c(plotBy(Narea ~ Date | ID, how="col", type='o', 
                   legend=F, col=x$volume,lty=1, ylim=c(0,3),ylab="", xlab="", data=x), 
                   title(main=paste("volume = ", unique(x$volume)), line=-1.5, font.main=1, adj=.05, cex.main=1),
                   title(ylab=narealab, mgp=ypos)))
dev.copy2pdf(file="output/stats_plots/Narea_raw.pdf")
dev.off()

#----------------------------------------------------------------------------------
#treatment means of Nmass, leaf mass, Narea, and N%

windows()
plot(Npercentage.mean ~ Date, data = Nleaf_means, col=volume, cex=2, pch=pchs[volume], 
     ylim=c(0,4), ylab="", xlab="")
title(ylab="Leaf Nitrogen (%)", mgp=ypos)
title(main=datemean, line=-1.5, font.main=1, adj=.05, cex.main=1)
d_ply(Nleaf_means, .(volume), function(x) add_trend_line("Date", "Npercentage.mean", x, ))
legend("topright", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')

dev.copy2pdf(file="output/stats_plots/Npercentage_means.pdf")
dev.off()

#leafmass
windows()
plot(mass.mean ~ Date, data = Nleaf_means, col=volume, cex=2, pch=pchs[volume], 
     ylim=c(0,.4), ylab="", xlab="")
title(ylab="leafmass (g)", mgp=ypos)
title(main=volmean, line=-1.5, font.main=1, adj=.05, cex.main=1)
d_ply(Nleaf_means, .(volume), function(x) add_trend_line("Date", "mass.mean", x, ))
legend("bottomleft", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')

dev.copy2pdf(file="output/stats_plots/leafmass_means.pdf")
dev.off()

#N on area basis
windows()
plot(Narea.mean ~ Date, data = Nleaf_means, col=volume, cex=2, pch=pchs[volume], 
     ylim=c(0,1.3), ylab="", xlab="")
title(ylab=narealab, mgp=ypos2)
title(main=datemean, line=-1.5, font.main=1, adj=.05, cex.main=1)
d_ply(Nleaf_means, .(volume), function(x) add_trend_line("Date", "Narea.mean", x, ))
legend("bottomleft", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')

dev.copy2pdf(file="output/stats_plots/Narea_means.pdf")
dev.off()


N_end <- subset(leafchem_noNA, Date == "2013-05-16")

#windows()

ypos2 <- c(.75,1,0)


png(filename = "output/presentations/LeafN_area.png", width = 3.25, height = 2.25, units = "in", res= 400, bg = "transparent")
par(cex.axis=.5, cex.lab=.5, tck=.02)
bar(Nmass, c(volume), leafchem_noNA, col=palette(), half.errbar=FALSE, xlab="", 
    legend=FALSE, ylim=c(0, 0.008), ylab="", mgp = c(3, .1, 0))
#legend("topleft", leglab, pch=15,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')
title(ylab=nitro, mgp=ypos2)
title(main="Harvest", line=-1, cex.main=0.5, adj=.1)

dev.off()


#----------------------------------------------------------------------------------
#fit seperate linear regression models for each volume through time for each pot
masslist_func <- function(dfr){
  
  sub_dfr <- subset(dfr, select = c("ID", "campaign", 'Nmass'))
  dfr.new <- groupedData(Nmass ~ campaign|ID, data=sub_dfr)
  models_ID <- lmList(Nmass ~ campaign,  data=dfr.new)
}

arealist_func <- function(dfr){
  
  sub_dfr <- subset(dfr, select = c("ID", "campaign", 'Narea'))
  dfr.new <- groupedData(Narea ~ campaign|ID, data=sub_dfr)
  models_ID <- lmList(Narea ~ campaign,  data=dfr.new)
}

#run LMlist for all pots, split by volume
Nmass_lm_ls<- dlply(Nmass_lm , .(volume) , masslist_func) 
Narea_lm_ls<- dlply(Narea_lm, .(volume) , arealist_func)    

#return coefs and CI(need to add a 'ID' name column)
Nmass_coefs <- ldply(Nmass_lm_ls, coef)
Narea_coefs <- ldply(Narea_lm_ls, coef)
#plot confidence intervals
#Nmass_CI_plots <-  lapply(Nmass_lm_ls, function(x) plot(intervals(x)))
#Narea_CI_plots <-  lapply(Narea_lm_ls, function(x) plot(intervals(x)))
lapply(Nmass_lm_ls, function(x) plot(intervals(x)))
lapply(Narea_lm_ls, function(x) plot(intervals(x)))






