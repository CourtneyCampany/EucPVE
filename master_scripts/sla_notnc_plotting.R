#Does TNC-free LMA change through time within a container volume?

#source functions and packages
source("functions and packages/startscripts.R")

#read in plot design, lma data, merge
plotsumm <- read.csv("raw data/plot_summary.csv")
  plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

lma <- read.csv("raw data/seedling leaf mass area.csv")
  lma$ID <- paste(lma$plot, lma$pot, sep = "-")
lma <- merge(lma, plotsumm[3:4], all=TRUE)
  lma$volume <- as.factor(lma$volume)

#run function to add campaign dates, caluclate sla
lma <- add_campaign_date(lma)

#remove missing values
lma_noNA <- subset(lma, !is.na(area))

#read in TNC data, substract mass of TNC from mass of leaves and then calculate TNC-free SLA
#Read in spot measurements and merge plot design
TNC <- read.csv("raw data/leaf_TNC.csv")
  #run function to add campaign Date
  TNC <- add_campaign_noID(TNC)
  TNC$volume <- as.factor(TNC$volume)
  TNC$ID <- as.character(TNC$ID)
  TNC$tnc_mgperg <- with(TNC, starch_mgperg+sugars_mgperg)

SLA_TNCfree <- merge(lma, TNC, by=c("Date", "ID"))

SLA_TNCfree$mass_noTNC <- with(SLA_TNCfree, mass-((mass*tnc_mgperg)/1000))
  #TNC free sla and lma
  SLA_TNCfree$sla_free <- with(SLA_TNCfree, area/mass_noTNC)
  SLA_TNCfree$lma_free <- with(SLA_TNCfree, mass_noTNC/area)

#remove missing values
SLA_TNCfree_noNA <- subset(SLA_TNCfree, !is.na(sla_free))


leaf_TNCfree <- subset(SLA_TNCfree_noNA, select = c("ID", "campaign", "volume", "sla_free", "lma_free","Date"))
  volorder<-order(leaf_TNCfree$volume, by=leaf_TNCfree$Date)
  leaf_TNCfree <- SLA_TNCfree[volorder,]

#treatment means
sla_tnc_campaign <- summaryBy(sla_free+lma_free ~ volume+Date, data=leaf_TNCfree, FUN=c(mean, se))

#---------------------------------------------------------------------------------------------------
#Visualize changes in SLA across campaigns (Date messes things up so stay with campaign# (interval=2wks))

#SLA

#Plot Raw specific leaf area
windows()
par(mfrow=c(3,3))
SLA_TNCfree_plots<- dlply(leaf_TNCfree, .(volume), function(x) c(plotBy(sla_free ~ Date | ID, how="col", type='o', 
                                                                        legend=F, lty=1,col=x$volume, ylim=c(0, 300), xlab="", pch=16, ylab="", data=x), 
                                                                 title(main=paste("volume = ", unique(x$volume)), line=-1.5, font.main=1, 
                                                                       adj=.05, cex.main=1),title(ylab=slalab, mgp=ypos)))

dev.copy2pdf(file="output/stats_plots/SLA_TNCfree_raw.pdf")
dev.off()

#Plot treatment means across campaigns
windows()
plot(sla_free.mean ~ Date, data = sla_tnc_campaign, col=volume, cex=2, pch=pchs[volume], 
     ylim=c(50,250), ylab="", xlab="")
title(ylab=slalab, mgp=ypos)
title(main=datemean, line=-1.5, font.main=1, adj=.05, cex.main=1)
d_ply(sla_tnc_campaign, .(volume), function(x) add_trend_line("Date", "sla_free.mean", x, ))
legend("topright", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')

dev.copy2pdf(file="output/stats_plots/SLA_TNCfree_means.pdf")
dev.off()
