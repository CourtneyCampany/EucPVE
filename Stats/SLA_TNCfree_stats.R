#Does TNC-free LMA change through time within a container volume?

#source functions and packages
source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

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
TNC <- read.csv("raw data/leaf_tnc.csv")

#run function to add campaign Date
TNC <- add_campaign_noID(TNC)
  TNC$volume <- gsub("free", "1000", TNC$volume)
  TNC$volume <- as.factor(TNC$volume)
  TNC$ID <- gsub("'", "", TNC$ID)
  TNC$tnc_mgperg <- with(TNC, starch_mgperg+sugars_mgperg)


#merge tnc with leaf data
SLA_TNCfree <- merge(lma[,c(1,4,6:9)], TNC,  all=TRUE)
  SLA_TNCfree$mass_noTNC <- with(SLA_TNCfree, mass-((mass*tnc_mgperg)/1000))
  #TNC free sla and lma
  SLA_TNCfree$sla_free <- with(SLA_TNCfree, area/mass_noTNC)
  SLA_TNCfree$lma_free <- with(SLA_TNCfree, mass_noTNC/area)

#remove missing values
SLA_TNCfree_noNA <- subset(SLA_TNCfree, !is.na(sla_free))
  row.names(SLA_TNCfree_noNA) <- NULL
#convert tnc to mass and % basis
  SLA_TNCfree_noNA$leafstarch <- with(SLA_TNCfree_noNA, (mass*starch_mgperg)/1000)
  SLA_TNCfree_noNA$leafsugar <- with(SLA_TNCfree_noNA, (mass*sugars_mgperg)/1000)
  SLA_TNCfree_noNA$percstarch <- with(SLA_TNCfree_noNA, (leafstarch/mass)*100)
  SLA_TNCfree_noNA$percsugar <- with(SLA_TNCfree_noNA, (leafsugar/mass)*100)

#---------------------------------------------------------------------------------------------
#new dfr creating starch and sugar content per leaf for use in other analyses
TNC_content <- SLA_TNCfree_noNA[, c(1,3:6, 9:15)]

write.csv(TNC_content, "calculated data/TNC_content", row.names=FALSE)
#---------------------------------------------------------------------------------------------

leaf_TNCfree <- subset(SLA_TNCfree_noNA, select = c("ID", "campaign", "volume", "sla_free", "lma_free","Date"))
volorder<-order(leaf_TNCfree$volume, by=leaf_TNCfree$Date)
leaf_TNCfree <- leaf_TNCfree[volorder,]

#treatment means
sla_tnc_campaign <- summaryBy(sla_free+lma_free ~ volume+Date, data=leaf_TNCfree, FUN=c(mean, se))

#---------------------------------------------------------------------------------------------------
#Visualize changes in SLA across campaigns (Date messes things up so stay with campaign# (interval=2wks))
require(plotBy)
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
#---------------------------------------------------------------------------------------------------
#LMA

windows()
par(mfrow=c(3,3))
lma_TNCfree_plots<- dlply(leaf_TNCfree, .(volume), function(x) c(plotBy(lma_free ~ Date | ID, how="col", type='o', 
                          legend=F, lty=1,col=x$volume, ylim=c(0, .02), xlab="", pch=16, ylab="", data=x), 
                          title(main=paste("volume = ", unique(x$volume)), line=-1.5, font.main=1, 
                          adj=.05, cex.main=1),title(ylab=lmalab, mgp=ypos)))

dev.copy2pdf(file="output/stats_plots/lma_TNCfree_raw.pdf")
dev.off()

#Plot treatment means across campaigns
windows()
plot(lma_free.mean ~ Date, data = sla_tnc_campaign, col=volume, cex=2, pch=pchs[volume], 
     ylim=c(0.0, 0.015), ylab="", xlab="")
title(ylab=lmalab, mgp=ypos)
title(main=datemean, line=-1.5, font.main=1, adj=.05, cex.main=1)
d_ply(sla_tnc_campaign, .(volume), function(x) add_trend_line("Date", "lma_free.mean", x, ))
legend("bottomright", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')

dev.copy2pdf(file="output/stats_plots/lmaTNCfree_means.pdf")
dev.off()




#----------------------------------------------------------------------------------------------
#fit seperate linear regression models for each volume through time for each pot
LMlist_func <- function(dfr){
  
  sub_dfr <- subset(dfr, select = c("ID", "campaign", "sla_free"))
  sla.new <- groupedData(sla_free ~ campaign|ID, data=sub_dfr)
  models_ID <- lmList(sla_free ~ campaign,  data=sla.new)
}

#run LMlist for all pots, split by volume
slafree_stats_ls<- dlply(SLA_TNCfree_lm, .(volume) , LMlist_func)    

#return coefs and CI(need to add a 'ID' name column)
slafree_coefs <- ldply(slafree_stats_ls, coef)
#plot confidence intervals
#slafree_CI_plots <-  lapply(slafree_stats_ls, function(x) plot(intervals(x)))
lapply(slafree_stats_ls, function(x) plot(intervals(x)))



