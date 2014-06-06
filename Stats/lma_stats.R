
#Does LMA change through time within a container volume?

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
  
#run function to add campaign dates, caluclate lma(g m-2 and sla m g-1)
lma <- add_campaign_date(lma)
lma$sla <- with(lma, (area/mass)/10000)
lma$massarea <- with(lma, (mass/area)*10000)
  
#remove missing values
lma_noNA <- subset(lma, !is.na(sla))

write.csv(lma_noNA, "calculated data/leafmassarea.csv", row.names=FALSE)
  
#subset with only sla and parameters of lm
lma_lm <- subset(lma_noNA, select = c("ID", "volume", "campaign", "sla", "massarea"))
lma_lm <- add_campaign_date(lma_lm)
#treatment means
sla_means<- summaryBy(sla +massarea~ volume+Date, data=lma_lm, FUN=c(mean, se))

#----------------------------------------------------------------------------------------------
#Visualize changes in SLA across campaigns (raw)

#sla
windows()
par(mfrow=c(3,3))
SLA_plots<- dlply(lma_lm, .(volume), function(x) c(plotBy(sla ~ Date | ID, how="col", type='o', 
            legend=F, lty=1,col=x$volume, ylim=c(0, 300), xlab="", pch=16, ylab="", data=x), 
            title(main=paste("volume = ", unique(x$volume)), line=-1.5, font.main=1, 
             adj=.05, cex.main=1),title(ylab=slalab, mgp=ypos)))

                    
dev.copy2pdf(file="output/stats_plots/sla_raw.pdf")
dev.off()

#lma
windows()
par(mfrow=c(3,3))
lma_plots<- dlply(lma_lm, .(volume), function(x) c(plotBy(massarea ~ Date | ID, how="col", type='o', 
            legend=F, lty=1,col=x$volume, ylim=c(0, .02), xlab="", pch=16, ylab="", data=x), 
            title(main=paste("volume = ", unique(x$volume)), line=-1.5, font.main=1, 
            adj=.05, cex.main=1),title(ylab=lmalab, mgp=ypos)))


dev.copy2pdf(file="output/stats_plots/lma_raw.pdf")
dev.off()

#sla and lma treatment means plots---------------------------------------------------------------------

#Plot treatment means across campaigns
#sla
windows()
plot(sla.mean ~ Date, data = sla_means, col=volume, cex=2, pch=pchs[volume], 
     ylim=c(0,250), ylab="", xlab="")
title(ylab=slalab, mgp=ypos)
title(main=datemean, line=-1.5, font.main=1, adj=.05, cex.main=1)
d_ply(sla_means, .(volume), function(x) add_trend_line("Date", "sla.mean", x, ))
legend("topright", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')

dev.copy2pdf(file="output/stats_plots/SLA_means.pdf")
dev.off()

#lma
windows()
plot(massarea.mean ~ Date, data = sla_means, col=volume, cex=2, pch=pchs[volume], 
     ylim=c(0,.015), ylab="", xlab="")
title(ylab=lmalab, mgp=ypos)
title(main=datemean, line=-1.5, font.main=1, adj=.95, cex.main=1)
d_ply(sla_means, .(volume), function(x) add_trend_line("Date", "massarea.mean", x, ))
legend("bottomright", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')

dev.copy2pdf(file="output/stats_plots/lma_means.pdf")
dev.off()









#can add abline function to add trendlines (plotBy)
enhance="lm"

#----------------------------------------------------------------------------------------------
#fit seperate linear regression models for each volume through time for each pot
LMlist_func <- function(dfr){
  
  sub_dfr <- subset(dfr, select = c("ID", "campaign", "sla"))
  lma.new <- groupedData(sla ~ campaign|ID, data=sub_dfr)
  models_ID <- lmList(sla ~ campaign,  data=lma.new)
}

#run LMlist for all pots, split by volume
sla_stats_ls<- dlply(lma_lm, .(volume) , LMlist_func)    

#return coefs and CI(need to add a 'ID' name column)
sla_coefs <- ldply(sla_stats_ls, coef)
#plot confidence intervals
sla_CI_plots <-  lapply(sla_stats_ls, function(x) plot(intervals(x)))

#-----------------------------------------------------------------------------------------------

#fitting a mixed effects model with plot as random factor

#reintroduce plot to dataset
IDsp <- strsplit(lma_lm$ID, "")
plot <- sapply(IDsp, "[", 1)

lma_lme <- cbind(lma_lm, plot)

#test wiht susbet of one volume
sla_25 <- lma_lme[lma_lme$volume == "25",]

#random intercept
lme_25 <- lme(sla ~ campaign, data=sla_25, random=~1|plot)
summary(lme_25)
#random effects of both slope and intercept
lme_25_re <- update(lme_25, random = ~campaign|plot)
summary(lme_25_re)
#compare models
anova(lme_25, lme_25_re)
#extract coefs
coef(lme_25)

#maximum likehood model
lme_25_ml <- update(lme_25, method="ML")

#compare two model fits
plot(compareFits(coef(lme_25), coef(lme_25_ml)))
plot( augPred(lme_25_ml), aspect="xy", grid=T)

