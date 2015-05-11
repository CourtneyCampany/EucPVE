source("functions and packages/startscripts.R")

#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
  plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

##SLA[tncfree]------------------------------------------------------------------------------------------

sla_tnc <- read.csv("calculated data/TNC_content")
  sla_tnc$volume <- as.factor(sla_tnc$volume) 
  sla_tnc$Date <- as.Date(sla_tnc$Date)
  sla_tnc$sla <- with(sla_tnc, area/mass)
  sla_tnc$lma <- with(sla_tnc, mass/area)

leaf_TNCfree <- subset(sla_tnc, select = c("ID", "volume", "sla_free", "lma_free","sla", "lma","Date"))
  volorder<-order(leaf_TNCfree$volume, by=leaf_TNCfree$Date)
  leaf_TNCfree <- leaf_TNCfree[volorder,]

#treatment means
sla_tnc_campaign <- summaryBy(sla_free+lma_free+ sla+ lma ~ volume+Date, data=leaf_TNCfree, FUN=c(mean, se))
#treatment means
sla_tnc_agg <- summaryBy(sla_free+lma_free+ sla+ lma~volume, data=leaf_TNCfree, FUN=c(mean, se))

#SRL------------------------------------------------------------------------------------------------------
srl <- read.csv("raw data/SRLmass.csv")
  srl$ID <- paste(srl$plot, srl$pot, sep = "-")
  srl <- merge(srl, plotsumm[3:4], all=TRUE)
  srl <- subset(srl, !is.na(volume))
  srl$SRL <- with(srl, (total_length_cm/100)*(srl_fw*(ss_dw/ss_fw)))
  srl$volume <- as.factor(srl$volume)
  row.names(srl) <- NULL
#srl <- vollab_func(srl)

##srl_agg for paper table
srl_agg <- summaryBy(SRL ~ volume, data=srl, FUN=c(mean, se))
write.csv(srl_agg, "calculated data/srl_means.csv", row.names=FALSE)

##Stats for sla and srl--------------------------------------------------------------------------------------
require(nlme)
require(visreg)
library(multcomp)

srl$volume <-  relevel(srl$volume, ref="1000")

#srl (not different)
srl_container <- lme(SRL ~ volume, random= ~1|ID, data=srl)
  anova(srl_container)
  summary(srl_container)
  visreg(srl_container)
  
  tukey_srl<- glht(srl_container, linfct = mcp(volume = "Tukey"))
  srl_siglets <-cld(tukey_srl)
  srl_siglets2 <- srl_siglets$mcletters$Letters
  write.csv(srl_siglets2, "master_scripts/sigletters/sigletts_plant/sl_srl.csv", row.names=FALSE)  
  
  
  


#sla
sla_lm <- lme(sla_free ~ volume, random= ~1|ID, data=sla_tnc)
  anova(sla_lm)
  summary(sla_lm)

  tukey_sla<- glht(sla_lm, linfct = mcp(volume = "Tukey"))
  siglets_sla <- cld(tukey_sla)
  visreg(sla_lm)

#when did sla free happen?
sla_lm2 <- lme(sla_free ~ volume, random= ~1|ID, data=sla_tnc, subset=Date=="2013-03-07")
  anova(sla_lm2)
  visreg(sla_lm2)
  tukey_srl2<- glht(sla_lm2, linfct = mcp(volume = "Tukey"))
  cld(tukey_srl2)


#2panel plot of leaf and root morphology------------------------------------------------------------------
SigLetters <- siglets_sla$mcletters$Letters
#can use sla through time (might be better)

#Plot objects
# xAT <- seq.Date(from=as.Date("2013-1-1"), length=25, by="week")

# plot(sla_free.mean ~ Date, data = sla_tnc_campaign, cex=2,col=volume, pch=pchs[volume], 
#      ylim=c(50,250), ylab="", xlab="", xaxt="n")
#   title(ylab=slalab, mgp=ypos)
#   axis.Date(1, at=xAT, labels=TRUE)  
# d_ply(sla_tnc_campaign, .(volume), function(x) add_trend_line("Date", "sla_free.mean", x, ))
# legend("topright", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n', cex=1.3)


windows(14,8)
###remove colors for not (col=palette())
par(cex.axis=1.3, cex.lab=1.3,mfrow=c(1,2),oma=c(0.1,0.1,0.1,0.1) )
#panel2 (srl)
bar(SRL, volume, srl,half.errbar=FALSE, ylab="", ylim=c(0,.8), names.arg=leglab,col="grey", legend=FALSE, xlab="")
title(ylab=srllab, mgp=c(2.4,1,0))

#panel1 (sla)
bar(sla_free, volume, sla_tnc,half.errbar=FALSE,ylim=c(0,180),ylab="",names.arg=leglab,col="grey", legend=FALSE, xlab="")
title(ylab=slalab, mgp=c(2.4,1,0))
mtext("Soil Volume  (L)", side=1, adj=-.325, cex=1.3, line=2.75)
text(c(.7,1.9,3.1,4.3,5.5,6.75,7.9), 50, SigLetters, cex=1.3)
