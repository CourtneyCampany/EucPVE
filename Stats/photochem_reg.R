#source functions and packages
source("functions and packages/startscripts.R")
require(lmerTest)
require(visreg)

photo_chem <- read.csv("calculated data/Amax_chem.csv")
  #run volume format func
  photo_chem<- vollab_func(photo_chem)

#simple model mass based--------------------------------------------------------------------
#full and simple model to make predict plots with bins


#simple models first, #visulaise with visreg/effects pacakges
Amass_simple <- lm(A_mass~ starch*Nmass_notnc, data=photo_chem)
   anova(Amass_simple)
   summary(Amass_simple)
   visreg(Amass_simple)
 
#full model
Amass_full <- lmer(A_mass ~ starch+Nmass_notnc+starch:Nmass_notnc + (1|ID), data=photo_chem)
  anova(Amass_full)
  summary(Amass_full)
  c <- fixef(Amass_full)


#--------------------------------------------------------------------------------------
#calculate means(all dates)
photo_chem_agg <- summaryBy(A_mass+starch+Nmass_notnc ~ volume, FUN=c(mean, se), data=photo_chem)
volumeorder <- order(photo_chem_agg$A_mass.mean, by=photo_chem_agg$volume)
photo_chem_agg<-photo_chem_agg[volumeorder,]

#plot stuff
manX <- seq(0, .25, .05)
meanX <- seq(0, .20, .05)

nX <- seq(0, 015, .005)
nmeanX <- seq(0, .01, .005)
#---------------------------------------------------------------------------------------
#amax and N interaction

windows(16,6)
par(mfrow=c(1,3),omi=c(1,1,0.2,0.2), mar=c(0,0,0,0), cex=1.3)

#raw
plot(A_mass ~ Nmass_notnc, data = photo_chem, col=volume, xlim=c(0, .02),ylim=c(100, 800), pch=pchs[volume], 
     axes=FALSE, xlab="", ylab="")
box()
axis(1, at = nX, labels = nX)
axis(2, labels=TRUE)
mtext(Amasslab, side=2, line=2.5, cex=1.3)
title(main="Amax*Leaf N * volume, p < 0.0122", line=-1.5, cex.main=.75, font.main=1,adj=.05)

d_ply(photo_chem, .(volume), function(x) add_trend_line("Nmass_notnc", "A_mass", x, ))

#volume means
plot(A_mass.mean ~ Nmass_notnc.mean, data = photo_chem_agg, col=volume, xlim=c(0, .0125),ylim=c(100, 800),
     pch=pchs[volume], ylab="", axes=FALSE,cex=2)
box()
axis(1, at = nmeanX, labels = nmeanX)
mtext(nfree, side=1, line=2.5, cex=1.3)
title(main=volmean, line=-1.5, font.main=1, adj=.05, cex.main=.75)
legend("bottomright", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n', cex=.75)

#means by date
plot(A_mass.mean ~ Nmass_notnc.mean, data = photo_chem_campaign, col=volume, xlim=c(0, .0125),
     ylim=c(100, 800),pch=pch2[volume], cex=2, ylab="", axes=FALSE)
box()
axis(1, at = nmeanX, labels = nmeanX)
title(main=datemean, line=-1.5, font.main=1, adj=.05, cex.main=.75)
d_ply(photo_chem_campaign, .(volume), function(x) c(add_trend_line("Nmass_notnc.mean", "A_mass.mean", x),
                                    text(x$A_mass.mean~x$Nmass_notnc.mean, labels=unique(x$campaign), 
                                         font=2,cex=.75)))
dev.copy2pdf(file="output/stats_plots/amaxnfree.pdf")
dev.off()
#---------------------------------------------------------------------------------------------------

#amax and starch interaction

windows(16,6)
par(mfrow=c(1,3),omi=c(1,1,0.2,0.2), mar=c(0,0,0,0), cex=1.3)

#raw
plot(A_mass ~ starch, data = photo_chem, col=volume, xlim=c(0, .3),ylim=c(100, 800), pch=pchs[volume], 
     axes=FALSE, xlab="", ylab="")
box()
axis(1, at = manX, labels = manX)
axis(2, labels=TRUE)
mtext(Amasslab, side=2, line=2.5, cex=1.3)
title(main="Amax*starch* volume, p < 0.0001", line=-1.5, font.main=1, adj=.1, cex.main=.75)

d_ply(photo_chem, .(volume), function(x) add_trend_line("starch", "A_mass", x, ))

#volume means
plot(A_mass.mean ~ starch.mean, data = photo_chem_agg, col=volume, xlim=c(0, .2),ylim=c(100, 800),
     pch=pchs[volume], ylab="", axes=FALSE,cex=2)
box()
axis(1, at = meanX, labels = meanX)
mtext(starchlab, side=1, line=2.5, cex=1.3)
title(main=volmean, line=-1.5, font.main=1, adj=.1, cex.main=.75)
legend("topright", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n', cex=.75)
#means by date
plot(A_mass.mean ~ starch.mean, data = photo_chem_campaign, col=volume, xlim=c(0, .2),
     ylim=c(100, 800),pch=pch2[volume], cex=2, ylab="", axes=FALSE)
box()
axis(1, at = meanX, labels = meanX)
title(main=datemean, line=-1.5, font.main=1, adj=.1, cex.main=.75)
d_ply(photo_chem_campaign, .(volume), function(x) c(add_trend_line("starch.mean", "A_mass.mean", x),
                                                    text(x$A_mass.mean~x$starch.mean, labels=unique(x$campaign), 
                                                         font=2,cex=.75)))
dev.copy2pdf(file="output/stats_plots/amaxstarch.pdf")
dev.off()


