#this script is for sourcing the allocation plots into master
# source("functions and packages/startscripts.R")

library(smatr)

#harvest mass dfr
seedlingmass<- read.csv("calculated data/seedling mass.csv") 
  seedlingmass$Msr <- with(seedlingmass, stemmass + Croot + fineroot)
  #volume as factor after new variable calculations
  seedlingmass$volume <- as.factor(seedlingmass$volume)

#treatment means
mass_agg <- summaryBy(.~volume, data=seedlingmass, FUN=mean, keep.names=TRUE)

# this shows that LMF is higher for free seedlings because they were bigger.
Mf_mod2 <- sma(leafmass ~ Msr * volume, log="xy", data=seedlingmass)
#summary(Mf_mod2)

####allocation by components

#new dfr with only components
masses <- mass_agg[, c(1:2, 4:5,7)]
  masses$volume <- as.factor(masses$volume)

mass_perc <- data.frame(volume=as.factor(mass_agg$volume), 
                        stem_perc = with(mass_agg, stemmass/totalmass),
                        leaf_perc = with(mass_agg, leafmass/totalmass),
                        cr_perc = with(mass_agg, Croot/totalmass),
                        fr_perc = with(mass_agg, fineroot/totalmass))


mass_perc2 <- mass_perc[,2:5]
i <- c(4,3,1,2) #correct order of variables
treecols <- c("darkgoldenrod4","lightgoldenrod3",  "olivedrab","forestgreen")
treecols2 <- c("forestgreen","olivedrab", "lightgoldenrod3","darkgoldenrod4")
treelab <- c("Leaf", "Stem", expression(Root[coarse]), expression(Root[fine]))


ratio <- subset(seedlingmass, select = c("ID", "volume", "fineroot", "leafmass", "root", "shoot"))
  ratio$volume <- as.factor(ratio$volume)

ratio_lm <- lm(log10(leafmass) ~ log10(fineroot), data=ratio)
#confint(ratio_lm)

#treatment means
ratio_agg <- summaryBy( .~ volume , data = ratio,  FUN=c(mean,se))

library(magicaxis)

#PLotting of LMF model (use as a 2panel with allocation stacked)---------------------------------------------

# windows(11,16)
####multipanel plot of 
par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(3,1),  
    omi=c(.5,0,0.1,0.1))

#1. partitioning
par(mar = c(4, 7, 2, 7.3), xpd = TRUE)
barplot(t(as.matrix(mass_perc2))[i,], names.arg=leglab, col=treecols, width=2, xlab= "Soil Volume  (l)", 
        ylab="Seedling Mass Partitioning", ylim=c(0, 1))
legend("topright", inset = c(-0.205, 0), fill = treecols2, legend=treelab, cex=1)
text(1.3, .94, "(a)", cex=1.51)

#2.mass allocation (plant size corrected)
par(mar=c(4.3,7,1,.3))
plot(Mf_mod2, xlab="Stem+Root Mass  (g)" , ylab="Leaf Mass  (g)", col=palette(), pch=pchs, cex=2, lwd=2)
legend("bottomright", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, 
       col=palette(), bty='n',cex=1)
text(x=7.1, 62.5, "(b)", cex=1.51)

#3. Fine root to leaves
#windows(6,6)
par(mar=c(4.3,7,1,.3),xpd = FALSE)
with(ratio_agg, plot(log10(fineroot.mean), log10(leafmass.mean), ylim=c(0,2.25), xlim=c(0,2.25),
                     xlab = "Fine Root Mass  (g)",
                     ylab = "Leaf Mass  (g)",
                    axes=FALSE, type='n'))
abline(0,1, lwd=2, lty=2, col="grey35")
text(x=.09, 2.2, "(c)", cex=1.51)
ablineclip(ratio_lm, lwd=2, col="grey35",x1=min(log10(ratio_agg$fineroot.mean)), x2=max(log10(ratio_agg$fineroot.mean)))

with(ratio_agg, points(log10(fineroot.mean), log10(leafmass.mean), pch=pchs, col=palette(), cex=2))

with(ratio_agg, arrows(x0=log10(fineroot.mean), y0=log10(leafmass.mean), x1=log10(fineroot.mean+fineroot.se), angle=90, 
                       length=0.05,col=palette(), lwd=1))
with(ratio_agg, arrows(x0=log10(fineroot.mean), y0=log10(leafmass.mean), x1=log10(fineroot.mean-fineroot.se), angle=90, 
                       length=0.05,col=palette(), lwd=1))

with(ratio_agg, arrows(x0=log10(fineroot.mean), y0=log10(leafmass.mean), y1=log10(leafmass.mean+leafmass.se), angle=90, 
                       length=0.05,col=palette(), lwd=1))
with(ratio_agg, arrows(x0=log10(fineroot.mean), y0=log10(leafmass.mean), y1=log10(leafmass.mean-leafmass.se), angle=90, 
                       length=0.05,col=palette(), lwd=1))
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)


