#this script is for sourcing the allocation plots into master
source("functions and packages/startscripts.R")

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

#PLotting of LMF model (use as a 2panel with allocation stacked)---------------------------------------------

windows(14,7)

par(cex.axis=.96, cex.lab=1.2,mfrow=c(1,2),oma=c(0.1,0.1,0.1,0.1), las=1)   

#1. partitioning
par(mar = c(5, 4, 2, 5.3), xpd = TRUE)
barplot(t(as.matrix(mass_perc2))[i,], names.arg=leglab, col=treecols, width=2, xlab= "", 
        ylab="", ylim=c(0, 1))
#space = c(.2,.2,.2,.2,.2,.2,.8))
title(ylab="Seedling Mass Partitioning", mgp=c(2.75,1,0))
title(xlab="Soil Volume  (l)", mgp=ypos)
legend("topright", inset = c(-0.205, 0), fill = treecols2, legend=treelab, cex=1)

#plot2
par(mar=c(5,4,2,2))
plot(Mf_mod2, xlab="" , ylab="", col=palette(), pch=pchs, cex=1.5, lwd=2)
title(ylab=expression(Leaf~Mass~~(g)), mgp=c(2.25,1,0))
title(xlab=expression(Stem+Root~Mass~~(g)), mgp=ypos)
legend("topleft", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, 
       col=palette(), bty='n',cex=1.2)

dev.copy2pdf(file= "master_scripts/manuscript_figs/massfractions.pdf")
dev.off()
