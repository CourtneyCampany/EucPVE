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

#PLotting of LMF model (use as a 2panel with allocation stacked)---------------------------------------------
plot(Mf_mod2, xlab= expression(log[10]~Stem+Root~Mass~~(g)), ylab="", col=palette(), pch=pchs)
  title(ylab=expression(log[10]~Leaf~Mass~~(g)), mgp=ypos)
  legend("topleft", leglab, pch=pchs,text.font=3, inset=0.02, title=expression(Pot~volume~(l)), 
  col=palette(), bty='n')


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
treelab <- c("Leaves", "Stems", "Coarse Roots", "Fine Roots")


#plot
par(mar = c(5.1, 4.1, 4.1, 7.3), xpd = TRUE)
barplot(t(as.matrix(mass_perc2))[i,], names.arg=leglab, col=treecols, width=2, xlab= "", 
        space = c(.2,.2,.2,.2,.2,.2,.8))
legend("topright", inset = c(-0.225, 0), fill = treecols2, legend=treelab)
