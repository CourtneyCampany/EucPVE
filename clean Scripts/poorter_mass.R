source("functions and packages/startscripts.R")

library(visreg)
library(multcomp)

#harvest mass dfr
seedlingmass<- read.csv("calculated data/seedling mass.csv") 

# rmf calculate
seedlingmass$RMF <- with(seedlingmass, root/totalmass)
seedlingmass$BVR <- with(seedlingmass, totalmass/volume)
seedlingmass$Msr <- with(seedlingmass, stemmass + Croot + fineroot)
seedlingmass$Mlr <- with(seedlingmass, leafmass + Croot + fineroot)

#treatment means
mass_agg <- summaryBy(.~volume, data=seedlingmass, FUN=mean, keep.names=TRUE)
mass_agg_nofree <- subset(mass_agg, volume != 1000)

#volume as factor after new variable calculations
seedlingmass$volume <- as.factor(seedlingmass$volume)

### Analyze Mass increase with each fold increase from 5l to test Poorter---------------------------------------------

library(scales)

#new dataframe of with mass as fold increase
mass_fold <- mass_agg[,c("volume","leafmass", "stemmass", "fineroot", "Croot", "totalmass" )]
  mass_fold$Fold <- c(1,2,3,4,5,7,200) #fold increase for each pot size

###mass increase with fold increase
fold_increase <- data.frame( volume = mass_fold$volume,
                             leaf = mass_fold$leafmass/mass_fold$leafmass[1],
                             stem = mass_fold$stemmass/mass_fold$stemmass[1], 
                             froot = mass_fold$fineroot/mass_fold$fineroot[1],
                             croot = mass_fold$Croot/mass_fold$Croot[1], 
                             mass = mass_fold$totalmass/mass_fold$totalmass[1], 
                             fold = c(1,2,3,4,5,7,200))

#subset with fold and no free
fold_pot_mod <- fold_increase[2:6,2:7]

#linear, Fit through origin, so that 0,0 means smallest pot size
mass_mod <- lm(I(mass-1) ~ I(fold-1) -1, data=fold_pot_mod)
fold_pot_mod$masspred <- predict(mass_mod, fold_pot_mod) + 1

summary(mass_mod) 
coef(mass_mod)
# means a 34% increase in mass with doubling of pot size
visreg(mass_mod)

#nls
mass_mod_nls <- nls(mass ~ 1 + a*fold^b, start=list(a=1,b=2), data=fold_pot_mod)
m <- coef(mass_mod_nls) 

#plot
with(fold_pot_mod,{
  plot(fold, mass, ylim=c(0,5), xlim=c(0,8))
  lines(fold, masspred, lty=2, col="red")
  curve(1 + m[[1]]*x^m[[2]], add=T,  col="blue")
})



###Test whether leaf mass fraction is different with treatment----------------------------------------------------
  #must account for variation in plant size
library(smatr)
#using (+) in the model equation we keeping slope constant and testing the elevation differences among lines
#using (*) we are testing slope

Mf_mod <- sma(leafmass ~ Msr + volume, log="xy", data=seedlingmass)
summary(Mf_mod)
# here the slope is 1.13, which is the exponent b1 in Mf = b0*Msr^b1
# if b1 > 1, then we have an increasing LMF with plant size.

Mf_mod2 <- sma(leafmass ~ Msr * volume, log="xy", data=seedlingmass)
summary(Mf_mod2)
# this shows that LMF is higher for free seedlings because they were bigger.

#PLotting of LMF model (use as a 2panel with allocation stacked)
plot(Mf_mod2, xlab= expression(log[10]~Stem+Root~Mass~~(g)), ylab="", col=palette(), pch=pchs)
  title(ylab=expression(log[10]~Leaf~Mass~~(g)), mgp=ypos)
  legend("topleft", leglab, pch=pchs,text.font=3, inset=0.02, title=expression(Pot~volume~(l)), 
       col=palette(), bty='n')

#SMF
Ms_mod <- sma(stemmass ~ Mlr + volume, log="xy", data=seedlingmass)
summary(Ms_mod)
plot(Ms_mod, xlab= expression(log[10]~Leaf+Root~Mass~~(g)), ylab="", col=palette(), pch=pchs)

Ms_mod2 <- sma(stemmass ~ Mlr * volume, log="xy", data=seedlingmass)
  summary(Ms_mod2)
plot(Ms_mod2, xlab= expression(log[10]~Leaf+Root~Mass~~(g)), ylab="", col=palette(), pch=pchs)

#RMF
Mr_mod <- sma(root ~ shoot + volume, log="xy", data=seedlingmass)
summary(Mr_mod)
plot(Mr_mod, xlab= expression(log[10]~Leaf+Root~Mass~~(g)), ylab="", col=palette(), pch=pchs)

Mr_mod2 <- sma(root ~ shoot * volume, log="xy", data=seedlingmass)
summary(Mr_mod2)
plot(Mr_mod2, xlab= expression(log[10]~Leaf+Root~Mass~~(g)), ylab="", col=palette(), pch=pchs)
     
###differences in LMF, SMF, and RMF only exists as a functional of plant size, not treatments

###Stacked Bar Plot of Mass Components by Volume-------------------------------------------------

#new dfr with only components
#mass_perc <- data.frame(volume = mass_agg$volume, leaf_perc = with(mass_agg, leafmass/totalmass))
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







  

#plot with a abline for poorters 43% from start value, then add my numbers----------------------------------------
#might show my exponential with nls for this here vs linear expectation


#start with 5l pot and build up
startmass <- mass_agg$totalmass[1]

massincrease <- vector()
massincrease[1] <- startmass

#new vector with 2x volumes
soilvolume <- c(5, 10, 20, 40)

#now calculate mass increase with poorters 43% with 2x
for(i in 2:length(soilvolume)) {
  
  massincrease[i] <- massincrease[i-1] + (massincrease[i-1] *.43)
}

poortermass <- as.data.frame(cbind(massincrease, soilvolume))
#uses simulated model mass to with percent increase and real values
plot(massincrease~soilvolume , pch=16, cex=1.5, ylim=c(0,100), ylab="Seeling Mass (g)", xlab="Soil Volume (l)")
lines(massincrease~soilvolume, lty=2, lwd=2 )
points(mass_agg_nofree$totalmass~ mass_agg_nofree$volume, pch=pchs, col=palette(), cex=1.5)


#plot and analyze BVR--------------------------------------------------------------------------------------------
boxplot(BVR~volume, data=seedlingmass, mean)
#poorter mean bvr=9.5, very few experiments have values lower than 2
BVR_lm <- lm(BVR ~ volume, data=seedlingmass)
  #check for normaility assumptions
  plot(BVR_lm)
extract_func(BVR_lm)
tukey_BVR <- glht(BVR_lm, linfct=mcp(volume="Tukey"))
  summary(tukey_BVR)
  plot(tukey_BVR)
anova(BVR_lm)
#different across pot size

bar(BVR, c(volume), seedlingmass, col=palette(), half.errbar=FALSE, xlab="", 
    legend=FALSE,ylim=c(0,4) , ylab="", mgp = c(3, .1, 0))

####figure that shows dry mass scaled to free plant or sacled to largest pot

maxfree <- with(subset(seedlingmass,volume ==1000),max(totalmass)) 
max35 <- with(subset(seedlingmass,volume ==35),max(totalmass)) 

#new variable that scales mass to 35 or free
seedlingmass$scale_1000 <- with(seedlingmass, totalmass/maxfree)
seedlingmass$scale_35<- with(seedlingmass, totalmass/max35)

mass_agg$scale_1000 <- with(mass_agg, totalmass/maxfree)

#plots of scaled mass to largest pots or free with BVR
plot(scale_1000~BVR, data=seedlingmass, col=volume, pch=16, ylim=c(0,1.2))
abline(1,0, lty=2)

plot(scale_1000~BVR, data=mass_agg, col=volume, pch=pchs, ylim=c(0,1.2))
abline(1,0, lty=2)

with(subset(seedlingmass, volume !=1000), plot(scale_35~BVR, col=volume, pch=16,ylim=c(0,1.2)))
abline(1,0, lty=2)







