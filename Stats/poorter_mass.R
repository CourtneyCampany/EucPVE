source("functions and packages/startscripts.R")

require(visreg)
library(multcomp)
require(quantmod)
library(reshape)

seedlingmass<- read.csv("calculated data/seedling mass.csv") 

# rmf calculate
seedlingmass$RMF <- with(seedlingmass, root/totalmass)
seedlingmass$BVR <- with(seedlingmass, totalmass/volume)

#treatment means
mass_agg <- summaryBy(.~volume, data=seedlingmass, FUN=mean, keep.names=TRUE)
mass_agg_nofree <- subset(mass_agg, volume != 1000)

#volue as factor after new variable calculations
seedlingmass$volume <- as.factor(seedlingmass$volume)

#new dataframe of with mass as fold increase

mass_agg <- summaryBy(. ~ volume, data=seedlingmass, FUN=mean, keep.names=TRUE)

mass_fold <- mass_agg[,c("volume","leafmass", "stemmass", "fineroot", "Croot", "totalmass" )]
  mass_fold$Fold <- c(1,2,3,4,5,7,200) #fold increase for each pot size

Bo <- mass_fold[1,] #5l has intial value for step increase



#volume = mass_fold$volume,

fold_increase <- data.frame( volume = mass_fold$volume,
                            leaf = mass_fold$leafmass/mass_fold$leafmass[1],
                            stem = mass_fold$stemmass/mass_fold$stemmass[1], 
                            froot = mass_fold$fineroot/mass_fold$fineroot[1],
                            croot = mass_fold$Croot/mass_fold$Croot[1], 
                            mass = mass_fold$totalmass/mass_fold$totalmass[1], 
                            fold = c(1,2,3,4,5,7,200))

fold_pot_mod <- subset(fold_increase[2:6,2:7])

#linear model of fold increase for each componen
leaf_mod <- lm(leaf ~ fold, data=fold_pot_mod)
  summary(leaf_mod)
stem_mod <- lm(stem ~ fold, data=fold_pot_mod)
  summary(stem_mod)
froot_mod <- lm(froot ~ fold, data=fold_pot_mod)
  summary(froot_mod)
croot_mod <- lm(croot ~ fold, data=fold_pot_mod)
  summary(croot_mod)
mass_mod <- lm(mass ~ fold, data=fold_pot_mod)
  summary(mass_mod) 





####for box plotting
fold_pot <- subset(fold_increase[1:6,])

fold_perc <- data.frame(fold = fold_pot$fold, leaf_perc=Delt(fold_pot$leaf),
                       stem_perc = Delt(fold_pot$stem),
                       froot_perc = Delt(fold_pot$froot),
                       croot_perc = Delt(fold_pot$croot),
                       mass_perc = Delt(fold_pot$mass))

names(fold_perc) <- c("fold", "Leaf", "Stem", "Fine Root", "Coarse Root", "Seedling Mass")

fold_agg <- subset(fold_perc, fold != 1)
fold_box <- fold_agg[2:6]

#####35 represents a two fold incrase......
windows()
par(oma=c(0,3,0,0))
boxplot(fold_box, at=c(1,2,3,4,6),border="forestgreen")
  title(ylab="mean % change in mass with fold increase \nin pot size from 5l", mgp=c(2.25,1,0))
  abline(.43,0, lty=2, col="red")
  mtext("43% increase with 2 fold increase (Poorter et al.)", side=4, line=-20,las=1, padj=.2)


#####this isnt close.......





#plot and analyze RMF------------------------------------------------------------------
RMF_lm <- lm(RMF ~ as.factor(volume), data=seedlingmass)
extract_func(RMF_lm)
anova(RMF_lm)
###RMF not different across volumes
bar(RMF, c(volume), seedlingmass, col=palette(), half.errbar=FALSE, xlab="", 
    legend=FALSE,ylim=c(0,.8) , ylab="", mgp = c(3, .1, 0))

#plot and analyze BVR------------------------------------------------------------------
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

#start value
#pre seedling data for intial biomass and leaf area (use mean)--------------------------------------------------
seedling_pre <- read.csv("raw data/seedling_initial.csv")
seedling_pre$seedling_mass <- with(seedling_pre, leaf_mass+root_mass+wood_mass)
#average mass of seedlings at start
mass_mean <- mean(seedling_pre$seedling_mass)



#dataframe with percent diff in total mass---------------------------------------------
 allocation <- data.frame(cr = mass_agg$Croot/mass_agg$totalmass,
                              fr = mass_agg$fineroot/mass_agg$totalmass,
                              leaf = mass_agg$leafmass/mass_agg$totalmass,
                              stem = mass_agg$stemmass/mass_agg$totalmass,
                              volume = mass_agg$volume)


# change in allocation of each component
allocation_diff <- data.frame(cr_diff= ((allocation$cr-allocation$cr[1])/allocation$cr)*100,
                              fr_diff = ((allocation$fr-allocation$fr[1])/allocation$fr)*100,
                              leaf_diff = ((allocation$leaf-allocation$leaf[1])/allocation$leaf)*100,
                              stem_diff = ((allocation$stem-allocation$stem[1])/allocation$stem)*100,
                              volume = mass_agg$volume)

#this is diff but need 
test <- ((allocation$cr-allocation$cr[1])/allocation$cr)*100

#plot with a abline for poorters 43% from start value, then add my numbers------------------

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

plot(massincrease~soilvolume , pch=16, cex=1.5, ylim=c(0,100), ylab="Seeling Mass (g)", xlab="Soil Volume (l)")
  lines(massincrease~soilvolume, lty=2, lwd=2 )
  points(mass_agg_nofree$totalmass~ mass_agg_nofree$volume, pch=pchs, col=palette(), cex=1.5)
