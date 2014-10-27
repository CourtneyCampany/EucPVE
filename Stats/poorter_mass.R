source("functions and packages/startscripts.R")

library(visreg)
library(multcomp)
library(quantmod)
library(reshape)

seedlingmass<- read.csv("calculated data/seedling mass.csv") 

# rmf calculate
seedlingmass$RMF <- with(seedlingmass, root/totalmass)
seedlingmass$BVR <- with(seedlingmass, totalmass/volume)

#treatment means
mass_agg <- summaryBy(.~volume, data=seedlingmass, FUN=mean, keep.names=TRUE)
mass_agg_nofree <- subset(mass_agg, volume != 1000)

#volume as factor after new variable calculations
seedlingmass$volume <- as.factor(seedlingmass$volume)

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

###analyze mass increase with pot size------------------------------------------------------------------
  #linera model may not be appropirate so use nls
  library(scales)

#linear
# Fit through origin, so that 0,0 means smallest pot size
mass_mod <- lm(I(mass-1) ~ I(fold-1) -1, data=fold_pot_mod)
  summary(mass_mod) 
  coef(mass_mod) * 2 # percent increase in mass when doubling pot size

#nls
mass_mod_nls <- nls(mass ~ 1 + a*fold^b, start=list(a=1,b=2), data=fold_pot_mod)
  m <- coef(mass_mod_nls) 

#plot
  with(fold_pot_mod,plot(fold, mass, ylim=c(0,5), xlim=c(0,8)))
  #add linear and nls fits
  ablinepiece(mass_mod)
  curve(1 + m[[1]]*x^m[[2]], add=T)
  
#nls
mass_mod_nls <- nls(mass ~ 1 + a*fold^b, start=list(a=1,b=2), data=fold_pot_mod)
  m <- coef(mass_mod_nls)
  #plot nls curve
  with(fold_pot_mod,plot(fold, mass, ylim=c(0,5), xlim=c(0,8)))
  curve(1 + p[[1]]*x^p[[2]], add=T)


###analyze leaf mass increase with pot size------------------------------------------------------------------

#linear
# Fit through origin, so that 0,0 means smallest pot size
leaf_mod <- lm(I(leaf-1) ~ I(fold-1) -1, data=fold_pot_mod)
  summary(leaf_mod) 

#nls
leaf_mod_nls <- nls(leaf ~ 1 + a*fold^b, start=list(a=1,b=2), data=fold_pot_mod)
  l <- coef(leaf_mod_nls)
  
#plot
with(fold_pot_mod,plot(fold, leaf, ylim=c(0,6), xlim=c(0,8)))
  #add linear and nls fits
  ablinepiece(leaf_mod)
  curve(1 + l[[1]]*x^l[[2]], add=T)


###analyze stem mass increase with pot size------------------------------------------------------------------

#linear first
# Fit through origin, so that 0,0 means smallest pot size
stem_mod <- lm(I(stem-1) ~ I(fold-1) -1, data=fold_pot_mod)
  summary(stem_mod) 

#nls
stem_mod_nls <- nls(stem ~ 1 + a*fold^b, start=list(a=1,b=2), data=fold_pot_mod)
  s <- coef(stem_mod_nls)

#plot nls curve
  with(fold_pot_mod,plot(fold, stem, ylim=c(0,6), xlim=c(0,8)))
  #add linear and nls fits
  ablinepiece(stem_mod)
  curve(1 + s[[1]]*x^s[[2]], add=T)


###analyze froot mass increase with pot size------------------------------------------------------------------

#linear first
# Fit through origin, so that 0,0 means smallest pot size
froot_mod <- lm(I(froot-1) ~ I(fold-1) -1, data=fold_pot_mod)
  summary(froot_mod) 

#nls
froot_mod_nls <- nls(froot ~ 1 + a*fold^b, start=list(a=1,b=2), data=fold_pot_mod)
  fr <- coef(froot_mod_nls)

#plot
with(fold_pot_mod,plot(fold, froot, ylim=c(0,6), xlim=c(0,8)))
  #add linear and nls fits
  ablinepiece(froot_mod)
  curve(1 + fr[[1]]*x^fr[[2]], add=T)



###analyze croot mass increase with pot size------------------------------------------------------------------

#linear first
# Fit through origin, so that 0,0 means smallest pot size
croot_mod <- lm(I(croot-1) ~ I(fold-1) -1, data=fold_pot_mod)
  summary(croot_mod) 

#nls
croot_mod_nls <- nls(froot ~ 1 + a*fold^b, start=list(a=1,b=2), data=fold_pot_mod)
  cr <- coef(croot_mod_nls)

#plot
with(fold_pot_mod,plot(fold, froot, ylim=c(0,6), xlim=c(0,8)))
  #add linear and nls fits
  ablinepiece(froot_mod)
  curve(1 + cr[[1]]*x^cr[[2]], add=T)


######roots appear to be linear but not aboveground or total mass



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
