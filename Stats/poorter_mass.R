source("functions and packages/startscripts.R")

require(visreg)
library(multcomp)

seedlingmass<- read.csv("calculated data/seedling mass.csv") 

# rmf calculate
seedlingmass$RMF <- with(seedlingmass, root/totalmass)
seedlingmass$BVR <- with(seedlingmass, totalmass/volume)

#treatment means
mass_agg <- summaryBy(totalmass~volume, data=seedlingmass, FUN=c(mean,se))
mass_agg_nofree <- subset(mass_agg, volume != 1000)

#volue as factor after new variable calculations
seedlingmass$volume <- as.factor(seedlingmass$volume)

#plot and analyze RMF------------------------------------------------------------------
RMF_lm <- lm(RMF ~ as.factor(volume), data=seedlingmass)
extract_func(RMF_lm)
anova(RMF_lm)
###RMF not different across volumes
bar(RMF, c(volume), seedlingmass, col=palette(), half.errbar=FALSE, xlab="", 
    legend=FALSE,ylim=c(0,.8) , ylab="", mgp = c(3, .1, 0))

#plot and analyze RBVR------------------------------------------------------------------
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





#start value
#pre seedling data for intial biomass and leaf area (use mean)--------------------------------------------------
seedling_pre <- read.csv("raw data/seedling_initial.csv")
seedling_pre$seedling_mass <- with(seedling_pre, leaf_mass+root_mass+wood_mass)
#average mass of seedlings at start
mass_mean <- mean(seedling_pre$seedling_mass)






#calculate BVR
#total plant mass : root volume ratio


# #dataframe with percent diff in total mass
# a<- Delt(mass_agg$totalmass.mean[1], mass_agg$totalmass.mean[2], type = c("arithmetic"))
# 
# pc.diff <- function(n1, n2) { ((n1-n2)/n2)*100} 
# pc.diff(mass_agg$totalmass.mean[4],mass_agg$totalmass.mean[2])
# 
# diffloop <- for


#plot with a abline for poorters 50% from start value, then add my numbers

#start with 5l pot and build up

startmass <- mass_agg$totalmass.mean[1]

massincrease <- vector()
massincrease[1] <- startmass

#new vector with 2x volumes
soilvolume <- c(5, 10, 20, 40)

#now calculate mass increase with poorters 50% with 2x


 for(i in 2:length(soilvolume)) {
  
  massincrease[i] <- massincrease[i-1] + (massincrease[i-1] *.50)
  
}

poortermass <- as.data.frame(cbind(massincrease, soilvolume))

plot(massincrease~soilvolume , pch=16, cex=1.5, ylim=c(0,100), ylab="Seeling Mass (g)", xlab="Soil Volume (l)")
  lines(massincrease~soilvolume, lty=2, lwd=2 )
  points(mass_agg_nofree$totalmass.mean~ mass_agg_nofree$volume, pch=pchs, col=palette(), cex=1.5)
