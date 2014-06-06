#Determine jmax and vcmax from so they can be included in the A model for each pot
#generate treatment means for each pot

source("functions and packages/load packages.R")
source("functions and packages/load model packages.R")
source("functions and packages/functions.R")

#read 2 ACi datasets (they are from two seperate dates during the experiment...deal with this later)
source("read data scripts/physiology read data.R")

#use acifunction to generate jmax and vcmax and get mean of parameters by volume
aci1_fit <- acifunction(aci1)
aci2_fit <- acifunction(aci2)

aci1_fit$Date <- as.Date("2013-03-14")
aci2_fit$Date <- as.Date("2013-05-14")

jmax_vcmax <- rbind(aci1_fit, aci2_fit)
jmax_vcmax_means <- summaryBy()

#write to csv
#stats show (aci_stats.R) that campaigns are not different so this written file is not used
write.csv(jmax_vcmax, file = "calculated data/jmax_vcmax.csv", row.names=FALSE)   
save(jmax_vcmax, file = "calculated data/jmax_vcmax.RData")

#take subset to test 
temp <- subset(aci1, ID =="4-5")

#run model
fit<-fitaci(temp, varnames = list(ALEAF="Photo", Tleaf = "Temp", Ci="Ci", PPFD="PPFD"), quiet=FALSE, Tcorrect=TRUE)
#look at plot, summary of the fit, extract the values, extract fitted values,
# look at the non linear regression fit, and plot modelled vs measured
plot(fit)
summary(fit)
coef(fit)
fitted(fit)
summary(fit$nlsfit)
with(fit$df, plot(Amodel, Ameas))
abline(0,1)
#use estimated parameters to estimate PS (useful later on)
fit$Photosyn(Ci=285)


####test if the two dates are different by volume

