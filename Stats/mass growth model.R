#develop a linear model equation for either h/D or leaf number with seedling biomass

source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

#harvest mass
biomass <- read.csv("calculated data/seedling mass.csv")
treemass <- biomass[,c(1,11)]

#leaf count harvest
leafharvest <- read.csv("raw data/harvest leaf area.csv")
leafharvest$ID <- paste(leafharvest$plot, leafharvest$pot, sep = "-")
leafno <- leafharvest[, c(4,11)]
leafnum <- subset(leafno, !is.na(leaf_count))
row.names(leafnum) <- NULL

#read survey data
height <- read.csv("calculated data/height.csv")
diameter <- read.csv("calculated data/diameter.csv")
leafno <- read.csv("calculated data/leaf_number.csv")

#format the hieght and diameter data and create a dfr from the last date
finaldate <- function(H, D){
  H$Date <- as.Date(H$Date)
  D$Date <- as.Date(D$Date)
  lastH <- subset(H, Date==max(Date))
  lastD <- subset(D, Date==max(Date))
  HD <- merge(lastH, lastD)
  HD <- HD[, 3:7]
}

allom <- finaldate(height, diameter)

#merge with leaf num and tree mass
allomvar <- merge(allom, leafnum)
allomvar <- merge(allomvar, treemass)
allomvar$d2h <- with(allomvar, (diameter^2)*height)

#re work labels for volume
allomvar$volume <- gsub("1000", "free", allomvar$volume)
allomvar$volume <- gsub("^5", "05", allomvar$volume)
allomvar$volume <- as.factor(allomvar$volume)

#visulaise each realtionship with mass

plot(totalmass~height, data=allomvar,col=volume, pch=pchs[volume])
#nonlinear and variance increase with height
plot(totalmass~diameter, data=allomvar,col=volume, pch=pchs[volume])
#nonlinear and equal variance
plot(totalmass~leaf_count, data=allomvar,col=volume, pch=pchs[volume])
#linear and variance increase with leaf count
plot(totalmass~d2h, data=allomvar,col=volume, pch=pchs[volume])
#linear and variance increase with leaf count

#models for each variable with mass
Hmodel <- lm(totalmass ~ height, data=allomvar)
Dmodel <- lm(totalmass ~ diameter, data=allomvar)
Lmodel <- lm(totalmass ~ leaf_count, data=allomvar)
DHmodel <- lm(totalmass ~ d2h, data=allomvar)

#see which one best predicts
summary(Hmodel)
summary(Dmodel)
summary(Lmodel)
summary(DHmodel)

#examine d2h model
plot(DHmodel)
confint(DHmodel, level=0.95)
xnew <- seq(min(allomvar$d2h), max(allomvar$d2h), length.out=100)
ynew <- data.frame(predict(DHmodel, newdata=data.frame(d2h=xnew), interval="confidence", level=0.95))

plot(totalmass~d2h, data=allomvar,col=volume, pch=pchs[volume], xlim=c(0, 40000), ylim=c(0,300))
ablineclip(DHmodel, x1=min(allomvar$d2h), x2=max(allomvar$d2h), lty=1, col="forestgreen")
lines(ynew$lwr~xnew, col="forestgreen",  lty=2)
lines(ynew$upr~xnew, col="forestgreen",  lty=2)
#larger values fall outside of the CI

#examine diameter
plot(Dmodel)
confint(Dmodel, level=0.95)
dxnew <- seq(min(allomvar$diameter), max(allomvar$diameter), length.out=100)
dynew <- data.frame(predict(Dmodel, newdata=data.frame(diameter=xnew), interval="confidence", level=0.95))

#plot
plot(totalmass~diameter, data=allomvar, col=volume, pch=pchs[volume], ylim=c(0, 300), xlim=c(0, 20))
ablineclip(Dmodel, x1=min(allomvar$diameter), x2=max(allomvar$diameter), lty=1, col="forestgreen")
lines(dynew$lwr~dxnew, col="forestgreen", lty=2)
lines(dynew$upr~dxnew, col="forestgreen", lty=2)

#--------------------------------------------------------------------------------------------------
#although both models have high r2 the d2h has unequal variances and diameter does not seem linear
#need to use log transformations in order to find the best model
#--------------------------------------------------------------------------------------------------

#replot both variables with logxy 
plot(totalmass~d2h, data=allomvar,log="xy", col=volume, pch=pchs[volume])
plot(totalmass~diameter, data=allomvar,log="xy", col=volume, pch=pchs[volume])
#this seems to fit linearity and results in equal variances (diameter seems not as good at low values)

#run news model with log for both variables

#diameter
logD_mod <- lm(log(totalmass)~I(log(diameter)),data=allomvar)
summary(logD_mod)
coefD <-as.data.frame(coef(logD_mod))
predict(logD_mod)
#The model is therefore:log(totalmass) = −1.768846+2.588310log(diameter)
plot(logD_mod,which=1:2)
library(visreg)
visreg(logD_mod)

# #d2h
# logDH_mod <- lm(log(totalmass)~I(log(d2h)),data=allomvar)
# summary(logDH_mod)
# coef(logDH_mod)
# predict(logDH_mod)
# plot(logDH_mod,which=1:2)
# #The model is therefore:log(totalmass) = −4.8933322+.9968101log(diameter)

#equation parameters
intcptD <-coefD[1,1]
slopeD <-coefD[2,1]

allomvar$Dmass <- exp(intcptD)*(allomvar$diameter^slopeD)
#plot predicted versus observed
plot(totalmass~diameter, data=allomvar,col=volume, pch=1)
points(Dmass~diameter, data=allomvar,col=volume, pch=16)

##use the equation to generate values, but with no cf
mass_predict <- diameter
mass_predict$treemass <- exp(intcptD)*(mass_predict$diameter^slopeD)

plot(treemass~diameter, data=subset(mass_predict, volume=="5"), col="red", pch=16)
  points(totalmass~diameter, data=allomvar,col=volume, pch=1)

# recalculate with the correction factor (Spruegel 1983)
#use Residual standard error for correction factor
cf <- exp(summary(logD_mod)$sigma^2/2)
slopeDcf<- cf*slopeD
#equation becomes:
mass_predict$treemass2 <- exp(intcptD)*(mass_predict$diameter^slopeDcf)

plot(treemass2~diameter, data=subset(mass_predict, volume=="5"), col="black", pch=16)
  points(treemass~diameter, data=subset(mass_predict, volume=="5"), col="black", pch=1)
  points(totalmass~diameter, data=allomvar,col=volume, pch=16)

#back transformations...additive(log) vs multiplicative(raw) error???
#consider the power model (if logxy).... power model Y = aXb

####ues model over all dates to predict seedling mass----------------------------------------------------

mass_date <- diameter
mass_date$predmass <-  exp(intcptD)*(mass_date$diameter^slopeDcf)
mass_date$volume<- as.factor(mass_date$volume)
mass_date$Date<- as.Date(mass_date$Date)

#predicted mass thorugh time with choosen model
write.csv(mass_date, "calculated data/mass_predicted.csv", row.names=FALSE)







