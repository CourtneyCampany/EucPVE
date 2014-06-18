source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

#use predicted mass from model (mass growth model script) to calculate BGI----------------------------

mass <- read.csv("calculated data/mass_predicted.csv")
mass_pred <- mass[,3:7]
#order dates
dateorder<-order(mass_pred$volume, by=mass_pred$Date)
mass_pred <- mass_pred[dateorder,]
mass_pred$Date <- as.Date(mass_pred$Date)

row.names(mass_pred) <- NULL

#need a dfr of unique dates, order and find diff between survey dates
allomdate <- as.data.frame(unique(mass_pred$Date))
names(allomdate)[1] <- "Date"
allomdate$Date <- as.Date(allomdate$Date)
num_days <- as.data.frame(as.numeric(diff(allomdate$Date)))
names(num_days)[1] <- "daysbetween"
days <- c(1, num_days$daysbetween)
daydiff <- cbind(allomdate, days)

#now merge the days between the mass_pred dfr by Date
BGI <- merge(mass_pred, daydiff, by="Date")
BGI$volume <- as.factor(BGI$volume)

#calculate 
BGI$massincrement <- BGI$predmass / BGI$days


#simple plot of all seedlings
plot(massincrement~Date, data=BGI, col=volume, pch=pchs[volume])

#means dfrs
BGI_agg <- summaryBy(massincrement+predmass+diameter~Date+volume, data=BGI, FUN=mean, keep.names=TRUE)
plot(massincrement~Date, data=BGI_agg, col=volume, pch=pchs[volume])
plot(predmass~Date, data=BGI_agg, col=volume, pch=pchs[volume])
plot(log(predmass)~Date, data=BGI_agg, col=volume, pch=pchs[volume])

#BGI model

bgi_mod <- lm(log(massincrement) ~ Date*volume, data=BGI)
anova(bgi_mod)
library(visreg)
visreg(bgi_mod,
       xvar = "Date",
       by = "volume",
       trans = exp,
       fill.par = list(col=palette()),
       line.par = list(col="black"),
       points.par = list(pch=21, bg=palette(), col="black", cex=.5),
       overlay = TRUE)


bgi_mod2 <- lme(log(massincrement) ~ Date*volume, random = ~ Date|ID, data=BGI)
library(car)
Anova(bgi_mod2)
plot(bgi_mod2)
qqnorm(bgi_mod2, ~ resid(.)|ID)
qqnorm(residuals.lm(bgi_mod2))
qqline(residuals.lm(bgi_mod2))




?visreg





