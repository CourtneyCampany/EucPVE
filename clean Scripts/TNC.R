#source functions
source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

#raw data
leaf_tnc <- read.csv("raw data/leaf_tnc.csv")
#add Date from campaign #
leaf_tnc<- add_campaign_date(leaf_tnc)

#re work labels for volume
leaf_tnc$volume <- gsub("1000", "free", leaf_tnc$volume)
leaf_tnc$volume <- gsub("^5", "05", leaf_tnc$volume)
leaf_tnc$volume <- as.factor(leaf_tnc$volume)

# removed 4-6. 7-1, 5-4 from v15, c#2
#tnc15 <- subset(leaf_tnc, volume=="15" & campaign =="2")

#remove missing values
leaf_tnc <- subset(leaf_tnc, !is.na(starch_mgperg))
row.names(leaf_tnc)<-NULL

#convert tnc to g g-1 (this is also the percentage)
leaf_tnc$starch <- with(leaf_tnc, starch_mgperg /1000)
leaf_tnc$sugars <- with(leaf_tnc, sugars_mgperg /1000)
leaf_tnc$tnc <- with(leaf_tnc, sugars+starch)

#save this dfr as calculated data (raw with no missing values)
write.csv(leaf_tnc, "calculated data/tnc_leaf.csv", row.names=FALSE)

#generate means from raw data
tnc_means <- summaryBy(starch+sugars+tnc~ volume+Date, data=leaf_tnc, FUN=c(mean, se))   
#save these means as dfr going forward?????

#---------------------------------------------------------------------------------------------------
#plot bits and plotting functions
xAT <- seq.Date(from=as.Date("2013-3-1"), length=12, by="2 weeks")

#bar plot function
tncbar <- function(d, v, label, ...){
     bargraph.CI(x.factor = d$volume,  response = d[,v],  ylab = label, xlab = "Pot Volume (l)", ...)
     box()
}
#---------------------------------------------------------------------------------------------------

#overal means plots
sugarplot <-tncbar(d =leaf_tnc, v ="sugars", label = suglab, ylim=c(0, .20), col="grey98",border=palette())
starchplot <-tncbar(d =leaf_tnc, v ="starch", label = starchlab, ylim=c(0, .20), col="grey98",border=palette())
tncplot <-tncbar(d =leaf_tnc, v ="tnc", label = tnclab, ylim=c(0, .25), col="grey98",border=palette())
#lmaplot <-tncbar(d =leaf_tnc, v ="lma_notnc", label = slalab, ylim=c(0, 100))


#plot volume means across dates

#Starch
windows()
par(mgp=ypos)
plot(starch.mean ~ Date, data=tnc_means,type='n',ylab=starchlab,  ylim=c(0,.3), axes=FALSE, xlab="")  
box()
axis(2, labels=TRUE)
axis.Date(1, at=tnc_means$Date)

with(tnc_means, arrows(Date, starch.mean, Date, starch.mean+starch.se, angle=90, col=volume,length=0.03))
with(tnc_means, arrows(Date, starch.mean, Date, starch.mean-starch.se, angle=90, col=volume,length=0.03))

points(starch.mean ~ Date, data=tnc_means, pch=pchs[volume], cex=PTcex, col=volume)    
legend("topright", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, col=palette(), bty='n')

#sugars
windows()
par(mgp=ypos)
plot(sugars.mean ~ Date, data=tnc_means,type='n',ylab=suglab,  ylim=c(0,.15), axes=FALSE, xlab="")  
box()
axis(2, labels=TRUE)
axis.Date(1, at=tnc_means$Date)

with(tnc_means, arrows(Date, sugars.mean, Date, sugars.mean+sugars.se, angle=90, col=volume,length=0.03))
with(tnc_means, arrows(Date, sugars.mean, Date, sugars.mean-sugars.se, angle=90, col=volume,length=0.03))

points(sugars.mean ~ Date, data=tnc_means, pch=pchs[volume], cex=PTcex, col=volume)    
legend("topleft", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, col=palette(), bty='n')


#TNC
windows()
par(mgp=ypos)
plot(tnc.mean ~ Date, data=tnc_means,type='n',ylab=tnclab,  ylim=c(0,.3), axes=FALSE, xlab="")  
box()
axis(2, labels=TRUE)
axis.Date(1, at=tnc_means$Date)

with(tnc_means, arrows(Date, tnc.mean, Date, tnc.mean+tnc.se, angle=90, col=volume,length=0.03))
with(tnc_means, arrows(Date, tnc.mean, Date, tnc.mean-tnc.se, angle=90, col=volume,length=0.03))

points(tnc.mean ~ Date, data=tnc_means, pch=pchs[volume],cex=PTcex, col=volume)    
#legend("topright", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, col=palette(), bty='n')


#STATS------------------------------------------------------------------------------------
lme1 <- lm(starch_mgperg ~ volume, #random= ~1|PS.
             data=leaf_tnc)
anova(lme1)
summary(lme1)

lme2 <- lme(starch_mgperg ~ volume, random= ~1|ID, data=leaf_tnc)
anova(lme2)
summary(lme2)
