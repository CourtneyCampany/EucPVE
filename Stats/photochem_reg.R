#source functions and packages
source("functions and packages/functions.R")
source("functions and packages/stats packages.R")
source("functions and packages/plot objects.R")
require(lmerTest)

photo_chem <- read.csv("calculated data/Amax_chem.csv")
  #run volume format func
  photo_chem<- vollab_func(photo_chem)

#simple model mass based--------------------------------------------------------------------
##use the full and simple model to make predict plots with bins
##make two panel plots with the bins

#simple models first, #visulaise with visreg/effects pacakges
#Amass_simple <- lm(A_mass~ starch*Nmass_notnc, data=photo_chem)
  anova(Amass_simple)
  summary(Amass_simple)
  visreg(Amass_simple)

#full model
Amass_full <- lmer(A_mass ~ starch+Nmass_notnc+starch:Nmass_notnc + (1|ID), data=photo_chem)
anova(Amass_full)
summary(Amass_full)
c <- coef(Amass_full)

#plot predicted and observed in bins------------------------------------------------------

#make bin levels and mid points for both starch and nitrogen
starchbin <- c(0, 0.04, 0.08, 0.12, 0.16, 100)
starchmid <- c(0.02, 0.06, 0.1, 0.14, 0.2)

nitrobin <- c(0, 0.0025, 0.005, 0.0075, 0.01, 100)
nitromid <- c(0.00125, 0.00375, 0.00625, 0.00875, 0.0125)

#set bins in raw dfr
photo_chem$starchbin <- cut(photo_chem$starch, breaks = starchbin)
photo_chem$nitrobin <- cut(photo_chem$Nmass_notnc, breaks = nitrobin)

#create models and extract coefs (for simple and full model)
#use two fits so the equations stay the same for pred, y=b+b1*x1+b2*x2
Afit <- lm(A_mass ~ Nmass_notnc * starch, data=photo_chem)
p <- coef(Afit)
Afit2 <- lm(A_mass ~ starch*Nmass_notnc, data=photo_chem)
p2 <- coef(Afit2)


Afit_full <- lmer(A_mass ~ Nmass_notnc+starch+Nmass_notnc:starch + (1|ID), data=photo_chem)
p3 <- coef(Afit_full)
Afit2_full <- lmer(A_mass ~ starch+Nmass_notnc+starch:Nmass_notnc + (1|ID), data=photo_chem)
p4 <- coef(Afit2_full)


#plot and predict (2 graphs)---------------------------------------------------------------------
cols <- c("green2", "cyan4", "blue2", "darkviolet", "red")
coln <- c("red", "darkviolet", "blue2", "cyan4", "green2")
binlab <- c("0-4", "4-8", "8-12", "12-16", ">16")
pchbin <- c(1,2,3,4,5)
legpch <- c(5, 10, 15, 20, 25, 35, "free")


#Amass vs N (simple and full for each)
windows()
#simple
with(photo_chem, plot(Nmass_notnc, A_mass, pch=pchs[volume], col=cols[starchbin], ylim=c(0,800)))
for(i in 1:length(starchbin)){
  
  x <- seq(min(photo_chem$Nmass_notnc), max(photo_chem$Nmass_notnc), length=101)
  y <- p[[1]] + p[[2]]*x + p[[3]]*starchmid[i] + p[[4]]*starchmid[i]*x
  lines(x,y, col=cols[i], lwd=2)
}
legend("topleft", binlab, pch=15, text.font=1, inset=0.01, col=cols,title=starchlab , bty='n')

#full
with(photo_chem, plot(Nmass_notnc, A_mass, pch=pchs[volume], col=cols[starchbin], ylim=c(0,800)))
for(i in 1:length(starchbin)){
  
  x <- seq(min(photo_chem$Nmass_notnc), max(photo_chem$Nmass_notnc), length=101)
  y <- p3[[1]] + p3[[2]]*x + p3[[3]]*starchmid[i] + p3[[4]]*starchmid[i]*x
  lines(x,y, col=cols[i], lwd=2)
}
legend("topleft", binlab, pch=15, text.font=1, inset=0.01, col=cols,title=starchlab , bty='n')




#Amass vs starch
windows()
with(photo_chem, plot(starch, A_mass, pch=19, col=coln[nitrobin], ylim=c(0,800), xlim=c(0, .275)))
for(i in 1:length(nitrobin)){
  
  x <- seq(0, max(photo_chem$starch), length=101)
  y <- p2[[1]] + p2[[2]]*x + p2[[3]]*nitromid[i] + p2[[4]]*nitromid[i]*x
  lines(x,y, col=coln[i], lwd=2)
}
legend("topright", binlab, pch=15, text.font=1, inset=0.01, col=coln,title=nfree , bty='n')




dfr <- expand.grid(starch = c(0.02, 0.06, 0.1, 0.14, 0.2),
                   Nmass_notnc = seq(0.0007432, 0.0175502, length=25))
dfr$Amass_pred <- predict(Afit_full, dfr, re.form=NA)
                 



#--------------------------------------------------------------------
photo_chem2 <- subset(photo_chem, volume !="1000")
coln2 <- c("red", "darkviolet", "blue2", "cyan4")

Amean <- mean(photo_chem2$A_mass)
Nmean <- mean(photo_chem2$Nmass_notnc)
Smean <- mean(photo_chem2$starch)

#use two fits so the equations and stay the same for pred, y=b+b1*x1+b2*x2
lmpot <- lm(A_mass ~ Nmass_notnc * starch, data=photo_chem2)
potcf <- coef(lmpot)
lmpot2 <- lm(A_mass ~ starch*Nmass_notnc, data=photo_chem2)
potcf2 <- coef(lmpot2)
#set bins
photo_chem2$starchbin <- cut(photo_chem2$starch, breaks = starchbin)
photo_chem2$nitrobin <- cut(photo_chem2$Nmass_notnc, breaks = nitrobin)


#Amass vs N (no free)
windows()
with(photo_chem2, plot(Nmass_notnc, A_mass, pch=16, col=cols[starchbin], ylim=c(0,800)))
for(i in 1:length(starchbin)){
  
  x <- seq(min(photo_chem2$Nmass_notnc), max(photo_chem2$Nmass_notnc), length=101)
  y <- potcf[[1]] + potcf[[2]]*x + potcf[[3]]*starchmid[i] + potcf[[4]]*starchmid[i]*x
  lines(x,y, col=cols[i], lwd=2)
}
legend("topleft", binlab, pch=15, text.font=1, inset=0.02, col=cols,title=starchlab , bty='n')
title(main="Within Pots")

#Amass vs starch (no free)
windows()
with(photo_chem2, plot(starch, A_mass, pch=19, col=coln2[nitrobin], ylim=c(0,800), xlim=c(0, .275)))
for(i in 1:length(nitrobin)){
  
  x <- seq(min(photo_chem2$starch), max(photo_chem2$starch), length=101)
  y <- potcf2[[1]] + potcf2[[2]]*x + potcf2[[3]]*nitromid[i] + potcf2[[4]]*nitromid[i]*x
  lines(x,y, col=coln2[i], lwd=2)
}
legend("topright", binlab, pch=15, text.font=1, inset=0.02, col=coln,title=nfree , bty='n')
title(main="Within Pots")

#only free-----------------------------------------------------------------------------------

photo_chem3 <- subset(photo_chem, volume =="1000")
coln3 <- c("darkviolet", "blue2", "cyan4", "green2")

#look at similar effects with pots and non pots
#make bin levels for both starch and nitrogen
starchbin3 <- c(0, 0.04, 0.08, 0.12, 0.16)
nitrobin3 <- c(0.0025, 0.005, 0.0075, 0.01, 100)
#make midpoints for pred
starchmid3 <- c(0.02, 0.06, 0.1, 0.14)
nitromid3 <- c(0.00375, 0.00625, 0.00875, 0.0125)

#use two fits so the equations and stay the same for pred, y=b+b1*x1+b2*x2
lmfree <- lm(A_mass ~ Nmass_notnc * starch, data=photo_chem3)
Fcf <- coef(lmfree)
lmfree2 <- lm(A_mass ~ starch*Nmass_notnc, data=photo_chem3)
Fcf2 <- coef(lmfree2)
#set bins in raw dfr
photo_chem3$starchbin3 <- cut(photo_chem3$starch, breaks = starchbin3)
photo_chem3$nitrobin3 <- cut(photo_chem3$Nmass_notnc, breaks = nitrobin3)
 
#Amass vs N (free only)
windows()
with(photo_chem3, plot(Nmass_notnc, A_mass, pch=16, col=cols[starchbin3], ylim=c(0,800)))
for(i in 1:length(starchbin3)){
   
x <- seq(min(photo_chem3$Nmass_notnc), max(photo_chem3$Nmass_notnc), length=101)
y <- Fcf[[1]] + Fcf[[2]]*x + Fcf[[3]]*starchmid3[i] + Fcf[[4]]*starchmid3[i]*x
lines(x,y, col=cols[i], lwd=2)
}
legend("bottomright", binlab, pch=15, text.font=1, inset=0.02, col=cols,title=starchlab , bty='n')
title(main="Free")
 
#Amass vs starch (free only)
windows()
with(photo_chem3, plot(starch, A_mass, pch=19, col=coln3[nitrobin3], ylim=c(0,800), xlim=c(0, .20)))
for(i in 1:length(nitrobin3)){ 
   x <- seq(min(photo_chem3$starch), max(photo_chem3$starch), length=101)
   y <- Fcf2[[1]] + Fcf2[[2]]*x + Fcf2[[3]]*nitromid3[i] + Fcf2[[4]]*nitromid3[i]*x
   lines(x,y, col=coln3[i], lwd=2)
 }
 legend("topright", binlab, pch=15, text.font=1, inset=0.02, col=coln,title=nfree , bty='n')
 title(main="Free")


#--------------------------------------------------------------------------------------
#calculate overall means

#means(all dates)
photo_chem_agg <- summaryBy(A_mass+starch+Nmass_notnc ~ volume, FUN=c(mean, se), data=photo_chem)
volumeorder <- order(photo_chem_agg$A_mass.mean, by=photo_chem_agg$volume)
photo_chem_agg<-photo_chem_agg[volumeorder,]
#plot stuff
manX <- seq(0, .25, .05)
meanX <- seq(0, .20, .05)

nX <- seq(0, 015, .005)
nmeanX <- seq(0, .01, .005)
#---------------------------------------------------------------------------------------
#amax and N interaction

windows(16,6)
par(mfrow=c(1,3),omi=c(1,1,0.2,0.2), mar=c(0,0,0,0), cex=1.3)

#raw
plot(A_mass ~ Nmass_notnc, data = photo_chem, col=volume, xlim=c(0, .02),ylim=c(100, 800), pch=pchs[volume], 
     axes=FALSE, xlab="", ylab="")
box()
axis(1, at = nX, labels = nX)
axis(2, labels=TRUE)
mtext(Amasslab, side=2, line=2.5, cex=1.3)
title(main="Amax*Leaf N * volume, p < 0.0122", line=-1.5, cex.main=.75, font.main=1,adj=.05)

d_ply(photo_chem, .(volume), function(x) add_trend_line("Nmass_notnc", "A_mass", x, ))

#volume means
plot(A_mass.mean ~ Nmass_notnc.mean, data = photo_chem_agg, col=volume, xlim=c(0, .0125),ylim=c(100, 800),
     pch=pchs[volume], ylab="", axes=FALSE,cex=2)
box()
axis(1, at = nmeanX, labels = nmeanX)
mtext(nfree, side=1, line=2.5, cex=1.3)
title(main=volmean, line=-1.5, font.main=1, adj=.05, cex.main=.75)
legend("bottomright", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n', cex=.75)

#means by date
plot(A_mass.mean ~ Nmass_notnc.mean, data = photo_chem_campaign, col=volume, xlim=c(0, .0125),
     ylim=c(100, 800),pch=pch2[volume], cex=2, ylab="", axes=FALSE)
box()
axis(1, at = nmeanX, labels = nmeanX)
title(main=datemean, line=-1.5, font.main=1, adj=.05, cex.main=.75)
d_ply(photo_chem_campaign, .(volume), function(x) c(add_trend_line("Nmass_notnc.mean", "A_mass.mean", x),
                                    text(x$A_mass.mean~x$Nmass_notnc.mean, labels=unique(x$campaign), 
                                         font=2,cex=.75)))
dev.copy2pdf(file="output/stats_plots/amaxnfree.pdf")
dev.off()
#---------------------------------------------------------------------------------------------------

#amax and starch interaction

windows(16,6)
par(mfrow=c(1,3),omi=c(1,1,0.2,0.2), mar=c(0,0,0,0), cex=1.3)

#raw
plot(A_mass ~ starch, data = photo_chem, col=volume, xlim=c(0, .3),ylim=c(100, 800), pch=pchs[volume], 
     axes=FALSE, xlab="", ylab="")
box()
axis(1, at = manX, labels = manX)
axis(2, labels=TRUE)
mtext(Amasslab, side=2, line=2.5, cex=1.3)
title(main="Amax*starch* volume, p < 0.0001", line=-1.5, font.main=1, adj=.1, cex.main=.75)

d_ply(photo_chem, .(volume), function(x) add_trend_line("starch", "A_mass", x, ))

#volume means
plot(A_mass.mean ~ starch.mean, data = photo_chem_agg, col=volume, xlim=c(0, .2),ylim=c(100, 800),
     pch=pchs[volume], ylab="", axes=FALSE,cex=2)
box()
axis(1, at = meanX, labels = meanX)
mtext(starchlab, side=1, line=2.5, cex=1.3)
title(main=volmean, line=-1.5, font.main=1, adj=.1, cex.main=.75)
legend("topright", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n', cex=.75)
#means by date
plot(A_mass.mean ~ starch.mean, data = photo_chem_campaign, col=volume, xlim=c(0, .2),
     ylim=c(100, 800),pch=pch2[volume], cex=2, ylab="", axes=FALSE)
box()
axis(1, at = meanX, labels = meanX)
title(main=datemean, line=-1.5, font.main=1, adj=.1, cex.main=.75)
d_ply(photo_chem_campaign, .(volume), function(x) c(add_trend_line("starch.mean", "A_mass.mean", x),
                                                    text(x$A_mass.mean~x$starch.mean, labels=unique(x$campaign), 
                                                         font=2,cex=.75)))
dev.copy2pdf(file="output/stats_plots/amaxstarch.pdf")
dev.off()


