#THis script has the code to calculate predictions from simple and full models
#but plots only from the full models now
#when testing just free plants the two models are the same

#simple model plots moved to end

#source functions and packages
source("functions and packages/startscripts.R")
require(lmerTest)

photo_chem <- read.csv("calculated data/Amax_chem.csv")
#run volume format func
photo_chem<- vollab_func(photo_chem)

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
# Afit <- lm(A_mass ~ Nmass_notnc * starch, data=photo_chem)
# p <- coef(Afit)
# Afit2 <- lm(A_mass ~ starch*Nmass_notnc, data=photo_chem)
# p2 <- coef(Afit2)


Afit_full <- lmer(A_mass ~ Nmass_notnc+starch+Nmass_notnc:starch + (1|ID), data=photo_chem)
f <- fixef(Afit_full)
Afit2_full <- lmer(A_mass ~ starch+Nmass_notnc+starch:Nmass_notnc + (1|ID), data=photo_chem)
f2 <- fixef(Afit2_full)


#plot and predict (2 graphs)---------------------------------------------------------------------
cols <- c("green2", "cyan4", "blue2", "darkviolet", "red")
coln <- c("red", "darkviolet", "blue2", "cyan4", "green2")
binlab <- c("0-4", "4-8", "8-12", "12-16", ">16")
pchbin <- c(1,2,3,4,5)
legpch <- c(5, 10, 15, 20, 25, 35, "free")


#Amass vs N (full model)
with(photo_chem, plot(Nmass_notnc, A_mass, pch=pchs[volume], col=cols[starchbin], 
                      ylab="", xlab=nmass_noTNC,ylim=c(0,800)))
for(i in 1:length(starchbin)){
  
  x <- seq(min(photo_chem$Nmass_notnc), max(photo_chem$Nmass_notnc), length=101)
  y <- f[[1]] + f[[2]]*x + f[[3]]*starchmid[i] + f[[4]]*starchmid[i]*x
  lines(x,y, col=cols[i], lwd=2)
}
title(ylab=Amasslab, mgp=ypos)
legend("topleft", binlab, pch=15, text.font=1, inset=0.005, col=cols,title=starchlab , bty='n')

#Amass vs starch (full model) 
with(photo_chem, plot(starch, A_mass, pch=19, col=coln[nitrobin], ylim=c(0,800), ylab="",
                      xlab=starchlab,xlim=c(0, .275)))
for(i in 1:length(nitrobin)){
  
  x <- seq(0, max(photo_chem$starch), length=101)
  y <- f2[[1]] + f2[[2]]*x + f2[[3]]*nitromid[i] + f2[[4]]*nitromid[i]*x
  lines(x,y, col=coln[i], lwd=2)
}
title(ylab=Amasslab, mgp=ypos)
legend("topright", binlab, pch=15, text.font=1, inset=0.01, col=coln,title=nfree , bty='n')


# ###using predict instead of eqautions####
# dfr <- expand.grid(starch = c(0.02, 0.06, 0.1, 0.14, 0.2),
#                    Nmass_notnc = seq(0.0007432, 0.0175502, length=25))
# dfr$Amass_pred <- predict(Afit_full, dfr, re.form=NA)


# #--------------------------------------------------------------------
# photo_chem2 <- subset(photo_chem, volume !="1000")
# coln2 <- c("red", "darkviolet", "blue2", "cyan4")
# 
# Amean <- mean(photo_chem2$A_mass)
# Nmean <- mean(photo_chem2$Nmass_notnc)
# Smean <- mean(photo_chem2$starch)
# 
# #use two fits so the equations and stay the same for pred, y=b+b1*x1+b2*x2
# 
# #simple
# lmpot <- lm(A_mass ~ Nmass_notnc * starch, data=photo_chem2)
# potc <- coef(lmpot)
# lmpot2 <- lm(A_mass ~ starch*Nmass_notnc, data=photo_chem2)
# potc2 <- coef(lmpot2)
# 
# #full
# lmrpot <- lmer(A_mass ~ Nmass_notnc+starch+Nmass_notnc:starch + (1|ID), data=photo_chem2)
# potf <- fixef(lmrpot)
# lmrpot2 <- lmer(A_mass ~ starch+Nmass_notnc+starch:Nmass_notnc + (1|ID), data=photo_chem2)
# potf2 <- fixef(lmrpot2)
# 
# #set bins
# photo_chem2$starchbin <- cut(photo_chem2$starch, breaks = starchbin)
# photo_chem2$nitrobin <- cut(photo_chem2$Nmass_notnc, breaks = nitrobin)
# 
# 
# #Amass vs N (no free)
# 
# #simple model
# windows()
# with(photo_chem2, plot(Nmass_notnc, A_mass, pch=16, col=cols[starchbin], ylim=c(0,800)))
# for(i in 1:length(starchbin)){
#   
#   x <- seq(min(photo_chem2$Nmass_notnc), max(photo_chem2$Nmass_notnc), length=101)
#   y <- potc[[1]] + potc[[2]]*x + potc[[3]]*starchmid[i] + potc[[4]]*starchmid[i]*x
#   lines(x,y, col=cols[i], lwd=2)
# }
# legend("topleft", binlab, pch=15, text.font=1, inset=0.02, col=cols,title=starchlab , bty='n')
# title(main="Within Pots")
# 
# #full model
# windows()
# with(photo_chem2, plot(Nmass_notnc, A_mass, pch=16, col=cols[starchbin], ylim=c(0,800)))
# for(i in 1:length(starchbin)){
#   
#   x <- seq(min(photo_chem2$Nmass_notnc), max(photo_chem2$Nmass_notnc), length=101)
#   y <- potf[[1]] + potf[[2]]*x + potf[[3]]*starchmid[i] + potf[[4]]*starchmid[i]*x
#   lines(x,y, col=cols[i], lwd=2)
# }
# legend("topleft", binlab, pch=15, text.font=1, inset=0.02, col=cols,title=starchlab , bty='n')
# title(main="Within Pots")
# 
# 
# 
# #Amass vs starch (no free)
# 
# #simple model
# windows()
# with(photo_chem2, plot(starch, A_mass, pch=19, col=coln2[nitrobin], ylim=c(0,800), xlim=c(0, .275)))
# for(i in 1:length(nitrobin)){
#   
#   x <- seq(min(photo_chem2$starch), max(photo_chem2$starch), length=101)
#   y <- potc2[[1]] + potc2[[2]]*x + potc2[[3]]*nitromid[i] + potc2[[4]]*nitromid[i]*x
#   lines(x,y, col=coln2[i], lwd=2)
# }
# legend("topright", binlab, pch=15, text.font=1, inset=0.02, col=coln,title=nfree , bty='n')
# title(main="Within Pots")
# 
# 
# #full model
# windows()
# with(photo_chem2, plot(starch, A_mass, pch=19, col=coln2[nitrobin], ylim=c(0,800), xlim=c(0, .275)))
# for(i in 1:length(nitrobin)){
#   
#   x <- seq(min(photo_chem2$starch), max(photo_chem2$starch), length=101)
#   y <- potf2[[1]] + potf2[[2]]*x + potf2[[3]]*nitromid[i] + potf2[[4]]*nitromid[i]*x
#   lines(x,y, col=coln2[i], lwd=2)
# }
# legend("topright", binlab, pch=15, text.font=1, inset=0.02, col=coln,title=nfree , bty='n')
# title(main="Within Pots")
# 
# 
# 
# 
# #only free-----------------------------------------------------------------------------------
# 
# photo_chem3 <- subset(photo_chem, volume =="1000")
# coln3 <- c("darkviolet", "blue2", "cyan4", "green2")
# 
# #look at similar effects with pots and non pots
# #make bin levels for both starch and nitrogen
# starchbin3 <- c(0, 0.04, 0.08, 0.12, 0.16)
# nitrobin3 <- c(0.0025, 0.005, 0.0075, 0.01, 100)
# #make midpoints for pred
# starchmid3 <- c(0.02, 0.06, 0.1, 0.14)
# nitromid3 <- c(0.00375, 0.00625, 0.00875, 0.0125)
# 
# #use two fits so the equations and stay the same for pred, y=b+b1*x1+b2*x2
# 
# ###simple and full model have the same coefs
# #simple model
# lmfree <- lm(A_mass ~ Nmass_notnc * starch, data=photo_chem3)
# Fc <- coef(lmfree)
# lmfree2 <- lm(A_mass ~ starch*Nmass_notnc, data=photo_chem3)
# Fc2 <- coef(lmfree2)
# 
# 
# #set bins in raw dfr
# photo_chem3$starchbin3 <- cut(photo_chem3$starch, breaks = starchbin3)
# photo_chem3$nitrobin3 <- cut(photo_chem3$Nmass_notnc, breaks = nitrobin3)
# 
# #Amass vs N (free only)----------------------
# 
# #simple
# windows()
# with(photo_chem3, plot(Nmass_notnc, A_mass, pch=16, col=cols[starchbin3], ylim=c(0,800)))
# for(i in 1:length(starchbin3)){
#   
#   x <- seq(min(photo_chem3$Nmass_notnc), max(photo_chem3$Nmass_notnc), length=101)
#   y <- Fc[[1]] + Fc[[2]]*x + Fc[[3]]*starchmid3[i] + Fc[[4]]*starchmid3[i]*x
#   lines(x,y, col=cols[i], lwd=2)
# }
# legend("bottomright", binlab, pch=15, text.font=1, inset=0.02, col=cols,title=starchlab , bty='n')
# title(main="Free")
# 
# 
# #Amass vs starch (free only)
# windows()
# with(photo_chem3, plot(starch, A_mass, pch=19, col=coln3[nitrobin3], ylim=c(0,800), xlim=c(0, .20)))
# for(i in 1:length(nitrobin3)){ 
#   x <- seq(min(photo_chem3$starch), max(photo_chem3$starch), length=101)
#   y <- Fcf2[[1]] + Fcf2[[2]]*x + Fcf2[[3]]*nitromid3[i] + Fcf2[[4]]*nitromid3[i]*x
#   lines(x,y, col=coln3[i], lwd=2)
# }
# legend("topright", binlab, pch=15, text.font=1, inset=0.02, col=coln,title=nfree , bty='n')
# title(main="Free")
# 
# 
# 
# 
# 
# 
# 
# ###simple model plots------------------------------------------------------------------------------
# #Amass vs N 
# windows()
# with(photo_chem, plot(Nmass_notnc, A_mass, pch=pchs[volume], col=cols[starchbin], ylim=c(0,800)))
# for(i in 1:length(starchbin)){
#   
#   x <- seq(min(photo_chem$Nmass_notnc), max(photo_chem$Nmass_notnc), length=101)
#   y <- p[[1]] + p[[2]]*x + p[[3]]*starchmid[i] + p[[4]]*starchmid[i]*x
#   lines(x,y, col=cols[i], lwd=2)
# }
# title(main="Simple Model", line=-1.5, font.main=1)
# legend("topleft", binlab, pch=15, text.font=1, inset=0.01, col=cols,title=starchlab , bty='n')
# 
# #Amass vs starch
# 
# #simple model
# windows()
# with(photo_chem, plot(starch, A_mass, pch=19, col=coln[nitrobin], ylim=c(0,800), xlim=c(0, .275)))
# for(i in 1:length(nitrobin)){
#   
#   x <- seq(0, max(photo_chem$starch), length=101)
#   y <- p2[[1]] + p2[[2]]*x + p2[[3]]*nitromid[i] + p2[[4]]*nitromid[i]*x
#   lines(x,y, col=coln[i], lwd=2)
# }
# legend("topright", binlab, pch=15, text.font=1, inset=0.01, col=coln,title=nfree , bty='n')







