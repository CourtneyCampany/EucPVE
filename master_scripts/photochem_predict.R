#THis script has the code to calculate predictions from simple and full models
#but plots only from the full models now
#when testing just free plants the two models are the same

#simple model plots moved to end

#source functions and packages
source("functions and packages/startscripts.R")
require(lme4)
require(lmerTest)
library(effects)
source("functions and packages/rsquared_glmm.R")
#runthe new r2 function to get r2 and p values for the model


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

#create models and extract coefs  
#use two fits so the equations stay the same for pred, y=b+b1*x1+b2*x2


#model and stats
Afit_full <- lmer(A_mass ~ Nmass_notnc+starch+Nmass_notnc:starch + (1|ID), data=photo_chem)
  anova(Afit_full)
  summary(Afit_full)
  #with lmer tnc not significant nor the interaction
  rsquared.glmm(list(Afit_full, Afit_N, Afit_TNC))
  plot(effect("Nmass_notnc:starch", Afit_full), multiline=TRUE)

  require(car)
  Anova(Afit_full)
  visreg(Afit_full, "starch", by="Nmass_notnc", overlay=TRUE)

#####model parameters for plotting
f <- fixef(Afit_full)

#try log A
Afit_logA <- lmer(log(A_mass) ~ Nmass_notnc+starch+Nmass_notnc:starch + (1|ID), data=photo_chem)
  anova(Afit_logA)
  summary(Afit_logA)
  visreg(Afit_logA)
  #TNC and N sig, no interaction

#old nlmesays is is significant
library(nlme)
Afit_full_lme <- lme(A_mass ~ Nmass_notnc+starch+Nmass_notnc:starch, random=~1|ID, data=photo_chem)
  anova(Afit_full_lme)
  summary(Afit_full_lme)


# remove interaction, starch very significant
Afit_almostfull <- lmer(A_mass ~ Nmass_notnc+starch + (1|ID), data=photo_chem)
  #need fixed effects from almost model
  fixed.effects(Afit_almostfull)
  #need something with ci and fixed effect size to 

# likelihood ratio test
Afit_almostfull2 <- lmer(A_mass ~ Nmass_notnc+ (1|ID), data=photo_chem)
  anova(Afit_full, Afit_almostfull2)




###same model with fixed effects in different order for ease of plotting preductions later
Afit2_full <- lmer(A_mass ~ starch+Nmass_notnc+starch:Nmass_notnc + (1|ID), data=photo_chem)
  f2 <- fixef(Afit2_full)


#plot and predict (2 graphs)---------------------------------------------------------------------
cols <- c("green3", "cyan4", "#0081FFFF", "darkviolet", "red")
#cols <- c("forestgreen", "#80FE1AFF","#ECFD08FF","#FF7500FF","#FF3300FF")
#coln <- c("#FF3300FF", "#FF7500FF","#ECFD08FF",  "#80FE1AFF", "forestgreen" )
coln <- c("red", "darkviolet", "#0081FFFF", "cyan4", "green3")
binlab <- c("0-4", "4-8", "8-12", "12-16", ">16")
binlab2 <- c(" 0    -.001", ".001-.003", ".003-.006", ".006-.008", ".008-.012")

pchbin <- c(1,2,3,4,5)
legpch <- c(5, 10, 15, 20, 25, 35, "free")


#Amass vs N (full model)
png(filename = "output/presentations/Amass_N.png", width = 12, height = 8, units = "in", res= 400)
par(cex.axis=1.3, cex.lab=1.3)
with(photo_chem, plot(Nmass_notnc, A_mass, pch=pchs[volume], col=cols[starchbin], 
                      ylab="", xlab=nmass_noTNC,ylim=c(0,800),cex=1.3))
for(i in 1:length(starchbin)){
  
  x <- seq(min(photo_chem$Nmass_notnc), max(photo_chem$Nmass_notnc), length=101)
  y <- f[[1]] + f[[2]]*x + f[[3]]*starchmid[i] + f[[4]]*starchmid[i]*x
  lines(x,y, col=cols[i], lwd=2)
}
title(ylab=Amasslab, mgp=ypos)
legend("topleft", binlab, pch=15, text.font=1.3, inset=0.02, col=cols,title=starchlab , bty='n', cex=1.3)
dev.off()


#Amass vs starch (full model) 
png(filename = "output/presentations/Amass_TNC.png", width = 12, height = 8, units = "in", res= 400)
par(cex.axis=1.3, cex.lab=1.3)
with(photo_chem, plot(starch, A_mass, pch=19, col=coln[nitrobin], ylim=c(0,800), ylab="",
                      xlab=starchlab,xlim=c(0, .275),cex=1.3))
for(i in 1:length(nitrobin)){
  
  x <- seq(0, max(photo_chem$starch), length=101)
  y <- f2[[1]] + f2[[2]]*x + f2[[3]]*nitromid[i] + f2[[4]]*nitromid[i]*x
  lines(x,y, col=coln[i], lwd=2)
}
title(ylab=Amasslab, mgp=ypos)
legend("topright", binlab2, pch=16, text.font=1.3, inset=0.02, col=coln,title=nfree , bty='n', cex=1.3)
dev.off()



###two panel of graph above, using predict------------------------------------------------------------

#using predict instead of eqautions
Npred <- expand.grid(starch = c(0.02, 0.06, 0.1, 0.14, 0.2),
                     Nmass_notnc = seq(0.0007432, 0.0175502, length=101))
  Npred$Amass_pred <- predict(Afit_full, Npred, re.form=NA)

TNCpred <- expand.grid(Nmass_notnc = c(0.00125, 0.00375, 0.00625, 0.00875, 0.0125),
                       starch = seq(min(photo_chem$starch), max(photo_chem$starch), length=101))
  TNCpred$Amass_pred <- predict(Afit2_full, TNCpred, re.form=NA)




with(photo_chem, plot(starch, A_mass, pch=19, col=coln[nitrobin], ylim=c(0,800), ylab="",
                      xlab=starchlab,xlim=c(0, .275),cex=1.3))
for(i in 1:length(nitrobin)){
  
  x <- subset(Npred$starch, 
  y <- Npred$Amass_pred
  lines(x,y, col=coln[i], lwd=2)
}





####plot Asat?
# with(photo_chem, plot(Nmass_notnc, Photo, pch=pchs[volume], col=cols[starchbin], 
#                       ylab="", xlab=nmass_noTNC,ylim=c(0,35),cex=1.3))
# for(i in 1:length(starchbin)){
#   
#   x <- seq(min(photo_chem$Nmass_notnc), max(photo_chem$Nmass_notnc), length=101)
#   y <- f[[1]] + f[[2]]*x + f[[3]]*starchmid[i] + f[[4]]*starchmid[i]*x
#   lines(x,y, col=cols[i], lwd=2)
# }



