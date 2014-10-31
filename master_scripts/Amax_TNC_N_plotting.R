#this script is for sourcing the Amax vs TNC and N plots into master
source("functions and packages/startscripts.R")

require(lme4)
require(scales)

photo_chem <- read.csv("calculated data/Amax_chem.csv")
#run volume format func
photo_chem<- vollab_func(photo_chem)

#model
Afit_full <- lmer(A_mass ~ Nmass_notnc+starch+Nmass_notnc:starch + (1|ID), data=photo_chem)

###same model with fixed effects in different order for ease of plotting preductions later
Afit2_full <- lmer(A_mass ~ starch+Nmass_notnc+starch:Nmass_notnc + (1|ID), data=photo_chem)


###two panel of graph above, using predict------------------------------------------------------------

#make bin levelsfor starch and nitrogen
starchbin <- c(0, 0.04, 0.08, 0.12, 0.16, 100)
nitrobin <- c(0, 0.0025, 0.005, 0.0075, 0.01, 100)

#plot objects
cols <- c("green3", "cyan4", "#0081FFFF", "darkviolet", "red")
coln <- c("red", "darkviolet", "#0081FFFF", "cyan4", "green3")

coln2 <- alpha(coln, .75)
cols2 <- alpha(cols, .75)

binlab <- c("0-4", "4-8", "8-12", "12-16", ">16")
binlab2 <- c(" 0    -.001", ".001-.003", ".003-.006", ".006-.008", ".008-.012")

#using predict instead of eqautions
Npred <- expand.grid(starch = c(0.02, 0.06, 0.1, 0.14, 0.2),
                     Nmass_notnc = seq(0.0007432, 0.0175502, length=101))
Npred$Amass_pred <- predict(Afit_full, Npred, re.form=NA)

TNCpred <- expand.grid(Nmass_notnc = c(0.00125, 0.00375, 0.00625, 0.00875, 0.0125),
                       starch = seq(min(photo_chem$starch), max(photo_chem$starch), length=101))
TNCpred$Amass_pred <- predict(Afit2_full, TNCpred, re.form=NA)

windows(12, 8)
par(cex.axis=1.5, cex.lab=1.5,
    mfrow=c(1,2),  
    omi=c(1,1,0.1,0.1),  # outer margin (inches)
    mar=c(0,0,0,0))   # margin around plots (they are tight together) 

# First Panel
plot(A_mass ~ starch, data=photo_chem,type='n',ylim=c(0,800), xlim=c(0, 0.275),ylab="", axes=FALSE, xlab="")  
box()
axis(1, labels=TRUE) 
axis(2, labels=TRUE)  
mtext(expression(A[mass]~~(n*mol~g^-1~s^-1)), outer=TRUE, line=2.5, side=2, cex=1.5)
points(A_mass ~ starch, pch=21, data=photo_chem, bg=coln2[nitrobin], ylim=c(0,800), ylab="",
       xlab=starchlab,xlim=c(0, .275),cex=1.3, ylab="")

for(i in 1:length(unique(TNCpred$Nmass_notnc))){
  m <- subset(TNCpred, Nmass_notnc == unique(TNCpred$Nmass_notnc)[i])
  with(m, lines(starch, Amass_pred, col=coln[i], lwd=2))
}
legend("topright", binlab, pch=21, text.font=1.3, inset=0.02, pt.bg=coln2,title=starchlab , bty='n', cex=1.3) 

# Second Panel
plot(A_mass ~ Nmass_notnc, data=photo_chem, type='n',ylab="", axes=FALSE, xlim=c(0, 0.02), ylim=c(0,800), xlab="")
box()
axis(1, labels=TRUE) 
axis(2, labels=FALSE, tcl=0.5)  

points(A_mass ~ Nmass_notnc, data=photo_chem, pch=21, bg=cols2[starchbin], 
       ylab="", xlab=nmass_noTNC,ylim=c(0,800),cex=1.3)

for(i in 1:length(unique(Npred$starch))){
  d <- subset(Npred, starch == unique(Npred$starch)[i])
  with(d, lines(Nmass_notnc, Amass_pred, col=cols[i], lwd=2))
}
legend("bottomright", binlab2, pch=21, text.font=1.3, inset=0.02, pt.bg=cols2,title=nfree , bty='n', cex=1.3)  