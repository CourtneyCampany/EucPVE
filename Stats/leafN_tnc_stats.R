source("functions and packages/functions.R")
source("functions and packages/load packages.R")
source("functions and packages/plot objects.R")

#read TNC and calculated leafN, merge

tnc_leaf <- read.csv("calculated data/leaf_tnc.csv")
N_leaf <- read.csv( "calculated data/leaf N content.csv")

leaf_traits <- merge(N_leaf, tnc_leaf[,c(1:4,8:11)])
leaf_traits$volume <- as.factor(leaf_traits$volume)
leaf_traits$Date <- as.Date(leaf_traits$Date)
leaf_traits$invest <- with(leaf_traits, Nmass/leafstarch)
leaf_traits$N_TNC <- with(leaf_traits, Nmass/(leafstarch+leafsugar))
#tnc free leaf N
leaf_traits$N_noTNC <- with(leaf_traits, (mass-(leafstarch+leafsugar))*Nperc)
leaf_traits$Narea_noTNC <- with(leaf_traits, N_noTNC/area)

#means

Nleafagg_noTNC<- summaryBy(N_noTNC +Narea_noTNC~ volume+Date, data=leaf_traits, FUN=c(mean, se))

#---------------------------------------------------------------------------------------
#plot raw data

#Nmass
windows()
par(mfrow=c(3,3))
leafN_mass<- dlply(leaf_traits, .(volume), function(x) c(plotBy(N_noTNC ~ Date | ID, how="col", type='o', 
              legend=F, col=x$volume, ylim=c(0, .02),lty=1, ylab="", xlab="", data=x), 
              title(main=paste("volume = ", unique(x$volume)), line=-1.5, font.main=1, adj=.05, cex.main=1),
              title(ylab=nmasslab, mgp=ypos)))
dev.copy2pdf(file="output/stats_plots/Nmass_noTNC_date.pdf")
dev.off()

#Nmass
windows()
par(mfrow=c(3,3))
leafN_mass<- dlply(leaf_traits, .(volume), function(x) c(plotBy(Narea_noTNC ~ Date | ID, how="col", type='o', 
             legend=F, col=x$volume, ylim=c(0, .0005),lty=1, ylab="", xlab="", data=x), 
             title(main=paste("volume = ", unique(x$volume)), line=-1.5, font.main=1, adj=.05, cex.main=1),
             title(ylab=narealab, mgp=ypos)))

dev.copy2pdf(file="output/stats_plots/Narea_noTNC_date.pdf")
dev.off()

#---------------------------------------------------------------------------------------
#treatment means

#Nmass
windows()
plot(N_noTNC.mean ~ Date, data = Nleafagg_noTNC, col=volume, cex=2, pch=pchs[volume], 
     ylim=c(0,.015), ylab="", xlab="")
title(ylab=nmasslab, mgp=ypos)
title(main=datemean, line=-1.5, font.main=1, adj=.05, cex.main=1)
d_ply(Nleafagg_noTNC, .(volume), function(x) add_trend_line("Date", "N_noTNC.mean", x, ))
legend("topright", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')

dev.copy2pdf(file="output/stats_plots/Nmass_noTNC_means.pdf")
dev.off()


#Narea
windows()
plot(Narea_noTNC.mean ~ Date, data = Nleafagg_noTNC, col=volume, cex=2, pch=pchs[volume], 
     ylim=c(0,.0004), ylab="", xlab="")
title(ylab=narealab, mgp=ypos)
title(main=datemean, line=-1.5, font.main=1, adj=.05, cex.main=1)
d_ply(Nleafagg_noTNC, .(volume), function(x) add_trend_line("Date", "Narea_noTNC.mean", x, ))
legend("topright", leglab, pch=pchs,text.font=1, inset=0.02, title=vollab, col=palette(), bty='n')

dev.copy2pdf(file="output/stats_plots/Narea_noTNC_means.pdf")
dev.off()




#old
#means(campaign)
leaf_traits_campaign <- ddply(leaf_traits, .(campaign, volume), summarise,
                              Nmass = mean(Nmass),  
                              Nmass_se = (sum(Nmass)/length(Nmass)),
                              leafstarch = mean(leafstarch),
                              leafstarch_se = (sum(leafstarch)/length(leafstarch)),
                              percstarch = mean(percstarch),
                              percstarch_se = (sum(percstarch)/length(percstarch)))


plot(leafstarch ~ Nmass, data = leaf_traits_campaign, col=volume ,pch=pchs[volume], cex=1.5,ylim=c(0,.06),  xlim=c(0, 0.01))
d_ply(leaf_traits_campaign, .(volume), function(x) add_trend_line("Nmass", "leafstarch", x))
legend("topright", leglab, pch=c(rep(16,6),17),text.font=3, inset=0.02,
       title=expression(Pot~volume~(l)), col=palette(), bty='n')
#--------------------------------------------------------------------------------
#means date

#means(all dates)
leaf_traits_date <- ddply(leaf_traits, .(volume), summarise,
                              Nmass = mean(Nmass),  
                              Nmass_se = (sum(Nmass)/length(Nmass)),
                              leafstarch = mean(leafstarch),
                              leafstarch_se = (sum(leafstarch)/length(leafstarch)),
                              percstarch = mean(percstarch),
                              percstarch_se = (sum(percstarch)/length(percstarch)),


plot(leafstarch ~ Nmass, data = leaf_traits_date, col=volume ,pch=pchs[volume], cex=1.5,ylim=c(0,.04),  xlim=c(0, 0.008))
legend("topright", leglab, pch=c(rep(16,6),17),text.font=3, inset=0.02,
       title=expression(Pot~volume~(l)), col=palette(), bty='n')



