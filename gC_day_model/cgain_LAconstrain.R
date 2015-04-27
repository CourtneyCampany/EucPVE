source("functions and packages/plot objects.R")
source("functions and packages/functions.R")

library(plyr)
library(doBy)
library(scales)


source("gC_day_model/model_start.R")
source("functions and packages/massmodel_LAconstrain.R")

####model sims with 120days of ALEAF constrained by LA-------------------------------------------------------------------

# list of Cday over 120 days by trt
Cday_120 <- dlply(Aleaf, .(volume))

c120_trt<- lapply(Cday_120, "[", 3)


#1. Model using mean allocation, 120 days of Cday, constrainted by LA

simLA <- list()
for (i in 1:7){
  dat120<- data.frame(modelLAlimit(gCday=as.vector(c120_trt[[i]][1:121,]), LA=as.vector(LA_sp[[i]][1:121,3]),
                                   frfrac=fr_frac_mean, crfrac=cr_frac_mean,stemfrac=stem_frac_mean, lma=lma_mean,
                                   leaffrac=lf,returnwhat="all"))
  simLA[[i]] <- dat120
}

#2. Model using allocation by treatment, 120 days of Cday, constrainted by LA

simLA_alloc <- list()
for (i in 1:7){
  alloc120<- data.frame(modelLAlimit(gCday=as.vector(c120_trt[[i]][1:121,]), LA=as.vector(LA_sp[[i]][1:121,3]),
                                   lma=lma_trt[i],frfrac=frfrac_trt[i], crfrac=crfrac_trt[i], stemfrac=stemfrac_trt[i],
                                   leaffrac=leaffrac_trt[i],returnwhat="all"))
  simLA_alloc[[i]] <- alloc120
}


####plotting--------------------------------------------------------------------------------------------------------------------

#harvest mass
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
mass_actual$Date <- as.Date("2013-05-21")
mass_actual$mass_adj <- with(mass_actual, mass/mass[7])

##date sequence and colors
Date<- seq(as.Date("2013-01-22"),as.Date("2013-05-21"), by="days")
cols <- gradient(7)


#1: plot mean treatment sim vs actual biomass
windows(10,8)
plot(1, xlab="", ylab="Biomass  (g)", ylim=c(0, 300), xlim=range(Date), type='n', axes=FALSE)
axis.Date(Date, side=1)
axis(2)
for(i in 1:length(simLA)){
  lines(Date, simLA[[i]]$biomass, col=cols[i], lwd=2)
}

box()
points(mass~Date, data=mass_actual, pch=pchs,  col=cols,cex=2)

text(x=15740, 285, "Leaf area constrained", cex=1)


dev.copy2pdf(file= "gC_day_model/model_output/biomass_LA.pdf")
dev.off() 

#2: plot alloc by treatment sim vs actual biomass
windows(10,8)
plot(1, xlab="", ylab="Biomass  (g)", ylim=c(0, 300), xlim=range(Date), type='n', axes=FALSE)
axis.Date(Date, side=1)
axis(2)
for(i in 1:length(simLA_alloc)){
  lines(Date, simLA_alloc[[i]]$biomass, col=cols[i], lwd=2)
}

box()
points(mass~Date, data=mass_actual, pch=pchs,  col=cols,cex=2)

text(x=15750, 285, "Leaf area constrained with harvest allocation", cex=1)

dev.copy2pdf(file= "gC_day_model/model_output/biomass_LA_alloc.pdf")
dev.off()

######now use these values and with Cday scaler to free seedling with 101 seqeunce------------------------------------------

#reduction in photosynthesis
cdayscale <- seq(1, min(Aleaf_agg$Cday_scale), length=121)

#cday means by treatment
Cday_means <- read.csv("calculated data/model_runs/gCday_means.csv")
  Cday_means$C_stnd_free <- with(Cday_means, carbon_day/carbon_day[7])

#####scaled model with 120 Cday, LA restrict and alloc =mean
free_scale_LA<- list()
for(j in 1:121){
  free_scale_LA[[j]] <- modelLAlimit(gCday=as.vector(c120_trt[[7]][1:121,])*cdayscale[j], LA=as.vector(LA_sp[[7]][1:121,3]),
                                      lma=lma_mean, frfrac=fr_frac_mean, crfrac=cr_frac_mean, stemfrac=stem_frac_mean,
                                      leaffrac=lf)
}
free_scale_LA2 <- as.data.frame(do.call(rbind,free_scale_LA))
free_scale_LA2$cday_scale <- cdayscale
free_scale_LA2$mass_scale <- with(free_scale_LA2, biomass/biomass[1])


##plot scaled model run to free
col_bl <- alpha("black", .50)

plot(mass_scale ~ cdayscale, data=free_scale_LA2, type='l', lwd=4,pch=16, col=col_bl, xlim=c(1,.6), ylim=c(0, 1))
points(mass_actual$mass_adj ~ Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)



