source("functions and packages/plot objects.R")
source("functions and packages/functions.R")

library(plyr)
library(doBy)
library(scales)


source("gC_day_model/model_start.R")
source("functions and packages/massmodel_LAconstrain.R")

####model #1------------------------------------------------------------------------------------------------------------

##120 days of ALEAF

#1. list of Cday over 120 days by trt
Cday_120 <- dlply(Aleaf, .(volume))

c120_trt<- lapply(Cday_120, "[", 3)

#2. self shading parameter by treatment
M <- read.csv("calculated data/M_volume.csv")
M_trt <- M[,2]


#1. Model using mean allocation, 120 days of Cday, constrainted by LA

simLA_M <- list()
for (i in 1:7){
  dat120<- data.frame(modelLAlimit(gCday=(as.vector(c120_trt[[i]][1:121,])*M_trt[i]), LA=as.vector(LA_sp[[i]][1:121,3]),
                                   frfrac=fr_frac_mean, crfrac=cr_frac_mean,stemfrac=stem_frac_mean, lma=lma_mean,
                                   leaffrac=lf,returnwhat="all"))
  simLA_M[[i]] <- dat120
}

#2. Model using allocation by treatment, 120 days of Cday, constrainted by LA

simLA_M2 <- list()
for (i in 1:7){
  alloc120<- data.frame(modelLAlimit(gCday=(as.vector(c120_trt[[i]][1:121,])*M_trt[i]), LA=as.vector(LA_sp[[i]][1:121,3]),
                                     lma=lma_trt[i],frfrac=frfrac_trt[i], crfrac=crfrac_trt[i], stemfrac=stemfrac_trt[i],
                                     leaffrac=leaffrac_trt[i],returnwhat="all"))
  simLA_M2[[i]] <- alloc120
}

#plotting----------------------------------------------------------------------------------------------------

##mass actual
#harvest mass
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
  mass_actual$Date <- as.Date("2013-05-21")
  mass_actual$mass_adj <- with(mass_actual, mass/mass[7])


Date<- seq(as.Date("2013-01-22"),as.Date("2013-05-21"), by="days")
cols <- gradient(7)

#1: plot mean treatment sim constrained by LA with self shading
windows(10,8)
plot(1, xlab="", ylab="Biomass  (g)",ylim=c(0, 300), xlim=range(Date), type='n', axes=FALSE)
axis.Date(Date, side=1)
axis(2)
for(i in 1:length(simLA_M)){
  lines(Date, simLA_M[[i]]$biomass, col=cols[i], lwd=2)
}

points(mass~Date, data=mass_actual, pch=pchs,  col=cols,cex=2)
text(x=15750, 285, "Leaf area constrained with self shading", cex=1)
box()
dev.copy2pdf(file= "gC_day_model/model_output/biomass_LA_M.pdf")
dev.off()


#1: plot allocat by treatment constrained by LA with self shading
windows(10,8)
plot(1, xlab="", ylab="Biomass  (g)", ylim=c(0, 300), xlim=range(Date), type='n', axes=FALSE)
axis.Date(Date, side=1)
axis(2)
for(i in 1:length(simLA_M2)){
  lines(Date, simLA_M2[[i]]$biomass, col=cols[i], lwd=2)
}

box()
points(mass~Date, data=mass_actual, pch=pchs,  col=cols,cex=2)

text(x=15765, 290, "Leaf area constrained with self shading and harvest allocation", cex=1)

dev.copy2pdf(file= "gC_day_model/model_output/biomass_LA_M_alloc.pdf")
dev.off()

######now use these values and with Cday scaler to free seedling with seqeunce------------------------------------------

#reduction in photosynthesis
cdayscale <- seq(1, min(Aleaf_agg$Cday_scale), length=121)

#cday means by treatment
Cday_means <- read.csv("calculated data/model_runs/gCday_means.csv")
  Cday_means$C_stnd_free <- with(Cday_means, carbon_day/carbon_day[7])

#####scaled model with 120 Cday, LA restrict,alloc =mean and selfshading
free_scale_LA<- list()
for(j in 1:121){
  free_scale_LA[[j]] <- modelLAlimit(gCday=as.vector(c120_trt[[7]][1:121,])*M_trt[7]*cdayscale[j], 
                                     LA=as.vector(LA_sp[[7]][1:121,3]),
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


