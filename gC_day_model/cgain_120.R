source("functions and packages/plot objects.R")
source("functions and packages/functions.R")

library(plyr)
library(doBy)
library(scales)

source("gC_day_model/model_start.R")
source("functions and packages/massmodel.R")

####120 days of ALEAF---------------------------------------------------------------------------------------------------

#list of Cday over 120 days by trt
  Cday_120 <- dlply(Aleaf, .(volume))

  c120_trt<- lapply(Cday_120, "[", 3)
  

#1: model using mean allocation and Cday values over 120 days by volume treatment 
sim120_all <- lapply(c120_trt, function(x) {data.frame(productionmodel(gCday=as.vector(x[1:121,]), lma=lma_mean, 
                               frfrac=fr_frac_mean,crfrac=cr_frac_mean,stemfrac=stem_frac_mean, 
                               leaffrac=lf, returnwhat="all"))
})


#2: add in allocation and lma by treatment

for (i in 1:7){
  sim120_alloc <- lapply(c120_trt, function(x) {data.frame(productionmodel(gCday=as.vector(x[1:121,]), 
                lma=lma_trt[i],frfrac=frfrac_trt[i], crfrac=crfrac_trt[i], stemfrac=stemfrac_trt[i],
                leaffrac=leaffrac_trt[i],returnwhat="all"))
                })
}



##Plotting setup-----------------------------------------------------------------------------------------------------

Date<- seq(as.Date("2013-01-22"),as.Date("2013-05-21"), by="days")
cols <- gradient(7)

#harvest mass (treatment means)
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
  mass_actual$Date <- as.Date("2013-05-21")
  mass_actual$mass_adj <- with(mass_actual, mass/mass[7])

#leaf area interpolated
leafarea_time2 <- read.csv("calculated data/cumulative leaf area.csv")
  leafarea_time2 <-datevol_func (leafarea_time2)

la_sp2 <- dlply(leafarea_time, .(volume))

#cday means by treatment
Cday_means <- read.csv("calculated data/model_runs/gCday_means.csv")
  Cday_means$C_stnd_free <- with(Cday_means, carbon_day/carbon_day[7])
  
#total carbon gain per plant (sum of leaf area times gCday) #### test for model vs LA constrained
  totalC_list <- list()
  for(i in 1:7){
  totalC_calc <- sim120_alloc[[i]][2] * c120_trt[[i]][1:120,]
  
  totalC_list[[i]] <- totalC_calc
  }                 

  totalC_trt <- lapply(totalC_list, function(x) sum(x))
  
  totalC_trt2 <- unlist(totalC_trt)

#1. Total C gain in g vs biomass

  windows(8,10)
  par(mar=c(5,5,2,2))
plot(mass_actual$mass ~ totalC_trt2,pch=pchs,col=palette(),cex=1.6, xlim=c(0,300), 
     ylim=c(0, 300), ylab="Biomass (g)", xlab="Total Carbon Gain (g)")
  for(i in 1:7){
    points(sim120_alloc[[i]][120,1]~ totalC_trt2[i], pch=pch2, col=cols[i], cex=1.6)
  }
  text(x=65, 265, "Uses leaf area from production model", cex=1)
  dev.copy2pdf(file= "gC_day_model/model_output/totalCgain.pdf")  
  dev.off() 
  

#2: plot mean treatment sim vs interpolated leaf area
windows(10,8)
par(mar=c(5,5,2,2))
plot(1, xlab="", ylab=LAm2, ylim=c(0, .6), xlim=range(Date), type='n', axes=FALSE)
axis.Date(Date, side=1)
axis(2)
for(i in 1:length(sim120_all)){
  lines(Date, sim120_all[[i]]$leafarea, col=cols[i], lwd=2)
}
d_ply(leafarea_time2, .(volume), function(x) points(x$canopysqm.mean ~ x$Date,  
                      col=x$volume, pch = pchs[x$volume],type='b', lwd=2))
box()
  text(x=15740, .58, "Cday 120 leaf area", cex=1)
  
  dev.copy2pdf(file= "gC_day_model/model_output/LA_cday120.pdf")  
  dev.off() 
  

#3: plot mean trt sim vs actual mass
windows(10,8)
par(mar=c(5,5,2,2))
plot(1, xlab="", ylab="Biomass  (g)", ylim=c(0, 200), xlim=range(Date), type='n', axes=FALSE)
axis.Date(Date, side=1)
axis(2)
for(i in 1:length(sim120_all)){
  lines(Date, sim120_all[[i]]$biomass, col=cols[i], lwd=2)
}
points(mass~Date, data=mass_actual, pch=pchs,  col=cols,cex=2)
box()
  
text(x=15740, 185, "Cday 120", cex=1)
  
dev.copy2pdf(file= "gC_day_model/model_output/biomass_cday120.pdf")  
dev.off() 
  
#4: plot trt sim vs actual mass
windows(10,8)
par(mar=c(5,5,2,2))
plot(1, xlab="", ylab="Biomass  (g)", ylim=c(0, 350), xlim=range(Date), type='n', axes=FALSE)
axis.Date(Date, side=1)
axis(2)
for(i in 1:length(sim120_alloc)){
  lines(Date, sim120_alloc[[i]]$biomass, col=cols[i], lwd=2)
  }
  points(mass~Date, data=mass_actual, pch=pchs,  col=cols,cex=2)
  box()
  
text(x=15740, 350, "Cday 120 with mass allocation", cex=1)
  
dev.copy2pdf(file= "gC_day_model/model_output/biomass_cday120_alloc.pdf")  
dev.off()   
  
  
  
######now use these values and with Cday scaler to free seedling with 101 seqeunce------------------------------------------
  
#reduction in photosynthesis
cdayscale <- seq(1, min(Aleaf_agg$Cday_scale), length=121)
  
#####run model for each volume with 120days of Cday
free_scale_sim<- list()
for(j in 1:121){
    free_scale_sim[[j]] <- productionmodel(gCday=as.vector(c120_trt[[7]][1:121,])*cdayscale[j], lma=lma_mean, 
                                           frfrac=fr_frac_mean, crfrac=cr_frac_mean, stemfrac=stem_frac_mean,leaffrac=lf)
  }
free_scaled_sim2 <- as.data.frame(do.call(rbind,free_scale_sim))
free_scaled_sim2$cday_scale <- cdayscale
free_scaled_sim2$mass_scale <- with(free_scaled_sim2, biomass/biomass[1])
  


##plot scaled model run to free
col_bl <- alpha("black", .50)

windows(10,8)
plot(mass_scale ~ cdayscale, data=free_scaled_sim2, type='l', lwd=4,pch=16, col=col_bl, xlim=c(1,.6), ylim=c(0, 1))
points(mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)
text(x=15740, 350, "Cday 120 scaled free", cex=1)
dev.copy2pdf(file= "gC_day_model/model_output/cday120_scaled_F.pdf")  
dev.off()   

