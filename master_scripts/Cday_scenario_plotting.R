source("gC_day_model/model_plot_objects.R")


#read in observed mass and Cday---------------------------------------------------------------------------------
#harvest mass and leaf area for model comparison
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
mass_actual$mass_adj <- with(mass_actual, mass/mass[7])

Cday_means <- read.csv("calculated data/model_runs/gCday_means.csv")
#new variables with gC standardized to largest pot and free
Cday_means$C_stnd_pot <- with(Cday_means, carbon_day/carbon_day[6])
Cday_means$C_stnd_free <- with(Cday_means, carbon_day/carbon_day[7])
Cday <- as.vector(Cday_means[,2]) 


scaletofree_func <- function(dfr){
  #Gcday and mass relative to largest value
  dfr$C_adj <- with(dfr, gCday/max(gCday))
  dfr$mass_adj <- with(dfr, biomass/max(biomass))
  return(dfr)
}

#scenario with leaf allocation +-50%
leafturn_low <- read.csv("calculated data/model_runs/sim_leaflow.csv")
  leafturn_low <- scaletofree_func(leafturn_low)
leafturn_high <- read.csv("calculated data/model_runs/sim_leafhigh.csv")
  leafturn_high <- scaletofree_func(leafturn_high)


#scenario with root respiration +-50%
rootresp_low <- read.csv("calculated data/model_runs/sim_rootresp_low.csv")
  rootresp_low <- scaletofree_func(rootresp_low)
rootresp_high <- read.csv("calculated data/model_runs/sim_rootresp_high.csv")
  rootresp_high <- scaletofree_func(rootresp_high)

#scenario with increase C allocation to fine roots +-50%
frexude_low <- read.csv("calculated data/model_runs/sim_frexudelow.csv")
  frexude_low <- scaletofree_func(frexude_low)
frexude_high <- read.csv("calculated data/model_runs/sim_frexudehigh.csv")
  frexude_high <- scaletofree_func(frexude_high)






#plotting---------------------------------------------------------------------------------------
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)

with(leafturn_low, plot(C_adj,mass_adj, xlim=c(1,.4),ylim=c(0, 1), ylab= expression(Biomass~Scaled[free]), 
                   xlab=expression(Daily~Carbon~Assimilation~Scaled[free]), cex=1.6, pch=16, col=col_gr2))
  points(leafturn_high$mass_adj,leafturn_high$C_adj, cex=1.6, pch=16, col=col_gr1)
  points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)


par(mar=c(5,5,2,2), cex.axis=0.8, las=1)

with(rootresp_low, plot(C_adj,mass_adj, xlim=c(1,.4),ylim=c(0, 1), ylab= expression(Biomass~Scaled[free]), 
                   xlab=expression(Daily~Carbon~Assimilation~Scaled[free]), cex=1.6, pch=16, col=col_br2))
  points(rootresp_high$mass_adj,rootresp_high$C_adj, cex=1.6, pch=16, col=col_br1)
  points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)


par(mar=c(5,5,2,2), cex.axis=0.8, las=1)

with(frexude_low, plot(C_adj,mass_adj, xlim=c(1,.4),ylim=c(0, 1), ylab= expression(Biomass~Scaled[free]), 
                        xlab=expression(Daily~Carbon~Assimilation~Scaled[free]), cex=1.6, pch=16, col=col_exude2))
points(frexude_high$mass_adj,frexude_high$C_adj, cex=1.6, pch=16, col=col_exude1)
points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)




windows(14,12)
par(cex.axis=1.21, cex.lab=1.51,las=1,mgp=c(3.5,1,0),mfrow=c(3,1),  
    omi=c(.5,0,0.1,0.1))   

# First Panel
par(mar=c(0,7,2,2))
with(leafturn_low, plot(C_adj,mass_adj, xlim=c(1,.48),ylim=c(0, 1.1), ylab= "", 
                        xlab="",  type='n'))
  points(leafturn_low$mass_adj,leafturn_low$C_adj, cex=1.6, pch=16,  col=col_gr2)
  points(leafturn_high$mass_adj,leafturn_high$C_adj, cex=1.6, pch=16, col=col_gr1)
  points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6) 
  axis(1,  labels=FALSE) 
  axis(2, labels=TRUE)  
  box()
#Second Panel
par(mar=c(0,7,0,2))
with(rootresp_low, plot(C_adj,mass_adj, xlim=c(1,.48),ylim=c(0, 1.1), ylab= expression(Biomass~Scaled[free]), 
                        xlab="",  type='n'))
  points(rootresp_low$mass_adj,rootresp_low$C_adj, cex=1.6, pch=16, col=col_br2)
  points(rootresp_high$mass_adj,rootresp_high$C_adj, cex=1.6, pch=16, col=col_br1)
  points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)
  axis(1, labels=FALSE)  
  axis(2)     
  box()
#Third Panel
par(mar=c(5,7,0,2))
with(frexude_low, plot(C_adj,mass_adj, xlim=c(1,.48),ylim=c(0, 1.1), ylab= "", type='n',
                       xlab=expression(Daily~Carbon~Assimilation~Scaled[free])))
  points(frexude_low$mass_adj,frexude_low$C_adj, cex=1.6, pch=16, col=col_exude2)
  points(frexude_high$mass_adj,frexude_high$C_adj, cex=1.6, pch=16, col=col_exude1)
  points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)
  box()
  axis(2, labels=TRUE)  
  axis(1,labels=TRUE)  

dev.copy2pdf(file= "master_scripts/manuscript_figs/gc_Day_scenario.pdf")
dev.off()
