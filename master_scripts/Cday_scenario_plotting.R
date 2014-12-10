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


#mean sim
##sim with sequence of cday with mean partioning
gCseq_sim_mean <- read.csv("calculated data/model_runs/sim_gCseq_obs.csv")
  gCseq_sim_mean <- scaletofree_func(gCseq_sim_mean)


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

#scenario with leaf allocation +-50%
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
plot(mass_adj~C_adj, xlim=c(1,.6),ylim=c(0, 1), data=leafturn_low, 
                        ylab= expression(Biomass~Scaled[free]), 
                        xlab=expression(Daily~Carbon~Assimilation~Scaled[free]), 
                        cex=1, pch=16, col=col_lf2)
  points(mass_adj~C_adj, data=leafturn_high, cex=1, pch=16, col=col_lf1)
  points(mass_adj~C_adj, data=gCseq_sim_mean, cex=1, pch=16, col=col_bl)
  points(mass_actual$mass_adj~Cday_means$C_stnd_free, pch=pchs, col=palette(), cex=1.6)

#scenario with root respiration +-75%
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
plot(mass_adj~C_adj, xlim=c(1,.6),ylim=c(0, 1), data=rootresp_low,
                        ylab= expression(Biomass~Scaled[free]), 
                        xlab=expression(Daily~Carbon~Assimilation~Scaled[free]), 
                        cex=1, pch=16, col=col_resp2)
  points(mass_adj~C_adj, data=rootresp_high, cex=1, pch=16, col=col_resp1)
  points(mass_adj~C_adj, data=gCseq_sim_mean, cex=1, pch=16, col=col_bl)
  points(mass_actual$mass_adj~Cday_means$C_stnd_free, pch=pchs, col=palette(), cex=1.6)

#scenario with increase C allocation to fine roots +-50%
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
plot(mass_adj~C_adj, xlim=c(1,.6),ylim=c(0, 1), data=frexude_low,
                        ylab= expression(Biomass~Scaled[free]), 
                        xlab=expression(Daily~Carbon~Assimilation~Scaled[free]), 
                        cex=1, pch=16, col=col_exude2)
  points(mass_adj~C_adj, data=frexude_high, cex=1, pch=16, col=col_exude1)
  points(mass_adj~C_adj, data=gCseq_sim_mean, cex=1, pch=16, col=col_bl)
  points(mass_actual$mass_adj~Cday_means$C_stnd_free, pch=pchs, col=palette(), cex=1.6)

#------------------------------------------------------------------------------------------------------------

windows(7,7)
par(cex.axis=1.21, cex.lab=1.51,las=1,mgp=c(3.5,1,0),mfrow=c(3,1),  
    omi=c(.5,0,0.1,0.1))   

# First Panel
par(mar=c(0,7,2,2))
plot(mass_adj~C_adj, xlim=c(1,.65),ylim=c(0, 1.1), data=leafturn_low, ylab= "", xlab="", type='n',axes=FALSE)
  points(mass_adj~C_adj, data=leafturn_low, cex=1, pch=16, col=col_lf2, type='l', lwd=4)
  points(mass_adj~C_adj, data=leafturn_high, cex=1, pch=16, col=col_lf1,type='l', lwd=4)
  points(mass_adj~C_adj, data=gCseq_sim_mean, cex=1, pch=16, col=col_bl,type='l', lwd=4)
  points(mass_actual$mass_adj~Cday_means$C_stnd_free, pch=pchs, col=palette(), cex=1.6)
  axis(1,  labels=FALSE) 
  axis(2, labels=TRUE)  
  box()

#Second Panel
par(mar=c(0,7,0,2))

plot(mass_adj~C_adj, xlim=c(1,.65),ylim=c(0, 1.1), data=rootresp_low, xlab="", 
                        ylab= expression(Biomass~Scaled[free]),type='n', axes=FALSE)
  points(mass_adj~C_adj, data=rootresp_low, cex=1, pch=16, col=col_resp2,type='l', lwd=4)
  points(mass_adj~C_adj, data=rootresp_high, cex=1, pch=16, col=col_resp1,type='l', lwd=4)
  points(mass_adj~C_adj, data=gCseq_sim_mean, cex=.5, pch=16, col=col_bl,type='l', lwd=4)
  points(mass_actual$mass_adj~Cday_means$C_stnd_free, pch=pchs, col=palette(), cex=1.6)
  axis(1, labels=FALSE)  
  axis(2)     
  box()

#Third Panel
par(mar=c(5,7,0,2))
plot(mass_adj~C_adj, xlim=c(1,.65),ylim=c(0, 1.1), data=frexude_low,
                       ylab= "", type='n',axes=FALSE,
                       xlab=expression(Daily~Carbon~Assimilation~Scaled[free]))
  points(mass_adj~C_adj, data=frexude_low, cex=1, pch=16, col=col_exude2,type='l', lwd=4)
  points(mass_adj~C_adj, data=frexude_high, cex=1, pch=16, col=col_exude1,type='l', lwd=4)
  points(mass_adj~C_adj, data=gCseq_sim_mean, cex=1, pch=16, col=col_bl,type='l', lwd=4)
  points(mass_actual$mass_adj~Cday_means$C_stnd_free, pch=pchs, col=palette(), cex=1.6)
  box()
  axis(2, labels=TRUE)  
  axis(1,labels=TRUE) 

dev.copy2pdf(file= "master_scripts/manuscript_figs/gc_Day_scenario.pdf")
dev.off()



# #######sequence of gC with allocation and lma by treatment--------------------------------------------------------
# alloc_sim <- read.csv("calculated data/model_runs/sim_gCseq_allocation.csv")
# alloc_sim$C_adj <- with(alloc_sim, gCday/gCday[1])
# 
# ###generates mass relative to largest mass value, which = the last value of the 101 sequence
# alloc_sim$maxmass <- c(rep(alloc_sim$biomass[1],101),rep(alloc_sim$biomass[102],101),rep(alloc_sim$biomass[203],101),
#                        rep(alloc_sim$biomass[304],101),rep(alloc_sim$biomass[405],101),rep(alloc_sim$biomass[506],101),
#                        rep(alloc_sim$biomass[607],101))
# 
# alloc_sim$mass_adj_free <- alloc_sim$biomass/alloc_sim$biomass[607]
# 
# windows(7,5)
# par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
# 
# with(subset(alloc_sim, volume==5),plot(C_adj,mass_adj_free, col=cols1, xlim=c(1,.6),ylim=c(0,1),
#                                        cex=1, pch=16,
#                                        ylab=expression(Biomass~Scaled[free]), 
#                                        xlab=expression(Daily~Carbon~Assimilation~Scaled[free])))
# 
# with(subset(alloc_sim, volume==10),points(C_adj,mass_adj_free, col=cols2, cex=1, pch=16))
# with(subset(alloc_sim, volume==15),points(C_adj,mass_adj_free, col=cols3, cex=1, pch=16))
# with(subset(alloc_sim, volume==20),points(C_adj,mass_adj_free, col=cols4, cex=1, pch=16))
# with(subset(alloc_sim, volume==25),points(C_adj,mass_adj_free, col=cols5, cex=1, pch=16))
# with(subset(alloc_sim, volume==35),points(C_adj,mass_adj_free, col=cols6, cex=1, pch=16))
# with(subset(alloc_sim, volume==1000),points(C_adj,mass_adj_free, col=cols7, cex=1,pch=17))
# points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1)
# legend("topright", leglab, pch=pchs,text.font=1, inset=0.01, 
#        title=vollab, col=palette(), bty='n',cex=1.0,)
