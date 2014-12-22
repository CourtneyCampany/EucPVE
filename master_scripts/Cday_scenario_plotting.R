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


#PLOTTING------------------------------------------------------------------------------------------------------------

windows(7,10)
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

legend("topright", modlab1, pch=15,text.font=1.51, inset=0.025, title="Leaf Allocation", 
       cex=1.51, col=modcol1, bty='n')
  text(.65, .05, "(a)", cex=2)

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
  legend("bottomleft", leglab, pch=c(rep(16,6),17),text.font=1.51, inset=0.025, title=vollab, 
      cex=1.51, col=palette(), bty='n')
  legend("topright", modlab1, pch=15,text.font=1.51, inset=0.025, title="Root Respiration", 
       cex=1.51, col=modcol2, bty='n')
  text(.65, .05, "(b)", cex=2)

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
  legend("topright", modlab1, pch=15,text.font=1.51, inset=0.025, title="Fine Root Allcation", 
       cex=1.51, col=modcol3, bty='n')
  text(.65, .05, "(c)", cex=2)

dev.copy2pdf(file= "master_scripts/manuscript_figs/gc_Day_scenario.pdf")
dev.off()





