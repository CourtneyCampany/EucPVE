# to.pdf(figure1(), filename="manuscript/figures/figure1.pdf", height=7, width=7)
# figure1 <- function(){


source("gC_day_model/model_plot_objects.R")

#read in observed mass and Cday---------------------------------------------------------------------------------
#harvest mass and leaf area for model comparison
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")
mass_actual$mass_adj <- with(mass_actual, mass/mass[7])

Cday_means <- read.csv("calculated data/model_runs/gCday_means.csv")
  Cday_means$C_stnd_free <- with(Cday_means, carbon_day/carbon_day[7])

Cday <- as.vector(Cday_means[,2]) 

##sim with sequence of cday with mean partioning
gCseq_sim_mean <- read.csv("calculated data/model_runs/sim_gCseq_obs.csv")
    gCseq_sim_mean <- scaletofree_func(gCseq_sim_mean)


#######sequence of gC with allocation and lma by treatment--------------------------------------------------------
alloc_sim <- read.csv("calculated data/model_runs/sim_gCseq_allocation.csv")
alloc_sim$C_adj <- with(alloc_sim, gCday/gCday[1])

###generates mass relative to largest mass value, which = the last value of the 101 sequence
alloc_sim$maxmass <- c(rep(alloc_sim$biomass[1],101),rep(alloc_sim$biomass[102],101),rep(alloc_sim$biomass[203],101),
                       rep(alloc_sim$biomass[304],101),rep(alloc_sim$biomass[405],101),rep(alloc_sim$biomass[506],101),
                       rep(alloc_sim$biomass[607],101))

alloc_sim$mass_adj_free <- alloc_sim$biomass/alloc_sim$biomass[607]




#######two panel scaled plotting-----------------------------------------------------------------------

windows(7,8)

par(cex.axis=.96, cex.lab=1.2,mfrow=c(2,1),oma=c(0.1,0.1,0.1,0.1), las=1)   

par(mar=c(0,5,2,2))
plot(mass_adj~C_adj, xlim=c(1,.65),ylim=c(0, 1.1), data=gCseq_sim_mean, xlab="", 
     ylab= "", type='n', axes=FALSE)

  points(mass_adj~C_adj, data=gCseq_sim_mean, cex=1.2, type='l', lwd=4,pch=16, col=col_bl)
  points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)
  axis(1, labels=FALSE)  
  axis(2)     
  box()
  text(1,1.075,"(a)", cex=1.2)
  legend("bottomleft", leglab, pch=pchs,text.font=1, inset=0.025, title=vollab, col=palette(), bty='n',cex=1.0)
  legend("topright", "Model simulations", pch=15,text.font=1, title="", col=col_bl, bty='n')

par(mar=c(5,5,0,2))
with(subset(alloc_sim, volume==5),plot(C_adj,mass_adj_free, col=lcols1, xlim=c(1,.65),ylim=c(0,1.1),
                                       cex=1, type='l', lwd=4,ylab="", 
                                       xlab=expression(Daily~Carbon~Assimilation~Scaled[free])))

  with(subset(alloc_sim, volume==10),points(C_adj,mass_adj_free, col=lcols2, cex=1,type='l', lwd=4))
  with(subset(alloc_sim, volume==15),points(C_adj,mass_adj_free, col=lcols3, cex=1, type='l', lwd=4))
  with(subset(alloc_sim, volume==20),points(C_adj,mass_adj_free, col=lcols4, cex=1, type='l', lwd=4))
  with(subset(alloc_sim, volume==25),points(C_adj,mass_adj_free, col=lcols5, cex=1, type='l', lwd=4))
  with(subset(alloc_sim, volume==35),points(C_adj,mass_adj_free, col=lcols6, cex=1, type='l', lwd=4))
  with(subset(alloc_sim, volume==1000),points(C_adj,mass_adj_free, col=lcols7, cex=1,type='l', lwd=4))
  points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)
  text(1, 1.075, "(b)", cex=1.2)

mtext(expression(Biomass~Scaled[free]), 2, line=2.5, cex=1.2, las=0, at=1.2)

dev.copy2pdf(file= "master_scripts/manuscript_figs/gC_day.pdf")
dev.off()



####pngs for presentations
png(filename = "master_scripts/manuscript_figs/png/Cday2.png", width = 11, height = 8.5, units = "in", res= 400)
par(mar=c(5,5,2,2), cex.axis=1.5,cex.lab=1.75 ,las=1)

with(subset(alloc_sim, volume==5),plot(C_adj,mass_adj_free, col=lcols1, xlim=c(1,.65),ylim=c(0,1.1),
                                       cex=1, type='l', lwd=4,
                                       ylab=expression(Biomass~Scaled[free]), 
                                       xlab=expression(Daily~Carbon~Assimilation~Scaled[free])))

  with(subset(alloc_sim, volume==10),points(C_adj,mass_adj_free, col=lcols2, cex=1,type='l', lwd=4))
  with(subset(alloc_sim, volume==15),points(C_adj,mass_adj_free, col=lcols3, cex=1, type='l', lwd=4))
  with(subset(alloc_sim, volume==20),points(C_adj,mass_adj_free, col=lcols4, cex=1, type='l', lwd=4))
  with(subset(alloc_sim, volume==25),points(C_adj,mass_adj_free, col=lcols5, cex=1, type='l', lwd=4))
  with(subset(alloc_sim, volume==35),points(C_adj,mass_adj_free, col=lcols6, cex=1, type='l', lwd=4))
  with(subset(alloc_sim, volume==1000),points(C_adj,mass_adj_free, col=lcols7, cex=1,type='l', lwd=4))
  points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)
  box()
  legend("topright", leglab, pch=pchs,text.font=1, inset=0.025, title=vollab, col=palette(), bty='n',cex=1.0)
dev.off()

#a only
png(filename = "master_scripts/manuscript_figs/png/Cday.png", width = 11, height = 8.5, units = "in", res= 400)
par(mar=c(5,5,2,2), cex.axis=1.5,cex.lab=1.75 ,las=1)

plot(mass_adj~C_adj, xlim=c(1,.65),ylim=c(0, 1.1), data=gCseq_sim_mean, 
     xlab=expression(Daily~Carbon~Assimilation~Scaled[free]), ylab= expression(Biomass~Scaled[free]), type='n')

points(mass_adj~C_adj, data=gCseq_sim_mean, cex=1.2, type='l', lwd=4,pch=16, col=col_bl)
points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)
box()
legend("bottomleft", leglab, pch=pchs,text.font=1, inset=0.025, title=vollab, col=palette(), bty='n',cex=1.0)
legend("topright", "Model simulations", pch=15,text.font=1, title="", col=col_bl, bty='n')
dev.off()
