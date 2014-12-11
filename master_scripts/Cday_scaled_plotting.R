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

#######scaled plotting-----------------------------------------------------------------------
windows(7,5)
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
with(gCseq_sim_mean, plot(C_adj,mass_adj, xlim=c(1,.65),ylim=c(0, 1.1), cex=1, type='l', lwd=4,
                          ylab=expression(Biomass~Scaled[free]), 
                          xlab=expression(Daily~Carbon~Assimilation~Scaled[free]),
                          pch=16, col=col_bl))
  points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)

dev.copy2pdf(file= "master_scripts/manuscript_figs/gC_day.pdf")
dev.off()










