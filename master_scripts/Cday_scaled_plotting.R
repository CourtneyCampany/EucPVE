# to.pdf(figure1(), filename="manuscript/figures/figure1.pdf", height=7, width=7)
# figure1 <- function(){


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

#3. sequence of gC with allocation and lma by treatment
gCseq_alloc_sim <- readRDS("calculated data/model_runs/allocation_sim.rds")

#dataframe of same model run
alloc_sim <- read.csv("calculated data/model_runs/sim_gCseq_allocation.csv")
#Gcday relative to largest value (7.0)
alloc_sim$C_adj <- with(alloc_sim, gCday/gCday[1])

###generates mass relative to largest mass value, which = the last value of the 101 sequence
alloc_sim$maxmass <- c(rep(alloc_sim$biomass[1],101),rep(alloc_sim$biomass[102],101),rep(alloc_sim$biomass[203],101),
                       rep(alloc_sim$biomass[304],101),rep(alloc_sim$biomass[405],101),rep(alloc_sim$biomass[506],101),
                       rep(alloc_sim$biomass[607],101))

alloc_sim$mass_adj_free <- alloc_sim$biomass/alloc_sim$biomass[607]

#######scaled plotting-----------------------------------------------------------------------
windows(7,5)
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)

with(subset(alloc_sim, volume==5),plot(C_adj,mass_adj_free, col=cols1, xlim=c(1,.6),ylim=c(0,1),
                                       cex=1, pch=16,
                                       ylab=expression(Biomass~Scaled[free]), 
                                       xlab=expression(Daily~Carbon~Assimilation~Scaled[free])))

with(subset(alloc_sim, volume==10),points(C_adj,mass_adj_free, col=cols2, cex=1, pch=16))
with(subset(alloc_sim, volume==15),points(C_adj,mass_adj_free, col=cols3, cex=1, pch=16))
with(subset(alloc_sim, volume==20),points(C_adj,mass_adj_free, col=cols4, cex=1, pch=16))
with(subset(alloc_sim, volume==25),points(C_adj,mass_adj_free, col=cols5, cex=1, pch=16))
with(subset(alloc_sim, volume==35),points(C_adj,mass_adj_free, col=cols6, cex=1, pch=16))
with(subset(alloc_sim, volume==1000),points(C_adj,mass_adj_free, col=cols7, cex=1,pch=17))
points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1)
legend("topright", leglab, pch=pchs,text.font=1, inset=0.01, 
       title=vollab, col=palette(), bty='n',cex=1.0,)

dev.copy2pdf(file= "master_scripts/manuscript_figs/gC_day.pdf")
dev.off()

# ###simple version
# with(gCseq_sim_obs, plot(C_adj,mass_adj, xlim=c(1,.5),ylim=c(0, 1), ylab= "", xlab="", cex=1.6, pch=16, col=col_bl))
# points( mass_actual$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)