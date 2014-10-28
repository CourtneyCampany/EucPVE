
#read in model data runs, gCday means, and harvest mass means-----------------------------------------------------

#seq of gC, alloction is equal
gCseq_sim <- read.csv("calculated data/model_runs/sim_gCseq.csv")
 
###new parameters with mass and gC relative to largest value
  gCseq_sim$mass_adj <- with(gCseq_sim, biomass/biomass[1])
  #Gcday relative to largest value (7.0)
  gCseq_sim$C_adj <- with(gCseq_sim, gCday/gCday[1])


#sequence of gC with allocation and lma by treatment
gCseq_alloc_sim <- readRDS("calculated data/model_runs/allocation_sim.rds")

#dataframe of same model run
alloc_sim <- read.csv("calculated data/model_runs/sim_gCseq_allocation.csv")
  #Gcday relative to largest value (7.0)
  alloc_sim$C_adj <- with(alloc_sim, gCday/gCday[1])

###generates mass relative to largest mass value, which = the last value of the 101 sequence
  alloc_sim$maxmass <- c(rep(alloc_sim$biomass[1],101),rep(alloc_sim$biomass[102],101),rep(alloc_sim$biomass[203],101),
          rep(alloc_sim$biomass[304],101),rep(alloc_sim$biomass[405],101),rep(alloc_sim$biomass[506],101),
          rep(alloc_sim$biomass[607],101))

  alloc_sim$mass_adj <- alloc_sim$biomass/alloc_sim$maxmass
  alloc_sim$mass_adj_free <- alloc_sim$biomass/alloc_sim$biomass[607]



#simple model with mean gCday by treament (sum mass and daily accrual)
mass_sim <- read.csv("calculated data/model_runs/mass_sim.csv")
  mass_sim$mass_adj <- with(mass_sim, biomass/biomass[7])

mass_sim_alldays <- readRDS("calculated data/model_runs/mass_sim_alldays.rds")

#new dataframe with parameters relative to largest pot of free
mass_adj <- data.frame(volume=mass_sim$volume, mass35_adj = mass_sim$biomass/mass_sim$biomass[6],
                       massfree_adj = mass_sim$biomass/mass_sim$biomass[7], 
                       la_35_adj = mass_sim$leafarea/mass_sim$leafarea[6],
                       la_free_adj = mass_sim$leafarea/mass_sim$leafarea[7])




#harvest mass and leaf area for model comparison
mass_actual <- read.csv("calculated data/harvest_mass_means.csv")

Cday_means <- read.csv("calculated data/model_runs/gCday_means.csv")
  #new variables with gC standardized to largest pot and free
  Cday_means$C_stnd_pot <- with(Cday_means, carbon_day/carbon_day[6])
  Cday_means$C_stnd_free <- with(Cday_means, carbon_day/carbon_day[7])
Cday <- as.vector(Cday_means[,2]) 


#plot objects----------------------------------------------------------------------------------------------------
numdays <- as.numeric(as.Date("2013-05-21") - as.Date("2013-01-21"))

gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
pchs = c(rep(16,6),17)
ypos <- c(2.5,1,0)
vollab <- expression(Pot~volume~(l))
leglab <- c(5, 10, 15, 20, 25, 35, "free")

cols <- as.vector(palette())
require(scales)
cols1 <- alpha(cols[1], 0.45)
cols2 <- alpha(cols[2], 0.45)
cols3 <- alpha(cols[3], 0.45)
cols4 <- alpha(cols[4], 0.45)
cols5 <- alpha(cols[5], 0.45)
cols6 <- alpha(cols[6], 0.45)
cols7 <- alpha(cols[7], 0.45)

col_bl <- alpha("black", .45)

treelab<- paste("Seedling Mass Production over ",numdays," days (g)", sep="")
sub35 <- expression(Scaled[35])
subfree <- expression(Scaled[free])
treelab35 <- paste("Seedling Mass Production over ",numdays," days ",sub35,"  (g)", sep="")
treelabfree <- paste("Seedling Mass Production over ",numdays," days ",subfree,"  (g)", sep="")
treelabfree <- paste("Relative Mass Production over ",numdays," days ","  (g)", sep="")

cdaylab <- expression(Daily~Carbon~Gain~~(g~m^-2~d^-1))
cday35lab <- expression(Daily~Carbon~Gain~Scaled[35]~~(g~m^-2~d^-1))
cdayfreelab <- expression(Daily~Carbon~Gain~Scaled[free]~~(g~m^-2~d^-1))


#plotting---------------------------------------------------------------------------------------------

##simple plotting with output of gCday volume means (n=7) versus observed mass and leaf area

#plotting

par(mfrow=c(1,2))
plot(mass_sim$biomass, mass_actual$mass)
  abline(0,1)
plot(mass_sim$leafarea, mass_actual$leafarea)
  abline(0,1)

plot(Cday, mass_sim$biomass, ylim=c(0,225))
  points(Cday, mass_actual$mass, col="red")

plot(mass_sim$biomass, mass_actual$mass)
  abline(0,1)
plot(mass_actual$leafarea, mass_sim$leafarea,ylim=c(0,.5))
  abline(0,1)

#A and plot mass, leafmass, and LMF vs A
plot(Cday, mass_sim$leafmass, pch=pchs, col=palette())
plot(Cday, mass_sim$LMF,  pch=pchs, col=palette())
plot(Cday, mass_sim$biomass,  pch=pchs, col=palette())

#plot simple model has a function of mass standardized to largest pot and free (both mass and gC)

plot(mass_adj$mass35_adj[1:6]~Cday_means$C_stnd_pot[1:6],  ylab=treelab35, xlab=cday35lab, xlim=c(1.2,0), 
     ylim=c(0, 1.2),pch=16,col=palette())
  abline(1,0, lty=2)

plot(mass_adj$massfree_adj~Cday_means$C_stnd_free,  ylab=treelabfree, xlab=cdayfreelab, xlim=c(1.2,0),
     ylim=c(0, 1.2),col=palette(), pch=pchs)
  abline(1,0, lty=2)
######this sim uses a sequence of gC day and keeps lma and allocation equal

#windows()
png(filename = "output/presentations/Cmodel_meanC_massactual.png", width = 12, height = 8, units = "in", res= 400)
par(cex.axis=1.3, cex.lab=1.3)
with(gCseq_sim, plot(biomass~gCday, ylim=c(0,175), xlim=c(0,8), ylab= "", xlab=cdaylab,pch=16,cex=1.6, col=col_bl))
points( mass_actual$mass~Cday,pch=pchs,col=palette(),cex=1.6)
#points( modelmass$gCday~modelmass$biomass,pch=pchs,col=palette())
title(ylab=treelab, mgp=ypos)
legend("bottomleft", leglab, pch=pchs,text.font=1.3, inset=0.01, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n',cex=1.3,)

dev.off()

#######scaled plotting-----------------------------------------------------------------------

###gCday sim plot 

#mass and gC adjusted to smallest gCday value
with(gCseq_sim, plot(mass_adj~C_adj, xlim=c(1,0), ylim=c(0,1), 
                     ylab= "Mass Production adjusted", xlab="gC Day adjusted", cex=1.6, pch=16, col=col_bl))
  points( mass_sim$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)


####sim with lma and allocation

#mass and gC relative to the largest pot (no free) 
#gCday no scaled
with(subset(alloc_sim, volume==5),plot(mass_stnd_pot~gCday, col=cols1, ylim=c(0,1), xlim=c(0,8), cex=1.6, ylab="", 
                                       xlab=cdaylab))
  with(subset(alloc_sim, volume==10),points(mass_stnd_pot~gCday, col=cols2, cex=1.6))
  with(subset(alloc_sim, volume==15),points(mass_stnd_pot~gCday, col=cols3, cex=1.6))
  with(subset(alloc_sim, volume==20),points(mass_stnd_pot~gCday, col=cols4, cex=1.6))
  with(subset(alloc_sim, volume==25),points(mass_stnd_pot~gCday, col=cols5, cex=1.6))
  with(subset(alloc_sim, volume==35),points(mass_stnd_pot~gCday, col=cols6, cex=1.6))
  title(ylab=treelab, mgp=ypos)

#mass and gC relative to the free seedling
with(subset(alloc_sim, volume==5),plot(C_adj~mass_adj_free, col=cols1, ylim=c(0,1), 
                                       xlim=c(1,0), cex=1.6, ylab="Biomass adjusted", xlab="gC day adjusted",pch=16))

  with(subset(alloc_sim, volume==10),points(C_adj~mass_adj_free, col=cols2, cex=1.6, pch=16))
  with(subset(alloc_sim, volume==15),points(C_adj~mass_adj_free, col=cols3, cex=1.6,pch=16))
  with(subset(alloc_sim, volume==20),points(C_adj~mass_adj_free, col=cols4, cex=1.6,pch=16))
  with(subset(alloc_sim, volume==25),points(C_adj~mass_adj_free, col=cols5, cex=1.6,pch=16))
  with(subset(alloc_sim, volume==35),points(C_adj~mass_adj_free, col=cols6, cex=1.6,pch=16))
  with(subset(alloc_sim, volume==1000),points(C_adj~mass_adj_free, col=cols7, cex=1.6,pch=17))
  with(gCseq_sim, points(C_adj~mass_adj, xlim=c(1,0), ylim=c(0,1), ylab= "", xlab="", cex=1.6, pch=16, col=col_bl))
  points( mass_sim$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)



#mass and gC relative to largest pot

#new dataframe
alloc_pot <- subset(alloc_sim[c(1:6, 11)], volume != "1000")

  alloc_pot$maxmass <- c(rep(alloc_pot$biomass[1],101),rep(alloc_pot$biomass[102],101),rep(alloc_pot$biomass[203],101),
                       rep(alloc_pot$biomass[304],101),rep(alloc_pot$biomass[405],101),rep(alloc_pot$biomass[506],101))
  alloc_pot$mass_adj <- alloc_pot$biomass/alloc_pot$maxmass


#plot
with(subset(alloc_pot, volume==5),plot(C_adj~mass_adj, col=cols1, ylim=c(0,1), 
            xlim=c(1,0), cex=1.6, ylab="Biomass adjusted", xlab="gC day adjusted",pch=16))

with(subset(alloc_pot, volume==10),points(C_adj~mass_adj, col=cols2, cex=1.6, pch=16))
with(subset(alloc_pot, volume==15),points(C_adj~mass_adj, col=cols3, cex=1.6,pch=16))
with(subset(alloc_pot, volume==20),points(C_adj~mass_adj, col=cols4, cex=1.6,pch=16))
with(subset(alloc_pot, volume==25),points(C_adj~mass_adj, col=cols5, cex=1.6,pch=16))
with(subset(alloc_pot, volume==35),points(C_adj~mass_adj, col=cols6, cex=1.6,pch=16))
with(gCseq_sim, points(C_adj~mass_adj, xlim=c(1,0), ylim=c(0,1), ylab= "", xlab="", cex=1.6, pch=16, col=col_bl))
points( mass_sim$mass_adj~Cday_means$C_stnd_free,pch=pchs,col=palette(),cex=1.6)



