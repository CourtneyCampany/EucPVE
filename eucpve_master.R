#master script for eucpve project

#source functions, packages, anbd plot objects
source("functions and packages/startscripts.R")

#growth allometry
source("master_scripts/allometryplotting.R")
#leaf area
source("master_scripts/leafarea_plotting.R")
#rootshoot ratios
source("master_scripts/rootshoot_plotting.R")
#aci curvest, volume means
source("master_scripts/aci_plotting.R")
#water potential (mean of two sampling dates)
source("clean scripts/seedling water potential.R")

#Cgain vs Ctotal
cgain<- read.csv("calculated data/euc_cgain.csv")
cgain$volume <- as.factor(cgain$volume)

windows()
with(cgain, plot(carbon_gain, totalC, pch=pchs[volume], col=volume))
abline(0,1)
abline(lm(totalC ~ carbon_gain, data=cgain), lty=5)

#Soil Nitrogen bar plot
source("master_scripts/soilN_plotting.R")