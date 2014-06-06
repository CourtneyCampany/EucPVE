#source functions
source("functions and packages/functions.R")

#read data
source("read data scripts/harvest read data.R")
leafCN <- read.csv("raw data/leafCN.csv")
leafarea <- read.csv("calculated data/leafareabypot.csv")
TNC <- read.csv("raw data/TNC.csv")


lma <- read.csv("raw data/seedling leaf mass area.csv")
lma$ID <- paste(lma$plot, lma$pot, sep = "-")
lma <- merge(lma, plotsumm[3:4], all=TRUE)

#run formatting functions for leafCN/TNC raw data and campaign date
CN_leaf <- leafCN_format(leafCN)
CN_leaf <- add_campaign_date(CN_leaf)
CN_leaf <- merge(CN_leaf, plotsumm[3:4], all=TRUE)

lma <- add_campaign_date(lma)

#TNC is means by volume now, maybe go back to raw data
TNC <- add_campaign_noID(TNC)


#calculate C and N content of leaves
leaf_chem <- merge(CN_leaf, lma, all=TRUE)

leaf_chem$leafC <- with(leaf_chem, mass*Cperc)
leaf_chem$leafN <- with(leaf_chem, mass*Nperc)

plot(leafC~Date, col=volume, data=leaf_chem)
plot(leafN~Date, col=volume, data=leaf_chem)
#analyze leaf chem with Asat, area, mass on campaign dates


leaf_chem <- merge(leaf_chem, TNC)





#requires Dates of A campaigns and leaf mass predictons



#then summarise though time with starchs and sugars


leaf_harvest <- subset(harvest_mass, select = C("ID", "leafmass", "leaf_area", "leaf_count"))


#TNC (need to convert the amount of tnc (mg/g dwt) to canopy content to compare with C and N)