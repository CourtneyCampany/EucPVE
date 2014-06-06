#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#master file with most havest data
harvest_mass <- read.csv("raw data/harvest aboveground mass.csv")
harvest_mass$ID <- paste(harvest_mass$plot, harvest_mass$pot, sep = "-")
  #remove blank plot position
  harvest_mass <- subset(harvest_mass, !is.na(Leaf_bag))

  #substract bag mass from dry weights
  harvest_mass$leafmass <- with(harvest_mass, Leafmass.bag - Leaf_bag)
  harvest_mass$stemmass <- with(harvest_mass, stemmass.bag - stem_bag)


#additional mass from subsamples
srlmass <- read.csv("raw data/SRLmass.csv")
srlmass$ID <- paste(srlmass$plot, srlmass$pot, sep = "-")
srlmass <- subset(srlmass, !is.na(ss_fw))

#harvest leaf area
leafharvest <- read.csv("raw data/harvest leaf area.csv")
leafharvest$ID <- paste(leafharvest$plot, leafharvest$pot, sep = "-")