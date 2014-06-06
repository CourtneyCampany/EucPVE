#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#read in gas exchange master file, all dates
gasexchange <- read.csv("raw data/AsatAmax_master.csv")
gasexchange$Date <- as.Date(gasexchange$Date)
gasexchange$ID <- paste(gasexchange$plot, gasexchange$pot, sep = "-")

#read in dark respiration (two dates)
resp_dark <- read.csv("raw data/Rdark_cleanCi.csv")
resp_dark$Date <- as.Date(resp_dark$Date)
resp_dark$ID <- paste(resp_dark$plot, resp_dark$pot, sep = "-")

#read in water potential data
waterpotential <- read.csv("raw data/waterpotential.csv")
waterpotential$ID <- paste(waterpotential$plot, waterpotential$pot, sep = "-")
waterpotential$Date <- as.Date(waterpotential$Date)

#read in aci data
aci1 <- read.csv("raw data/ACi#1.csv")
aci1$ID <- paste(aci1$plot, aci1$pot, sep = "-")

aci2 <- read.csv("raw data/ACi#2.csv")
aci2$ID <- paste(aci2$plot, aci2$pot, sep = "-")