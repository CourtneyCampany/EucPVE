#weekly survey data
height <- read.csv("raw data/seedling height.csv")
diam <- read.csv("raw data/seedling diameter.csv")
leafno <- read.csv("raw data/seedling leaf count.csv")
leafcount <- read.csv("raw data/seedling leaf count.csv")
leaflength <- read.csv("raw data/seedling leaf length.csv")


#LEAF MASS AREA
lma <- read.csv("raw data/seedling leaf mass area.csv")
lma$ID <- paste(lma$plot, lma$pot, sep = "-")

  #ACI leaves (subset of whole experiment with random leaves from each volume)
  acileaf <- read.csv("raw data/aci leaves.csv")
  acileaf$Date <- as.Date(acileaf$Date, format = "%m/%d/%Y")
  acileaf$ID <- paste(acileaf$plot, acileaf$pot, sep = "-")

#plot design
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#rsoil moisture
SMpre <- read.csv("raw data/pre-soil moisture.csv")
SM <- read.csv("raw data/soil moisture.csv")
