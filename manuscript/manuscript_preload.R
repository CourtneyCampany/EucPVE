
options(repos="http://cran.rstudio.com/")

r <- require(pacman)
if(!r)install.packages("pacman")
#pacman:::p_set_cranrepo()
pacman::p_load(RColorBrewer, doBy, sciplot, plyr,nlme, multcomp, plantecophys, scales,smatr,plotrix,
               lme4, lmerTest, broom, reporttools, visreg, pixiedust, lubridate, reshape,
               magicaxis, effects)


source("functions and packages/startscripts.R")
source("functions and packages/gamplotfunctions.R")

#table data
table1 <- read.csv("master_scripts/pve_table1.csv")
table2 <- read.csv("master_scripts/root_table.csv")
table3 <- read.csv("master_scripts/pve_table2.csv")
modeltable <- read.csv("master_scripts/modeltable.csv")
