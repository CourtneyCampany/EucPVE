
options(repos="http://cran.rstudio.com/")

r <- require(pacman)
if(!r)install.packages("pacman")
#pacman:::p_set_cranrepo()
pacman::p_load(RColorBrewer, doBy, sciplot, plyr,nlme, multcomp, plantecophys, scales,smatr,plotrix,
               lme4, lmerTest, broom, reporttools, visreg, pixiedust, lubridate, reshape,
               magicaxis, effects)


source("functions and packages/load packages.R")
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

source("functions and packages/gamplotfunctions.R")

#table data
table1 <- read.csv("master_scripts/pve_table1_new.csv")
table2 <- read.csv("master_scripts/pve_table2_new.csv")
table3 <- read.csv("master_scripts/pve_table3.csv")

