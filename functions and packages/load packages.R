library(RColorBrewer)
library(doBy)
library(sciplot)
library(lubridate)
library(reshape)
library(nlme)
library(plyr)
library(visreg)
library(multcomp)
library(smatr)
library(lme4)
library(lmerTest)
library(effects)

library(plotrix)
#library(plotBy)

library(plantecophys)
library(scales)

# possible markdown package installers
# 
# manuscript_library <- c("RColorBrewer", "doBy", "sciplot", "plyr","nlme", "multcomp", "plantecophys", "scales","smatr",
#                         "lme4", "lmerTest", "broom", "reporttools")
# 
# if (!library("manuscript_library")) {
#   install.packages("manuscript_library", repos = "http://cran.at.r-project.org")
# }
# 
# 
# 
# install.packages(setdiff(c("RColorBrewer", "doBy", "sciplot", "plyr","nlme", "multcomp", "plantecophys", "scales","smatr",
#                            "lme4", "lmerTest", "broom", "reporttools"), rownames(installed.packages())), 
#                             repos = "http://cran.at.r-project.org")




# usePackage <- function(p) {
#   if (!is.element(p, installed.packages()[,1]))
#     install.packages(p, dep = TRUE)
#   require(p, character.only = TRUE)
# }
# In case if you need to select CRAN mirror globally, here is one way to do it:
#   
#   r <- getOption("repos")
# r["CRAN"] <- "http://cran.us.r-project.org"
# options(repos = r)
# rm(r)
