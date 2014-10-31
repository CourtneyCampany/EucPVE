---
title: "EUC_PVE"
author: "Court Campany"
date: "Tuesday, July 22, 2014"
output: html_document
---

```
These are the plots and relevatn statistics for the Eucalpytus tereticornis  
pot study.  6 total contatiner volumes and 'free'seedlings all grown under  
natural field conditions during the summer of 2012
```

```r
library(knitr)
opts_knit$set(root.dir = '../')
```
```


seedling growth through time


```r
source("master_scripts/allometryplotting.R")
```

seedling total leaf area through time


```r
source("master_scripts/leafarea_plotting.R")
```

<img src="./master_files/figure-html/leafareaplot.png" title="plot of chunk leafareaplot" alt="plot of chunk leafareaplot" width="672" />

root:shoot and leaf:fine root at harvest


```r
source("master_scripts/rootshoot_plotting.R")
```

<img src="./master_files/figure-html/rootshootplots.png" title="plot of chunk rootshootplots" alt="plot of chunk rootshootplots" width="672" />

asat and amax


```r
source("master_scripts/A_plotting.R")
```

<img src="./master_files/figure-html/photosynthesisplotting.png" title="plot of chunk photosynthesisplotting" alt="plot of chunk photosynthesisplotting" width="672" />

aci curves and jmax/vcmax


```r
source("master_scripts/aci_plotting.R")
```

<img src="./master_files/figure-html/acimeansplots.png" title="plot of chunk acimeansplots" alt="plot of chunk acimeansplots" width="672" />

### seedling predawn and midday water potentials


```r
source("clean scripts/seedling water potential.R")
```

<img src="./master_files/figure-html/waterpotentialplots.png" title="plot of chunk waterpotentialplots" alt="plot of chunk waterpotentialplots" width="672" />

Modelled C gain versus harvest seedling total carbon


```r
cgain<- read.csv("calculated data/euc_cgain.csv")
cgain$volume <- as.factor(cgain$volume)

windows()
with(cgain, plot(carbon_gain, totalC, pch=pchs[volume], col=volume))
abline(0,1)
abline(lm(totalC ~ carbon_gain, data=cgain), lty=5)
```

<img src="./master_files/figure-html/CgainvsCmass.png" title="plot of chunk CgainvsCmass" alt="plot of chunk CgainvsCmass" width="672" />

Soil N percent pre expeeriment and at harvest


```r
source("master_scripts/soilCN_plotting.R")
```

<img src="./master_files/figure-html/SoilNpercent1.png" title="plot of chunk SoilNpercent" alt="plot of chunk SoilNpercent" width="672" /><img src="./master_files/figure-html/SoilNpercent2.png" title="plot of chunk SoilNpercent" alt="plot of chunk SoilNpercent" width="672" />

Predicted responses of Amax on a mass basis to leaf starch and N levels




