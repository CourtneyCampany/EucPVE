Summary stats for leaf mass area
========================================================






```r
# read in plot design, lma data, merge
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

lma <- read.csv("raw data/seedling leaf mass area.csv")
lma$ID <- paste(lma$plot, lma$pot, sep = "-")
lma <- merge(lma, plotsumm[3:4], all = TRUE)
lma$volume <- as.factor(lma$volume)

# run function to add campaign dates, caluclate sla
lma <- add_campaign_date(lma)
lma$sla <- with(lma, area/mass)

# remove missing values
lma_noNA <- subset(lma, !is.na(sla))

# subset with only sla and parameters of lm
lma_lm <- subset(lma_noNA, select = c("ID", "volume", "campaign", "sla"))
# ----------------------------------------------------------------------------------------------
# Visualize changes in SLA across campaigns (Date messes things up so stay
# with campaign# (interval=2wks))

# function to create groupData set
grp_func <- function(dfr) {
    
    sub_dfr <- subset(dfr, select = c("ID", "campaign", "sla"))
    lma.new <- groupedData(sla ~ campaign | ID, data = sub_dfr)
}
# create groupdData objects by volume
groupData_ls <- dlply(lma_lm, .(volume), grp_func)
# generate trellis plots
lapply(groupData_ls, plot)
```

```
## $`5`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```
## 
## $`10`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 

```
## 
## $`15`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-23.png) 

```
## 
## $`20`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-24.png) 

```
## 
## $`25`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-25.png) 

```
## 
## $`35`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-26.png) 

```
## 
## $`1000`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-27.png) 

```r

# ----------------------------------------------------------------------------------------------
# fit seperate linear regression models for each pot through time, grouped
# by volume
LMlist_func <- function(dfr) {
    
    sub_dfr <- subset(dfr, select = c("ID", "campaign", "sla"))
    lma.new <- groupedData(sla ~ campaign | ID, data = sub_dfr)
    models_ID <- lmList(sla ~ campaign, data = lma.new)
}

# run LMlist for all pots, split by volume
sla_stats_ls <- dlply(lma_lm, .(volume), LMlist_func)

# return coefs and CI(need to add a 'ID' name column)
ldply(sla_stats_ls, coef)
```

```
##    volume (Intercept) campaign
## 1       5       99.17  -3.5309
## 2       5      107.38  -4.5302
## 3       5      118.01  -5.7479
## 4       5      105.56  -0.6442
## 5       5      102.65  -0.5954
## 6       5      120.39  -6.8179
## 7       5      152.44 -15.3166
## 8      10      111.62  -5.6557
## 9      10       93.95  -1.3776
## 10     10      119.06  -6.2012
## 11     10       75.05   6.9425
## 12     10      128.03  -5.8311
## 13     10      130.42  -7.2258
## 14     10      128.23 -10.2242
## 15     15      133.95  -8.0343
## 16     15      138.29 -12.4766
## 17     15      149.29 -13.1013
## 18     15      143.97 -11.0564
## 19     15      177.69 -13.6774
## 20     15      185.71 -14.6081
## 21     15      111.52   3.9990
## 22     20       84.12  -0.9286
## 23     20      113.60  -8.0105
## 24     20      115.22  -5.8177
## 25     20      105.27   0.9500
## 26     20       86.99   3.8762
## 27     20      116.01  -5.4513
## 28     20      143.11  -8.4255
## 29     25       99.04  -3.6339
## 30     25      114.05  -8.2996
## 31     25      124.30  -5.4084
## 32     25      101.53  -3.2529
## 33     25      150.75 -11.4352
## 34     25      169.55 -10.2079
## 35     25      159.48 -14.5251
## 36     35      117.63  -7.7315
## 37     35      140.24  -5.7654
## 38     35      156.47 -15.2228
## 39     35      167.69 -12.9316
## 40     35      162.77 -14.1099
## 41     35      166.54 -13.6102
## 42     35      139.73  -5.4490
## 43   1000      143.25  -9.6944
## 44   1000      171.52 -10.8833
## 45   1000      185.67 -18.2927
## 46   1000      182.90 -15.1602
## 47   1000      198.08 -18.3682
## 48   1000      185.76 -13.2150
## 49   1000      193.44 -15.1646
```

```r
# plot confidence intervals
lapply(sla_stats_ls, function(x) plot(intervals(x)))
```

```
## $`5`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-28.png) 

```
## 
## $`10`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-29.png) 

```
## 
## $`15`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-210.png) 

```
## 
## $`20`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-211.png) 

```
## 
## $`25`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-212.png) 

```
## 
## $`35`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-213.png) 

```
## 
## $`1000`
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-214.png) 

```r

```


