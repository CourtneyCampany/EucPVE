library(doBy)
library(sciplot)

#sun and shade leaf data from Licor and TDL

gmes <- read.csv("gmes_oct.csv")

#paste leaftype with partype
gmes$leaftype <- paste(gmes$leaf, gmes$par_type, sep = "-")

#means(three types: sun, shadelow, shadehigh)
gmes_agg <- summaryBy(. ~ leaftype + temp, FUN = c(mean,sd, length), keep.names=TRUE, data=gmes)

#standard errors
gmes_agg$photo_SE <- with(gmes_agg, photo.sd/sqrt(photo.length))
gmes_agg$gmes_SE <- with(gmes_agg, gmes.sd/sqrt(gmes.length))
gmes_agg$cond_SE <- with(gmes_agg, cond.sd/sqrt(cond.length))
gmes_agg$E_SE <- with(gmes_agg, E.sd/sqrt(E.length))
gmes_agg$par_SE <- with(gmes_agg, par.sd/sqrt(par.length))


windows(8,6)

#sun and shade leaves with high light
with(subset(gmes, leaftype !="shade-low"), bargraph.CI(temp, gmes, leaf,
                                          legend=TRUE,  args.legend = (x="topleft"),
                                          ylim = c(0,0.5),
                                          #ylab = expression(gmes~~(mol~m^-2~s^-1)), 
                                         ))
box()

#sun and shade leaves with natural light
with(subset(gmes, leaftype !="shade-high"), bargraph.CI(temp, gmes, leaf,
                                                      legend=TRUE,  args.legend = (x="topleft"),
                                                      ylim = c(0,0.5),
                                                      #ylab = expression(gmes~~(mol~m^-2~s^-1)), 
                                                      ))
box()


#gmes capacity with shade leaves 
with(subset(gmes, leaftype !="sun-high"), bargraph.CI(temp, gmes, par_type,
                                                        legend=TRUE,  args.legend = (x="topleft"),
                                                        ylim = c(0,0.4),
                                                        #ylab = expression(gmes~~(mol~m^-2~s^-1)), 
))
box()


#photosynthesis

#sun and shade leaves with natural light
with(subset(gmes, leaftype !="shade-high"), bargraph.CI(temp, photo, leaf,
                                                        legend=TRUE,  args.legend = (x="topleft"),
                                                        ylim = c(0,25),
                                                        #ylab = expression(gmes~~(mol~m^-2~s^-1)), 
))
box()




###paired t-test between leaf types by temperature treatment
##need a function that splits each leaf type by pairing group and runs a paired t-test


gmesstats <- function(pairings, leaf, par_type, temp, ...) {
  
  
  
  
  #t.test(sunelevated, sunambeint, paired=T)
  
}





