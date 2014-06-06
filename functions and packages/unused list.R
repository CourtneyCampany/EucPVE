volume <- unique(plotsumm$volume)

#function to add volume to met data for each campaign
met_volume<- function(vol,cam){
  merge(subset(eucpve_met,campaign==cam), subset(ps_param,campaign==cam&volume==vol))
}

lista <- list()
listb <- list()
for (i in 1:length(volume)){
  lista[[i]] <- met_volume(vol=volume[i],cam="a")
  lista[[i]] <- met_volume(vol=volume[i],cam="b") 
}

model_parameters <- rbind.fill(rbind.fill(testa),rbind.fill(testb))
