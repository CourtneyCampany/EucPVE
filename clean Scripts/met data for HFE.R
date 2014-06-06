source("functions and packages/load model packages.R")

setToken("fgCPRjG5Aoy9Dnx9pnpv")

#I need to download met data across the entire pot experiment

#Search HIEv for the ROS weather station data during the pot experiment
ros_search <- searchHIEv(filename="ROS_WS", startDate="2013-01-01", endDate="2013-06-01")

#search for which one of the files above has all the data I need
ros_ex15 <-downloadTOA5(filename="ROS_WS_Table15min_20130531.dat")
ros_ex5 <-downloadTOA5(filename="ROS_WS_Table05min_20130430.dat")

with(ros_ex15, plot(DateTime, PTemp_C_Avg, type = "l"))
with(ros_ex5, plot(DateTime, RH , type = "l"))

#so i want the 5 minute data for the duration of the experiment
ros_ws_exp <-  downloadTOA5(filename="ROS_WS_Table05min", startDate="2013-01-01", endDate="2013-06-01")

#test with a graph and summary to see if the date range is covered
with(ros_ws_exp, plot(DateTime, RH, type = "l"))
summary(row_ws_exp)

#need to aggregate to a 15 minute time step (within HIEv package)
# First add the nearest 315min timestep.
ros_ws_exp$DateTime15 <- nearestTimeStep(ros_ws_exp$DateTime, 15, "ceiling")

# Then aggregate all numeric variables using summaryBy

metdata_15 <- summaryBy(. ~ DateTime15, data=ros_ws_exp, FUN=mean, na.rm=TRUE)

eucpve_met <- metdata_15[,1:5]

write.csv(eucpve_met, "calculated data/eucpve_met.csv", row.names=FALSE)
