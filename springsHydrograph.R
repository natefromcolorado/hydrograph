require(reshape2)

mainDir = "H:/Engineering_and_Hydro_Science/Projects/Groundwater_Resource_Assessment/End of Month Report Groundwater SJRWMD/2014 06/springs/"
springs.data = read.csv(paste(mainDir, "springs_data_2014.06.03rev1.csv", sep = ""), header = TRUE) ### all
start.date <- as.Date("01/01/1980",format = "%m/%d/%Y")  # Begin date for the analysis
end.date <- as.Date("12/31/1999",format = "%m/%d/%Y")  # End data for the analysis

wide.data = dcast(springs.data, date~spring, value.var = "discharge")
# wide.data$date = as.Date(wide.data$date, format = "%Y-%m-%d") # create data for monthYear
wide.data$date = as.Date(wide.data$date, format = "%m/%d/%Y") # create data for monthYear
wide.data = wide.data[order(wide.data$date),] # order the dataframe
wide.data = wide.data[which((wide.data$date >= start.date) & 
                              (wide.data$date <= end.date)),] # period of record 

for(w in 2:ncol(wide.data)){
  png(file =paste(mainDir, "hydrograph/",colnames(wide.data[w]), " hydrograph.png",sep = ""))
plot.date = strptime(wide.data$date, format = "%Y-%m-%d")
  plot(plot.date, wide.data[,w],
     main = colnames(wide.data[w]),
     xlab = "Date",
     ylab = "Discharge (cfs)",
     cex = 0.5,
     )
abline(v=axis.POSIXct(1, x=pretty(plot.date)),col = "lightgray", lty = "dotted", lwd = par("lwd"))
abline(h=pretty(wide.data[,w]),col = "lightgray", lty = "dotted", lwd = par("lwd"))
dev.off()
}