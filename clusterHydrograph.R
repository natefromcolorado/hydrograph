#REQUIRED PACKAGES
library("wq")  #FOR FUNCTION year()
library("timeDate") #FOR FUNCTION timeDate()

# BEGIN USER INPUT
file.path = "G:/Statistics/R/mydata/clusterWellData/"
file.name = "Cluster_for_R_All_2014.09.16.csv"
scripts.path.name <- "G:/Statistics/R/Scripts/"
destination.path = "G:/Statistics/R/Output/clusterHydroGraph/Cluster_All_POR/"

## INPUT FILE SETUP IS STACKED WITH COLUMNS FOR EACH VARIABLE

alldata <- read.csv(paste(file.path, file.name, sep=""), header = TRUE)
start.date <- timeDate("01/01/1900")  # The script compares data after this date
end.date <- timeDate("01/01/2035")  # The script compares data before this date
  # mode defines the aggregation time-period: "D=Daily" "M=Month" "S=Season "Y=Yearly"
  # "D" may not be appropriate where the sampling frequency changesmode <- "M"
type.data = "Water Level (ft, NAVD88)"
column.label <- "Cluster"
well.label = "Station"
tvar.label <- "date"      									# Column header in spreadsheet file for date
y.variable.label <- "Level"									# Column header in spreadsheet file for value to be analyzed
exclude.stations <- c("")    #  Stations to exclude from the analysis.  
  #  Stations may be excluded if they do not have enough seasonal data to
script.name <- "clusterHydroGraph"
scripts.path <- "G:/Statistics/R/Scripts/"


cluster.list = unique(alldata$Cluster)
w=8
#     LOOP THROUGH CLUSTERS
for (w in 1:length(cluster.list))
   {
  cluster = cluster.list[w]
  cluster.data = subset(alldata, alldata$Cluster == cluster)
  y.low = min(cluster.data$Level)
  y.high = max(cluster.data$Level)+((max(cluster.data$Level)-y.low)/5)
    ttmp.cluster.date = cluster.data$date
    ttmp.cluster.date = as.Date(ttmp.cluster.date, format="%m/%d/%Y")
#     ttmp.cluster = timeDate(ttmp.cluster.date)
#     cluster.data$date <- as.POSIXct(strptime(ttmp.cluster, format = "%m/%d/%Y", "GMT"))  
  x.low  = min(ttmp.cluster.date) #For plotting, the minimum time
  x.high = max(ttmp.cluster.date) #For plotting, the maximum time
  x.range = x.high - x.low #For setting thetime grid using abline
    station.list = unique(as.character(cluster.data$Station)) # all the stations in the cluster
    casing.depth = unique(cluster.data$Casing_Depth) 
    constructed.depth = unique(cluster.data$Constructed_Depth) 
    station.id = unique(as.character(cluster.data$Station_ID))
    aquifer.vec = as.character()
    lse.vec = as.character()

#### Loop through individual stations
 sta = 1
  for (sta in 1:length(station.list))
    {
    station = station.list[sta]
    station.data = subset(cluster.data, cluster.data$Station == station) 
    aquifer = unique(as.character(station.data$Aquifer))
    aquifer.vec = c(aquifer.vec, aquifer)
    lse = unique(station.data$LSE)
    lse.vec = c(lse.vec, lse)
    t.0 = station.data$date # time variable
    y.0 = station.data$Level # y variable 
    data = data.frame(t.0,y.0) # make a dataframe
    names(data)[1:2] <- c(tvar.label,y.variable.label) # create lables for the dataframe
    
    # the following options are available to handle NAs
    data <- na.omit(data)  			# omit NAs
    #data <- na.gam.replace(data)		# replace NAs with mean of available data
    #data <- na.exclude(data)			# same as na.omit
    #data <- na.fail(data)				# produces an error if missing values are present
    
    eval(parse(text=paste("ytmp.0=data$", y.variable.label, sep="")))
    eval(parse(text=paste("ttmp.0=data$", tvar.label, sep="")))
    
    ttmp.0 = data[,1]
    ttmp.0 = as.Date(ttmp.0, format="%m/%d/%Y")
    ttmp.0 = timeDate(ttmp.0)
    data$date <- as.POSIXct(strptime(data$date, format = "%m/%d/%Y", "GMT")) #create the time series for x-axis
    ytmp <- ytmp.0[ttmp.0 >= start.date & ttmp.0 < end.date] # remove data for the period of interest
    ttmp <- ttmp.0[ttmp.0 >= start.date & ttmp.0<=end.date] # remove data for the period of interest
    
    if(sta == 1)
    {
      png(paste(file = destination.path, cluster, #jpeg destination 
                 "-",min(years(ttmp)),
                 "-", max(years(ttmp)),
                 ".png", sep =""), 
            width = 1000, height = 700, res=100)  
      
      plot(ttmp,ytmp, type = "l",
           ylab = type.data, 
           main = paste("Well Cluster: ", cluster, sep = ""), 
           xlab = "Date", 
           ylim = c(y.low, y.high), 
           xlim = c(as.POSIXct(x.low),as.POSIXct(x.high)), # for xlim the date needs to be as a POSIXct
           lty = sta, col = sta)
      
#     ANNUAL GRID LIGHT GRAY # The abline function is the best way to create a grid
      if (x.range < 7000){
      abline(v = c(as.POSIXct(
        seq(as.Date("1900-01-01"),as.Date("2015-01-01"), by = 365.25), "GMT")
                   ), 
             col = "lightgray", lty = "dotted")
      }else{
        abline(v = c(as.POSIXct(
        seq(as.Date("1900-01-01"),as.Date("2015-01-01"), by = 1826.25), "GMT")
                   ), 
             col = "lightgray", lty = "dotted")
      }
    }else{
      lines(ttmp,ytmp, lty = sta, col = sta)
    }
  }
# Legend in upper right with all the well characteristics  
  color = c("blk", "red", "grn", "blu","sky") #color for identification in the legend from the default par()
  leg.text = c(paste(
                      "(",color[seq(length(station.list))],") ", 
                      station.list[seq(length(station.list))], 
                      " | Aquifer: ", aquifer.vec[seq(length(station.list))],
                      " | Station_ID: ", station.id[seq(length(station.list))],
                      " | Casing Depth (ft): ", casing.depth[seq(length(station.list))],
                      " | Constructed Depth (ft): ", constructed.depth[seq(length(station.list))], 
                      " | LSE (ft): ", lse.vec[seq(length(station.list))], 
                      sep = ""
                    )
               )
  legend(
    "topright", 
    leg.text, merge = FALSE, 
    lty=c(seq(length(station.list))), 
    cex = c(rep(0.75, length(station.list))),
    col=c(seq(length(station.list))), 
    border=FALSE, 
    xjust = 0)
  dev.off()
}

