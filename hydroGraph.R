# BEGIN USER INPUT

library("zoo")
library("timeSeries")
library("wq")
library("Kendall")
library("tseries")
library("boot")
library("epicalc")

file.path = "G:/Statistics/R/mydata/Groundwater/"
file.name = "District_Merged_UFA_stations_QAed_Stack.csv"
scripts.path.name <- "G:/Statistics/R/Scripts/"
destination.path = "G:/Statistics/R/Output/hydrograph/Annual/"

alldata <- read.csv(paste(file.path, file.name, sep=""), header = TRUE)
start.date <- timeDate("01/01/1900")  # The script compares data after this date
end.date <- timeDate("01/01/2035")  # The script compares data before this date
  # mode defines the aggregation time-period: "D=Daily" "M=Month" "S=Season "Y=Yearly"
  # "D" may not be appropriate where the sampling frequency changesmode <- "M"
mode = "Y"
type.data = "Level (ft, NAVD88)"
column.label <- "SN"
tvar.label <- "date"      									# Column header in spreadsheet file for date
y.variable.label <- "Level"									# Column header in spreadsheet file for value to be analyzed
exclude.stations <- c("")    #  Stations to exclude from the analysis.  
  #  Stations may be excluded if they do not have enough seasonal data to
script.name <- "breakPoint"
script.version <- "Version: USGS.Splu.R.VER1"
scripts.path <- "G:/Statistics/R/Scripts/"
  
eval(parse(text=paste("station.list <- unique(alldata$", column.label, ")", sep=""))) 
if (exclude.stations[1] != "") {
  for (c in 1:length(exclude.stations)) {
    station.list <- station.list[station.list != exclude.stations[c]]
  }
}

station.list.char = as.character(station.list)
data.0 <- alldata
dataset.label <- column.label
#w = 5                               # When debugging run the scipt for one station (Add } to end of script) 
for (w in 1:length(station.list.char)) {
  subset.label <- station.list.char[w]
  file.name <- paste(file.name,as.character(station.list[w]))  # for chart labels
  station.list.char = as.character(station.list)
  dataset.label <- column.label
  
  # SEASON DEFINITION 
  # seasons <- as.numeric(c(1,1,1,2,2,2,3,3,3,4,4,1))  # Used if mode is "S".  Can be changed to alter how months are grouped into seasons
seasons <- as.numeric(c(1,1,1,1,1,2,2,2,2,1,1,1))

  # FUNCTIONS
source(paste(scripts.path,"Functions/fac2num.R",sep="")) #call function
  
  # SCRIPT START
source(paste(scripts.path,"defineData.R",sep="")) #call defineData.R script
  
  # select data for the desired time window
  ytmp <- ytmp.0[ttmp.0>=start.date & ttmp.0<=end.date]
  ttmp <- ttmp.0[ttmp.0>=start.date & ttmp.0<=end.date]
  ytmp <- fac2num(ytmp)
  
  if(length(ytmp) < 5) # too few data points
  {
    exit()
  }
source(paste(scripts.path,"aggregateData.R",sep="")) #call aggregateData.R script

png(paste(file = destination.path,subset.label,"-",min(years(ttmp)),"-", max(years(ttmp)),".png", sep =""), width = 1000, height = 700, res=100)  
  plot(t,y, type = "p",ylab = type.data, main = subset.label, xlab = "Date")
  lines(t,y)
  dev.off()
}

#########################################################################
