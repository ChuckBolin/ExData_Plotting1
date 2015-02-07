# Reads the datafile rows, loads into a data.frame, cleans
# up header names, and convertes global.active.power from
# factor to numeric. RETURNS data.frame for two days
# *********************************************************
setup=function(){
  tab500kRows=read.table("data/household_power_consumption.txt", header=TRUE,nrows=500000,sep=";")
  df=tab500kRows[66637:69516,] #2.1.2007-2.2.2007 (2 days of 1 minute readings)
  names(df)=tolower(colnames(df))
  names(df)=gsub("_",".",colnames(df))
  df$global.active.power=as.numeric(as.character(df$global.active.power))
  
  return(df)  
}

# Creates assignment plot 2 using a data.frame created by
# setup() function
# *********************************************************
createPlot2=function(df=as.data.frame(),...){

  #setup png
  png(filename="plot2.png")
  
  # plots the data
  dateRange=as.Date(df$date,"%d/%m/%Y")
  
  plot(df$global.active.power, 
       type="l", 
       ylab="Global Active Power(kilowatts)",
       xlab="Index",       
       xaxt="n"
       )
  
  axis(1, at=dateRange, labels=weekdays(dateRange,abbreviate=TRUE))
  
  #completes action, saves plot
  dev.off()  
}
