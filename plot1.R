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
  date.time=paste(as.character(df$date,"%m/%d/%Y"),as.character(df$time,"%H:%M"),sep=" ")
  dt=strptime(date.time, format="%d/%m/%Y %H:%M")
  df$date.time=dt
  
  names(df$date.time)="date.time"
  return(df)   
}

# Creates assignment plot 1 using a data.frame created by
# setup() function
# *********************************************************
createPlot1=function(df=as.data.frame(),saveFile=TRUE,...){

  #setup png
  if(saveFile==TRUE)
    png(filename="plot1.png")
  
  # plots the histogram
  hist(df$global.active.power,
       main="Global Active Power",col="red",
       xlab="Global Active Power(kilowatts)",
       ylab="Frequency",
       plot=TRUE,
       freq=TRUE,
       axes=TRUE,
       breaks=12)
       
  #completes action, saves plot
  if(saveFile==TRUE)
    dev.off()  
}
