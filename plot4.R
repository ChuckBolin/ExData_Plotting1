# Reads the datafile rows, loads into a data.frame, cleans
# up header names, and converts global.active.power from
# factor to numeric. RETURNS data.frame for two days
# *********************************************************
setup=function(){
  # read 500,000 rows and select specific row range 
  tab500kRows=read.table("data/household_power_consumption.txt", header=TRUE,nrows=500000,sep=";")
  df=tab500kRows[66637:69516,] #2.1.2007-2.2.2007 (2 days of 1 minute readings)
  
  # remove _ and force to lower case  
  names(df)=tolower(colnames(df))
  names(df)=gsub("_",".",colnames(df))
  
  # convert factors
  df$global.active.power=as.numeric(as.character(df$global.active.power))
  df$global.reactive.power=as.numeric(as.character(df$global.reactive.power))
  df$voltage=as.numeric(as.character(df$voltage))
  df$sub.metering.1=as.numeric(df$sub.metering.1)
  df$sub.metering.2=as.numeric(df$sub.metering.2)
  df$sub.metering.3=as.numeric(df$sub.metering.3)
  
  # convert time factor
  date.time=paste(as.character(df$date,"%m/%d/%Y"),as.character(df$time,"%H:%M"),sep=" ")
  dt=strptime(date.time, format="%d/%m/%Y %H:%M")
  
  # add another column and name
  df$date.time=dt
  names(df$date.time)="date.time"
  
  return(df)  
}

# Creates assignment plot 4 using a data.frame created by
# setup() function
# *********************************************************
createPlot4=function(df=as.data.frame(),saveFile=TRUE,...){

  #setup png
  if(saveFile==TRUE)
    png(filename="plot4.png")
  
  # create 2x2 frame to hold four plots
  par(mfrow=c(2,2))
  par(mar=c(4,4,1,1))
  
  # plots the top-left data chart
  #****************************************************  TOP-LEFT
  plot(df$date.time, df$global.active.power,
       type="l", xlab="",
       ylab="Global Active Power(kilowatts)"
       )
  box(which="plot", lty="solid", col="black")

  # plots the top-right data chart
  #****************************************************  TOP-RIGHT
  plot(df$date.time, df$voltage,
       type="l", xlab="datetime",
       ylab="Voltage",
       )
  box(which="plot", lty="solid", col="black")
  
  # plots the bottom-left data chart
  #****************************************************  BOTTOM-LEFT
  plot(df$date.time, df$sub.metering.1 - 2,
       type="l", xlab="",
       ylab="Global Active Power(kilowatts)",        
       axes=TRUE
       )

  # plots the second series
  lines(df$date.time, df$sub.metering.2 - 2, col="red")
  
  # plots the third series
  lines(df$date.time, df$sub.metering.3, col="blue")

  # adds legend to the top-right
  legend("topright", 
         c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         col=c("black","red","blue"),
         lwd = .75,
         cex = .75)
           
  # adds border around plot       
  box(which="plot", lty="solid", col="black")

  # plots the bottom-right data chart
  #****************************************************    BOTTOM-RIGHT
  plot(df$date.time, df$global.reactive.power,
       type="l", xlab="datetime",
       ylab="Global_reactive_power",
       ylim=c(0.0,0.5),       
       axes=TRUE
       )
  #axis(2,at=c(0.0,0.1,0.2,0.3,0.4,0.5))       
  box(which="plot", lty="solid", col="black")       

  
  #completes action, saves plot
  if(saveFile==TRUE)
    dev.off()  
}
