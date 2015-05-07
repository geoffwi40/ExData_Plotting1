library(data.table)

##########################################################################
# Script to produce the third plot for Explatory Data Analysis Project 1
# Note: Uses the Data tables package to access the fread() function
#       This enables searching by a string for the start position to 
#       read a file
#########################################################################

#########################################
# Function to read in the data 
#########################################

Read_data<-function(start_date,end_date) {

  ## convert the dates to date types and find the number of days of data to read in
  ## then user this derive the number of rows based upon their being 1 row per minute 
  start<-as.Date(start_date)
  end<-as.Date(end_date)
  days<-end-start+1
  rows<-as.numeric(days)*24*60
  
  ## construct a date search string so the read can find the first line
  ## Note: The file doesn't have any leading zeros in dates, so strip these out
  start_date_chr<-as.character(start,format="%d/%m/%Y")
  start_date_chr<-sub("^0","",start_date_chr)
  start_date_chr<-sub("/0","/",start_date_chr)
  
  ## Firstly get the first row for the columns names
  col_data<-fread("household_power_consumption.txt",sep=";",header=TRUE,nrow=0)
  
  ## Now read in the file and set the column names
  data<-fread("household_power_consumption.txt",sep=";",header=FALSE,skip=start_date_chr,nrow=rows,na.strings="?")
  setnames(data,names(data),names(col_data))
    
  ## Convert the Date and time columns from a text to a R date format (combined)
  ## and add as a new column
  DateTime_chr<-paste(data$Date,data$Time)
  data[,DateTime:=as.POSIXct(strptime(DateTime_chr,format="%d/%m/%Y %H:%M:%S"))]
  
  return(data)
}


##################################
# Main script
##################################

data<-Read_data("2007-02-01","2007-02-02")
png("plot4.png", width=480,height=480)

par(mfrow=c(2,2))
# top left plot
plot(data$DateTime,data$Global_active_power,type="n",xlab="",ylab="Global Active Power (kilowatts)")
lines(data$DateTime,data$Global_active_power)

# top right plot
plot(data$DateTime,data$Voltage,type="n",xlab="datetime",ylab="Voltage")
lines(data$DateTime,data$Voltage)

# bottom left plot
plot(data$DateTime,data$Sub_metering_1,type="n",xlab="",ylab="Energy sub metering")
lines(data$DateTime,data$Sub_metering_1)
lines(data$DateTime,data$Sub_metering_2,col="red")
lines(data$DateTime,data$Sub_metering_3,col="blue")
legend("topright",col=c("black","red","blue"),lty=c(1,1),bty="n",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))


# bottom right plot
plot(data$DateTime,data$Global_reactive_power,type="n",xlab="datetime",ylab="Global_reactive_power")
lines(data$DateTime,data$Global_reactive_power)


dev.off()



