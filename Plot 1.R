library(data.table)

##########################################################################
# Script to produce the first plot for Explatory Data Analysis Project 1
# Note: Uses the Data tables package
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
hist(data$Global_active_power,col="red",xlab="Global Active power (kilowatts)",main="Global Active Power")



