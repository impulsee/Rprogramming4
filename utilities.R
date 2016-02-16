## This file used for common functions used in this course project (e.g. load and prepare data)

## According to this topic http://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes-in-r
## - good way to load subset data is use sqldf, so i want to use it
if(!require("sqldf")){
    install.packages("sqldf")
}
library(sqldf)
## Change my current Time Local, to generate english names of dates in datetime
Sys.setlocale("LC_TIME", "C")


## This function loads the data from web. First it tries load with wget method, if error - loads without it.
downloadData<-function(url, destfile){    
    if(!file.exists(destfile)){
        ## first try wget, if no - normal method
        tryCatch(download.file(url, destfile=destfile, method = "wget") , error = function(e) e )
        e<-download.file(url, destfile=destfile)
    }
}

## This function tries to extract data from downloaded data if file doesn't exist
extractDownloadedData<-function(srcfile, dstfile){
    if (!file.exists(dstfile)){
        if (file.exists(srcfile)){
            unzip(srcfile, file = dstfile)
        }
        else{
            warning("No file to unzip! Use downloadData first");
        }
    }
}
##This function return data from files for two dates. 
##ToDo Extract Data from first date to lastdate, not for two dates
getdata<-function (url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                   dstfile = "exdata_cp1.zip",
                   dstunzipfile = "household_power_consumption.txt", 
                   DateFirst= "'1/2/2007'",
                   DateSecond = "'2/2/2007'"){
    downloadData(url,dstfile)
    extractDownloadedData(dstfile,dstunzipfile)
    sqlStatement<-paste("select * from file where Date=",DateFirst," or Date=",DateSecond,sep = "")
    data<-read.csv.sql(dstunzipfile,sql = sqlStatement, sep = ";")
    read.csv.sql
    dateTime <- strptime(dateTime <- paste(data$Date, data$Time, sep=' '), format="%d/%m/%Y %H:%M:%S")
    data$dateTime <- dateTime
    data
}

##This function returns data from global variable (if it exists), so I getting data only one when using 
##my plot functions in one session
getCachedData <- function() {
    if(!exists('mycacheddata')) {
        mycacheddata<<-getdata()
    }
    mycacheddata
}
