# Data Expl Course Project 1 PLOT4 file
if( sum(installed.packages()[,"Package"]=="data.table") == 0 )
{
    install.packages("data.table")
}

readPowerConsumptionData <- function( fnm, dateList = NULL )
{
# Reads data from required data file into a data.table
# -- to read only specific dates from the file pass a list of dates (as character strings)
#    in as the argument dateList.  If dateList is NULL (default) reads entire file
#
# (This should be a stand-alone file sourced by this script, so that it isn't replicated
#  in each script in the project, but the project description wanted all work in one file)
# 
# The original data was sourced from the text file included in the following zip file:
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#
# This file was downloaded manually and stored in the source directory for this project as:
# household_power_consumption.txt
#
# The file has the following columns with fields separated by";"
# 
# Date:                  Date in format dd/mm/yyyy
# Time:                  time in format hh:mm:ss
# Global_active_power:   household global minute-averaged active power (in kilowatt)
# Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
# Voltage:               minute-averaged voltage (in volt)
# Global_intensity:      household global minute-averaged current intensity (in ampere)
# Sub_metering_1:        energy sub-metering No. 1 (in watt-hour of active energy). 
#                        It corresponds to the kitchen, containing mainly a dishwasher,
#                        an oven and a microwave (hot plates are not electric but gas 
#                        powered).
# Sub_metering_2:        energy sub-metering No. 2 (in watt-hour of active energy). 
#                        It corresponds to the laundry room, containing a washing-machine, 
#                        a tumble-drier, a refrigerator and a light.
# Sub_metering_3:        energy sub-metering No. 3 (in watt-hour of active energy). 
#                        It corresponds to an electric water-heater and an air-conditioner.
# 
    require("data.table")

    # if dateList is specified, a grep command finding the header, and the specified
    # dates is used as the file argument for fread, if not, just the filename is used
    srcFile <- ""
    if( length(dateList) != 0 )
    {
        srcFile <- "grep '"
        for( dt in dateList)
        {
            fldSep = if( srcFile=="grep '" ) { "^" } else { "\\|^" }
            srcFile <- paste( srcFile, fldSep, dt, sep="" )
        }
        srcFile <- paste( srcFile, "\\|^Date' ", fnm, sep="" )
    }
    else
    {
        srcFile <- fnm
    }

    # fread into a data.table as all character vectors (avoids conversion messages)
    # NA values in file are specified as "?" -> converted to R NA value on read
    inDT <- fread( srcFile, head=TRUE, sep=";", na.strings="?", 
                   colClasses=c("character","character","character","character",
                                "character","character","character","character",
                                "character") )
    # transform data.table elements to proper representation: Date for Date, POSIXct
    # date/time for Time and numeric for all the rest
    transform( inDT, 
               Date = as.Date(Date,format="%d/%m/%Y"), 
               Time = format(paste(Date,Time),format="%d/%m/%Y %H:%M:%S"),
               Global_active_power = as.numeric(Global_active_power),
               Global_reactive_power = as.numeric(Global_reactive_power),
               Voltage = as.numeric(Voltage),
               Global_intensity = as.numeric(Global_intensity),
               Sub_metering_1 = as.numeric(Sub_metering_1),
               Sub_metering_2 = as.numeric(Sub_metering_2),
               Sub_metering_3 = as.numeric(Sub_metering_3) )
}

dtList <- c( "1/2/2007", "2/2/2007" )
pcData <- readPowerConsumptionData( "./household_power_consumption.txt", dtList )

png(filename = "plot4.png", width = 480, height = 480)

par(mfrow=c(2,2))

with( pcData,
    {
        plot(Global_active_power,
             type="l", xaxt="n", 
             main="",
             xlab="",
             ylab="Global Active Power (kilowatts)" )
        
        lbl <- unique(Date) # get labels for days
        lblAt <- sapply( lbl, function(x) {min(which(Date==x))} ) # get at values for lables
        lbl <- unique( c( unique(Date), unique(Date)+1 ) ) # add next day
        lbl <- weekdays( lbl, abb=TRUE )
        lblAt <- c( lblAt, nrow(pcData)+1 ) # get at values for lables
        axis(1, lblAt, lbl )
        
        plot(Voltage,
             type="l", xaxt="n", 
             main="",
             xlab="datetime",
             ylab="Voltage" )
        axis(1, lblAt, lbl )
        
        plot(Sub_metering_1,
             type="l", xaxt="n", col="black",
             main="",
             xlab="",
             ylab="Energy sub metering")
        lines(Sub_metering_2, col="red")
        lines(Sub_metering_3, col="blue")
        axis(1, lblAt, lbl )
        
        axis(1, lblAt, lbl )
        legend("topright",col=c("black","red","blue"), 
               leg=names(pcData)[7:9], lty=par("lty"), bty="n")
        
        plot(Global_reactive_power,
             type="l", xaxt="n", 
             main="",
             xlab="datetime",
             ylab="Global_reactive_power" )
        
        axis(1, lblAt, lbl )
    })

dev.off()

print( "Created plot4.png" )
