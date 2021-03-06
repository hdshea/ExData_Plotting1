# Data Expl Course Project 1 PLOT3 file
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
    #  in each script in the project, but the project description wanted all work in one file.
    #  Nonetheless, I have saved it as a separate file as well - readPowerConsumptionData.R -
    #  where there is substantially more documentation.)
    #
    require("data.table")
    
    # if dateList is specified, a grep command finding the header, and the specified
    # dates is used as the file argument for fread, if not, just the filename is used
    # (This may be a bit machine dependent.)
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

#
#  script for plotting to plot3.png
#
dtList <- c( "1/2/2007", "2/2/2007" )
pcData <- readPowerConsumptionData( "./household_power_consumption.txt", dtList )

png(filename = "plot3.png", width = 480, height = 480)

# x label information
lbl <- unique(pcData$Date) # get labels for days
lblAt <- sapply( lbl, function(x) {min(which(pcData$Date==x))} ) # get at values for lables
lbl <- c( lbl, max(lbl)+1 ) # add next day
lbl <- weekdays( lbl, abb=TRUE ) # change from dates to day abbreviations
lblAt <- c( lblAt, nrow(pcData)+1 ) # get at values for lables

# plotting
plot(pcData$Sub_metering_1,
     type="l", xaxt="n", col="black",
     main="",
     xlab="",
     ylab="Energy sub metering")
lines(pcData$Sub_metering_2, col="red")
lines(pcData$Sub_metering_3, col="blue")
axis(1, lblAt, lbl )
legend("topright",col=c("black","red","blue"), leg=names(pcData)[7:9], lty=par("lty"))

dev.off()

print( "Created plot3.png" )
