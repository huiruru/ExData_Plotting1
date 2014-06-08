## Function takes zip file and outputs to png the 4 plots as specified in assignment
## Should you test, please place zip file into correct directory
## Note: your zip file and output file names may be different from default

produce_plot4 <- function(my_zip_file = "exdata-data-household_power_consumption.zip", output_file = "plot4.png")
{   
  # Assuming only one file in zip file, unzip and set the file name into variable
  raw_file <- unzip(my_zip_file, list=TRUE)$Name
  
  # Set up connection
  read_connection <- unz(my_zip_file, raw_file)
  
  # Temp text file to read in only data neccessary for plots
  temp_file <- "temp_file.txt"
  
  # Only reads lines from connection for data between feb 1 and feb 2 of 2007
  # from connection and deposit into temp text file
  cat(grep("(^Date)|(^[1|2]/2/2007)", readLines(read_connection), value=TRUE), sep="\n", file=temp_file)
  
  # Close connection after reading
  close(read_connection)
  
  # Fast reads temp text file into data table named my_plot_data
  my_plot_data <- fread(temp_file, sep=";", na.strings="?")
  
  # Adds new column with the correctly formatted date time from the date and time columns
  my_plot_data[,"datetime"] <- as.POSIXct(strptime(paste(my_plot_data$Date, my_plot_data$Time), "%d/%m/%Y %H:%M:%S"))
  
  # Set up png file for output
  png(file = output_file)
  
  # Set up parameters for the 4 base plots
  par(mfcol= c(2, 2), mfrow = c(2, 2))
  
  # Draw plots
  with(my_plot_data,
       {
          #plot1
           plot(
                 my_plot_data$datetime
                ,my_plot_data$Global_active_power
                ,type = "l"
                ,xlab = ""
                ,ylab = "Global Active Power"
                ,xaxt = "n"
               )
           axis(
                 1
                ,at=c(min(my_plot_data$datetime),min(my_plot_data$datetime)+86400,min(my_plot_data$datetime)+2*86400)
                ,labels = c("Thu", "Fri", "Sat")
               )
          
          #plot2
           plot(
                 my_plot_data$datetime
                ,my_plot_data$Voltage
                ,type = "l"
                ,xlab = "datetime"
                ,ylab = "Voltage"
                ,xaxt = "n"
               )
            axis(
                  1
                 ,at=c(min(my_plot_data$datetime),min(my_plot_data$datetime)+86400,min(my_plot_data$datetime)+2*86400)
                 ,labels = c("Thu", "Fri", "Sat")
                )
  
          #plot3
           plot(
                  my_plot_data$datetime
                 ,my_plot_data$Sub_metering_1
                 ,type = "l"
                 ,col = "black"
                 ,xlab = ""
                 ,ylab = "Energy sub metering"
                 ,xaxt = "n"
                )
              #add sub_metering_2 data
              lines(
                     my_plot_data$datetime
                    ,my_plot_data$Sub_metering_2
                    ,col = "red"
                    ,type = "l"
                   )
              #add sub_metering_3 data
              lines(
                     my_plot_data$datetime
                    ,my_plot_data$Sub_metering_3
                    ,col = "blue"
                    ,type = "l"
                   )
          #customize x axis to proper dates
          axis(
                 1
                ,at = c(min(my_plot_data$datetime), min(my_plot_data$datetime)+86400,min(my_plot_data$datetime)+2*86400)
                ,labels = c("Thu", "Fri", "Sat")
              )
          #customize legend
          legend(
                   x="topright"
                  ,bty = "n"
                  ,c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
                  ,lty = c(1, 1, 1)
                  ,lwd = c(2.5, 2.5, 2.5)
                  ,col = c("black", "red", "blue")
                )
  
           #plot4
           plot(
                  my_plot_data$datetime
                 ,my_plot_data$Global_reactive_power
                 ,type = "l"
                 ,xlab = "datetime"
                 ,ylab = "Global_reactive_power"
                 ,xaxt = "n"
               )
           axis(
                  1
                 ,at = c(min(my_plot_data$datetime)
                 ,min(my_plot_data$datetime)+86400,min(my_plot_data$datetime)+2*86400)
                 ,labels = c("Thu", "Fri", "Sat")
              )
      }
  )
  
  #close graphics device
  dev.off()
  
}