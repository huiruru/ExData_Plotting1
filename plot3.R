## Function takes zip file and outputs to png the plot specified in assignment
## Should you test, please place zip file into correct directory
## Note: your zip file and output file names may be different from default

produce_plot3 <- function(my_zip_file = "exdata-data-household_power_consumption.zip", output_file = "plot3.png")
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
  png(file = output_file, width = 480, height = 480)
  
  # Draws the line plot with proper x and y, labels, customize x axis, and legend
  with(my_plot_data,
       plot(
             my_plot_data$datetime
            ,my_plot_data$Sub_metering_1
            ,type = "l"
            ,col = "black"
            ,xlab = ""
            ,ylab = "Energy sub metering"
            ,xaxt = "n")
       )
  
      # Add sub_metering_2 data
      lines(
               my_plot_data$datetime
              ,my_plot_data$Sub_metering_2
              ,col = "red"
              ,type = "l"
           )
  
      # Add sub_metering_3 data
      lines(
               my_plot_data$datetime
              ,my_plot_data$Sub_metering_3
              ,col = "blue"
              ,type = "l"
           )
  
      # Customize x axis to proper dates
      axis(
             1
            ,at = c(min(my_plot_data$datetime), min(my_plot_data$datetime)+86400,min(my_plot_data$datetime)+2*86400)
            ,labels = c("Thu", "Fri", "Sat")
          )
    
      # Customize legend
      legend(
              x="topright"
             ,c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
             ,lty = c(1, 1, 1)
             ,lwd = c(2.5, 2.5, 2.5)
             ,col = c("black", "red", "blue")
            )
    
  #close graphics device
  dev.off()
  
}